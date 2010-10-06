from numpy import *
from scipy.ndimage.filters import convolve1d
from collections import deque
from mputils import shmem_copy, shmem_empty_like, shmem_as_ndarray
import multiprocessing as mp

# Approximates the derivative of the image.
#
# The derivative order along rows is determined by row_order, and
# along columns by col_order.  If the dest argument is specified, it
# determines the destination array for the results of the derivative.
def derivative(image, row_order, col_order, dest=None):
    def deriv_filter(order):
        K = array([0.5, 0, -0.5], image.dtype);
        f = array([1], image.dtype);
        for i in range(order):
            f = convolve(f, K)
        return f

    # Create an output array if necessary
    if dest == None:
        result = empty_like(image)
    else:
        result = dest

    # Carry out convolutions
    row_input = image
    if (col_order > 0):
        convolve1d(image, deriv_filter(col_order),
                   output=result, axis=1, mode='constant', cval=0)
        row_input = result
    if (row_order > 0):
        convolve1d(row_input, deriv_filter(row_order),
                   output=result, axis=0, mode='constant', cval=0)

    # If no convolutions perform, copy in image
    if row_order == 0 and col_order == 0:
        result[:] = image

    return result

# Calculate ridge metrics for a region of the image
#
# Worker function for creating the metrics Dq and Dqq.  Note that the
# Dq and Dqq arguments are modified by this function.  The start_row
# and num_rows arguments determine which region of the image this
# function works on.
def _make_metrics(Dx, Dy,
                  Dxx, Dyy, Dxy, Dq, Dqq,
                  start_row=0, num_rows=None):

    A = zeros((2,2), dtype=Dx.dtype)
    if num_rows == None:
        num_rows = Dx.shape[0]
    for row,col in ndindex((num_rows, Dx.shape[1])):
        k = (row+start_row, col)
        A[0,0] = Dxx[k]
        A[1,0] = Dxy[k]
        A[0,1] = Dxy[k]
        A[1,1] = Dyy[k]
        w,v = linalg.eigh(A)

        if abs(w[0]) > abs(w[1]):
            dqq = w[0]
            dq = v[0,0]*Dx[k] + v[1,0]*Dy[k]
        else:
            dqq = w[1]
            dq = v[0,1]*Dx[k] + v[1,1]*Dy[k]

        Dq[k] = dq
        Dqq[k] = dqq

# Multiprocess wrapper function for _make_metrics().
def _mp_make_metrics(rows, cols,
                     Dx, Dy,
                     Dxx, Dyy, Dxy, Dq, Dqq,
                     start_row, num_rows):
    shape = (rows,cols)
    _make_metrics(shmem_as_ndarray(Dx).reshape(shape),
                  shmem_as_ndarray(Dy).reshape(shape),
                  shmem_as_ndarray(Dxx).reshape(shape),
                  shmem_as_ndarray(Dyy).reshape(shape),
                  shmem_as_ndarray(Dxy).reshape(shape),
                  shmem_as_ndarray(Dq).reshape(shape),
                  shmem_as_ndarray(Dqq).reshape(shape),
                  start_row, num_rows)

class RidgePointInfo:
    def __init__(self, Dqq, Dxx, Dyy, Dxy, value, row, col):
        self.row = row
        self.col = col
        self.Dqq = Dqq
        self.Dxx = Dxx
        self.Dyy = Dyy
        self.Dxy = Dxy
        self.value = value
class RidgeSegmentInfo:
    def __init__(self, start, end):
        self.start = start
        self.end = end
    def reverse(self):
        p = self.end
        self.end = self.start
        self.start = p
        return self

class RidgeExtraction:
    # Sets up ridge extraction for image. If threads argument is
    # specified, it determines the number of parallel processes used
    # in calculations.
    def __init__(self, image, threads=None):
        # Set up shared memory matrices. We have to keep the
        # references to the multiprocessing.RawArray around, or things
        # don't work properly. :-(
        self.image, self.image_raw = shmem_copy(image)
        self.Dx, self.Dx_raw = shmem_empty_like(image)
        self.Dy, self.Dy_raw = shmem_empty_like(image)
        self.Dxx, self.Dxx_raw = shmem_empty_like(image)
        self.Dyy, self.Dyy_raw = shmem_empty_like(image)
        self.Dxy, self.Dxy_raw = shmem_empty_like(image)

        # Calculate directional derivatives.
        derivative(self.image, 1, 0, dest=self.Dx)
        derivative(self.image, 0, 1, dest=self.Dy)
        derivative(self.Dx, 1, 0, dest=self.Dxx)
        derivative(self.Dy, 0, 1, dest=self.Dyy)
        derivative(self.Dx, 0, 1, dest=self.Dxy)

        # Calculate metrics
        self.Dq, self.Dq_raw = shmem_empty_like(image)
        self.Dqq, self.Dqq_raw = shmem_empty_like(image)

        if threads == None:
            _make_metrics(self.Dx, self.Dy, self.Dxx, self.Dyy, self.Dxy,
                         self.Dq, self.Dqq)
        else:
            # Multiprocessor magic happens here
            n_rows = self.image.shape[0]
            n_cols = self.image.shape[1]
            def create_process(n):
                start = (n * n_rows) / threads
                end = ((n+1) * n_rows) / threads
                length = end - start
                mp_args = (n_rows, n_cols,
                           self.Dx_raw, self.Dy_raw, self.Dxx_raw,
                           self.Dyy_raw, self.Dxy_raw, self.Dq_raw,
                           self.Dqq_raw, start, length)
                mp_fun = _mp_make_metrics
                return mp.Process(target=mp_fun, args=mp_args)

            pool = [create_process(n) for n in range(threads)]
            for p in pool: p.start()
            for p in pool: p.join()

        # Don't need to keep these around any more
        del self.Dx
        del self.Dx_raw
        del self.Dy
        del self.Dy_raw

    # Linear interpolation
    def itp(self, delta, a, b):
        result = (1-delta)*a + delta*b
        return result

    # Check whether the edge joining start and end contains a ridge
    # point
    #
    # Worker function used by ridge_points() and ridge_segments().
    def ridge_on_edge(self,start, end):
        i = start
        j = end
        dqi = self.Dq[i]
        dqj = self.Dq[j]
        if dqi * dqj >= 0:
            return None

        # Interpolate zero-crossing of Dq
        dqqi = self.Dqq[i]
        dqqj = self.Dqq[j]

        x = dqi / (dqi - dqj)

        dqq = self.itp(x,dqqi,dqqj)

        if dqq > 0:
            return None

        # Ridge found!
        return RidgePointInfo(dqq,
                              self.itp(x, self.Dxx[i], self.Dxx[j]),
                              self.itp(x, self.Dyy[i], self.Dyy[j]),
                              self.itp(x, self.Dxy[i], self.Dxy[j]),
                              self.itp(x, self.image[i], self.image[j]),
                              self.itp(x, i[0], j[0]),
                              self.itp(x, i[1], j[1]))

    # Find ridge points
    #
    # Tests each row-wise and column-wise edge joining two adjacent
    # pixels to check if it contains a zero of Lq with Lqq < 0.
    def ridge_points(self):
        for k in ndindex(self.image.shape[0] - 1, self.image.shape[1] - 1):
            r = self.ridge_on_edge (k, (k[0]+1, k[1]))
            if (r != None):
                yield r
            r = self.ridge_on_edge (k, (k[0],   k[1]+1))
            if (r != None):
                yield r

    # Find ridge segments (cached version)
    #
    # Finds ridge segments by considering the squares formed by four
    # adjacent pixels, and testing for zeros of Lq along the edges of
    # each square.  If exactly two zeros are found, and they are
    # associated with Lqq < 0, then a ridge segment is detected as
    # joining them.
    #
    # The zeros along the previous row and column of pixels are
    # cached, so that each zero of Lq is calculated only once. This
    # speeds up the algorithm.
    def ridge_segments_cached(self):
        # Set up cache for ridge points in previous row/column
        last_row = [self.ridge_on_edge((0,x),(0,x+1))
                           for x in range(self.image.shape[1]-1)]

        for row in range(self.image.shape[0]-1):
            last_col = self.ridge_on_edge((row,0),(row+1,0))
            for col in range(self.image.shape[1]-1):
                # Check for zeros of Dq along each edge of each square
                # formed from four adjacent pixels.
                edge_points = []

                # First check whether there are cached ridge points for
                # the top and left edges of the current square
                if last_row[col] != None:
                    edge_points.append(last_row[col])
                if last_col != None:
                    edge_points.append(last_col)

                # Now check the right and bottom edges of the square, and
                # cache them
                next_col = self.ridge_on_edge((row,col+1),(row+1,col+1))
                if next_col != None:
                    edge_points.append(next_col)
                last_col = next_col

                next_row = self.ridge_on_edge((row+1,col),(row+1,col+1))
                if next_row != None:
                    edge_points.append(next_row)
                last_row[col] = next_row

                # If exactly two ridge points are found on the edges of
                # this square, we add a ridge segment.
                if len(edge_points) != 2:
                    continue

                yield RidgeSegmentInfo(*edge_points)

    # Find ridge segments (uncached version)
    #
    # Finds ridge segments by considering the squares formed by four
    # adjacent pixels, and testing for zeros of Lq along the edges of
    # each square.  If exactly two zeros are found, and they are
    # associated with Lqq < 0, then a ridge segment is detected as
    # joining them.
    #
    # This version of the algorithm is only used for verifying the
    # behaviour ridge_segments_cached().
    def ridge_segments_uncached(self):
        EDGE_INFO = (((0,0),(0,1)),
                     ((0,0),(1,0)),
                     ((0,1),(1,1)),
                     ((1,0),(1,1)))

        for k in ndindex(self.image.shape[0] - 1, self.image.shape[1] - 1):
            edge_points = []
            for edge in EDGE_INFO:
                i = (k[0] + edge[0][0], k[1] + edge[0][1])
                j = (k[0] + edge[1][0], k[1] + edge[1][1])
                r = self.ridge_on_edge(i,j)
                if r != None:
                    edge_points.append(r)
                    if len(edge_points) > 2:
                        break

            if len(edge_points) == 2:
                yield RidgeSegmentInfo(*edge_points)

    # By default, use cached version of ridge segment extraction
    # algorithm.
    ridge_segments = ridge_segments_cached

