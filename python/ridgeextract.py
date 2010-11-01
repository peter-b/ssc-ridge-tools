from numpy import *
from scipy.ndimage.filters import convolve1d
from collections import deque

def derivative(image, row_order, col_order, dest=None):
    def deriv_filter(order):
        K = array([0.5, 0, -0.5], image.dtype);
        f = array([1], image.dtype);
        for i in range(order):
            f = convolve(f, K)
        return f

    if dest == None:
        dest = empty_like(image)

    row_input = image
    result = None
    if (col_order > 0):
        convolve1d(image, deriv_filter(col_order),
                   output=dest, axis=1, mode='constant', cval=0)
        row_input = dest
        result = dest
    if (row_order > 0):
        convolve1d(row_input, deriv_filter(row_order),
                   output=dest, axis=0, mode='constant', cval=0)
        result = dest

    if result == None: # No filtering, just copy over data
        dest[:,:] = image

    return result

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

    def __init__(self, image):
        # Calculate directional derivatives
        Dx = derivative(image, 1, 0)
        Dy = derivative(image, 0, 1)
        Dxx = derivative(Dx, 1, 0)
        Dyy = derivative(Dy, 0, 1)
        Dxy = derivative(Dx, 0, 1)

        # Calculate metrics

        Dq = empty_like (image)
        Dqq = empty_like (image)

        for k in ndindex(*image.shape):
            A = array([[Dxx[k], Dxy[k]], [Dxy[k], Dyy[k]]])
            w,v = linalg.eigh(A)

            if abs(w[0]) > abs(w[1]):
                dqq = w[0]
                dq = v[0,0]*Dx[k] + v[1,0]*Dy[k]
            else:
                dqq = w[1]
                dq = v[0,1]*Dx[k] + v[1,1]*Dy[k]

            Dq[k] = dq
            Dqq[k] = dqq

        self.image = image
        self.Dx = Dx
        self.Dxx = Dxx
        self.Dyy = Dyy
        self.Dxy = Dxy
        self.Dq = Dq
        self.Dqq = Dqq

    # Linear interpolation
    def itp(self, delta, a, b):
        result = (1-delta)*a + delta*b
        return result

    # Check whether the edge joining start and end contains a ridge
    # point
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
    def ridge_points(self):
        for k in ndindex(self.image.shape[0] - 1, self.image.shape[1] - 1):
            r = self.ridge_on_edge (k, (k[0]+1, k[1]))
            if (r != None):
                yield r
            r = self.ridge_on_edge (k, (k[0],   k[1]+1))
            if (r != None):
                yield r

    # Find ridge segments
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

    ridge_segments = ridge_segments_cached

