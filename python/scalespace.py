from numpy import *
from scipy.ndimage.filters import convolve1d

def scalespace(image, scale):
    def scale_filter(n):
        K = array([1.0/6, 2.0/3, 1.0/6], image.dtype)
        f = array([1], image.dtype)
        for i in range(n):
            f = convolve(f,K)
        return f

    result = empty_like(image)
    filt = scale_filter(int(scale*3.0))
    convolve1d(image, filt, output=result, axis=0, mode='constant', cval=0)
    convolve1d(result, filt, output=result, axis=1, mode='constant', cval=0)

    return result
