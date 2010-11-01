from numpy import *
from ctypes import *
import multiprocessing as mp

def _ctype_to_numpy(t):
    mapping = {
        c_char : int8,
        c_wchar : int16,
        c_byte : int8,
        c_ubyte : uint8,
        c_short : int16,
        c_ushort : uint16,
        c_int : int32,
        c_long : int32,
        c_ulong : uint32,
        c_float : float32,
        c_double : float64
        }
    return dtype(mapping[t])

def _numpy_to_ctype(t):
    mapping = {
        int8 : c_char,
        int16 : c_wchar,
        int8 : c_byte,
        uint8 : c_ubyte,
        int16 : c_short,
        uint16 : c_ushort,
        int32 : c_int,
        int32 : c_long,
        uint32 : c_ulong,
        float32 : c_float,
        float64 : c_double
        }
    return mapping[t.type]

def shmem_as_ndarray(raw_array):
    class Dummy(object): pass
    d = Dummy()
    address = raw_array._wrapper.get_address()
    size = raw_array._wrapper.get_size()
    t = _ctype_to_numpy(raw_array._type_)
    d.__array_interface__ = {
        'data' : (address, False),
        'typestr' : dtype(uint8).str,
        'descr' : dtype(uint8).descr,
        'shape' : (size,),
        'strides' : None,
        'version' : 3
        }
    result = asarray(d).view(dtype=t)
    return result

def shmem_empty_like(array):
    N = 1
    for x in array.shape:
        N = N*x
    shmem_array = mp.RawArray(_numpy_to_ctype(array.dtype),N)
    result = shmem_as_ndarray(shmem_array)
    result = result.reshape(array.shape)
    result.fill(0)
    return (result, shmem_array)

def shmem_copy(array):
    (result,shmem_array) = shmem_empty_like(array)
    result[:] = array
    return (result,shmem_array)
