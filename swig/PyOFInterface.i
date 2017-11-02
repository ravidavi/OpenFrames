%module PyOFInterface

%{
#include "OpenFrames/OF_Interface.h"
%}

// Blanket apply INPUT type handling to other pointers (except char *)
%apply int* INPUT {int*};
%apply unsigned int* INPUT {unsigned int*};
%apply bool* INPUT {bool*};
%apply float* INPUT {float*};
%apply double* INPUT {double*};

// Typemaps for callback functions
%typemap(in) void (*fcn)(unsigned int*, bool*) {
    $1 = (void (*)(unsigned int* winID, bool* success))PyInt_AsLong($input);;
}
%typemap(in) void (*fcn)(unsigned int*) {
    $1 = (void (*)(unsigned int* winID))PyInt_AsLong($input);;
}

// Specify OUTPUT type handling for outputs only
// TODO: size, x, y, z, px, py, pz are inputs for some and outputs for others
//%typemap(in, numinputs=0) unsigned int* state { $1 = 0; }
//%typemap(argout) unsigned int* state { $result = PyInt_FromLong(*$1); }
%apply unsigned int* OUTPUT {unsigned int* retid};
%apply unsigned int* OUTPUT {unsigned int* state};
%apply int* OUTPUT {int* valid};
%apply bool* OUTPUT {bool* valid};
%apply int* OUTPUT {int* numchildren};

// Blanket apply INPUT type handling to other pointers (except char *)
%apply int* INPUT {int*};
%apply unsigned int* INPUT {unsigned int*};
%apply bool* INPUT {bool*};
%apply float* INPUT {float*};
%apply double* INPUT {double*};

// Include all interfaces in the header
%include "OpenFrames/OF_Interface.h"


%pythoncode
%{

from ctypes import CFUNCTYPE, POINTER, c_uint, c_bool, c_void_p, cast

# a ctypes callback prototype
_MakeCurrentCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
_UpdateContextCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
_SwapBuffersCallbackType = CFUNCTYPE(None, POINTER(c_uint))

# callback functions must stay on the heap
_make_current_callbacks = {}
_update_context_callbacks = {}
_swap_buffers_callbacks = {}


# wrap callbacks
def make_current_wrapper(window_proxy_id, ret_success):
    if window_proxy_id[0] in _make_current_callbacks:
        ret_success[0] = _make_current_callbacks[window_proxy_id[0]]()
    else:
        ret_success[0] = False


def update_context_wrapper(window_proxy_id, ret_success):
    if window_proxy_id[0] in _make_current_callbacks:
        ret_success[0] = _update_context_callbacks[window_proxy_id[0]]()
    else:
        ret_success[0] = False


def swap_buffers_wrapper(window_proxy_id):
    if window_proxy_id[0] in _make_current_callbacks:
        _swap_buffers_callbacks[window_proxy_id[0]]()


# wrap the python callback with a ctypes function pointer
_make_current_c_function = _MakeCurrentCallbackType(make_current_wrapper)
_update_context_c_function = _UpdateContextCallbackType(update_context_wrapper)
_swap_buffers_c_function = _SwapBuffersCallbackType(swap_buffers_wrapper)


# save the callback for this window proxy
# get the function pointer of the ctypes wrapper by casting it to void* and taking its value
def ofwin_setmakecurrentfunction(make_current_function):
    _make_current_callbacks[ofwin_getid()] = make_current_function
    f_ptr = cast(_make_current_c_function, c_void_p).value
    _PyOFInterface.ofwin_setmakecurrentfunction(f_ptr)


def ofwin_setupdatecontextfunction(update_context_function):
    _update_context_callbacks[ofwin_getid()] = update_context_function
    f_ptr = cast(_update_context_c_function, c_void_p).value
    _PyOFInterface.ofwin_setupdatecontextfunction(f_ptr)


def ofwin_setswapbuffersfunction(swap_buffers_function):
    _swap_buffers_callbacks[ofwin_getid()] = swap_buffers_function
    f_ptr = cast(_swap_buffers_c_function, c_void_p).value
    _PyOFInterface.ofwin_setswapbuffersfunction(f_ptr)

%}
