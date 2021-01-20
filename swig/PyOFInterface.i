/***********************************
Copyright 2021 Ravishankar Mathur

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
***********************************/

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
    $1 = (void (*)(unsigned int* winID, bool* success))PyLong_AsVoidPtr($input);;
}
%typemap(in) void (*fcn)(unsigned int*) {
    $1 = (void (*)(unsigned int* winID))PyLong_AsVoidPtr($input);;
}

// Specify OUTPUT type handling for outputs only
// TODO: size, x, y, z, px, py, pz are inputs for some and outputs for others
//%typemap(in, numinputs=0) unsigned int* state { $1 = 0; }
//%typemap(argout) unsigned int* state { $result = PyInt_FromLong(*$1); }
%apply unsigned int* OUTPUT {unsigned int* retid};
%apply unsigned int* OUTPUT {unsigned int* state};
%apply int* OUTPUT {int* val};
%apply int* OUTPUT {int* valid};
%apply bool* OUTPUT {bool* valid};
%apply bool* OUTPUT {bool* isPaused};
%apply int* OUTPUT {int* numchildren};
%apply double* OUTPUT {double* retvar};

// Blanket apply INPUT type handling to other pointers (except char *)
%apply int* INPUT {int*};
%apply unsigned int* INPUT {unsigned int*};
%apply bool* INPUT {bool*};
%apply float* INPUT {float*};
%apply double* INPUT {double*};

// Function signatures where outputs are inputs elsewhere
%apply double* OUTPUT {double* time};
void ofwin_gettime(double *time);
%ignore ofwin_gettime(double*);
%clear double *time;
%apply double* OUTPUT {double* tscale};
void ofwin_gettimescale(double *tscale);
%ignore ofwin_gettimescale(double*);
%clear double *tscale;

// Function signatures for functions with conflicting parameter names
void offrame_getposition(double* retvar, double* retvar, double* retvar);
void offrame_getattitude(double* retvar, double* retvar, double* retvar, double* retvar);
void ofmodel_getmodelposition(double* retvar, double* retvar, double* retvar);
void ofmodel_getmodelscale(double* retvar, double* retvar, double* retvar);
void ofmodel_getmodelpivot(double* retvar, double* retvar, double* retvar);

// Ignore redefinition of the above functions
%ignore offrame_getposition(double*, double*, double*);
%ignore offrame_getattitude(double*, double*, double*, double*);
%ignore ofmodel_getmodelposition(double*, double*, double*);
%ignore ofmodel_getmodelscale(double*, double*, double*);
%ignore ofmodel_getmodelpivot(double*, double*, double*);

// Include all interfaces in the header
%include "OpenFrames/OF_Interface.h"


%pythoncode
%{

from ctypes import CFUNCTYPE, POINTER, c_uint, c_bool, c_int, c_float, c_void_p, cast
import atexit

# intialize OpenFrames when module is loaded
of_initialize()

# cleanup on exit to avoid a message from OpenFrames_Interface_C
@atexit.register
def __goodbye():
    of_cleanup()

# a ctypes callback prototype
_MakeCurrentCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
_UpdateContextCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
_SwapBuffersCallbackType = CFUNCTYPE(None, POINTER(c_uint))
_KeyPressCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_uint), POINTER(c_uint), POINTER(c_int))
_MouseMotionCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_uint), POINTER(c_uint), POINTER(c_float), POINTER(c_float))
_MouseButtonCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_uint), POINTER(c_uint), POINTER(c_uint))

# callback functions must stay on the heap
_make_current_callbacks = {}
_update_context_callbacks = {}
_swap_buffers_callbacks = {}
_key_press_callbacks = {}
_mouse_motion_callbacks = {}
_button_press_callbacks = {}
_button_release_callbacks = {}


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


def key_press_wrapper(window_proxy_id, row, col, key):
    if window_proxy_id[0] in _key_press_callbacks:
        _key_press_callbacks[window_proxy_id[0]](row[0], col[0], key[0])


def mouse_motion_wrapper(window_proxy_id, row, col, x, y):
    if window_proxy_id[0] in _mouse_motion_callbacks:
        _mouse_motion_callbacks[window_proxy_id[0]](row[0], col[0], x[0], y[0])


def button_press_wrapper(window_proxy_id, row, col, button):
    if window_proxy_id[0] in _button_press_callbacks:
        _button_press_callbacks[window_proxy_id[0]](row[0], col[0], button[0])


def button_release_wrapper(window_proxy_id, row, col, button):
    if window_proxy_id[0] in _button_release_callbacks:
        _button_release_callbacks[window_proxy_id[0]](row[0], col[0], button[0])


# wrap the python callback with a ctypes function pointer
_make_current_c_function = _MakeCurrentCallbackType(make_current_wrapper)
_update_context_c_function = _UpdateContextCallbackType(update_context_wrapper)
_swap_buffers_c_function = _SwapBuffersCallbackType(swap_buffers_wrapper)
_key_press_c_function = _SwapBuffersCallbackType(key_press_wrapper)
_mouse_motion_c_function = _SwapBuffersCallbackType(mouse_motion_wrapper)
_button_press_c_function = _SwapBuffersCallbackType(button_press_wrapper)
_button_release_c_function = _SwapBuffersCallbackType(button_release_wrapper)


# save the callback for this window proxy
# get the function pointer of the ctypes wrapper by casting it to void* and taking its value
def ofwin_setmakecurrentfunction(make_current_function):
    _make_current_callbacks[ofwin_getid()] = make_current_function
    f_ptr = cast(_make_current_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setmakecurrentfunction(f_ptr)


def ofwin_setupdatecontextfunction(update_context_function):
    _update_context_callbacks[ofwin_getid()] = update_context_function
    f_ptr = cast(_update_context_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setupdatecontextfunction(f_ptr)


def ofwin_setswapbuffersfunction(swap_buffers_function):
    _swap_buffers_callbacks[ofwin_getid()] = swap_buffers_function
    f_ptr = cast(_swap_buffers_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setswapbuffersfunction(f_ptr)


def ofwin_setkeypresscallback(key_press_function):
    _key_press_callbacks[ofwin_getid()] = key_press_function
    f_ptr = cast(_key_press_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setkeypresscallback(f_ptr)


def ofwin_setmousemotioncallback(mouse_motion_function):
    _mouse_motion_callbacks[ofwin_getid()] = mouse_motion_function
    f_ptr = cast(_mouse_motion_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setmousemotioncallback(f_ptr)


def ofwin_setbuttonpresscallback(button_press_function):
    _button_press_callbacks[ofwin_getid()] = button_press_function
    f_ptr = cast(_button_press_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setbuttonpresscallback(f_ptr)


def ofwin_setbuttonreleasecallback(button_release_function):
    _button_release_callbacks[ofwin_getid()] = button_release_function
    f_ptr = cast(_button_release_c_function, c_void_p).value
    _${SWIG_MODULE_NAME}.ofwin_setbuttonreleasecallback(f_ptr)

%}
