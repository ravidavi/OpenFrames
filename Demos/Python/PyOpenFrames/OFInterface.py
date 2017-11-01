# -*- coding: utf-8 -*-
"""
Implements OpenFrames interface function calls through the OpenFrames_Interface_C library

"""

from ctypes import *
import ctypes.util
import atexit


# First time the module is loaded, load OpenFrames library and initialize global interface data
try:
    lib = ctypes.util.find_library('OpenFrames_Interface_Cd')
    if lib is None:
        raise Exception("Could not find library OpenFrames_Interface_C")
    OpenFramesInterfaceC = cdll.LoadLibrary(lib)
    OpenFramesInterfaceC.of_initialize()
except Exception as e:
    print("OpenFrames not loaded!\nThe error was %s" % e)


# Cleanup on exit to avoid a message from OpenFrames_Interface_C
@atexit.register
def __goodbye():
    """
    Called when this module is closed

    """
    OpenFramesInterfaceC.of_cleanup()


MakeCurrentCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
UpdateContextCallbackType = CFUNCTYPE(None, POINTER(c_uint), POINTER(c_bool))
SwapBuffersCallbackType = CFUNCTYPE(None, POINTER(c_uint))


################################################################################
#
# WindowProxy Interfaces
#
################################################################################

def ofwin_activate(window_proxy_id):
    """
    Set the current active WindowProxy

    Parameters
    ----------
    window_proxy_id : int
        The WindowProxy to activate

    """
    c_id = c_uint(window_proxy_id)
    OpenFramesInterfaceC.ofwin_activate(addressof(c_id))


def ofwin_createproxy(x, y, width, height, number_of_rows, number_of_cols, embedded, window_proxy_id):
    """
    Create a new WindowProxy and activate it

    If a WindowProxy with window_proxy_id has already been created, then this function does not create a new
    WindowProxy. It only activates the existing WindowProxy with window_proxy_id.

    Parameters
    ----------
    x : int
        X-coordinate (in pixels) of the screen of the upper-right corner of the window
    y : int
        Y-coordinate (in pixels) of the screen of the upper-right corner of the window
    width : int
        Width of the window (in pixels)
    height : int
        Height of the window (in pixels)
    number_of_rows : int
        Number of rows in the window grid
    number_of_cols : int
        Number of columns in the window grid
    embedded : bool
        True if the user wants to provide their own OpenGL window (default is False)
    window_proxy_id : int
        The unique identifier used to reference this WindowProxy

    """
    c_x = c_int(x)
    c_y = c_int(y)
    c_width = c_uint(width)
    c_height = c_uint(height)
    c_number_of_rows = c_uint(number_of_rows)
    c_number_of_cols = c_uint(number_of_cols)
    c_embedded = c_bool(embedded)
    c_id = c_uint(window_proxy_id)
    OpenFramesInterfaceC.ofwin_createproxy(addressof(c_x), addressof(c_y), addressof(c_width), addressof(c_height),
                                           addressof(c_number_of_rows), addressof(c_number_of_cols),
                                           addressof(c_embedded), addressof(c_id))


def ofwin_start():
    """
    Start animation for the active WindowProxy

    """
    OpenFramesInterfaceC.ofwin_start()


def ofwin_isrunning():
    """
    Checks to see if the active WindowProxy is running

    Returns
    -------
    bool
        True if active WindowProxy is running

    """
    c_running = c_uint(0)
    OpenFramesInterfaceC.ofwin_isrunning(addressof(c_running))
    return c_running.value


def ofwin_setscene(row, column):
    """
    Set the scene at the specified grid position

    Parameters
    ----------
    row : int
        The row of the WindowProxy grid to set frame_manager to
    column : int
        The column of the WindowProxy grid to set frame_manager to

    """
    c_row = c_uint(row)
    c_column = c_uint(column)
    OpenFramesInterfaceC.ofwin_setscene(addressof(c_row), addressof(c_column))


def ofwin_setmakecurrentfunction(make_current_function):
    """
    Set the callback functions for making a context current

    Parameters
    ----------
    make_current_function : function
        The function to callback to make a context current for OpenGL

    """
    make_current_c_function = MakeCurrentCallbackType(make_current_function)
    OpenFramesInterfaceC.ofwin_setmakecurrentfunction(make_current_function)


def ofwin_setupdatecontextfunction(update_context_function):
    """
    Set the callback functions for updating a context

    Parameters
    ----------
    update_context_function : function
        The function to callback to update a context for OpenGL

    """
    update_context_c_function = UpdateContextCallbackType(update_context_function)
    OpenFramesInterfaceC.ofwin_setupdatecontextfunction(update_context_function)


def ofwin_setswapbuffersfunction(swap_buffers_function):
    """
    Set the callback functions for swapping buffers

    Parameters
    ----------
    swap_buffers_function : function
        The function to callback to swap buffers for OpenGL

    """
    swap_buffers_c_function = SwapBuffersCallbackType(swap_buffers_function)
    OpenFramesInterfaceC.ofwin_setswapbuffersfunction(swap_buffers_function)


def ofwin_resizewindow(x, y, width, height):
    """
    Notify the active WindowProxy of changes to the OpenGL context size in which it is allowed to draw

    Parameters
    ----------
    x : int
        Upper left corner of context
    y : int
        Upper right corner of context
    width : int
        Width of context
    height : int
        Height of context

    """
    c_x = c_int(x)
    c_y = c_int(y)
    c_width = c_uint(width)
    c_height = c_uint(height)
    OpenFramesInterfaceC.ofwin_resizewindow(addressof(c_x), addressof(c_y), addressof(c_width), addressof(c_height))


def ofwin_keypress(key):
    """
    Pass a key press to the active WindowProxy

    Parameters
    ----------
    key : int
        The key code

    """
    c_key = c_uint(key)
    OpenFramesInterfaceC.ofwin_keypress(addressof(c_key))


def ofwin_buttonpress(x, y, button):
    """
    Pass a mouse button press to the active WindowProxy

    Parameters
    ----------
    x : float
        x-coordinate of the press within the window
    y : float
        x-coordinate of the press within the window
    button : int
        The button pressed

    """
    c_x = c_float(x)
    c_y = c_float(y)
    c_button = c_uint(button)
    OpenFramesInterfaceC.ofwin_buttonpress(addressof(c_x), addressof(c_y), addressof(c_button))


def ofwin_buttonrelease(x, y, button):
    """
    Pass a mouse button release to the active WindowProxy

    Parameters
    ----------
    x : float
        x-coordinate of the release within the window
    y : float
        x-coordinate of the release within the window
    button : int
        The button release

    """
    c_x = c_float(x)
    c_y = c_float(y)
    c_button = c_uint(button)
    OpenFramesInterfaceC.ofwin_buttonrelease(addressof(c_x), addressof(c_y), addressof(c_button))


def ofwin_mousemotion(x, y):
    """
    Pass mouse motion to the active WindowProxy

    Parameters
    ----------
    x : float
        x-coordinate of the mouse motion within the window
    y : float
        y-coordinate of the mouse motion within the window

    """
    c_x = c_float(x)
    c_y = c_float(y)
    OpenFramesInterfaceC.ofwin_mousemotion(addressof(c_x), addressof(c_y))


################################################################################
#
# FrameManager Interfaces
#
################################################################################

def offm_activate(frame_manager_id):
    """
    Set frame_manager_id as currently active frame manager

    """
    c_frame_manager_id = c_uint(frame_manager_id)
    OpenFramesInterfaceC.offm_activate(addressof(c_frame_manager_id))


def offm_create(frame_manager_id):
    """
    Create a new FrameManager and activate it

    If a FrameManager with frame_manager_id has already been created, then this function does not create a new
    FrameManager. It only activates the existing FrameManager with frame_manager_id.

    Parameters
    ----------
    frame_manager_id : int
        The unique identifier used to reference this FrameManager

    """
    c_frame_manager_id = c_uint(frame_manager_id)
    OpenFramesInterfaceC.offm_create(addressof(c_frame_manager_id))


def offm_setframe():
    """
    Assign the currently active ReferenceFrame to the currently active FrameManager

    """
    OpenFramesInterfaceC.offm_setframe()


################################################################################
#
# ReferenceFrame Interfaces
#
################################################################################

def offrame_activate(name):
    """
    Set name as currently active reference frame

    """
    c_name = c_char_p(name.encode())
    OpenFramesInterfaceC.offrame_activate(c_name)


def offrame_create(name):
    """
    Create a new ReferenceFrame with the given name and activate it

    If a ReferenceFrame with name has already been created, then this function removes that ReferenceFrame.

    Parameters
    ----------
    name : string
        Name for this ReferenceFrame

    """
    c_name = c_char_p(name.encode())
    OpenFramesInterfaceC.offrame_create(c_name)


def offrame_showaxes(axes):
    """
    Show/hide the current frame's coordinate axes
    
    Attributes
    ----------
    axes : int
        Bits indicate the axes to be shown

    """
    c_axes = c_uint(axes)
    OpenFramesInterfaceC.offrame_showaxes(addressof(c_axes))


################################################################################
#
# CoordinateAxes Interfaces
#
################################################################################

def ofcoordaxes_create(name):
    """
    Create a new CoordinateAxes with the given name and activate it

    If a ReferenceFrame with name has already been created, then this function removes that ReferenceFrame.

    Parameters
    ----------
    name : string
        Name for this CoordinateAxes

    """
    c_name = c_char_p(name.encode())
    OpenFramesInterfaceC.ofcoordaxes_create(c_name)

def ofcoordaxes_setaxislength(length):
    """
    Set the length of each axis of the active CoordinateAxes

    Parameters
    ----------
    length : float
        The new length of the axes

    """
    c_length = c_double(length)
    OpenFramesInterfaceC.ofcoordaxes_setaxislength(addressof(c_length))
