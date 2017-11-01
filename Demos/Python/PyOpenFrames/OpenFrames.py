# -*- coding: utf-8 -*-
"""
Python wrapper to implement objects around the OpenFrames interface calls

This is no longer used in favor of direct calls to OFInterface.

"""

from .OFInterface import *


class WindowProxy(object):
    """
    Creates and interfaces to an OpenFrames WindowProxy

    Attributes
    ----------
    _id : c_uint
        The unique ID associated with this window proxy

    """
    _window_proxy_counter = 0

    def __init__(self, x, y, width, height, number_of_rows, number_of_cols, embedded=False):
        """
        Instantiate a WindowProxy

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
        embedded : bool, optional
            True if the user wants to provide their own OpenGL window (default is False)

        """
        self._id = WindowProxy._next_window_proxy_id()
        self._make_current_function = None
        self._update_context_function = None
        self._swap_buffers_function = None
        self._make_current_c_function = None
        self._update_context_c_function = None
        self._swap_buffers_c_function = None
        ofwin_createproxy(x, y, width, height, number_of_rows, number_of_cols, embedded, self._reference())

    def set_scene(self, frame_manager, row, column):
        """
        Set the scene at the specified grid position

        Parameters
        ----------
        frame_manager : FrameManager
            The scene to set
        row : int
            The row of the WindowProxy grid to set frame_manager to
        column : int
            The column of the WindowProxy grid to set frame_manager to

        """
        self.activate()
        frame_manager.activate()
        ofwin_setscene(row, column)

    def start(self):
        """
        Start animation

        """
        self.activate()
        ofwin_start()

    def is_running(self):
        self.activate()
        return ofwin_isrunning()

    def set_make_current_function(self, make_current_function):
        """
        Set the callback functions for making a context current

        Parameters
        ----------
        make_current_function : function
            The function to callback to make a context current for OpenGL

        """
        self.activate()
        self._make_current_function = make_current_function
        self._make_current_c_function = MakeCurrentCallbackType()
        ofwin_setmakecurrentfunction(self._make_current_c_function)

    def set_update_context_function(self, update_context_function):
        """
        Set the callback functions for updating a context

        Parameters
        ----------
        update_context_function : function
            The function to callback to update a context for OpenGL

        """
        self.activate()
        self._update_context_function = update_context_function
        self._update_context_c_function = UpdateContextCallbackType(self.update_context)
        ofwin_setupdatecontextfunction(self._update_context_c_function)

    def set_swap_buffers_function(self, swap_buffers_function):
        """
        Set the callback functions for swapping buffers

        Parameters
        ----------
        swap_buffers_function : function
            The function to callback to swap buffers for OpenGL

        """
        self.activate()
        self._swap_buffers_function = swap_buffers_function
        self._swap_buffers_c_function = SwapBuffersCallbackType(self.swap_buffers)
        ofwin_setswapbuffersfunction(self._swap_buffers_c_function)

    def resize_window(self, x, y, width, height):
        self.activate()
        ofwin_resizewindow(x, y, width, height)

    def key_press(self, key):
        self.activate()
        ofwin_keypress(key)

    def button_press(self, x, y, button):
        self.activate()
        ofwin_buttonpress(x, y, button)

    def button_release(self, x, y, button):
        self.activate()
        ofwin_buttonrelease(x, y, button)

    def mouse_motion(self, x, y):
        self.activate()
        ofwin_mousemotion(x, y)

    def activate(self):
        """
        Set this instance as currently active window proxy

        """
        ofwin_activate(self._reference())

    def make_current(self, window_proxy_id, success):
        """
        Passes a callback for making a context current to the previously set function

        Parameters
        ----------
        window_proxy_id : POINTER(c_uint)
            The ID of the WindowProxy that this callback is intended for
        success : POINTER(c_bool)
            Output shall be set to True if a context was successfully made current

        """
        if window_proxy_id[0] == self._id:
            if self._make_current_function is not None:
                success[0] = self._make_current_function()
            else:
                raise UnboundLocalError('make_current() callback has not been defined')
        else:
            raise Exception('make_current() callback to incorrect WindowProxy')

    def update_context(self, window_proxy_id, success):
        """
        Passes a callback for updating a context to the previously set function

        Parameters
        ----------
        window_proxy_id : POINTER(c_uint)
            The ID of the WindowProxy that this callback is intended for
        success : POINTER(c_bool)
            Output shall be set to True if a context was successfully made current

        """
        if window_proxy_id[0] == self._id:
            if self._update_context_function is not None:
                success[0] = self._update_context_function()
            else:
                raise UnboundLocalError('update_context() callback has not been defined')
        else:
            raise Exception('update_context() callback to incorrect WindowProxy')

    def swap_buffers(self, window_proxy_id):
        """
        Passes a callback for updating a context to the previously set function

        Parameters
        ----------
        window_proxy_id : POINTER(c_uint)
            The ID of the WindowProxy that this callback is intended for

        """
        if window_proxy_id[0] == self._id:
            if self._swap_buffers_function is not None:
                self._swap_buffers_function()
            else:
                raise UnboundLocalError('swap_buffers() callback has not been defined')
        else:
            raise Exception('swap_buffers() callback to incorrect WindowProxy')

    def _reference(self):
        """
        Get a reference to this object to use for OFInterface library calls

        Returns
        -------
        int

        """
        return self._id

    @staticmethod
    def _next_window_proxy_id():
        """
        Generates a new WindowProxy ID

        Returns
        -------
        int
            Unique identification number

        """
        WindowProxy._window_proxy_counter += 1
        return WindowProxy._window_proxy_counter


class FrameManager(object):
    _frame_manager_counter = 0

    def __init__(self):
        self._id = FrameManager._next_frame_manager_id()
        offm_create(self._reference())

    def set_frame(self, reference_frame):
        """
        Assign the a ReferenceFrame to this FrameManager

        Parameters
        ----------
        reference_frame : ReferenceFrame
            The ReferenceFrame to assign to this FrameManager

        """
        self.activate()
        reference_frame.activate()
        offm_setframe()

    def activate(self):
        """
        Set this instance as currently active frame manager

        """
        offm_activate(self._reference())

    def _reference(self):
        """
        Get a reference to this object to use for OFInterface library calls

        Returns
        -------
        int

        """
        return self._id

    @staticmethod
    def _next_frame_manager_id():
        """
        Generates a new FrameManager ID

        Returns
        -------
        int
            Unique identification number

        """
        FrameManager._frame_manager_counter += 1
        return FrameManager._frame_manager_counter


class ReferenceFrame(object):
    def __init__(self, name):
        """
        Create a new ReferenceFrame with the given name and activate it

        If a ReferenceFrame with that name already exists, then this instance becomes an alias for that ReferenceFrame
        in the OpenFrames_Interface_C library.

        Parameters
        ----------
        name : string
            Name for this ReferenceFrame

        """
        self._name = name
        offrame_create(self._reference())

    def show_axes(self, axes):
        self.activate()
        offrame_showaxes(axes)

    def activate(self):
        """
        Set this instance as currently active reference frame

        """
        offrame_activate(self._reference())

    def _reference(self):
        """
        Get a reference to this object to use for OFInterface calls

        Returns
        -------
        string

        """
        return self._name


class CoordinateAxes(ReferenceFrame):
    def __init__(self, name):
        self._name = name
        ofcoordaxes_create(name)

    def set_axis_length(self, length):
        self.activate()
        ofcoordaxes_setaxislength(length)