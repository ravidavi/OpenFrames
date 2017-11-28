# -*- coding: utf-8 -*-
"""
Qt Widgets for implementing OpenFrames

"""

from PyQt5.QtWidgets import QWidget, QGridLayout, QSizePolicy
from PyQt5.QtGui import QWindow, QOpenGLContext
from PyQt5.QtCore import Qt, QSize
from .PyOFInterfaceC import *


DEFAULT_WIDTH = 320
DEFAULT_HEIGHT = 240


class Window(QWindow):
    """
    A QWindow for rendering of WindowProxy

    Attributes
    ----------
    _context : QOpenGLContext
        Context used to render the window contents
    _window_proxy_id : int
        The WindowProxy responsible for drawing
    _saved_size : QSize
        If a resize event occurs before WindowProxy is started, then the size is saved here so that it can be set when
        WindowProxy is started
    _proxy_started_once : bool
        True after the first time that _of has been started

    """
    def __init__(self, width=1, height=1):
        """
        Create an instance of OFWindow

        Attributes
        ----------
        width : int, optional
            The width of the grid in window proxy, default = 1
        height : int, optional
            The height of the grid in window proxy, default = 1

        """
        super().__init__()
        self._context = None
        self._proxy_started = False
        self._saved_size = None
        self.setSurfaceType(QWindow.OpenGLSurface)

        self._window_proxy_id = 0

        ofwin_createproxy(0, 0, DEFAULT_WIDTH, DEFAULT_HEIGHT, 1, 1, True, self._window_proxy_id)
        ofwin_setmakecurrentfunction(self.make_current)
        ofwin_setupdatecontextfunction(self.make_current)
        ofwin_setswapbuffersfunction(self.swap_buffers)

    def exposeEvent(self, event):
        """
        Overrides QWindow.exposeEvent()

        """
        if not self._proxy_started:
            self._proxy_started = True
            ofwin_activate(self._window_proxy_id)
            ofwin_start()
            if self._saved_size is not None:
                ofwin_resizewindow(0, 0, self._saved_size.width(), self._saved_size.height())
                self._saved_size = None

    def hideEvent(self, event):
        """
        Overrides QWindow.exposeEvent()

        """
        ofwin_activate(self._window_proxy_id)
        ofwin_stop()
        self._proxy_started = False

    def resizeEvent(self, event):
        """
        Overrides QWindow.resizeEvent()

        """
        ofwin_activate(self._window_proxy_id)
        if ofwin_isrunning() == 1:
            ofwin_resizewindow(0, 0, event.size().width(), event.size().height())
        else:
            self._saved_size = event.size()

    def mousePressEvent(self, event):
        """
        Overrides QWindow.mousePressEvent()

        """
        ofwin_activate(self._window_proxy_id)
        if ofwin_isrunning() == 1:
            button = Window._map_qt_button_to_of_button(event.button())
            if button != 0:
                ofwin_buttonpress(event.x(), event.y(), button)

    def mouseReleaseEvent(self, event):
        """
        Overrides QWindow.mouseReleaseEvent()

        """
        ofwin_activate(self._window_proxy_id)
        if ofwin_isrunning() == 1:
            button = Window._map_qt_button_to_of_button(event.button())
            if button != 0:
                ofwin_buttonrelease(event.x(), event.y(), button)

    def mouseMoveEvent(self, event):
        """
        Overrides QWindow.mouseMoveEvent()

        """
        ofwin_activate(self._window_proxy_id)
        if ofwin_isrunning() == 1:
            ofwin_mousemotion(event.x(), event.y())

    def keyPressEvent(self, event):
        """
        Overrides QWindow.keyPressEvent()

        """
        ofwin_activate(self._window_proxy_id)
        if ofwin_isrunning() == 1:
            key = Window._map_qt_key_event_to_osg_key(event)
            ofwin_keypress(key)

    # TODO call glGetError() to print any errors that may have occurred
    def make_current(self):
        """
        Makes _context current for the surface of this window

        Returns
        -------
        bool
            True if successful
            False if an error occurs

        """
        success = False
        if self._context is None:
            self._context = QOpenGLContext()
            self._context.create()
            success = self._context.makeCurrent(self)
            if success:
                # self.initializeOpenGLFunctions()
                self._context.doneCurrent()
            else:
                return success
        if self._context is not None:
            success = self._context.makeCurrent(self)
            # err = glGetError()
        return success

    def swap_buffers(self):
        """
        Swaps the buffer from _context to the surface of this window

        """
        if self._context is not None:
            self._context.swapBuffers(self)

    @staticmethod
    def _map_qt_button_to_of_button(qt_button):
        """
        Maps a Qt.MouseButton enumeration to an int for OpenFrames

        Parameters
        ----------
        qt_button : Qt.MouseButton
            The button to map

        Returns
        -------
        int
            The corresponding button for OpenFrames

        """
        if qt_button == Qt.LeftButton:
            return 1
        elif qt_button == Qt.RightButton:
            return 3
        elif qt_button == Qt.MiddleButton:
            return 2
        elif qt_button == Qt.BackButton:
            return 6
        elif qt_button == Qt.ForwardButton:
            return 7
        else:
            return 0

    @staticmethod
    def _map_qt_key_event_to_osg_key(event):
        """
        Maps a QKeyEvent to an int for OpenFrames

        Parameters
        ----------
        event : PyQt5.QtGui.QKeyEvent.QKeyEvent
            The key event to map

        Returns
        -------
        int
            The corresponding key code for OpenFrames

        """
        if Qt.Key_A <= event.key() <= Qt.Key_Z:
            if event.modifiers() & Qt.ShiftModifier:
                key = event.key()
            else:
                key = event.key() + 0x20
        else:
            key = event.key()
        return key


class Widget(QWidget):
    """
    Encapsulates a QWindow into a widget

    QWindow is preferred for rendering over a QWidget because there is more control over OpenGL. A QOpenGLWidget
    limitations impose undesirable effects onto WindowProxy. To use QOpenGLWidget, WindowProxy would need to draw to
    a QOffscreenSurface and blit the result onto the QOpenGLWidget at appropriate times.

    Attributes
    ----------
    _size_hint : QSize
        The hint that this widget provides to Qt for sizing

    """
    def __init__(self, window_type=Window):
        """
        Instantiates a widget that creates a new object of window_type and encapsulates it

        Attributes
        ----------
        window_type : class, optional
            The class type to encapsulated by this widget, default class is Window.

        """
        super().__init__()

        self._size_hint = QSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        window = window_type()
        container = QWidget.createWindowContainer(window, self)
        self.setLayout(QGridLayout())
        self.layout().addWidget(container)

    def sizeHint(self):
        """
        Overrides QWidget.sizeHint() to provide the user set size hint

        Returns
        -------
        QSize
            Size hint to Qt

        """
        return self._size_hint

    def set_size_hint(self, width, height):
        """
        Set the preferred size for this widget

        The default size policy is QSizePolicy.Expanding. Therefore, Qt tries to make this widget as large as possible.
        Under this policy, Qt is allowed to shrink the widget below this size if necessary.

        Parameters
        ----------
        width : int
            The desired width
        height : int
            The desired height

        References
        ----------
        [1] https://doc.qt.io/qt-5/qsizepolicy.html#Policy-enum

        """
        self._size_hint.setWidth(width)
        self._size_hint.setHeight(height)
