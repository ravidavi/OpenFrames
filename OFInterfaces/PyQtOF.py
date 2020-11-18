# -*- coding: utf-8 -*-
"""
Qt Widgets for implementing OpenFrames

"""

from PyQt5.QtWidgets import QWidget, QGridLayout, QSizePolicy
from PyQt5.QtGui import QWindow, QOpenGLContext
from PyQt5.QtCore import Qt, QSize, QCoreApplication, QEventLoop
from . import PyOF
import time


DEFAULT_WIDTH = 320
DEFAULT_HEIGHT = 240

class Window(QWindow):
    """
    A QWindow for rendering of WindowProxy

    Attributes
    ----------
    _window_proxy_id : int
        The WindowProxy responsible for drawing
    _saved_size : QSize
        If a resize event occurs before WindowProxy is started, then the size is saved here so that it can be set when
        WindowProxy is started
    _proxy_started : bool
        True after the first time that _of has been started

    """
    def __init__(self, nrow=1, ncol=1, id=0):
        """
        Create an instance of OFWindow

        Attributes
        ----------
        nrow : int, optional
            Number of rows in WindowProxy grid, default = 1
        ncol : int, optional
            Number of columns in WindowProxy grid, default = 1
        id : int, optional
            Identifier for the WindowProxy (each WindowProxy should have a unique identifier), default = 0

        """
        super().__init__()
        self._proxy_started = False
        self._saved_size = None
        self.setSurfaceType(QWindow.OpenGLSurface)

        self._window_proxy_id = id
        
        self._window_proxy = PyOF.WindowProxy(0, 0, int(DEFAULT_WIDTH*self.devicePixelRatio()), int(DEFAULT_HEIGHT*self.devicePixelRatio()),  nrow, ncol, True, False)
        self._window_proxy.setID(id)
        self._gcCallback = OFQtGraphicsContextCallback(self)
        self._window_proxy.setGraphicsContextCallback(self._gcCallback)

    def exposeEvent(self, event):
        """
        Overrides QWindow.exposeEvent()

        """
        if not self._proxy_started:
            self._proxy_started = True
            
            ret = self._window_proxy.startThread();
            while (not self._window_proxy.isAnimating() and not self._window_proxy.doneAnimating()):
                QCoreApplication.processEvents(QEventLoop.AllEvents, 100)
                
            if self._window_proxy.getAnimationState() == PyOF.WindowProxy.FAILED:
                print('PyQtOF: could not start WindowProxy thread')
                                                          
            if self._saved_size is not None:
                self._window_proxy.resizeWindow(0, 0, int(self._saved_size.width()*self.devicePixelRatio()), int(self._saved_size.height()*self.devicePixelRatio()));
                self._saved_size = None
                
    def hideEvent(self, event):
        """
        Overrides QWindow.hideEvent()

        """
        self._window_proxy.shutdown();
        while self._window_proxy.isRunning():
            QCoreApplication.processEvents(QEventLoop.AllEvents, 100)
            
        self._proxy_started = False

    def resizeEvent(self, event):
        """
        Overrides QWindow.resizeEvent()

        """
        
        if self._window_proxy.isRunning():
            self._window_proxy.resizeWindow(0, 0, int(event.size().width()*self.devicePixelRatio()), int(event.size().height()*self.devicePixelRatio()))
        else:
            self._saved_size = event.size()

    def mousePressEvent(self, event):
        """
        Overrides QWindow.mousePressEvent()

        """
        if self._window_proxy.isRunning():
            button = Window._map_qt_button_to_of_button(event.button())
            if button != 0:
                self._window_proxy.buttonPress(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()), button)

    def mouseReleaseEvent(self, event):
        """
        Overrides QWindow.mouseReleaseEvent()

        """
        if self._window_proxy.isRunning():
            button = Window._map_qt_button_to_of_button(event.button())
            if button != 0:
                self._window_proxy.buttonRelease(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()), button)

    def mouseMoveEvent(self, event):
        """
        Overrides QWindow.mouseMoveEvent()

        """
        if self._window_proxy.isRunning():
            self._window_proxy.mouseMotion(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()))

    def keyPressEvent(self, event):
        """
        Overrides QWindow.keyPressEvent()

        """
        if self._window_proxy.isRunning():
            key = Window._map_qt_key_event_to_osg_key(event)
            self._window_proxy.keyPress(key)

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

class OFQtGraphicsContextCallback(PyOF.GraphicsContextCallback):
    """
    A callback that interfaces OpenFrames with PyQt's OpenGL rendering context

    Attributes
    ----------
    _context : QOpenGLContext
        Context used to render the window contents
    _surface : QSurface
        Surface (window) on which contents will be drawn
    """
    def __init__(self, surface):
        super().__init__()
        self._surface = surface
        self._context = None

    def swapBuffers(self):
        """
        Swaps the front/back rendering buffer

        """
        if self._context is not None:
            self._context.swapBuffers(self._surface)
        
    def makeCurrent(self):
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
            success = self._context.makeCurrent(self._surface)
            if success:
                self._context.doneCurrent()
            else:
                return success
        if self._context is not None:
            success = self._context.makeCurrent(self._surface)
            # err = glGetError()
            
        return success
    
    def updateContext(self):
        """
        Updates _context when it becomes invalid, e.g. when resizing the window

        Returns
        -------
        bool
            True if successful
            False if an error occurs

        """   
        return self.makeCurrent()

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

        self._size_hint = QSize(DEFAULT_WIDTH*self.devicePixelRatio(), DEFAULT_HEIGHT*self.devicePixelRatio())
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
