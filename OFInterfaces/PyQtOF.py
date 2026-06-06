# -*- coding: utf-8 -*-
"""
Qt Widgets to integrate OpenFrames

"""

from sys import platform
from qtpy.QtWidgets import QWidget, QDockWidget, QGridLayout, QSizePolicy, QLabel
from qtpy.QtGui import QWindow, QOpenGLContext
from qtpy.QtCore import Qt, QSize, QCoreApplication, QEventLoop
from . import PyOF

DEFAULT_WIDTH = 320
DEFAULT_HEIGHT = 240

class OFWindow(QWindow):
    """
    A QWindow for rendering of WindowProxy
    
    QWindow is preferred for rendering over a QWidget because there is more control over OpenGL. A QOpenGLWidget
    limitations impose undesirable effects onto WindowProxy. To use QOpenGLWidget, WindowProxy would need to draw to
    a QOffscreenSurface and blit the result onto the QOpenGLWidget at appropriate times.

    Attributes
    ----------
    _proxyStarted : bool
        True after the first time that the WindowProxy has been started
    _autoPauseAnimation : bool
        True if animation should be paused based on window visibility

    """
    def __init__(self, nrow=1, ncol=1):
        """
        Create an instance of OFWindow

        Attributes
        ----------
        nrow : int, optional
            Number of rows in WindowProxy grid, default = 1
        ncol : int, optional
            Number of columns in WindowProxy grid, default = 1

        """
        super().__init__()
        self._proxyStarted = False
        self.setSurfaceType(QWindow.OpenGLSurface)
        
        self.windowProxy = PyOF.WindowProxy(0, 0, int(DEFAULT_WIDTH*self.devicePixelRatio()), int(DEFAULT_HEIGHT*self.devicePixelRatio()),  nrow, ncol, True, False)
        self._gcCallback = OFQtGraphicsContextCallback(self)
        self.windowProxy.setGraphicsContextCallback(self._gcCallback)
        
        self._autoPauseAnimation = True

    def stopRendering(self):
        """
        Stop OpenFrames rendering

        """
        self.windowProxy.shutdown();
        while self.windowProxy.isRunning():
            QCoreApplication.processEvents(QEventLoop.AllEvents, 100)
            
        self._proxyStarted = False
            
    def moveEvent(self, event):
        # Called when this window is resized within a QDockWidget
        # TODO: Is this superceded by QDockWidget:moveEvent?
        print(f'OFWindow:moveEvent()')

        # macOS requires OpenGL context updates on the main thread
        if platform == "darwin":
            self._gcCallback.updateAndReleaseContext()

    def exposeEvent(self, event):
        """
        Overrides QWindow.exposeEvent()
        Per QWindow documentation, rendering should be started/stopped based on this event.

        """
        
        print(f'OFWindow:exposeEvent() {self.isExposed()}')
        
        # macOS requires OpenGL context updates on the main thread
        if platform == "darwin":
            self._gcCallback.updateAndReleaseContext()

        # Enable rendering when window is exposed
        if self.isExposed():
            if self._autoPauseAnimation:
                self.windowProxy.pauseAnimation(False)

            if not self._proxyStarted:
                self._proxyStarted = True
                
                ret = self.windowProxy.startThread();
                while (not self.windowProxy.isAnimating() and not self.windowProxy.doneAnimating()):
                    QCoreApplication.processEvents(QEventLoop.AllEvents, 100)
                    
                if self.windowProxy.getAnimationState() == PyOF.WindowProxy.FAILED:
                    print('PyQtOF: could not start WindowProxy thread')
        
        # Disable rendering when window is not exposed               
        else:
            if self._autoPauseAnimation:
                self.windowProxy.pauseAnimation(True)

    def resizeEvent(self, event):
        """
        Overrides QWindow.resizeEvent()

        """
        
        print(f"OFWindow resized to: {event.size().width()}x{event.size().height()}")

        # macOS requires OpenGL context updates on the main thread
        if platform == "darwin":
            self._gcCallback.updateAndReleaseContext()
            
        self.windowProxy.resizeWindow(0, 0, int(event.size().width()*self.devicePixelRatio()), int(event.size().height()*self.devicePixelRatio()))

    def mousePressEvent(self, event):
        """
        Overrides QWindow.mousePressEvent()

        """
        if self.windowProxy.isRunning():
            button = OFWindow._map_qt_button_to_of_button(event.button())
            if button != 0:
                self.windowProxy.buttonPress(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()), button)

    def mouseReleaseEvent(self, event):
        """
        Overrides QWindow.mouseReleaseEvent()

        """
        if self.windowProxy.isRunning():
            button = OFWindow._map_qt_button_to_of_button(event.button())
            if button != 0:
                self.windowProxy.buttonRelease(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()), button)

    def mouseMoveEvent(self, event):
        """
        Overrides QWindow.mouseMoveEvent()

        """
        if self.windowProxy.isRunning():
            self.windowProxy.mouseMotion(int(event.x()*self.devicePixelRatio()), int(event.y()*self.devicePixelRatio()))

    def keyPressEvent(self, event):
        """
        Overrides QWindow.keyPressEvent()

        """
        if self.windowProxy.isRunning():
            key = OFWindow._map_qt_key_event_to_osg_key(event)
            self.windowProxy.keyPress(key)

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
        event : qtpy.QtGui.QKeyEvent.QKeyEvent
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
        
    def updateAndReleaseContext(self):
        """
        Updates _context when it becomes invalid. Must be called from the main thread.
        
        This version is intended for macOS, which requires context updates (NSOpenGLContext setView and update)
        to be done on the main thread. Qt calls these functions from QOpenGLContext::makeCurrent() if
        the context needs to be updated. So on macOS, this function must be called from the main thread if
        the OpenGL context is updated (e.g. resized, exposed, etc.).
        
        See https://github.com/ravidavi/OpenFrames/issues/5
        """
        
        self._surface.windowProxy.pauseAnimation(True)
        self.makeCurrent()
        self._context.doneCurrent()
        self._surface.windowProxy.pauseAnimation(False)

class OFWidget(QWidget):
    """
    Encapsulates a QWindow into a QWidget

    Attributes
    ----------
    _sizeHint : QSize
        The hint that this widget provides to Qt for sizing

    """
    def __init__(self, window_type=OFWindow):
        """
        Instantiates a widget that creates a new object of window_type and encapsulates it

        Attributes
        ----------
        window_type : class, optional
            The class type to encapsulated by this widget, default class is Window.

        """
        super().__init__()

        self._sizeHint = QSize(DEFAULT_WIDTH*self.devicePixelRatio(), DEFAULT_HEIGHT*self.devicePixelRatio())
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.ofwindow = window_type()
        container = QWidget.createWindowContainer(self.ofwindow, self)
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
        return self._sizeHint

    def setSizeHint(self, width, height):
        """
        TODO: Why is this here? It doesn't seem to do anything.
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
        self._sizeHint.setWidth(width*self.devicePixelRatio())
        self._sizeHint.setHeight(height*self.devicePixelRatio())
 
    def closeEvent(self, event):
        self.stopRendering()
        
    def stopRendering(self):
        self.ofwindow.stopRendering()

class OFDockWidget(QDockWidget):
    """
    Encapsulates a QWindow into a QDockWidget

    Attributes
    ----------
    _sizeHint : QSize
        The hint that this widget provides to Qt for sizing

    """
    def __init__(self, parent=None, window_type=OFWindow):
        super().__init__()
        self._sizeHint = QSize(DEFAULT_WIDTH*self.devicePixelRatio(), DEFAULT_HEIGHT*self.devicePixelRatio())
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.setMinimumSize(100, 100)
        
        #self.ofwidget = OFWidget(window_type)
        #self.setWidget(self.ofwidget)
        #self.setWidget(QLabel("OpenGL Here"))
        
        self.ofwindow = window_type()
        ofcontainerwidget = QWidget.createWindowContainer(self.ofwindow)
        self.setWidget(ofcontainerwidget)
        
        # Connect the topLevelChanged signal
        #self.topLevelChanged.connect(self.handle_top_level_changed)


    def sizeHint(self):
        """
        Overrides QWidget.sizeHint() to provide the user set size hint

        Returns
        -------
        QSize
            Size hint to Qt

        """
        return self._sizeHint

    def setSizeHint(self, width, height):
        """
        TODO: Why is this here? It doesn't seem to do anything.
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
        self._sizeHint.setWidth(width*self.devicePixelRatio())
        self._sizeHint.setHeight(height*self.devicePixelRatio())
 
    def closeEvent(self, event):
        print("*** OFDockWidget closeEvent")
        self.stopRendering()
        
    def stopRendering(self):
        self.ofwindow.stopRendering()
        
    def handle_top_level_changed(self, is_floating):
        if is_floating:
            print("*** OFDockWidget is now floating.")
        else:
            print("*** OFDockWidget is now docked.")

    def moveEvent(self, event):
        # Called when this widget goes from floating to docked
        # TODO: This is called much more often, need to find more elegant approach to handling docking
        self.ofwindow._gcCallback.updateAndReleaseContext()
        print(f'OFDockWidget:moveEvent()')
