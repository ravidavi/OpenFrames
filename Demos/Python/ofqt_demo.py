# -*- coding: utf-8 -*-
"""
Launches a demonstration of OpenFrames managed within a qtpy framework

Copyright (c) 2021 Emergent Space Technologies, Inc.
"""

# OS-specific modifications before importing modules
import os
import sys
import platform
currpath = os.path.abspath(os.path.dirname(__file__))

if platform.system() == 'Windows': # Windows
    # Python 3.8 no longer searches the topmost (bin) directory
    # when loading shared library dependencies, so we must add it explicitly
    if sys.version_info[:2] >= (3,8):
        os.add_dll_directory(currpath)
else: # OSX/Linux
    # Tell OSG where to find plugins
    osglibpath = currpath + os.sep + ".." + os.sep + "lib"
    os.environ['OSG_LIBRARY_PATH'] = osglibpath
    
    if platform.system() == 'Darwin':
        # On OSX 10.15+, some fonts (e.g. Arial.ttf) are moved to the Supplemental folder
        os.environ['OSG_FILE_PATH'] = str(os.environ.get('OSG_FILE_PATH')) + os.pathsep + "/System/Library/Fonts/Supplemental"

# Import modules
from qtpy.QtWidgets import *
from qtpy.QtGui import QSurfaceFormat
from qtpy.QtCore import Qt
import OFInterfaces.PyQtOF as PyQtOF
import OFInterfaces.PyOF as PyOF

from qtpy.QtCore import (QCoreApplication, QDate, QDateTime, QLocale,
    QMetaObject, QObject, QPoint, QRect,
    QSize, QTime, QUrl, Qt)
from qtpy.QtGui import (QBrush, QColor, QConicalGradient, QCursor,
    QFont, QFontDatabase, QGradient, QIcon,
    QImage, QKeySequence, QLinearGradient, QPainter,
    QPalette, QPixmap, QRadialGradient, QTransform)
from qtpy.QtWidgets import (QApplication, QDockWidget, QMainWindow, QMenuBar,
    QSizePolicy, QStatusBar, QWidget)

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        if not MainWindow.objectName():
            MainWindow.setObjectName(u"MainWindow")
        MainWindow.resize(800, 600)
        
        self.centralwidget = QWidget(MainWindow)
        self.centralwidget.setObjectName(u"centralwidget")
        MainWindow.setCentralWidget(self.centralwidget)
        
        self.menubar = QMenuBar(MainWindow)
        self.menubar.setObjectName(u"menubar")
        self.menubar.setGeometry(QRect(0, 0, 800, 21))
        MainWindow.setMenuBar(self.menubar)
        
        self.statusbar = QStatusBar(MainWindow)
        self.statusbar.setObjectName(u"statusbar")
        MainWindow.setStatusBar(self.statusbar)
        
        self.dockWidget = QDockWidget(MainWindow)
        self.dockWidget.setObjectName(u"dockWidget")
        self.dockWidgetContents = QWidget()
        self.dockWidgetContents.setObjectName(u"dockWidgetContents")
        self.dockWidget.setWidget(self.dockWidgetContents)
        MainWindow.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, self.dockWidget)
        
        self.dockWidget_2 = QDockWidget(MainWindow)
        self.dockWidget_2.setObjectName(u"dockWidget_2")
        self.dockWidgetContents_2 = QWidget()
        self.dockWidgetContents_2.setObjectName(u"dockWidgetContents_2")
        self.dockWidget_2.setWidget(self.dockWidgetContents_2)
        MainWindow.addDockWidget(Qt.DockWidgetArea.RightDockWidgetArea, self.dockWidget_2)
        
        self.dockWidget_3 = QDockWidget(MainWindow)
        self.dockWidget_3.setObjectName(u"dockWidget_3")
        self.dockWidgetContents_3 = QWidget()
        self.dockWidgetContents_3.setObjectName(u"dockWidgetContents_3")
        self.dockWidget_3.setWidget(self.dockWidgetContents_3)
        MainWindow.addDockWidget(Qt.DockWidgetArea.TopDockWidgetArea, self.dockWidget_3)

        self.retranslateUi(MainWindow)

        QMetaObject.connectSlotsByName(MainWindow)
    # setupUi

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(QCoreApplication.translate("MainWindow", u"MainWindow", None))
    # retranslateUi

class MacMainWindow(QMainWindow, Ui_MainWindow):
    """
    Reusable widget used to display a list of bodies and add/delete to a "selected" list.

    """

    def __init__(self):
        """
        Constructor. Calls APIs to get list of available and selected bodies.

        @param id: client using the widget.
        """
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.ofDockWidget = PyQtOF.OFDockWidget(window_type=MyOFDemoWin2)
        self.addDockWidget(Qt.DockWidgetArea.TopDockWidgetArea, self.ofDockWidget)

    def applyFont(self):
        pass
        
    def closeEvent(self, event):
        self.ofDockWidget.stopRendering()
        
class MyOFDemoWin1(PyQtOF.OFWindow):
    """
    Inherits PyQtOF.Window for a simple window showing only a Coordinate Axes
    This window is embedded in a tab widget (see below)

    """
    def __init__(self):
        """
        Instantiate a window
        """
        super().__init__(1, 1) # 1x1 window

        # Create scene root
        root = PyOF.CoordinateAxes("CoordinateAxes")

        # Create a manager to handle access to the scene
        fm = PyOF.FrameManager(root);

        # Add the scene to the window
        self.windowProxy.setScene(fm, 0, 0);

class MyOFDemoWin2(PyQtOF.OFWindow):
    """
    Inherits PyQtOF.Window for a simple standalone window showing only a Sphere
    
    """
    def __init__(self):
        """
        Instantiate a window
        """
        super().__init__(1, 1) # 1x1 window

        # Create scene root
        root = PyOF.Sphere("Sphere")

        # Create a manager to handle access to the scene
        fm = PyOF.FrameManager(root);

        # Add the scene to the window
        self.windowProxy.setScene(fm, 0, 0);

class TabWindow(QWidget):
    """
    Inherits QWidget for a simple standalone window showing a Tab widget
    
    """
    def __init__(self):
        QWidget.__init__(self)
        layout = QGridLayout()
        self.setLayout(layout)

        label = QLabel("Widget in a Tab.")

        self.ofwidget = PyQtOF.OFWidget(MyOFDemoWin1)
        self.ofwidget.setWindowTitle('qtpy OpenFrames Window 1')
        self.ofwidget.setGeometry(50, 50, 1024, 768)

        tabwidget = QTabWidget()
        tabwidget.addTab(self.ofwidget, "OpenFrames Tab")
        tabwidget.addTab(label, "Label Tab")

        layout.addWidget(tabwidget, 0, 0)
      
    def closeEvent(self, event):
        self.ofwidget.stopRendering()
        
if __name__ == '__main__':
    app = QApplication(sys.argv)
    
    # macOS requires some OpenGL context management to be performed from the main thread
    # which means multiple threads must be using the OpenGL context.
    # See: https://codereview.qt-project.org/c/qt/qtbase/+/155170
    if platform.system() == 'Darwin': # macOS
        app.setAttribute(Qt.ApplicationAttribute.AA_DontCheckOpenGLContextThreadAffinity)

    # Set depth buffer and MSAA
    fmt = QSurfaceFormat()
    fmt.setDepthBufferSize(24)
    fmt.setSamples(4)
    QSurfaceFormat.setDefaultFormat(fmt)
    
    # Create main window with docked widgets
    exMainWindow = MacMainWindow()
    exMainWindow.show()

    # Create tab window
    #exTabWindow = TabWindow()
    #exTabWindow.show()
    
    # Create standalone window
    #exStandaloneWindow = PyQtOF.OFWidget(MyOFDemoWin2)
    #exStandaloneWindow.setWindowTitle('qtpy OpenFrames Window 2')
    #exStandaloneWindow.setGeometry(100, 100, 1024, 768)
    #exStandaloneWindow.show()
    
    # Start Qt application
    ret = app.exec_()
    sys.exit(ret)
