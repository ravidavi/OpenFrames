# -*- coding: utf-8 -*-
"""
Launches a demonstration of OpenFrames managed within a PyQt5 framework

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
from PyQt5.QtWidgets import *
from PyQt5.QtGui import QSurfaceFormat
from PyQt5.QtCore import Qt
import OFInterfaces.PyQtOF as PyQtOF
import OFInterfaces.PyOF as PyOF

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
        self.ofwidget.setWindowTitle('PyQt5 OpenFrames Window 1')
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
    
    # Create first window
    ex1 = TabWindow()
    ex1.show()
    
    # Create second window
    ex2 = PyQtOF.OFWidget(MyOFDemoWin2)
    ex2.setWindowTitle('PyQt5 OpenFrames Window 2')
    ex2.setGeometry(100, 100, 1024, 768)
    ex2.show()
    
    # Start Qt application
    ret = app.exec_()
    sys.exit(ret)
