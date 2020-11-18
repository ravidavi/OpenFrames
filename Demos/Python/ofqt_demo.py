# -*- coding: utf-8 -*-
"""
Launches a demonstration of OpenFrames managed within a PyQt5 framework

"""

import sys
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QSurfaceFormat
import OFInterfaces.PyQtOF as PyQtOF
import OFInterfaces.PyOF as PyOF

class MyOFDemoWin1(PyQtOF.Window):
    """
    Inherits PyQtOF.Window for a simple window showing only a Coordinate Axes

    Attributes
    ----------
    _frame_manager_id : int
        The FrameManager placed in the first grid location of the window proxy
    _ref_frame_name : basestring
        The name of the primary reference frame

    """
    def __init__(self):
      """
      Instantiate a window
      """
      super().__init__(1, 1, 1) # 1x1 window with id 1

      self._frame_manager_id = 1
      self._ref_frame_name = "CoordinateAxes"
      
      root = PyOF.CoordinateAxes(self._ref_frame_name)
      
      # Create a manager to handle access to the scene
      fm = PyOF.FrameManager(root);
      
      # Add the scene to the window
      self._window_proxy.setScene(fm, 0, 0);

class MyOFDemoWin2(PyQtOF.Window):
    """
    Inherits PyQtOF.Window for a simple window showing only a Sphere
    
    Attributes
    ----------
    _frame_manager_id : int
    The FrameManager placed in the first grid location of the window proxy
    _ref_frame_name : basestring
    The name of the primary reference frame
    
    """
    def __init__(self):
      """
      Instantiate a window
      """
      super().__init__(1, 1, 2) # 1x1 window with id 2
      
      self._frame_manager_id = 2
      self._ref_frame_name = "Sphere"
      
      root = PyOF.Sphere(self._ref_frame_name)
      
      # Create a manager to handle access to the scene
      fm = PyOF.FrameManager(root);
      
      # Add the scene to the window
      self._window_proxy.setScene(fm, 0, 0);

if __name__ == '__main__':
    app = QApplication(sys.argv)
    
    # Set depth buffer and MSAA
    fmt = QSurfaceFormat()
    fmt.setDepthBufferSize(24)
    fmt.setSamples(4)
    QSurfaceFormat.setDefaultFormat(fmt)
    
    # Create first window
    ex1 = PyQtOF.Widget(MyOFDemoWin1)
    ex1.setWindowTitle('PyQt5 OpenFrames Window 1')
    ex1.setGeometry(50, 50, 1024, 768)
    ex1.show()
    
    # Create second window
    ex2 = PyQtOF.Widget(MyOFDemoWin2)
    ex2.setWindowTitle('PyQt5 OpenFrames Window 2')
    ex2.setGeometry(100, 100, 1024, 768)
    ex2.show()
    
    # Start Qt application
    ret = app.exec_()
    sys.exit(ret)
