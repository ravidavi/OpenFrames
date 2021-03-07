# -*- coding: utf-8 -*-
"""
Launches a demonstration of OpenFrames managed within a PyQt5 framework

"""

import sys
import math
from PyQt5.QtWidgets import *
from PyQt5.QtGui import QSurfaceFormat
import OFInterfaces.PyQtOF as PyQtOF
import OFInterfaces.PyOF as PyOF

class MyOFDemoWin(PyQtOF.OFWindow):
    """
    Inherits PyQtOF.Window for a simple window showing only a Coordinate Axes
    This window is embedded in a tab widget (see below)

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
      
      self.root = PyOF.CoordinateAxes(self._ref_frame_name)
      mainModel = PyOF.Model("Model0")
      #mainModel.setModel("OsirisRex-2013-comp.lwo")
      mainModel.setModel("OsirisRex-2013-comp.lwo")
      self.root.addChild(mainModel)
      numModels = 10
      for i in range(numModels):
        model = PyOF.Model("Model" + str(i))
        model.shareModel(mainModel)
        model.setPosition(5000.0*math.cos(2.0*math.pi*i/numModels), 5000.0*math.sin(2.0*math.pi*i/numModels), 0)
        self.root.addChild(model)
      
      # Create a manager to handle access to the scene
      self.fm = PyOF.FrameManager(self.root);
      
      # Add the scene to the window
      self._window_proxy.setScene(self.fm, 0, 0);
        
if __name__ == '__main__':
    app = QApplication(sys.argv)
    
    # Set depth buffer and MSAA
    fmt = QSurfaceFormat()
    fmt.setDepthBufferSize(24)
    fmt.setSamples(4)
    QSurfaceFormat.setDefaultFormat(fmt)
    
    # Create first window
    ex1 = PyQtOF.OFWidget(MyOFDemoWin)
    ex1.ofwindow.root.setName("CoordAxes1")
    ex1.setWindowTitle('PyQt5 OpenFrames Window 1')
    ex1.setGeometry(100, 100, 1024, 768)
    ex1.show()
    
    # Create second window that shares the first window's scene
    ex2 = PyQtOF.OFWidget(MyOFDemoWin)
    fm1 = ex1.ofwindow.fm
    ex2.ofwindow._window_proxy.setScene(fm1, 0, 0)
    ex2.setWindowTitle('PyQt5 OpenFrames Window 2')
    ex2.setGeometry(200, 200, 1024, 768)
    ex2.show()
    
    # Start Qt application
    ret = app.exec_()
    sys.exit(ret)
