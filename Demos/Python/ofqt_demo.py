# -*- coding: utf-8 -*-
"""
Launches a demonstration of OpenFrames managed within a PyQt5 framework

"""

import sys
from PyQt5.QtWidgets import QApplication
import OpenFrames.PyQtOF as PyQtOF
from OpenFrames.PyOFInterface import *


class MyOFDemo(PyQtOF.Window):
    """
    Inherits PyQtOF.Window for a simple demonstration showing only a Coordinate Axes

    Attributes
    ----------
    _frame_manager_id : int
        The FrameManager placed in the first grid location of the window proxy
    _ref_frame_name : basestring
        The name of the primary reference frame

    """
    def __init__(self):
        """
        Instantiate a demonstration

        """
        super().__init__()

        self._frame_manager_id = 0
        self._ref_frame_name = "Origin"

        ofcoordaxes_create(self._ref_frame_name)

        offm_create(self._frame_manager_id)
        offm_setframe()
        ofwin_setscene(0, 0)


if __name__ == '__main__':
    of_initialize()
    app = QApplication(sys.argv)
    ex = PyQtOF.Widget(MyOFDemo)
    ex.setWindowTitle('PyQt5 OpenFrames Demonstration')
    ex.setGeometry(50, 50, 1024, 768)
    ex.show()
    ret = app.exec_()
    of_cleanup()
    sys.exit(ret)
