# -*- coding: utf-8 -*-
"""
Simple PyOF example

"""

import OFInterfaces.PyOF as PyOF


# Create a reference frame for the scene root
root = PyOF.CoordinateAxes("Origin")

# Create a frame manager to control access to the scene
fm = PyOF.FrameManager(root)

# Setup a window into which the scene will be rendered
myWindow = PyOF.WindowProxy(50, 50, 800, 600, 1, 1, False, False)
myWindow.setScene(fm, 0, 0)
myWindow.startThread();
myWindow.join();
