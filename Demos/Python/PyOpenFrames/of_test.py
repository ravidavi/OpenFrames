# -*- coding: utf-8 -*-
"""
Simple OFInterface test

"""

from PyOpenFrames.OFInterface import *


win_id = 1
fm_id = 2
frame_name = "O"

ofwin_createproxy(50, 50, 800, 600, 1, 1, False, win_id)

ofcoordaxes_create(frame_name)

offm_create(fm_id)
offm_setframe()

ofwin_setscene(0, 0)

ofwin_start()

running = True
while running:
    running = ofwin_isrunning()
