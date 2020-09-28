"""
/***********************************
 Copyright 2020 Ravishankar Mathur
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 ***********************************/
 """

import math
import numpy as np
import OpenFrames.PyOF as PyOF


def dcmToQuat(dcm):
    # check for largest term
    q_sq = np.zeros(4)
    q_sq[0] = (1 + 2*dcm[0][0] - np.trace(dcm))/4
    q_sq[1] = (1 + 2*dcm[1][1] - np.trace(dcm))/4
    q_sq[2] = (1 + 2*dcm[2][2] - np.trace(dcm))/4
    q_sq[3] = (1 + np.trace(dcm))/4
    
    idx = np.argmax(q_sq)
    
    q = np.zeros(4)
    
    if idx == 0:
        q[0] = math.sqrt(q_sq[0])
        
        q[1] = (dcm[0][1] + dcm[1][0])/(4 * q[0])
        q[2] = (dcm[2][0] + dcm[0][2])/(4 * q[0])
        q[3] = (dcm[1][2] - dcm[2][1])/(4 * q[0])
    elif idx == 1:
        q[1] = math.sqrt(q_sq[1])
        
        q[0] = (dcm[0][1] + dcm[1][0])/(4 * q[1])
        q[2] = (dcm[1][2] + dcm[2][1])/(4 * q[1])
        q[3] = (dcm[2][0] - dcm[0][2])/(4 * q[1])
    elif idx == 2:
        q[2] = math.sqrt(q_sq[2])
        
        q[0] = (dcm[2][0] + dcm[0][2])/(4 * q[2])
        q[1] = (dcm[1][2] + dcm[2][1])/(4 * q[2])
        q[3] = (dcm[0][1] - dcm[1][0])/(4 * q[2])
    else:
        q[3] = math.sqrt(q_sq[3])
        
        q[0] = (dcm[1][2] - dcm[2][1])/(4 * q[3])
        q[1] = (dcm[2][0] - dcm[0][2])/(4 * q[3])
        q[2] = (dcm[0][1] - dcm[1][0])/(4 * q[3])
    
    # Enforce norm
    q /= np.linalg.norm(q)
    
    return q


# based off of osg::Matrixd::makeLookAt()
def getAttitudeQuat(eye, center, up):
    eye = np.array([PyOF.getOsgVec3d(eye, i) for i in range(3)])
    center = np.array([PyOF.getOsgVec3d(center, i) for i in range(3)])
    up = np.array([PyOF.getOsgVec3d(up, i) for i in range(3)])

    f = center - eye
    f /= np.linalg.norm(f)
    s = np.cross(f, up)
    s /= np.linalg.norm(s)
    u = np.cross(s, f)
    u /= np.linalg.norm(u)
    
    mat = np.column_stack((s, u, -f));
    q = dcmToQuat(mat)
    
    # get inverse
    q = PyOF.osgQuat(-q[0], -q[1], -q[2], q[3])
    
    return q
    

# Create the interface that represents a window
myWindow = PyOF.WindowProxy(30, 30, 1280, 720, 1, 1, False, False);

# Create a ReferenceFrame for the root
root = PyOF.ReferenceFrame("Root");
view = PyOF.View(root, root);
myWindow.getGridPosition(0, 0).addView(view);
view.setDefaultViewDistance(15.0);
view.resetView();

# Create a custom cone (where we specify clock & cone angles)
customCone = PyOF.PolyhedralCone("Custom Cone");
customCone.setConeColor(0.5, 0.5, 0.5, 0.5);
customCone.setConeLength(5.0);
root.addChild(customCone);
view = PyOF.View(root, customCone);
myWindow.getGridPosition(0, 0).addView(view);
view.setDefaultViewParameters(PyOF.osgVec3d(0, 0, 5.0), PyOF.osgVec3d(0,0,0), PyOF.osgVec3d(0, 1.0, 0));
view.resetView();

# Set some clock/cone angles for the custom cone
clockAngles = [10.0, 30.0, 90.0, 180.0, 270.0]
clockAngles = PyOF.AngleArray([angle * math.pi/180 for angle in clockAngles])
coneAngles = [10.0, 30.0, 40.0, 60.0, 30.0]
coneAngles = PyOF.AngleArray([angle * math.pi/180 for angle in coneAngles])
customCone.setVertexAngles(clockAngles, coneAngles);

# Place apex at desired location and point boresight in desired direction
# Vectors are relative to the parent object's reference frame
origin = PyOF.osgVec3d(-10, 0, 0);   # Cone apex location
direction = PyOF.osgVec3d(0, 0, 1);  # Cone boresight direction
up = PyOF.osgVec3d(1, 0, 0);         # Cone +Y axis
customCone.setPosition(origin);
q = getAttitudeQuat(PyOF.osgVec3d(0, 0, 0), direction, up)
customCone.setAttitude(q);


# Create an elliptic cone with specified semimajor/semiminor half-angles
ellipticCone = PyOF.EllipticCone("Elliptic Cone");
ellipticCone.setConeColor(0.1, 0.5, 0.6, 0.5);
ellipticCone.setConeLength(5.0);
ellipticCone.setPrimaryAngles(45.0 * math.pi/180, 20.0 * math.pi/180);
root.addChild(ellipticCone);
view = PyOF.View(root, ellipticCone);
myWindow.getGridPosition(0, 0).addView(view);
view.setDefaultViewParameters(PyOF.osgVec3d(0, 0, 5.0), PyOF.osgVec3d(0,0,0), PyOF.osgVec3d(0, 1.0, 0));
view.resetView();

# Place apex at desired location and point boresight in desired direction
# Vectors are relative to the parent object's reference frame
origin = PyOF.osgVec3d(10, 0, 0);   # Cone apex location
direction = PyOF.osgVec3d(0, 1, 0); # Cone boresight direction
up = PyOF.osgVec3d(1, 0, 1);        # Cone +Y axis 
ellipticCone.setPosition(origin);
q = getAttitudeQuat(PyOF.osgVec3d(0, 0, 0), direction, up)
ellipticCone.setAttitude(q);

# Create a rectangular cone with specified x/y half-angles
rectangularCone = PyOF.RectangularCone("Rectangular Cone");
rectangularCone.setPosition(0, 0, 10.0);
rectangularCone.setConeColor(0.1, 0.5, 0.6, 0.5);
rectangularCone.setConeLength(5.0);
rectangularCone.setPrimaryAngles(45.0 * math.pi/180, 20.0 * math.pi/180);
root.addChild(rectangularCone);

# Create a manager to handle access to the scene
fm = PyOF.FrameManager();
fm.setFrame(root);

# Add the scene to the window
myWindow.setScene(fm, 0, 0);

myWindow.startThread(); # Start window animation
myWindow.join(); # Wait for window animation to finish

