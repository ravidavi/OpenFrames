/***********************************
 Copyright 2018 Ravishankar Mathur
 
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

#include <OpenFrames/FramePointer.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/WindowProxy.hpp>

using namespace OpenFrames;

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  root->setPosition(10.0, 10.0, 10.0);

  // Create a ReferenceFrame that will point towards another frame
  Sphere* current = new Sphere("Pointing");
  const double radius = 0.1;
  current->setRadius(radius);
  current->showAxes(ReferenceFrame::X_AXIS);
  current->showAxesLabels(ReferenceFrame::NO_AXES);
  current->moveXAxis(osg::Vec3d(radius, 0.0, 0.0), 1.0);
  root->addChild(current);
  
  // Create a ReferenceFrame for the destination
  Sphere* dest = new Sphere("Destination");
  dest->setRadius(radius);
  dest->showAxes(ReferenceFrame::NO_AXES);
  dest->showAxesLabels(ReferenceFrame::NO_AXES);
  root->addChild(dest);
  
  // Tell the pointing frame to point towards the destination
  FramePointer* fp = new FramePointer();
  fp->setPointingFrames(root, current, dest);
  current->getTransform()->setUpdateCallback(fp);
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  
  myWindow->startThread(); // Start window animation
  
  // This will control how quickly the destination's position is updated
  FramerateLimiter limiter;
  limiter.setDesiredFramerate(30.0); // In frames per second
  
  // Compute and update destination's position
  double tstep = 2.0*osg::PI/(60.0*limiter.getDesiredFramerate());
  for(double t = 0.0; myWindow->isRunning(); t += tstep)
  {
    limiter.frame();
    dest->setPosition(cos(t), sin(t), 0.0);
    current->setPosition(2.0*cos(t), -2.0*sin(t), cos(t));
  }
  
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
