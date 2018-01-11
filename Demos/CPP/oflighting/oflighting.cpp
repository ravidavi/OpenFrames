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

#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/WindowProxy.hpp>

using namespace OpenFrames;

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 1, 1, false, false);
  
  Sphere* sphere = new Sphere("My Sphere");
  sphere->setColor(1.0, 1.0, 1.0, 1.0);
  sphere->showAxes(ReferenceFrame::NO_AXES);
  sphere->showAxesLabels(ReferenceFrame::NO_AXES);
  sphere->showNameLabel(false);
  //sphere->setTextureMap("Images/EarthTexture.bmp");
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(sphere);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
