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

#include <OpenFrames/PolyhedralCone.hpp>
#include <OpenFrames/WindowProxy.hpp>

using namespace OpenFrames;

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1280, 720, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");

  // Create a custom cone (where we specify clock & cone angles)
  {
    PolyhedralCone* customCone = new PolyhedralCone("Custom Cone");
    customCone->setPosition(-5.0, 0.0, 0.0);
    customCone->setConeColor(0.5, 0.5, 0.5, 0.5);
    customCone->setConeLength(5.0);
    root->addChild(customCone);

    // Set some clock/cone angles for the custom cone
    PolyhedralCone::AngleArray clockAngles =
    {
      osg::DegreesToRadians(10.0),
      osg::DegreesToRadians(30.0),
      osg::DegreesToRadians(90.0),
      osg::DegreesToRadians(180.0),
      osg::DegreesToRadians(270.0),
    };
    PolyhedralCone::AngleArray coneAngles =
    {
      osg::DegreesToRadians(10.0),
      osg::DegreesToRadians(30.0),
      osg::DegreesToRadians(40.0),
      osg::DegreesToRadians(60.0),
      osg::DegreesToRadians(30.0),
    };
    customCone->setVertexAngles(clockAngles, coneAngles);
  }
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->getCurrentView()->setDefaultViewDistance(10.0);
  myWindow->getGridPosition(0, 0)->getCurrentView()->resetView();
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
