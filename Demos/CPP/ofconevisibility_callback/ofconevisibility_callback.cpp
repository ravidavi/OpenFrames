/***********************************
 Copyright 2023 Ravishankar Mathur
 
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

#include <OpenFrames/EllipticCone.hpp>
#include <OpenFrames/SensorVisibilityCallback.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>

#include <iostream>


using namespace OpenFrames;

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1280, 720, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  root->showAxes(ReferenceFrame::NO_AXES);
  root->showAxesLabels(ReferenceFrame::NO_AXES);
  root->showNameLabel(false);
  OpenFrames::View *view = new OpenFrames::View(root, root);
  myWindow->getGridPosition(0, 0)->addView(view);
  view->setDefaultViewDistance(15.0);
  view->resetView();

  // Create an elliptic cone with specified semimajor/semiminor half-angles
  EllipticCone *ellipticCone = new EllipticCone("Elliptic Cone");
  {
    ellipticCone->setConeColor(0.1, 0.5, 0.6, 0.5);
    ellipticCone->setConeLength(20.0);
    ellipticCone->setPrimaryAngles(osg::DegreesToRadians(45.0), osg::DegreesToRadians(20.0));
    root->addChild(ellipticCone);
    OpenFrames::View *view = new OpenFrames::View(root, ellipticCone);
    myWindow->getGridPosition(0, 0)->addView(view);
    view->setDefaultViewParameters(osg::Vec3d(0, 0, 5.0), osg::Vec3d(), osg::Vec3(0, 1.0, 0));
    view->resetView();

    // Place apex at desired location and point boresight in desired direction
    // Vectors are relative to the parent object's reference frame
    osg::Vec3d origin(0, 0, 0);    // Cone apex location
    osg::Vec3d direction(0, 1, 0); // Cone boresight direction
    osg::Vec3d up(1, 0, 1);        // Cone +Y axis 
    ellipticCone->makeConeLookAt(origin, direction, up);
  }

  // Create a sphere that will block the cone's view
  Sphere *blocker = new Sphere("Blocker", 0, 1, 0, 1);
  {
    blocker->setRadius(3.0);
    blocker->showAxes(ReferenceFrame::NO_AXES);
    blocker->showAxesLabels(ReferenceFrame::NO_AXES);
    blocker->setPosition(-1, 5, 0);
    root->addChild(blocker);
  }

  // Create a target sphere that will move through the scene 
  // The target's color will change based on cone visibility
  Sphere *target = new Sphere("Target", 1, 0, 0, 1);
  {
    osg::ref_ptr<Trajectory> traj1 = new Trajectory;
    double pos[3];
    double rmag = 4.0;
    const double eps = 1.0e-14;
    for(double t = 0.0; t <= 2.0*osg::PI + eps; t += osg::PI / 90.0)
    {
      // Compute position that will enter the elliptical cone
      pos[0] = rmag * cos(t) + 8;
      pos[1] = 20.0;
      pos[2] = rmag * sin(t) - 2.0;

      // Add position
      traj1->addTime(t);
      traj1->addPosition(pos);
    }

    // Follow the trajectory (by default in LOOP mode)
    TrajectoryFollower *tf1 = new TrajectoryFollower(traj1);

    // Tell sphere to follow trajectory
    target->setRadius(2.0);
    target->showAxes(ReferenceFrame::NO_AXES);
    target->showAxesLabels(ReferenceFrame::NO_AXES);
    target->showNameLabel(false);
    target->setPosition(3, 10, -3); // For cases when update callback is commented out
    target->getTransform()->setUpdateCallback(tf1); // Comment out to use constant position above
    root->addChild(target);

    OpenFrames::View *view = new OpenFrames::View(root, target);
    myWindow->getGridPosition(0, 0)->addView(view);
  }
  
  /***************
  Line segments between frames
  */
  SensorVisibilityCallback *lsCallback = new SensorVisibilityCallback(root);
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(2, 0, 0)); // Segment between sensor and target
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(0, 2, 0)); // Segment between sensor and target
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(0, 0, 2)); // Segment between sensor and target
  lsCallback->ignoreReferenceFrame(ellipticCone);

  CustomLineSegments *cls = new CustomLineSegments("CustomLineSegment", 1, 1, 1, 1);
  cls->setLineSegmentCallback(lsCallback);
  cls->setLineWidth(2.0);
  cls->setLineShader("../../Demos/Shaders/Line_Pulse.frag");
  cls->showAxes(ReferenceFrame::NO_AXES);
  cls->showAxesLabels(ReferenceFrame::NO_AXES);
  cls->showNameLabel(false);
  root->addChild(cls);

  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);

  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
