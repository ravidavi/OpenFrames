/***********************************
   Copyright 2019 Ravishankar Mathur

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

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/Math>

using namespace OpenFrames;

const double pathRadius = 100.0;
osg::ref_ptr<WindowProxy> theWindow;

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  // Pause/unpause animation
  if(*key == 'p')
  {
    theWindow->pauseTime(!theWindow->isTimePaused());
  }
  
  // Reset time to epoch. All ReferenceFrames that are following
  // a Trajectory will return to their starting positions.
  else if(*key == 'r')
  {
    theWindow->setTime(0.0);
  }
  
  // Speed up time
  else if((*key == '+') || (*key == '='))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() + 0.01);
  }
  
  // Slow down time
  else if((*key == '-') || (*key == '_'))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() - 0.01);
  }
}

/** 
 Demonstrates using OSG's particle system with a model in OpenFrames
 **/
int main()
{
  // Create the root frame
  ReferenceFrame* root = new ReferenceFrame("root");
  root->moveXAxis(osg::Vec3d(), pathRadius / 2.0);
  root->moveYAxis(osg::Vec3d(), pathRadius / 2.0);
  root->moveZAxis(osg::Vec3d(), pathRadius / 2.0);

  // Create a drawable trajectory to hold all trajectory artists
  // This will also act as the root of the reference frame hierarchy
  DrawableTrajectory *drawtraj = new DrawableTrajectory("drawtraj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);
  root->addChild(drawtraj);
  
  // Create a circular trajectory that the model will follow
  osg::ref_ptr<Trajectory> traj = new Trajectory;
  {
    // Each trajectory is 1/numtraj1 of the full circle
    double pos[3];
    osg::Quat att;
    double rmag = 100.0;
    const double eps = 1.0e-14;
    for(double t = 0.0; t <= 2.0*osg::PI + eps; t += osg::PI / 90.0)
    {
      // Compute position along circle
      pos[0] = rmag * cos(t);
      pos[1] = rmag * sin(t);
      pos[2] = 0.0;

      // Compute orientation tangent to circle
      att.makeRotate(t, osg::Vec3d(0, 0, 1));

      // Add position
      traj->addTime(t);
      traj->addPosition(pos);
      traj->addAttitude(att.x(), att.y(), att.z(), att.w());
    }

    // Create a CurveArtist for the trajectory. By default the CurveArtist
    // will use plot the trajectory's x/y/z positions.
    CurveArtist *ca = new CurveArtist(traj);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(1, 0, 0);
    drawtraj->addArtist(ca);
  }

  // Load the model and have it follow the trajectory
  Model *cessnafire = new Model("cessna");
  cessnafire->setModel("cessna.osg");
  cessnafire->getTransform()->setUpdateCallback(new TrajectoryFollower(traj));
  cessnafire->setModelAttitude(osg::Quat(osg::PI, osg::Vec3d(0, 0, 1))); // Model faces in -Y direction
  root->addChild(cessnafire);
  
  // Create a window
  theWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  theWindow->setKeyPressCallback(KeyPressCallback); // Specify keypress callback

  // View the model
  View *view = new View(root, cessnafire);
  theWindow->getGridPosition(0, 0)->addView(view);

  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  theWindow->setScene(fm, 0, 0);
  theWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  theWindow->startThread(); // Start window animation
  theWindow->join(); // Wait for window animation to finish
  
  return 0;
}
