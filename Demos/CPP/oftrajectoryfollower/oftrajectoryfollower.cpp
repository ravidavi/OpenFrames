/***********************************
   Copyright 2017 Ravishankar Mathur

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
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/Math>

using namespace OpenFrames;

double tscale = 0.1; // Animation speedup relative to real time
TimeManagementVisitor *tmv;
ReferenceFrame *root;

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  static bool paused = false;
  static bool stereo = false;
  
  // Pause/unpause animation
  if(*key == 'p')
  {
    paused = !paused;
    tmv->setPauseState(true, paused);
    root->getTransform()->accept(*tmv);
    tmv->setPauseState(false, paused);
  }
  
  // Reset time to epoch. All ReferenceFrames that are following
  // a Trajectory will return to their starting positions.
  else if(*key == 'r')
  {
    tmv->setReset(true);
    root->getTransform()->accept(*tmv);
    tmv->setReset(false);
  }
  
  // Speed up time
  else if((*key == '+') || (*key == '='))
  {
    tscale += 0.01;
    tmv->setTimeScale(true, tscale);
    root->getTransform()->accept(*tmv);
    tmv->setTimeScale(false, tscale);
  }
  
  // Slow down time
  else if((*key == '-') || (*key == '_'))
  {
    tscale -= 0.01;
    tmv->setTimeScale(true, tscale);
    root->getTransform()->accept(*tmv);
    tmv->setTimeScale(false, tscale);
  }
}

/** 
 Demonstrates TrajectoryFollower with multiple followed trajectories
 **/
int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  myWindow->setKeyPressCallback(KeyPressCallback); // Specify keypress callback
  
  // Create the object that will handle keyboard input
  // This includes pausing, resetting, modifying time, etc...
  osg::ref_ptr<TimeManagementVisitor> mytmv = new TimeManagementVisitor;
  tmv = mytmv.get();
  
  // Create a drawable trajectory to hold all trajectory artists
  // This will also act as the root of the reference frame hierarchy
  DrawableTrajectory *drawtraj = new DrawableTrajectory("drawtraj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);
  root = drawtraj;
  
  /***************
   Use-case 1: Trajectories that are continuous and nonoverlapping
   The trajectories form a continuous circle around the origin
   */
  const int numtraj1 = 3;
  osg::ref_ptr<Trajectory> trajset1[numtraj1];
  TrajectoryFollower *tf1 = new TrajectoryFollower;
  for(int i = 0; i < numtraj1; ++i)
  {
    trajset1[i] = new Trajectory; // Create trajectory
    
    // Each trajectory is 1/numtraj1 of the full circle
    double pos[3];
    double rmag = 1.0;
    double t0 = (double)i / (double)numtraj1 * 2.0*osg::PI;
    double tf = (double)(i+1) / (double)numtraj1 * 2.0*osg::PI;
    const double eps = 1.0e-14;
    for(double t = t0; t <= tf+eps; t += osg::PI/90.0)
    {
      // Compute position along circle
      pos[0] = rmag*cos(t);
      pos[1] = 0.0;
      pos[2] = rmag*sin(t);
      
      // Add position
      trajset1[i]->addTime(t);
      trajset1[i]->addPosition(pos);
    }
    
    // Create a CurveArtist for the trajectory.  By default the CurveArtist
    // will use x/y/z positions from the trajectory for plotting.
    CurveArtist *ca = new CurveArtist(trajset1[i]);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(1, 0, 0);
    drawtraj->addArtist(ca);
    
    // Follow the trajectory (by default in LOOP mode)
    tf1->followTrajectory(trajset1[i]);
  }
  
  // Create a drawable trajectory to hold the center marker
  // Tell frame to follow the trajectories
  DrawableTrajectory *frame1 = new DrawableTrajectory("Continuous", 1, 0, 0, 1);
  frame1->showAxes(ReferenceFrame::NO_AXES);
  frame1->showAxesLabels(ReferenceFrame::NO_AXES);
  frame1->getTransform()->setUpdateCallback(tf1);
  drawtraj->addChild(frame1);
  
  // Create a MarkerArtist to draw the center marker
  MarkerArtist *frame1centermarker = new MarkerArtist();
  frame1centermarker->setMarkerShader("Shaders/Marker_Circle.frag");
  frame1centermarker->setMarkerSize(15);
  frame1centermarker->setMarkerColor(MarkerArtist::START, 1, 0, 0);
  frame1->addArtist(frame1centermarker);
  
  /***************
   Use-case 2: Trajectories that overlap
   The trajectories overlap and get farther from the origin
   */
  const int numtraj2 = 6;
  osg::ref_ptr<Trajectory> trajset2[numtraj2];
  TrajectoryFollower *tf2 = new TrajectoryFollower;
  for(int i = 0; i < numtraj2; ++i)
  {
    trajset2[i] = new Trajectory; // Create trajectory
    
    // Each trajectory is a portion of the full circle, but overlaps
    // with the previous trajectory and gets a bit farther away
    double pos[3];
    double rmag = 1.0 + (double)i / (double)numtraj2;
    double t0 = (double)i / (2.0*numtraj2) * 2.0*osg::PI;
    double tf = ((double)i+2.5) / (2.0*numtraj2) * 2.0*osg::PI;
    const double eps = 1.0e-14;
    for(double t = t0; t <= tf+eps; t += osg::PI/90.0)
    {
      // Compute position along circle
      pos[0] = 3.0 + rmag*cos(t);
      pos[1] = 0.0;
      pos[2] = rmag*sin(t);
      
      // Add position
      trajset2[i]->addTime(t);
      trajset2[i]->addPosition(pos);
    }
    
    // Create a CurveArtist for the trajectory.  By default the CurveArtist
    // will use x/y/z positions from the trajectory for plotting.
    CurveArtist *ca = new CurveArtist(trajset2[i]);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(0, 1, 0);
    drawtraj->addArtist(ca);
    
    // Follow the trajectory (by default in LOOP mode)
    tf2->followTrajectory(trajset2[i]);
  }
  
  // Create a drawable trajectory to hold the center marker
  // Tell frame to follow the trajectories
  DrawableTrajectory *frame2 = new DrawableTrajectory("Overlapping", 0, 1, 0, 1);
  frame2->showAxes(ReferenceFrame::NO_AXES);
  frame2->showAxesLabels(ReferenceFrame::NO_AXES);
  frame2->getTransform()->setUpdateCallback(tf2);
  drawtraj->addChild(frame2);
  
  // Create a MarkerArtist to draw the center marker
  MarkerArtist *frame2centermarker = new MarkerArtist();
  frame2centermarker->setMarkerShader("Shaders/Marker_Plus.frag");
  frame2centermarker->setMarkerSize(15);
  frame2centermarker->setMarkerColor(MarkerArtist::START, 0, 1, 0);
  frame2->addArtist(frame2centermarker);
  
  /***************
   Use-case 3: Trajectories with gaps
   The trajectories have gaps between them and get farther from the origin
   */
  const int numtraj3 = 6;
  osg::ref_ptr<Trajectory> trajset3[numtraj3];
  TrajectoryFollower *tf3 = new TrajectoryFollower;
  for(int i = 0; i < numtraj3; ++i)
  {
    trajset3[i] = new Trajectory; // Create trajectory
    
    // Each trajectory is a portion of the full circle, but has a gap
    // after the previous trajectory and gets a bit farther away
    double pos[3];
    double rmag = 1.0 + (double)i / (double)numtraj3;
    double t0 = (double)i / (double)numtraj3 * 2.0*osg::PI;
    double tf = ((double)i+0.5) / (double)numtraj3 * 2.0*osg::PI;
    const double eps = 1.0e-14;
    for(double t = t0; t <= tf+eps; t += osg::PI/90.0)
    {
      // Compute position along circle
      pos[0] = rmag*cos(t);
      pos[1] = 0.0;
      pos[2] = 3.0 + rmag*sin(t);
      
      // Add position
      trajset3[i]->addTime(t);
      trajset3[i]->addPosition(pos);
    }
    
    // Create a CurveArtist for the trajectory.  By default the CurveArtist
    // will use x/y/z positions from the trajectory for plotting.
    CurveArtist *ca = new CurveArtist(trajset3[i]);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(0, 0, 1);
    drawtraj->addArtist(ca);
    
    // Follow the trajectory (by default in LOOP mode)
    tf3->followTrajectory(trajset3[i]);
  }
  
  // Create a drawable trajectory to hold the center marker
  // Tell frame to follow the trajectories
  DrawableTrajectory *frame3 = new DrawableTrajectory("Gaps", 0, 0, 1, 1);
  frame3->showAxes(ReferenceFrame::NO_AXES);
  frame3->showAxesLabels(ReferenceFrame::NO_AXES);
  frame3->getTransform()->setUpdateCallback(tf3);
  drawtraj->addChild(frame3);
  
  // Create a MarkerArtist to draw the center marker
  MarkerArtist *frame3centermarker = new MarkerArtist();
  frame3centermarker->setMarkerShader("Shaders/Marker_X.frag");
  frame3centermarker->setMarkerSize(15);
  frame3centermarker->setMarkerColor(MarkerArtist::START, 0, 0, 1);
  frame3->addArtist(frame3centermarker);
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(drawtraj);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
