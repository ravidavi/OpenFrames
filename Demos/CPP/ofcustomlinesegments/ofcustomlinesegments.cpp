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

/**
 Demonstrates use of CustomLineSegments to connect ReferenceFrames with line segments
 **/

#include <OpenFrames/CustomLineSegments.hpp>
#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/Math>

using namespace OpenFrames;

ReferenceFrame *root;
WindowProxy *theWindow;

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

/** Callback that dynamicall computes the line segment vertex data */
class LineSegmentCallback : public CustomLineSegments::Callback
{
public:
  LineSegmentCallback() {}

  // Required callback function
  virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB)
  {
    ReferenceFrame *frameA = _framePairs[segID].first;
    ReferenceFrame *frameB = _framePairs[segID].second;

    osg::Vec3d posAd, posBd;
    frameA->getPosition(posAd);
    frameB->getPosition(posBd);
    posA.set(posAd);
    posB.set(posBd);
    colorA = frameA->getColor();
    colorA.a() = 0.1;
    colorB = frameB->getColor();
    colorB.a() = 0.1;
  }

  // Add a pair of ReferenceFrames that will have a line segment drawn between them
  void addFramePair(ReferenceFrame *frameA, ReferenceFrame *frameB)
  {
    _framePairs.push_back(FramePair(frameA, frameB));
  }

  unsigned int getNumFramePairs() const
  {
    return _framePairs.size();
  }

protected:
  virtual ~LineSegmentCallback() {}

  typedef std::pair<ReferenceFrame*, ReferenceFrame*> FramePair;
  std::vector<FramePair> _framePairs;
};

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  myWindow->setKeyPressCallback(KeyPressCallback); // Specify keypress callback
  theWindow = myWindow;
  
  // Create a root ReferenceFrame
  root = new ReferenceFrame("drawtraj", 1, 0, 0, 0.9);
  root->showAxes(ReferenceFrame::NO_AXES);
  root->showAxesLabels(ReferenceFrame::NO_AXES);
  root->showNameLabel(false);
  
  /***************
  Frame 1: Circular trajectory
  */
  osg::ref_ptr<Trajectory> traj1 = new Trajectory;    
  double pos[3];
  double rmag = 1.0;
  const double eps = 1.0e-14;
  for(double t = 0.0; t <= 2.0*osg::PI + eps; t += osg::PI / 90.0)
  {
    // Compute position along circle
    pos[0] = rmag * cos(t);
    pos[1] = 0.0;
    pos[2] = rmag * sin(t);

    // Add position
    traj1->addTime(t);
    traj1->addPosition(pos);
  }

  // Follow the trajectory (by default in LOOP mode)
  TrajectoryFollower *tf1 = new TrajectoryFollower(traj1);

  // Create a frame to follow the trajectory
  Sphere *frame1 = new Sphere("Circle", 1, 0, 0, 1);
  frame1->setRadius(0.1);
  frame1->showAxes(ReferenceFrame::NO_AXES);
  frame1->showAxesLabels(ReferenceFrame::NO_AXES);
  frame1->getTransform()->setUpdateCallback(tf1);
  root->addChild(frame1);
  
  /***************
  Frame 2: Vertical trajectory
  */
  osg::ref_ptr<Trajectory> traj2 = new Trajectory;
  { // First point along x-axis
    traj2->addTime(0.0);
    traj2->addPosition(rmag + 1.0, 0.0, 0.0);
  }
  { // Second point above first
    traj2->addTime(5.0);
    traj2->addPosition(rmag + 1.0, 0.0, 5.0);
  }

  // Follow the trajectory (by default in LOOP mode)
  TrajectoryFollower *tf2 = new TrajectoryFollower(traj2);

  // Create a frame to follow the trajectory
  Sphere *frame2 = new Sphere("Vertical", 0, 1, 0, 1);
  frame2->setRadius(0.1);
  frame2->showAxes(ReferenceFrame::NO_AXES);
  frame2->showAxesLabels(ReferenceFrame::NO_AXES);
  frame2->getTransform()->setUpdateCallback(tf2);
  root->addChild(frame2);
  
  /***************
  Line segments between frames
  */
  LineSegmentCallback *lsCallback = new LineSegmentCallback;
  lsCallback->addFramePair(frame1, frame2); // Segment between frames 1 and 2

  CustomLineSegments *cls = new CustomLineSegments("CustomLineSegment", 1, 1, 1, 1);
  cls->setLineSegmentCallback(lsCallback);
  cls->setNumSegments(lsCallback->getNumFramePairs());
  cls->setLineWidth(2.0);
  cls->setLineShader("Shaders/Line_Pulse.frag");
  cls->showAxes(ReferenceFrame::NO_AXES);
  cls->showAxesLabels(ReferenceFrame::NO_AXES);
  root->addChild(cls);

  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
