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

/**
 Demonstrates use of CustomLineSegments to connect ReferenceFrames with line segments
 **/

#include <OpenFrames/CustomLineSegments.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/Math>

using namespace OpenFrames;

/** Callback that dynamically computes the line segment vertex data */
class LineSegmentCallback : public CustomLineSegments::Callback
{
public:
  LineSegmentCallback() {}

  // Required: get number of segments
  virtual unsigned int getNumSegments() const
  {
    return _framePairs.size();
  }

  // Required: get data for given segment
  virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) const
  {
    // Vertex A corresponds to Frame A
    ReferenceFrame *frameA = _framePairs[segID].first;
    posA.set(frameA->getPosition());
    colorA = frameA->getColor();
    colorA.a() = 0.4; // Make line color slightly transparent

    // Vertex B corresponds to Frame B
    ReferenceFrame *frameB = _framePairs[segID].second;
    posB.set(frameB->getPosition());
    colorB = frameB->getColor();
    colorB.a() = 0.4; // Make line color slightly transparent
  }

  // Add a pair of ReferenceFrames that will have a line segment drawn between them
  void addSegment(ReferenceFrame *frameA, ReferenceFrame *frameB)
  {
    // Make sure this callback's data is not being used
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(mMutex);

    _framePairs.push_back(FramePair(frameA, frameB));
  }

protected:
  virtual ~LineSegmentCallback() {}

  // A frame pair is two ReferenceFrames that should have a line drawn between them
  typedef std::pair<ReferenceFrame*, ReferenceFrame*> FramePair;
  std::vector<FramePair> _framePairs; // List of frame pairs
};

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  
  // Create a root ReferenceFrame
  ReferenceFrame *root = new ReferenceFrame("drawtraj", 1, 0, 0, 0.9);
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
    // Compute position along circle centered at (0, 0, -2)
    pos[0] = rmag * cos(t);
    pos[1] = 0.0;
    pos[2] = rmag * sin(t) - 2.0;

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
    traj2->addPosition(rmag + 1.0, 0.0, 4.0);
  }
  { // Repeat first point
    traj2->addTime(7.0);
    traj2->addPosition(rmag + 1.0, 0.0, 0.0);
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
  Frame 3: Horizontal trajectory
  */
  osg::ref_ptr<Trajectory> traj3 = new Trajectory;
  { // First point along z-axis
    traj3->addTime(0.0);
    traj3->addPosition(0.0, 0.0, rmag + 1.0);
  }
  { // Second point left of first
    traj3->addTime(3.0);
    traj3->addPosition(-5.0, 0.0, rmag + 1.0);
  }
  { // Repeat first point
    traj3->addTime(5.0);
    traj3->addPosition(0.0, 0.0, rmag + 1.0);
  }

  // Follow the trajectory (by default in LOOP mode)
  TrajectoryFollower *tf3 = new TrajectoryFollower(traj3);

  // Create a frame to follow the trajectory
  Sphere *frame3 = new Sphere("Horizontal", 0, 0, 1, 1);
  frame3->setRadius(0.1);
  frame3->showAxes(ReferenceFrame::NO_AXES);
  frame3->showAxesLabels(ReferenceFrame::NO_AXES);
  frame3->getTransform()->setUpdateCallback(tf3);
  root->addChild(frame3);
  
  /***************
  Line segments between frames
  */
  LineSegmentCallback *lsCallback = new LineSegmentCallback;
  lsCallback->addSegment(frame1, frame2); // Segment between frames 1 and 2
  lsCallback->addSegment(frame2, frame3); // Segment between frames 2 and 3

  CustomLineSegments *cls = new CustomLineSegments("CustomLineSegment", 1, 1, 1, 1);
  cls->setLineSegmentCallback(lsCallback);
  cls->setLineWidth(2.0);
  cls->setLineShader("Shaders/Line_Pulse.frag");
  cls->showAxes(ReferenceFrame::NO_AXES);
  cls->showAxesLabels(ReferenceFrame::NO_AXES);
  cls->showNameLabel(false);
  root->addChild(cls);

  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  myWindow->getGridPosition(0, 0)->getCurrentView()->setDefaultViewDistance(4.0);
  myWindow->getGridPosition(0, 0)->getCurrentView()->resetView();

  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
