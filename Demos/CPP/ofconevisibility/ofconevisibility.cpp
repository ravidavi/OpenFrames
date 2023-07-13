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

#include <OpenFrames/CustomLineSegments.hpp>
#include <OpenFrames/EllipticCone.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/PolyhedralCone.hpp>
#include <OpenFrames/RectangularCone.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>

#include <osg/CoordinateSystemNode>
#include <osgUtil/RayIntersector>

using namespace OpenFrames;

/** Callback that dynamically computes the line segment vertex data */
class LineSegmentCallback : public CustomLineSegments::Callback
{
public:
  LineSegmentCallback(OpenFrames::ReferenceFrame* root)
    : _root(root)
  {
    _rayIntersector = new osgUtil::RayIntersector();
    _iv.setIntersector(_rayIntersector);
    _iv.setTraversalMask(~0x1);
  }

  // Required: get number of segments
  virtual unsigned int getNumSegments() const
  {
    return _frameData.size();
  }

  // Required: get data for given segment
  virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) const
  {
    // Vertex A corresponds to Frame A (the sensor)
    TransformAccumulator* frameAXform = std::get<0>(_frameData[segID]);
    osg::Matrixd matLocalToWorld_A = frameAXform->getLocalToWorld();
    osg::Vec3d startLocal = std::get<2>(_frameData[segID]);
    osg::Vec3d startWorld = matLocalToWorld_A.preMult(startLocal);
    posA = startWorld;
    ReferenceFrame *frameA = frameAXform->getOrigin();
    colorA = frameA->getColor();
    colorA.a() = 0.4; // Make line color slightly transparent

    // Vertex B corresponds to Frame B (the target)
    TransformAccumulator *frameBXform = std::get<1>(_frameData[segID]);
    ReferenceFrame *frameB = frameBXform->getOrigin();
    osg::Matrixd matLocalToWorld_B = frameBXform->getLocalToWorld();
    osg::Vec3d endLocal = std::get<3>(_frameData[segID]);
    osg::Vec3d endWorld = matLocalToWorld_B.preMult(endLocal);    
    colorB = frameB->getColor();
    colorB.a() = 0.4; // Make line color slightly transparent

    // Test whether point B is in sensor A's FOV
    PolyhedralCone *sensor = dynamic_cast<PolyhedralCone*>(frameA);
    bool isVisible = true;
    if(sensor)
    {
      // Get vector to target's origin in sensor's space
      osg::Vec3d targetVec = osg::Matrixd::inverse(matLocalToWorld_A).preMult(endWorld);

      isVisible = sensor->isVisible(targetVec);
    }

    // If visible from sensor then do intersection test
    if(isVisible)
    {
      // Create intersection test from start to end points
      _iv.reset();
      _rayIntersector->setStart(startWorld);
      _rayIntersector->setDirection(endWorld - startWorld);

      // Test for intersection and set segment endpoint accordingly
      _root->getGroup()->accept(_iv);
      osgUtil::RayIntersector::Intersection intersection = _rayIntersector->getFirstIntersection();
      if(intersection.distance == -1) posB = posA;
      else posB = intersection.getWorldIntersectPoint();
    }
    else
      posB = posA; // Otherwise set line segment to zero length
  }

  // Add a pair of ReferenceFrames that will have a line segment drawn between them
  void addSegment(ReferenceFrame *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB)
  {
    // Make sure this callback's data is not being used
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(mMutex);

    _frameData.push_back(FrameData(new TransformAccumulator(_root, frameA), new TransformAccumulator(_root, frameB), posA, posB));
  }

protected:
  virtual ~LineSegmentCallback() {}

  // A frame pair is two ReferenceFrames that should have a line drawn between them
  ReferenceFrame *_root;
  typedef std::tuple< osg::ref_ptr<TransformAccumulator>, osg::ref_ptr<TransformAccumulator>, osg::Vec3d, osg::Vec3d> FrameData;
  std::vector<FrameData> _frameData; // List of frame data

  // Intersection test
  osg::ref_ptr<osgUtil::RayIntersector> _rayIntersector;
  mutable osgUtil::IntersectionVisitor _iv;
};

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
    ellipticCone->getGroup()->setNodeMask(0x1);
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
    osg::Matrixd mat;
    mat.makeLookAt(osg::Vec3d(), direction, up);
    ellipticCone->setPosition(origin);
    ellipticCone->setAttitude(mat.getRotate().inverse());
  }

  // Create a sphere that will block the cone's view
  //Sphere *blocker = new Sphere("Blocker", 0, 1, 0, 1);
  Model *blocker = new Model("Blocker", 0, 1, 0, 1);
  {
    //blocker->setRadius(3.0);
    blocker->setModel("osgEarth/earthmap.earth");
    blocker->setModelScale(0.000001, 0.000001, 0.000001);
    blocker->showAxes(ReferenceFrame::NO_AXES);
    blocker->showAxesLabels(ReferenceFrame::NO_AXES);
    blocker->setPosition(-3, 10, 0);
    root->addChild(blocker);
  }

  // Create a target sphere that will move through the scene 
  // The target's color will change based on cone visibility
  //Sphere *target = new Sphere("Target", 1, 0, 0, 1);
  Model *target = new Model("Target", 1, 0, 0, 1);
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
    //target->setRadius(2.0);
    target->setModel("C:/Users/ravid/Desktop/GMAT_Scripts/moon_lro_localelevation.earth");
    target->setModelScale(0.000001, 0.000001, 0.000001);
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
  LineSegmentCallback *lsCallback = new LineSegmentCallback(root);
  osg::EllipsoidModel ellipsoidModel;
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(1, 0, 0)); // Segment between sensor and target
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(0, 1, 0)); // Segment between sensor and target
  lsCallback->addSegment(ellipticCone, target, osg::Vec3d(0, 0, 0), osg::Vec3d(0, 0, 1)); // Segment between sensor and target

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
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
