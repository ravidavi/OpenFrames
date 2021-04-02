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

#include <OpenFrames/EllipticCone.hpp>
#include <OpenFrames/PolyhedralCone.hpp>
#include <OpenFrames/RectangularCone.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>

using namespace OpenFrames;

/** Updates a sensor cone's color based on visibility of a target object.
    Note that this assumes no obstructions between sensor and target. */
class ConeUpdateCallback : public osg::Callback
{
public:
  ConeUpdateCallback(ReferenceFrame *root, PolyhedralCone *sensor, ReferenceFrame *target)
    : _wasVisible(false)
  {
    // Get paths to sensor and target, used to compute target position relative to sensor
    _sensor = new TransformAccumulator(root, sensor);
    _target = new TransformAccumulator(root, target);
  }

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    if(_sensor->isValid() && _target->isValid())
    {
      // Get transform from target's local space to sensor's local space
      osg::Matrixd target_to_sensor = _target->getLocalToWorld() * _sensor->getWorldToLocal();

      // Get vector to target's origin in sensor's space
      osg::Vec3d targetVec = target_to_sensor.getTrans();

      // Change sensor color if target is visible
      // Since visibility won't change every frame, we track previous visiblity and only change
      // the sensor color if target visibility changes. This prevents unnecessary viz processing.
      PolyhedralCone *sensor = static_cast<PolyhedralCone*>(_sensor->getOrigin());
      bool isVisible = sensor->isVisible(targetVec, 0, 10.0); // Only check visibility to max distance of 10
      if(isVisible && !_wasVisible)
      {
        sensor->setConeColor(1.0, 0.5, 0.6, 0.5);
      }
      else if(!isVisible && _wasVisible)
      {
        sensor->setConeColor(0.1, 0.5, 0.6, 0.5);
      }
      _wasVisible = isVisible;
    }

    // Continue traversing callbacks
    return traverse(object, data);
  }

private:
  ~ConeUpdateCallback() {}

  osg::ref_ptr<TransformAccumulator> _sensor; // Path from root to sensor frame
  osg::ref_ptr<TransformAccumulator> _target; // Path from root to target frame

  bool _wasVisible; // Whether target was previously visible
};

int main()
{
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1280, 720, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  OpenFrames::View *view = new OpenFrames::View(root, root);
  myWindow->getGridPosition(0, 0)->addView(view);
  view->setDefaultViewDistance(15.0);
  view->resetView();

  // Create a custom cone (where we specify clock & cone angles)
  {
    PolyhedralCone* customCone = new PolyhedralCone("Custom Cone");
    customCone->setConeColor(0.5, 0.5, 0.5, 0.5);
    customCone->setConeLength(5.0);
    root->addChild(customCone);
    OpenFrames::View *view = new OpenFrames::View(root, customCone);
    myWindow->getGridPosition(0, 0)->addView(view);
    view->setDefaultViewParameters(osg::Vec3d(0, 0, 5.0), osg::Vec3d(), osg::Vec3(0, 1.0, 0));
    view->resetView();

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

    // Place apex at desired location and point boresight in desired direction
    // Vectors are relative to the parent object's reference frame
    osg::Vec3d origin(-10, 0, 0);   // Cone apex location
    osg::Vec3d direction(0, 0, 1);  // Cone boresight direction
    osg::Vec3d up(1, 0, 0);         // Cone +Y axis
    osg::Matrixd mat;
    mat.makeLookAt(osg::Vec3d(), direction, up);
    customCone->setPosition(origin);
    customCone->setAttitude(mat.getRotate().inverse());
  }

  // Create an elliptic cone with specified semimajor/semiminor half-angles
  {
    EllipticCone *ellipticCone = new EllipticCone("Elliptic Cone");
    ellipticCone->setConeColor(0.1, 0.5, 0.6, 0.5);
    ellipticCone->setConeLength(5.0);
    ellipticCone->setPrimaryAngles(osg::DegreesToRadians(45.0), osg::DegreesToRadians(20.0));
    root->addChild(ellipticCone);
    OpenFrames::View *view = new OpenFrames::View(root, ellipticCone);
    myWindow->getGridPosition(0, 0)->addView(view);
    view->setDefaultViewParameters(osg::Vec3d(0, 0, 5.0), osg::Vec3d(), osg::Vec3(0, 1.0, 0));
    view->resetView();

    // Place apex at desired location and point boresight in desired direction
    // Vectors are relative to the parent object's reference frame
    osg::Vec3d origin(10, 0, 0);   // Cone apex location
    osg::Vec3d direction(0, 1, 0); // Cone boresight direction
    osg::Vec3d up(1, 0, 1);        // Cone +Y axis 
    ellipticCone->makeConeLookAt(origin, direction, up);

    // Create a sphere that will move through the scene and change its color based on cone visibility
    osg::ref_ptr<Trajectory> traj1 = new Trajectory;
    double pos[3];
    double rmag = 3.0;
    const double eps = 1.0e-14;
    for(double t = 0.0; t <= 2.0*osg::PI + eps; t += osg::PI / 90.0)
    {
      // Compute position that will enter the elliptical cone
      pos[0] = rmag * cos(t) + 8;
      pos[1] = 2.0;
      pos[2] = rmag * sin(t) - 2.0;

      // Add position
      traj1->addTime(t);
      traj1->addPosition(pos);
    }

    // Follow the trajectory (by default in LOOP mode)
    TrajectoryFollower *tf1 = new TrajectoryFollower(traj1);

    // Create a sphere to follow the trajectory
    Sphere *target = new Sphere("Circle", 1, 0, 0, 1);
    target->setRadius(0.1);
    target->showAxes(ReferenceFrame::NO_AXES);
    target->showAxesLabels(ReferenceFrame::NO_AXES);
    target->getTransform()->setUpdateCallback(tf1);
    root->addChild(target);

    // Dynamically change cone color based on target visibility
    ConeUpdateCallback *coneCallback = new ConeUpdateCallback(root, ellipticCone, target);
    ellipticCone->getGroup()->setUpdateCallback(coneCallback);
  }

  // Create a rectangular cone with specified x/y half-angles
  {
    RectangularCone *rectangularCone = new RectangularCone("Rectangular Cone");
    rectangularCone->setPosition(0, 0, 10.0);
    rectangularCone->setConeColor(0.1, 0.5, 0.6, 0.5);
    rectangularCone->setConeLength(5.0);
    rectangularCone->setPrimaryAngles(osg::DegreesToRadians(45.0), osg::DegreesToRadians(20.0));
    root->addChild(rectangularCone);
  }
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
