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

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <iostream>
#include <cmath>
#include <osg/io_utils>
#include <osg/Math>
#include <osg/MatrixTransform>
#include <osgManipulator/TrackballDragger>
#include "orbitcalcs.hpp"

using namespace OpenFrames;
WindowProxy *theWindow;
Trajectory *traj2;

/** Callback that computes a new trajectory when the dragger is rotated */
class MyDraggerCallback : public osgManipulator::DraggerCallback
{
public:
  MyDraggerCallback(TrajectoryFollower* tf, Trajectory* trajOut, osg::MatrixTransform *xform)
  : _tf(tf), _trajOut(trajOut), _xform(xform)
  {}
  
  MyDraggerCallback(const MyDraggerCallback& org, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY)
  : osg::Object(org, copyop) {}
  
  virtual bool receive(const osgManipulator::Rotate3DCommand& cmd)
  {
    // Only compute new trajectory when rotation is finished
    //if(cmd.getStage() != osgManipulator::MotionCommand::FINISH) return false;
    
    // Get spacecraft's currently followed trajectory
    double lastTime = _tf->getLastTime();
    Trajectory* lastTraj = _tf->getLastTrajectory();
    const Trajectory::DataSource* posDataSource = _tf->getDataSource();
    unsigned int numPoints = lastTraj->getNumPoints(posDataSource);
    if(lastTraj == nullptr)
    {
      OSG_NOTICE << "MyDraggerCallback ERROR: Missing Trajectory" << std::endl;
      return false;
    }
    
    // Get location of spacecraft's current position within its trajectory
    int index;
    int val = lastTraj->getTimeIndex(lastTime, index);
    if(val < 0)
    {
      OSG_NOTICE << "MyDraggerCallback ERROR: Time out of range" << std::endl;
      return false;
    }
    else if(index == numPoints)
    {
      OSG_NOTICE << "MyDraggerCallback ERROR: Time at end of Trajectory" << std::endl;
      return false;
    }
    
    // Interpolate spacecraft position from trajectory position
    // Note that we could also obtain this from the spacecraft's ReferenceFrame
    // if it was made available to this callback
    osg::Vec3d pos, pos1, pos2;
    lastTraj->getPosition(index  , pos1[0], pos1[1], pos1[2]);
    lastTraj->getPosition(index+1, pos2[0], pos2[1], pos2[2]);
    const Trajectory::DataArray& times = lastTraj->getTimeList();
    double frac = (lastTime - times[index])/(times[index+1] - times[index]);
    pos = pos1 + (pos2 - pos1)*frac; // Linear interpolation for position
    
    // Interpolate spacecraft velocity from trajectory optional 0
    osg::Vec3d vel, vel1, vel2;
    lastTraj->getOptional(index  , 0, vel1[0], vel1[1], vel1[2]);
    lastTraj->getOptional(index+1, 0, vel2[0], vel2[1], vel2[2]);
    vel = vel1 + (vel2 - vel1)*frac; // Linear interpolation for velocity
    
    // Interpolate spacecraft attitude from trajectory attitude
    // Note that we could also obtain this from the spacecraft's ReferenceFrame
    // if it was made available to this callback
    osg::Quat att, att1, att2;
    lastTraj->getAttitude(index  , att1[0], att1[1], att1[2], att1[3]);
    lastTraj->getAttitude(index+1, att2[0], att2[1], att2[2], att2[3]);
    att.slerp(frac, att1, att2); // Spherical linear interpolation for attitude
    
    // Construct the spacecraft's local to world matrix from its position and attitude
    osg::Matrixd matSCToWorld(att);
    matSCToWorld.setTrans(pos);

    // Get the current transformation (a MatrixTransform) that applies to the spacecraft model
    // Note that this MatrixTransform is ONLY affected by the dragger, so the rotation of its Y-axis
    // is equal to the rotation of the spacecraft's velocity vector
    const osg::Matrixd& matDraggerToSC = _xform->getMatrix();
    
    // Compute the matrix that converts from the dragger to the world frame
    // This will be used to compute the new position and velocity
    osg::Matrixd matDraggerToWorld = matDraggerToSC*matSCToWorld;
    
    // The new spacecraft position is the origin of where the dragger moved the spacecraft
    // Note that for a TrackballDragger, this will be equal to the spacecraft's original position
    // since that dragger does not change the position.
    pos = matDraggerToWorld.getTrans();
    
    // The new velocity vector has the same magnitude as the old one, but is in the y-axis
    // direction of the dragger's transformation matrix
    // So convert a vector in the y-axis direction (with magnitude same as velocity) back
    // to the world frame, which gives the new velocity vector
    double vmag = vel.length();
    osg::Vec4 vel_local(0, vmag, 0, 0); // Last value is 0 to indicate vector instead of point
    osg::Vec4 vel_new = vel_local * matDraggerToWorld;
    vel.set(vel_new[0], vel_new[1], vel_new[2]);
    
    // Now populate the second trajectory using the new state
    fillTrajectory(pos, vel, _trajOut);
    
    return false;
  }
  
private:
  TrajectoryFollower* _tf;
  Trajectory* _trajOut;
  osg::MatrixTransform* _xform;
};

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
    theWindow->setTimeScale(theWindow->getTimeScale() + 0.05);
  }
  
  // Slow down time
  else if((*key == '-') || (*key == '_'))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() - 0.05);
  }
}

/** This example shows how to click on a spacecraft and rotate its
 velocity vector using an osgManipulator::TrackballDragger.
 **/
int main(int argc, char **argv)
{
  // use an ArgumentParser object to manage the program arguments.
  osg::ArgumentParser arguments(&argc,argv);
  osgViewer::Viewer viewer(arguments);
  
  // Create the interface that will draw a scene onto a window.
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 1, 1, false);
  myWindow->setKeyPressCallback(KeyPressCallback);
  theWindow = myWindow;
  
  // Create a Sphere to represent the Earth
  Sphere* earth = new Sphere("Earth", 1, 1, 1, 0.9);
  //earth->showAxes(ReferenceFrame::NO_AXES);
  //earth->showAxesLabels(ReferenceFrame::NO_AXES);
  //earth->showNameLabel(false);
  earth->setRadius(6371.0);
  earth->setTextureMap("Images/EarthTexture.bmp");
  
  // Create a dummy ReferenceFrame to hold the spacecraft. This is needed to allow
  // a View to track the spacecraft even though it is not directly added to the Earth
  ReferenceFrame *scRoot  = new ReferenceFrame("SCRoot");
  scRoot->showAxes(ReferenceFrame::NO_AXES);
  scRoot->showAxesLabels(ReferenceFrame::NO_AXES);
  scRoot->showNameLabel(false);
  earth->addChild(scRoot);
  
  // Create the spacecraft
  Model *sc = new Model("Spacecraft", 1, 0, 0, 0.9);
  sc->setModel("Models/Hubble.3ds");
  sc->showAxes(ReferenceFrame::Y_AXIS);
  sc->showAxesLabels(ReferenceFrame::Y_AXIS);
  sc->setYLabel("V");
  
  // Create a dragger to rotate the spacecraft
  osgManipulator::TrackballDragger* dragger = new osgManipulator::TrackballDragger();
  dragger->setupDefaultGeometry();
  dragger->setAxisLineWidth(5.0);
  
  // Draggers require a MatrixTransform, but OpenFrames uses its own type of transform
  // to position/orient objects. So create an intermediate MatrixTransform to hold the
  // spacecraft, which will go between the Earth and the Spacecraft
  osg::MatrixTransform *mt = new osg::MatrixTransform;
  mt->addChild(sc->getGroup());
  scRoot->getGroup()->addChild(dragger);
  scRoot->getGroup()->addChild(mt);
  dragger->addTransformUpdating(mt);
  
  // Size the dragger to encompass the spacecraft
  float scale = sc->getBound().radius();
  dragger->setMatrix(osg::Matrix::scale(scale, scale, scale) *
                     osg::Matrix::translate(-(sc->getBound().center())));
  
  // Tell dragger to handle its own events, and activate using the CTRL key
  dragger->setHandleEvents(true);
  dragger->setActivationModKeyMask(osgGA::GUIEventAdapter::MODKEY_CTRL);
  
  // Create the trajectory using
  // Trajectory(DOF, number of optionals)
  Trajectory *traj = new Trajectory(3, 1);
  
  // Tell spacecraft to follow trajectory (by default in LOOP mode)
  TrajectoryFollower *tf = new TrajectoryFollower(traj);
  tf->setFollowType(TrajectoryFollower::POSITION + TrajectoryFollower::ATTITUDE, TrajectoryFollower::LOOP);
  scRoot->getTransform()->setUpdateCallback(tf);
  
  // Create a drawable trajectory for the spacecraft window using
  // DrawableTrajectory(name, color(r,g,b,a))
  DrawableTrajectory *drawtraj = new DrawableTrajectory("traj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);
  earth->addChild(drawtraj);

  // Create a CurveArtist for the trajectory.  By default the CurveArtist
  // will use x/y/z positions from the trajectory for plotting.
  CurveArtist *ca = new CurveArtist(traj);
  ca->setWidth(2.0); // Line width for the trajectory
  ca->setColor(0, 1, 0);
  drawtraj->addArtist(ca);
  
  // Create the second trajectory that gets modified when the spacecraft is rotated
  traj2 = new Trajectory(3, 1);
  CurveArtist *ca2 = new CurveArtist(traj2);
  ca2->setWidth(1.0); // Line width for the trajectory
  ca2->setColor(1, 1, 0);
  drawtraj->addArtist(ca2);
  
  // Add a callback to the dragger that will modify the second trajectory
  dragger->addDraggerCallback(new MyDraggerCallback(tf, traj2, mt));
  
  // Create views
  View *view = new View(earth, scRoot);
  view->setDefaultViewDistance(40.0);
  
  // Create a manager to handle the spatial scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(earth);
  
  // Set up the scene
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black star background
  myWindow->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 6.0, 40000);
  myWindow->getGridPosition(0, 0)->addView(view);
  
  // Fill the trajectory with points
  double a = 7000.0; // [km] Semimajor axis
  double e = 0.0;    // Eccentricity
  double i = 0.0;    // [rad] Inclination
  double w = 0.0;    // [rad] Argument of periapsis
  double RAAN = 0.0; // [rad] Right ascension of ascending node
  fillTrajectory(a, e, i, w, RAAN, traj);

  // Create a second window to show the same scene, but looking at the Earth
  osg::ref_ptr<WindowProxy> myWindow2 = new WindowProxy(100, 100, 1024, 768, 1, 1, false);
  myWindow2->setScene(fm, 0, 0);
  myWindow2->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black star background
  myWindow2->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 6.0, 40000);
  myWindow2->synchronizeTime(myWindow);
  View *view2 = new View(earth, earth);
  myWindow2->getGridPosition(0, 0)->addView(view2);

  myWindow->startThread(); // Start window animation
  while(!myWindow->isAnimating()) OpenThreads::Thread::YieldCurrentThread();
  myWindow2->startThread();
  myWindow->join(); // Wait for window animation to finish
  myWindow2->join();
  
  return 0;
}
