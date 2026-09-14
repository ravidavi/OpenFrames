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

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <sstream>
#include <osg/Math>
#include "orbitcalcs.hpp"

using namespace OpenFrames;
WindowProxy *theWindow;
osgText::Text *hudText;

void showTrajData(const double& a, const double& e,
                  const double& i, const double& w, const double& RAAN)
{
  std::stringstream ss;
  ss << "Semimajor Axis: " << a << " km\n"
  << "Eccentricity: " << e << "\n"
  << "Inclination: " << i*180.0/osg::PI << " deg\n"
  << "Argument of Perigee: " << w*180.0/osg::PI << " deg\n"
  << "RAAN: " << RAAN*180.0/osg::PI << " deg";
  hudText->setText(ss.str());
}

/** Callback that computes a new trajectory when the dragger is rotated */
class MyDraggerCallback : public osgManipulator::DraggerCallback
{
public:
  MyDraggerCallback(const TrajectoryFollower* tf, Trajectory* trajOut, const FrameTransform* modelXform)
  : _tf(tf), _trajOut(trajOut), _modelXform(modelXform)
  {}
  
  MyDraggerCallback(const MyDraggerCallback& org, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY)
  : osg::Object(org, copyop) {}
  
  virtual bool receive(const osgManipulator::Rotate3DCommand& cmd)
  {
    if(!_tf.valid()) return false;
    
    // Get spacecraft's currently followed trajectory
    double lastTime = _tf->getLastTime();
    Trajectory* lastTraj = _tf->getLastTrajectory();
    if(lastTraj == nullptr) return false;
    
    // Lock the trajectory so it's not modified while we read it
    lastTraj->lockData();
    
    // Get location of spacecraft's current position within its trajectory
    const Trajectory::DataSource* posDataSource = _tf->getDataSource();
    unsigned int numPoints = lastTraj->getNumPoints(posDataSource);
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
    osg::Quat quatSCToWorld, att1, att2;
    lastTraj->getAttitude(index  , att1[0], att1[1], att1[2], att1[3]);
    lastTraj->getAttitude(index+1, att2[0], att2[1], att2[2], att2[3]);
    quatSCToWorld.slerp(frac, att1, att2); // Spherical linear interpolation for attitude
    
    // Unlock the trajectory so it can be modified again
    lastTraj->unlockData();
    
    // Get the dragger's rotation, which is stored in the Model's transform
    osg::Quat quatDragger;
    _modelXform->getAttitude(quatDragger);
    
    // Compute a Delta-V based on how the dragger changed the inertial velocity vector
    // First rotate velocity from inertial to SC coordinates
    osg::Vec3d velSC = quatSCToWorld.inverse() * vel;
    
    // Next apply dragger rotation to velocity
    osg::Vec3d velDragger = quatDragger * velSC;
    
    // Subtract to get Delta-V
    osg::Vec3d dvSC = velDragger - velSC;
    
    // Rotate Delta-V back to inertial frame and apply it to the velocity
    osg::Vec3d dvInertial = quatSCToWorld * dvSC;
    vel += dvInertial;
    
    // Convert cartesian state to Keplerian elements
    double ta, a, e, i, w, RAAN;
    CartToKep(pos, vel, ta, a, e, i, w, RAAN);
    
    // Now populate the second trajectory using the new state
    fillTrajectory(a, e, i, w, RAAN, _trajOut.get());
    showTrajData(a, e, i, w, RAAN);
    
    return false;
  }
  
private:
  osg::observer_ptr<const TrajectoryFollower> _tf;
  osg::observer_ptr<Trajectory> _trajOut;
  osg::observer_ptr<const FrameTransform> _modelXform;
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
  earth->showAxes(ReferenceFrame::NO_AXES);
  earth->showAxesLabels(ReferenceFrame::NO_AXES);
  earth->showNameLabel(false);
  earth->setRadius(6371.0);
  earth->setTextureMap("Images/EarthTexture.bmp");
  
  // Create the spacecraft
  Model *sc = new Model("Spacecraft", 1, 0, 0, 0.9);
  sc->setModel("Models/Hubble.3ds");
  sc->showAxes(ReferenceFrame::Y_AXIS);
  sc->showAxesLabels(ReferenceFrame::Y_AXIS);
  sc->setYLabel("V");
  earth->addChild(sc);
  
  // Create the trajectory using
  // Trajectory(DOF, number of optionals)
  Trajectory *traj = new Trajectory(3, 1);
  
  // Tell spacecraft to follow trajectory (by default in LOOP mode)
  TrajectoryFollower *tf = new TrajectoryFollower(traj);
  tf->setFollowType(TrajectoryFollower::POSITION + TrajectoryFollower::ATTITUDE, TrajectoryFollower::LOOP);
  sc->getTransform()->setUpdateCallback(tf);
  
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
  Trajectory* traj2 = new Trajectory(3, 1);
  CurveArtist *ca2 = new CurveArtist(traj2);
  ca2->setWidth(1.0); // Line width for the trajectory
  ca2->setColor(1, 1, 0);
  drawtraj->addArtist(ca2);
  
  // Add a callback that will modify the second trajectory when the spacecraft
  // is dragged. For now this is limited to a TrackballDragger.
  sc->addDraggerCallback(new MyDraggerCallback(tf, traj2, sc->getModelTransform()));
  
  // Create views
  View *view = new View(earth, sc);
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
  double a = 8000.0; // [km] Semimajor axis
  double e = 0.1;    // Eccentricity
  double i = 45.0*osg::PI/180.0;    // [rad] Inclination
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
  
  // Create text to go in HUD of second window
  osg::ref_ptr<osgText::Text> hudText_BottomLeft = new osgText::Text;
  hudText = hudText_BottomLeft;
  hudText_BottomLeft->setFont("arial.ttf");
  hudText_BottomLeft->setColor(osg::Vec4(1, 1, 0, 1));
  hudText_BottomLeft->setCharacterSizeMode(osgText::Text::SCREEN_COORDS);
  hudText_BottomLeft->setCharacterSize(20.0);    // In screen coordinates (pixels)
  hudText_BottomLeft->setFontResolution(40, 40); // In texels (texture pixels)
  hudText_BottomLeft->setLineSpacing(0.25);
  
  // Position HUD text
  // Screen coordinates go from (0,0) bottom-left to (1,1) top-right
  hudText_BottomLeft->setAlignment(osgText::Text::LEFT_BOTTOM);
  hudText_BottomLeft->setPosition(osg::Vec3(0.0, 0.0, 0.0));
  
  // Some graphics drivers have a bug where text can't be properly changed.
  // Get around this by initializing text using all likely characters.
  std::string dummyText("the quick brown fox jumps over the lazy dog");
  dummyText += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG";
  dummyText += "1234567890";
  dummyText += "[]{}()<>,.;:+-*/_";
  hudText_BottomLeft->setText(dummyText);
  
  // Attach HUD text
  osg::Geode* geode = new osg::Geode;
  geode->addDrawable(hudText_BottomLeft);
  myWindow2->getGridPosition(0, 0)->getHUD()->addChild(geode);
  
  // Set text to current orbit info
  showTrajData(a, e, i, w, RAAN);

  myWindow->startThread(); // Start window animation
  while(!myWindow->isAnimating()) OpenThreads::Thread::YieldCurrentThread();
  myWindow2->startThread();
  myWindow->join(); // Wait for window animation to finish
  myWindow2->join();
  
  return 0;
}
