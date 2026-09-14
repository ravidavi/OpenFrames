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

#include <OpenFrames/CoordinateAxes.hpp>
#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/RadialPlane.hpp>
#include <OpenFrames/SegmentArtist.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/Math>

using namespace OpenFrames;

CoordinateAxes *earthaxes;
RadialPlane *rp;
WindowProxy *theWindow;

const double rmag = 100000.0; // [km] Size of orbit
const double rp2 = 50000.0; // Periapse of second orbit
const double ecc2 = 0.5; // Eccentricity of second orbit
const double p2 = rp2*(1.0+ecc2); // Semilatus rectum of second orbit
const double inc2 = 45.0*osg::PI/180.0; // Inclination of second orbit

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  // Pause/unpause animation
  if(*key == 'p')
  {
    theWindow->pauseTime(!theWindow->isTimePaused());
  }
  
  // Change CoordinateAxes color
  else if(*key == 'c')
  {
    osg::Vec4 color = earthaxes->getColor();
    earthaxes->setColor(1.0-color[0], 1.0-color[1], 1.0-color[2], 1.0);
    
    color = rp->getPlaneColor();
    rp->setPlaneColor(1.0-color[0], 1.0-color[1], 1.0-color[2], 1.0-color[3]);
    
    color = rp->getLineColor();
    rp->setLineColor(1.0-color[0], 1.0-color[1], 1.0-color[2], 1.0);
  }
  
  // Decrease size of onscreen objects
  else if(*key == 'l')
  {
    earthaxes->setAxisLength(earthaxes->getAxisLength() - 10000.0);
    
    double radius, radSpace, lonSpace;
    rp->getParameters(radius, radSpace, lonSpace);
    rp->setParameters(radius - 10000.0, radSpace, lonSpace);
  }
  
  // Increase size of onscreen objects
  else if(*key == 'L')
  {
    earthaxes->setAxisLength(earthaxes->getAxisLength() + 10000.0);
    
    double radius, radSpace, lonSpace;
    rp->getParameters(radius, radSpace, lonSpace);
    rp->setParameters(radius + 10000.0, radSpace, lonSpace);
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

/** This example shows how to create multiple subwindows, and have
 * a ReferenceFrame follow a path defined by Trajectory points. It also
 * shows how to use Artists to draw a single Trajectory in several
 * different ways.
 **/
int main()
{
  // Create the interface that will draw a scene onto a window.
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 2, 1, false);
  myWindow->setTimeScale(0.1);
  myWindow->setWindowName("OpenFrames Split Window Example");
  theWindow = myWindow.get();
  
  // Create the objects that will populate the scene using
  // Sphere(name, color(r,g,b,a))
  // Model(name, color(r,g,b,a))
  Sphere *earth = new Sphere("Earth", 0, 1, 0, 0.9);
  Model *hubble = new Model("Hubble", 1, 0, 0, 0.9);
  Model *hubble2 = new Model("Spacecraft", 0, 1, 0, 0.9);
  
  // Set Earth parameters
  const double r_earth = 6371.0;
  earth->setRadius(r_earth);
  earth->setAutoLOD(true);
  earth->setTextureMap("Images/EarthTexture.bmp");
  
  // Create a set of Coordinate Axes for Earth axes
  // By default the tick marks will be solid circles
  earthaxes = new CoordinateAxes("earthaxes", 0, 0, 1, 1);
  earthaxes->setAxisLength(2.0*rmag);
  earthaxes->setTickSpacing(rmag, 0.25*rmag);
  earthaxes->setTickSize(10, 5);
  earthaxes->setXLabel("X");
  earthaxes->setYLabel("Y");
  earthaxes->setZLabel("Z");
  
  // Set the spacecraft parameters
  hubble->setModel("Models/Hubble.3ds");
  hubble->setModelScale(0.001, 0.001, 0.001); // 1-meter size
  hubble2->setModel("Models/Hubble.3ds");
  hubble2->setModelScale(0.001, 0.001, 0.001); // 1-meter size
  
  // Create a drawable trajectory to hold the hubble2 center marker
  DrawableTrajectory *h2center = new DrawableTrajectory("hubble2 center marker", 1, 0, 0, 1);
  h2center->showAxes(ReferenceFrame::NO_AXES);
  h2center->showAxesLabels(ReferenceFrame::NO_AXES);
  h2center->showNameLabel(false);
  
  // Create a MarkerArtist to draw the center marker
  MarkerArtist *h2centermarker = new MarkerArtist();
  h2centermarker->setMarkerShader("Shaders/Marker_CirclePulse.frag");
  h2centermarker->setMarkerSize(15);
  
  // Add the MarkerArtist to the drawable trajectory
  h2center->addArtist(h2centermarker);
  
  // Create the trajectory using
  // Trajectory(DOF, number of optionals)
  Trajectory *traj = new Trajectory(3, 1);  // For hubble
  Trajectory *traj2 = new Trajectory(3, 0); // For hubble2
  
  // Create a drawable trajectory for the spacecraft window using
  // DrawableTrajectory(name, color(r,g,b,a))
  DrawableTrajectory *drawtraj = new DrawableTrajectory("traj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);
  
  // Create a CurveArtist for the trajectory.  By default the CurveArtist
  // will use x/y/z positions from the trajectory for plotting.
  CurveArtist *ca = new CurveArtist(traj);
  ca->setWidth(2.0); // Line width for the trajectory
  ca->setColor(1, 0, 0);
  drawtraj->addArtist(ca);
  
  // Create a CurveArtist for the second trajectory.
  CurveArtist *ca2 = new CurveArtist(traj2);
  ca2->setWidth(2.0); // Line width for the trajectory
  ca2->setColor(0, 1, 0);
  ca2->setPattern(1.0, 0x00FF); // Dashed line
  drawtraj->addArtist(ca2);
  
  Trajectory::DataSource data; // Data source for artists
  data._src = Trajectory::POSOPT;
  
  // Create an artist for velocity vectors
  SegmentArtist *sa = new SegmentArtist(traj);
  sa->setColor(0.5, 0, 0);
  data._element = 0;
  sa->setStartXData(data);
  data._element = 1;
  sa->setStartYData(data);
  data._element = 2;
  sa->setStartZData(data);
  data._element = 0;
  data._opt = 1;
  sa->setEndXData(data);
  data._element = 1;
  sa->setEndYData(data);
  data._element = 2;
  sa->setEndZData(data);
  drawtraj->addArtist(sa);
  
  // Create a drawable trajectory for the time history window
  DrawableTrajectory *timehist = new DrawableTrajectory("TimeHist", 1, 0, 0, 0.9);
  timehist->showAxes(ReferenceFrame::NO_AXES);
  timehist->showAxesLabels(ReferenceFrame::NO_AXES);
  timehist->showNameLabel(false);
  
  // Create an artist to draw position vs time
  ca = new CurveArtist(traj);
  ca->setColor(1, 0, 0);
  data._src = Trajectory::POSOPT;
  data._opt = 0;
  data._element = 0; // Use X position for X coordinate
  data._scale = 1.0; // Draw positions to scale
  ca->setXData(data);
  data._element = 1; // Use Y position for Y coordinate
  ca->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = rmag/osg::PI; // Scale since time << distance
  ca->setZData(data);
  timehist->addArtist(ca);
  
  // Create an artist to draw start/intermediate/end markers
  MarkerArtist *ma = new MarkerArtist(traj);
  ma->setMarkers(MarkerArtist::START + MarkerArtist::INTERMEDIATE + MarkerArtist::END);
  ma->setAutoAttenuate(true);
  ma->setMarkerColor(MarkerArtist::START, 0, 1, 0);
  ma->setMarkerColor(MarkerArtist::END,   1, 0, 0);
  ma->setMarkerColor(MarkerArtist::INTERMEDIATE, 1, 1, 0);
  ma->setMarkerShader("Shaders/Marker_Rose.frag");
  ma->setMarkerSize(10);
  data._src = Trajectory::POSOPT;
  data._element = 0; // Use X position for X coordinate
  data._scale = 1.0; // Draw positions to scale
  ma->setXData(data);
  data._element = 1; // Use Y position for Y coordinate
  ma->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = rmag/osg::PI; // Scale since time << distance
  ma->setZData(data);
  timehist->addArtist(ma);
  
  // Draw markers at equally spaced data points
  //ma->setIntermediateType(MarkerArtist::DATA);
  //ma->setIntermediateSpacing(58.0); // Every 58 data points
  //ma->setIntermediateDirection(MarkerArtist::START); // From beginning of trajectory
  
  // Draw markers at equally spaced time intervals
  //ma->setIntermediateType(MarkerArtist::TIME);
  //ma->setIntermediateSpacing(1.0); // Every 1.0 time unit
  //ma->setIntermediateDirection(MarkerArtist::END); // From end of trajectory
  
  // Draw markers at equally spaced distances
  ma->setIntermediateType(MarkerArtist::DISTANCE);
  ma->setIntermediateSpacing(rmag/2.0); // Distance interval
  ma->setIntermediateDirection(MarkerArtist::END); // From end of trajectory
  
  // Create a set of Coordinate Axes for time history plot
  CoordinateAxes* axes = new CoordinateAxes("axes", 0.0, 0.8, 0.8, 1);
  axes->setAxisLength(2.0*rmag);
  axes->setTickSpacing(rmag, 0.5*rmag);
  axes->setTickSize(8, 5);
  axes->setTickShader(""); // Default to solid circle
  axes->setXLabel("X");
  axes->setYLabel("Y");
  axes->setZLabel("t");
  
  // Create a ReferenceFrame to show model's position in time history plot
  ReferenceFrame *trace = new ReferenceFrame("trace", 1, 1, 1, 1);
  
  // Create a drawable trajectory to hold the trace center marker
  DrawableTrajectory *drawcenter = new DrawableTrajectory("center marker", 1, 0, 0, 1);
  drawcenter->showAxes(ReferenceFrame::NO_AXES);
  drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
  drawcenter->showNameLabel(false);
  
  // Create a MarkerArtist to draw the center marker
  MarkerArtist *centermarker = new MarkerArtist();
  centermarker->setMarkerShader("Shaders/Marker_Target.frag");
  centermarker->setMarkerSize(15);
  
  // Add the MarkerArtist to the drawable trajectory
  drawcenter->addArtist(centermarker);
  
  // Create a RadialPlane to show trace frame's orientation
  rp = new RadialPlane("radial", 1, 1, 1, 1);
  rp->showAxes(ReferenceFrame::NO_AXES);
  rp->showAxesLabels(ReferenceFrame::NO_AXES);
  rp->showNameLabel(false);
  rp->setParameters(2.0*rmag, 0.5*rmag, 60.0*osg::PI/180.0);
  
  // Set up reference frame heirarchies.
  earth->addChild(drawtraj);
  earth->addChild(earthaxes);
  earth->addChild(hubble);
  earth->addChild(hubble2);
  hubble2->addChild(h2center);
  axes->addChild(timehist);
  axes->addChild(trace);
  trace->addChild(drawcenter);
  axes->addChild(rp);
  
  // Tell model to follow trajectory (by default in LOOP mode)
  TrajectoryFollower *tf = new TrajectoryFollower(traj);
  hubble->getTransform()->setUpdateCallback(tf);
  
  TrajectoryFollower *tf2 = new TrajectoryFollower(traj2);
  hubble2->getTransform()->setUpdateCallback(tf2);
  
  // Tell trace frame to follow time history
  tf = new TrajectoryFollower(traj);
  data._src = Trajectory::POSOPT;
  data._opt = 0;
  data._element = 0; // Use X position for X coordinate
  data._scale = 1.0; // Draw positions to scale
  tf->setXData(data);
  data._element = 1; // Use Y position for Y coordinate
  tf->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = rmag/osg::PI; // Scale since time << distance
  tf->setZData(data);
  trace->getTransform()->setUpdateCallback(tf);
  
  // Tell radial frame to follow time history's orientation
  tf = new TrajectoryFollower(traj);
  tf->setFollowType(TrajectoryFollower::ATTITUDE, TrajectoryFollower::LOOP);
  rp->getTransform()->setUpdateCallback(tf);
  rp->setPosition(0.0, 0.0, 0.0);
  
  // Create views
  View *view = new View(earth, earth);
  View *view2 = new View(earth, hubble);
  View *view3 = new View(axes, axes);
  View *view4 = new View(axes, trace);
  
  // Create views that look from hubble towards hubble2, using
  // various transformation types
  View *view5 = new View(earth, hubble, hubble2, View::RELATIVE_FRAME, View::DIRECT);
  View *view6 = new View(earth, hubble, hubble2, View::RELATIVE_FRAME, View::AZEL);
  
  // Create a manager to handle the spatial scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(earth);
  
  // Create a manager to handle the time history scene
  FrameManager* fm2 = new FrameManager;
  fm2->setFrame(axes);
  
  // Set up the scene
  theWindow->setScene(fm, 0, 0);
  theWindow->setScene(fm2, 1, 0);
  theWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  theWindow->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 6.0, 40000); // At most 40000 stars of magnitude range [-2.0, 6.0] from the HYGv3 database
  theWindow->getGridPosition(0, 0)->addView(view);
  theWindow->getGridPosition(0, 0)->addView(view2);
  theWindow->getGridPosition(0, 0)->addView(view5);
  theWindow->getGridPosition(0, 0)->addView(view6);
  theWindow->getGridPosition(1, 0)->addView(view3);
  theWindow->getGridPosition(1, 0)->addView(view4);
  
  // Add the actual positions and attitudes for the trajectory.
  osg::Quat att; // Quaternion for attitude transformations
  double pos[3], vel[3];
  double rmag2;
  for(double t = 0.0; t <= 2.0*osg::PI; t += osg::PI/90.0)
  {
    pos[0] = rmag*cos(t);
    pos[1] = rmag*sin(t);
    pos[2] = 0.0;
    vel[0] = pos[0] - 0.5*pos[1];
    vel[1] = pos[1] + 0.5*pos[0];
    vel[2] = 0.0;
    att.makeRotate(t, 1, 1, 1);
    
    // Add main trajectory position & velocity
    traj->addTime(2.0*t);
    traj->addPosition(pos);
    traj->setOptional(0, vel);
    traj->addAttitude(att[0], att[1], att[2], att[3]);
    
    // Perturb position for second trajectory
    rmag2 = p2/(1.0 + ecc2*cos(t));
    pos[0] = rmag2*cos(t);
    pos[1] = rmag2*sin(t)*cos(inc2);
    pos[2] = rmag2*sin(t)*sin(inc2);
    
    traj2->addTime(t);
    traj2->addPosition(pos);
  }
  
  // Specify the key press callback
  theWindow->setKeyPressCallback(KeyPressCallback);
  
  theWindow->startThread(); // Start window animation
  
  theWindow->join(); // Wait for window animation to finish
  
  return 0;
}
