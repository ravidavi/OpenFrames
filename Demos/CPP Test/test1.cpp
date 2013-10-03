/***********************************
   Copyright 2013 Ravishankar Mathur

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

#include <OpenFrames/CoordinateAxes>
#include <OpenFrames/CurveArtist>
#include <OpenFrames/DrawableTrajectory>
#include <OpenFrames/FrameManager>
#include <OpenFrames/FrameTransform>
#include <OpenFrames/MarkerArtist>
#include <OpenFrames/Model>
#include <OpenFrames/RadialPlane>
#include <OpenFrames/SegmentArtist>
#include <OpenFrames/Trajectory>
#include <OpenFrames/WindowProxy>

using namespace OpenFrames;

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

double tscale = 1.0; // Animation speedup relative to real time
Model *spacestation;
CoordinateAxes *axes;
TimeManagementVisitor *tmv;
WindowProxy *theWindow;

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, char *key)
{
	static bool paused = false;
	static bool stereo = false;

	// Pause/unpause animation
	if(*key == 'p')
	{
	  paused = !paused;
	  tmv->setPauseState(true, paused);
	  spacestation->getTransform()->accept(*tmv);
	  axes->getTransform()->accept(*tmv);
	  tmv->setPauseState(false, paused);
	}

	// Reset time to epoch. All ReferenceFrames that are following
	// a Trajectory will return to their starting positions.
	else if(*key == 'r')
	{
	  tmv->setReset(true);
	  spacestation->getTransform()->accept(*tmv);
	  axes->getTransform()->accept(*tmv);
	  tmv->setReset(false);
	}

	// Speed up time
	else if((*key == '+') || (*key == '='))
	{
	  tscale += 0.1;
	  tmv->setTimeScale(true, tscale);
	  spacestation->getTransform()->accept(*tmv);
	  axes->getTransform()->accept(*tmv);
	  tmv->setTimeScale(false, tscale);
	}

	// Slow down time
	else if((*key == '-') || (*key == '_'))
	{
	  tscale -= 0.1;
	  tmv->setTimeScale(true, tscale);
	  spacestation->getTransform()->accept(*tmv);
	  axes->getTransform()->accept(*tmv);
	  tmv->setTimeScale(false, tscale);
	}
/*
	// Turn on stereoscopic (3D) rendering
	else if (*key == 's')
	{
	  stereo = !stereo;
	  osg::DisplaySettings *ds;
	  ds = theWindow->getGridPosition(0,0)->getSceneView()->getDisplaySettings();
	  ds->setStereo(stereo);
	  ds = theWindow->getGridPosition(1,0)->getSceneView()->getDisplaySettings();
	  ds->setStereo(stereo);
	}
*/
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
	theWindow = myWindow.get();

	// Set the stereoscopic (3D) viewing distance
	//osg::DisplaySettings *ds = theWindow->getGridPosition(0,0)->getSceneView()->getDisplaySettings();
	//ds->setScreenDistance(2.0);
	//ds = theWindow->getGridPosition(1,0)->getSceneView()->getDisplaySettings();
	//ds->setScreenDistance(2.0);

	// Create the object that will handle keyboard input 
	// This includes pausing, resetting, modifying time, etc...
	osg::ref_ptr<TimeManagementVisitor> mytmv = new TimeManagementVisitor; 
	tmv = mytmv.get();

	// Create the models that will populate the scene using
	// Model(name, color(r,g,b,a))
	spacestation = new Model("Space Station", 0, 1, 0, 0.9);
	Model *hubble = new Model("Hubble", 1, 0, 0, 0.9);

	// Set the 3D models
	spacestation->setModel("../Models/SpaceStation.3ds");
	hubble->setModel("../Models/Hubble.3ds");

	// Create the trajectory using
	// Trajectory(DOF, number of optionals)
	Trajectory *traj = new Trajectory(3, 1);

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
	data._scale = 0.01; // Scale down since positions are large
	ca->setXData(data);
	data._element = 2; // Use Z position for Y coordinate
	ca->setYData(data);
	data._src = Trajectory::TIME; // Use time for Z coordinate
	data._scale = 1.0; // Don't scale time
	ca->setZData(data);
	timehist->addArtist(ca);

	// Create an artist to draw start/intermediate/end markers
	MarkerArtist *ma = new MarkerArtist(traj);
	ma->setMarkers(MarkerArtist::START + MarkerArtist::INTERMEDIATE + MarkerArtist::END);
	ma->setAutoAttenuate(true);
	ma->setMarkerColor(MarkerArtist::START, 0, 1, 0);
	ma->setMarkerColor(MarkerArtist::END,   1, 0, 0);
	ma->setMarkerColor(MarkerArtist::INTERMEDIATE, 1, 1, 0);
	ma->setMarkerImage("../Images/fuzzyparticle.tiff");
	ma->setMarkerSize(10);
	data._src = Trajectory::POSOPT;
	data._element = 0; // Use X position for X coordinate
	data._scale = 0.01; // Scale down since positions are large
	ma->setXData(data);
	data._element = 2; // Use Z position for Y coordinate
	ma->setYData(data);
	data._src = Trajectory::TIME; // Use time for Z coordinate
	data._scale = 1.0; // Don't scale time
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
	ma->setIntermediateSpacing(10.0); // Every 10 distance units
	ma->setIntermediateDirection(MarkerArtist::END); // From end of trajectory

	// Create a set of Coordinate Axes for time history plot
	axes = new CoordinateAxes("axes", 0.0, 0.8, 0.8, 1);
	axes->setAxisLength(2.0*M_PI);
	axes->setTickSpacing(M_PI, 0.25*M_PI);
	axes->setTickSize(8, 5);
	axes->setTickImage("../Images/circle.tiff");
	axes->setXLabel("X");
	axes->setYLabel("Z");
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
	centermarker->setMarkerImage("../Images/target.tiff");
	centermarker->setMarkerSize(10);

	// Add the markerartist to the drawable trajectory
	drawcenter->addArtist(centermarker);

	// Create a RadialPlane to show trace frame's orientation
	RadialPlane *rp = new RadialPlane("radial", 1, 1, 1, 1);
	rp->showAxes(ReferenceFrame::NO_AXES);
	rp->showAxesLabels(ReferenceFrame::NO_AXES);
	rp->showNameLabel(false);
	rp->setParameters(10.0, 2.5, 60.0*M_PI/180.0);

	// Set up reference frame heirarchies.
	spacestation->addChild(drawtraj);
	spacestation->addChild(hubble);
	axes->addChild(timehist);
	axes->addChild(trace);
	trace->addChild(drawcenter);
	axes->addChild(rp);

	// Tell model to follow trajectory (by default in LOOP mode)
	TrajectoryFollower *tf = new TrajectoryFollower(traj);
	tf->setTimeScale(tscale);
	hubble->getTransform()->setUpdateCallback(tf);

	// Tell trace frame to follow time history
	tf = new TrajectoryFollower(traj);
	tf->setTimeScale(tscale);
	data._src = Trajectory::POSOPT;
	data._opt = 0;
	data._element = 0; // Use X position for X coordinate
	data._scale = 0.01; // Scale down since positions are large
	tf->setXData(data);
	data._element = 2; // Use Z position for Y coordinate
	tf->setYData(data);
	data._src = Trajectory::TIME; // Use time for Z coordinate
	data._scale = 1.0;
	tf->setZData(data);
	trace->getTransform()->setUpdateCallback(tf);

	// Tell radial frame to follow time history's orientation
	tf = new TrajectoryFollower(traj);
	tf->setFollowType(TrajectoryFollower::ATTITUDE, TrajectoryFollower::LOOP);
	tf->setTimeScale(tscale);
	rp->getTransform()->setUpdateCallback(tf);
	rp->setPosition(0.0, 0.0, 0.0);

	// Create views
	View *view = new View(spacestation, spacestation);
	View *view2 = new View(spacestation, hubble);
	View *view3 = new View(axes, axes);
	View *view4 = new View(axes, trace);

	// Create a manager to handle the spatial scene
	FrameManager* fm = new FrameManager;
	fm->setFrame(spacestation);

	// Create a manager to handle the time history scene
	FrameManager* fm2 = new FrameManager;
	fm2->setFrame(axes);

	// Set up the scene
	theWindow->setScene(fm, 0, 0);
	theWindow->setScene(fm2, 1, 0);
	theWindow->getGridPosition(0, 0)->setSkySphereTexture("../Images/StarMap.tif");
	theWindow->getGridPosition(0, 0)->addView(view);
	theWindow->getGridPosition(0, 0)->addView(view2);
	theWindow->getGridPosition(1, 0)->addView(view3);
	theWindow->getGridPosition(1, 0)->addView(view4);

	// Add the actual positions and attitudes for the trajectory.
	osg::Quat att; // Quaternion for attitude transformations
	double pos[3], vel[3];
	pos[1] = vel[1] = 0.0;
	for(double t = 0.0; t <= 2.0*M_PI; t += M_PI/90.0)
	{
	  pos[0] = 500.0*sin(t);
	  pos[2] = 500.0*cos(t);
	  vel[0] = pos[0] + 0.5*pos[2];
	  vel[2] = pos[2] - 0.5*pos[0];
	  att.makeRotate(t, 0, 1, 0);

	  traj->addTime(t);
	  traj->addPosition(pos);
	  traj->setOptional(0, vel);
	  traj->addAttitude(att[0], att[1], att[2], att[3]);
	}

	// Specify the key press callback
	theWindow->setKeyPressCallback(KeyPressCallback);

	theWindow->startThread(); // Start window animation

	theWindow->join(); // Wait for window animation to finish

	return 0;
}
