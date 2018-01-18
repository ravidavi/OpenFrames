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
#include <OpenFrames/WindowProxy.hpp>
#include <iostream>
#include <cmath>
#include <osg/Math>

using namespace OpenFrames;

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
    theWindow->setTimeScale(theWindow->getTimeScale() + 0.05);
	}

	// Slow down time
	else if((*key == '-') || (*key == '_'))
	{
    theWindow->setTimeScale(theWindow->getTimeScale() - 0.05);
	}
}

/** This example shows how to create multiple subwindows, and have
  * a ReferenceFrame follow a path defined by Trajectory points. It also
  * shows how to use Artists to draw a single Trajectory in several
  * different ways.
**/
int main()
{
        double km = 1.0; // Graphics units per km
        std::cout.precision(15);
        std::cout.setf(std::ios::scientific, std::ios::floatfield);

	// Create the interface that will draw a scene onto a window.
	osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  myWindow->setTimeScale(1.0);
	theWindow = myWindow.get();

	// Create the objects that will populate the scene using
        // Sphere(name, color(r,g,b,a))
	// Model(name, color(r,g,b,a))
	Sphere *earth = new Sphere("Earth", 0, 1, 0, 0.9);
	Model *hubble = new Model("Hubble", 1, 0, 0, 0.9);

        // Set Earth parameters
        earth->setRadius(6371.0*km);
        earth->setTextureMap("Images/EarthTexture.bmp");

	// Set the spacecraft parameters
        // Scale model down to 1cm
	hubble->setModel("Models/Hubble.3ds");
        double modelScale = 0.00001*km/hubble->getModel()->getBound()._radius;
        hubble->setModelScale(modelScale, modelScale, modelScale);

	// Create the trajectory using
	// Trajectory(DOF, number of optionals)
	Trajectory *traj = new Trajectory(3, 0);

	// Create a drawable trajectory for the spacecraft window using
	// DrawableTrajectory(name, color(r,g,b,a))
	DrawableTrajectory *drawtraj = new DrawableTrajectory("traj", 1, 0, 0, 0.9);
	drawtraj->showAxes(ReferenceFrame::NO_AXES);
	drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
	drawtraj->showNameLabel(false);

        // Create an artist to draw start/intermediate/end markers
        MarkerArtist *ma = new MarkerArtist(traj);
        ma->setMarkers(MarkerArtist::START + MarkerArtist::INTERMEDIATE + MarkerArtist::END);
        ma->setAutoAttenuate(true);
        ma->setMarkerColor(MarkerArtist::START, 0, 1, 0); // Green
        ma->setMarkerColor(MarkerArtist::END,   1, 0, 0); // Red
        ma->setMarkerColor(MarkerArtist::INTERMEDIATE, 1, 1, 0); // Yellow
        ma->setMarkerShader("Shaders/Marker_Rose.frag");
        ma->setMarkerSize(10); // In pixels
        drawtraj->addArtist(ma);

	// Create a CurveArtist for the trajectory.  By default the CurveArtist
	// will use x/y/z positions from the trajectory for plotting.
	CurveArtist *ca = new CurveArtist(traj);
	ca->setWidth(2.0); // Line width for the trajectory
	ca->setColor(0, 1, 0);
	drawtraj->addArtist(ca);

	// Tell model to follow trajectory (by default in LOOP mode)
	TrajectoryFollower *tf = new TrajectoryFollower(traj);
        tf->setFollowType(TrajectoryFollower::POSITION + TrajectoryFollower::ATTITUDE, TrajectoryFollower::LIMIT);
	hubble->getTransform()->setUpdateCallback(tf);

	// Create a drawable trajectory for the spacecraft center marker
	// DrawableTrajectory(name)
	DrawableTrajectory *drawcenter = new DrawableTrajectory("center marker");
	drawcenter->showAxes(ReferenceFrame::NO_AXES);
	drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
	drawcenter->showNameLabel(false);

        // Create an artist to draw spacecraft center marker
        MarkerArtist *centermarker = new MarkerArtist;
	centermarker->setMarkerShader("Shaders/Marker_CirclePulse.frag");
	centermarker->setMarkerSize(15);

	// Add the markerartist to the drawable trajectory
	drawcenter->addArtist(centermarker);

	// Set up reference frame heirarchies.
	earth->addChild(drawtraj);
	earth->addChild(hubble);
        hubble->addChild(drawcenter);

	// Create views
	View *view = new View(earth, earth);
	View *view2 = new View(earth, hubble);

	// Create a manager to handle the spatial scene
	FrameManager* fm = new FrameManager;
	fm->setFrame(earth);

	// Set up the scene
	theWindow->setScene(fm, 0, 0);
        theWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0);
	//theWindow->getGridPosition(0, 0)->setSkySphereTexture("Images/StarMap.tif");
	theWindow->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 6.0, 40000);
	theWindow->getGridPosition(0, 0)->addView(view);
	theWindow->getGridPosition(0, 0)->addView(view2);

	// Add the actual positions and attitudes for the trajectory.
	osg::Quat att; // Quaternion for attitude transformations
	double t, pos[3];
	pos[2] = 0.0;
        const double rmag = 1000000000.0*km;
        const int numPoints = 360;
        for(int i = 0; i <= numPoints; ++i)
	{
          t = ((double)i)*2.0*osg::PI/((double)numPoints);
          pos[0] = rmag*std::cos(t);
          pos[1] = rmag*std::sin(t);
	  att.makeRotate(t, 0, 0, 1);

	  traj->addTime(10*t);
	  traj->addPosition(pos);
	  traj->addAttitude(att[0], att[1], att[2], att[3]);
	}

	// Specify the key press callback
	theWindow->setKeyPressCallback(KeyPressCallback);

	theWindow->startThread(); // Start window animation

	theWindow->join(); // Wait for window animation to finish

	return 0;
}
