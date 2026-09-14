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

#include <OpenFrames/OF_Interface.h>
#include <OpenThreads/Thread>
#include <iostream>

/** This example shows how to use the OpenFrames C interface **/
int main()
{
  // We will show a window multiple times. This shows how OpenFrames
  // can be initialized and cleaned up, then used again safely.
  // With each turn, the ReferenceFrame will get farther from the origin.
  int maxturn = 3;
  std::cout<< "Running " << maxturn << " times. Press escape or close the window to end the current turn." << std::endl;

  for(int turn = 0; turn < maxturn; ++turn)
  {
	std::cout<< "Running turn " << turn << std::endl;

	of_initialize(); // Setup to use Fortran/C Interface

	// Create the interface that will draw a scene onto a window.
	int winx = 300;
	int winy = 300;
	unsigned int width = 640;
	unsigned int height = 480;
	unsigned int gridx = 1;
	unsigned int gridy = 1;
	bool embedded = false;
	unsigned int id = 0;
	bool useVR = false;
	ofwin_createproxy(&winx, &winy, &width, &height, &gridx, &gridy, &embedded, &id, &useVR);

	// Create DrawableTrajectory to hold artists
	ofdrawtraj_create("Artists");

	// Offset and resize the DrawableTrajectory's axes 
	double pos[3] = {(double)turn, 0.0, 0.0}; // Base position for axes
	double length = 2.0, headRatio = 0.4; // headRatio = head/total length
	double bodyRadius = 0.1, headRadius = 0.4;
	offrame_movexaxis(pos, &length, &headRatio, &bodyRadius, &headRadius);
	offrame_moveyaxis(pos, &length, &headRatio, &bodyRadius, &headRadius);
	offrame_movezaxis(pos, &length, &headRatio, &bodyRadius, &headRadius);

	// Create marker artist, which by default will draw one marker at the origin
	ofmarkerartist_create("ma");
	ofmarkerartist_setmarkerimage("../Images/target.tiff");

	// Add artist to DrawableTrajectory
	ofdrawtraj_addartist("ma");

	// Create a view
	ofview_create("view");
        ofview_setviewframe("Artists", "Artists");

	// Reset the view to start out
	ofview_reset();

	// Create a frame manager that will allow access to the scene
	int fmid = 0;
	offm_create(&fmid);
	offm_setframe(); // Set the DrawableTrajectory as the root frame

	// Setup the window proxy
	ofwin_setscene(&id, &id);
	ofwin_addview(&id, &id);

  // Set up the HUD text
  ofwin_sethudtext(&id, &id, "Test HUD Text");

	// Create the actual window, start event handling and animations
	std::string winName = "Simple ReferenceFrame";
	ofwin_start();

	// Example of polling the window status in a loop
	unsigned int state;
	ofwin_isrunning(&state);
	while(state == 1)
	{
	  OpenThreads::Thread::YieldCurrentThread(); // Wait a bit
	  ofwin_isrunning(&state); // Check if window is still running
	}

	// Always call the cleanup function after you're done using the
	// OpenFrames Fortran/C interface
	of_cleanup();

  } // current turn

	// Go home and eat Rocky Road ice cream
	return 0;
}
