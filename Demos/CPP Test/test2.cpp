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

#include <OpenFrames/OF_FORTRAN>
#include <OpenFrames/WindowProxy>
#include <OpenFrames/MarkerArtist>
#include <iostream>

using namespace OpenFrames;

#ifdef WIN32
	#define FCN(name) name
#else
	#define FCN(name) name##__
#endif

int main()
{
	FCN(of_initialize)(); // Setup

	// Create the interface that will draw a scene onto a window.
	int winx = 30;
	int winy = 30;
	unsigned int width = 640;
	unsigned int height = 480;
	unsigned int gridx = 1;
	unsigned int gridy = 1;
	bool embedded = false;
	unsigned int id = 0;
	FCN(ofwin_createproxy)(&winx, &winy, &width, &height, &gridx, &gridy, &embedded, &id);

	// Create DrawableTrajectory to hold artists
	FCN(ofdrawtraj_create)("Artists", 7);

	// Offset and resize the DrawableTrajectory's axes 
	double pos[3] = {1.0, 0.0, 0.0}; // Base position for axes
	double length = 2.0, headRatio = 0.4; // headRatio = head/total length
	double bodyRadius = 0.1, headRadius = 0.4;
	FCN(offrame_movexaxis)(pos, &length, &headRatio, &bodyRadius, &headRadius);
	FCN(offrame_moveyaxis)(pos, &length, &headRatio, &bodyRadius, &headRadius);
	FCN(offrame_movezaxis)(pos, &length, &headRatio, &bodyRadius, &headRadius);

	// Create marker artist, which by default will draw one marker at the origin
	FCN(ofmarkerartist_create)("ma", 2);
	char filename[] = "../Images/target.tiff";
	int namelen = strlen(filename);
	FCN(ofmarkerartist_setmarkerimage)(filename, namelen);

	// Add artist to DrawableTrajectory
	FCN(ofdrawtraj_addartist)("ma", 2);

	// Create a view
	FCN(ofview_create)("view", 4);
	FCN(ofview_setviewframe)("Artists", 7, "Artists", 7);
	FCN(ofview_reset)();

	// Create a frame manager that will allow access to the scene
	int fmid = 0;
	FCN(offm_create)(&fmid);
	FCN(offm_setframe)(); // Set the DrawableTrajectory as the root frame

	// Setup the window proxy
	FCN(ofwin_setscene)(&id, &id);
	FCN(ofwin_addview)(&id, &id);

	  // Create the actual window, start event handling and animations
	std::string winName = "Simple ReferenceFrame";
	FCN(ofwin_start)();
	unsigned int state;
	FCN(ofwin_isrunning)(&state);
	while(state == 1)
	{
	  OpenThreads::Thread::YieldCurrentThread(); 
	  FCN(ofwin_isrunning)(&state);
	}

	  // Go home and eat Rocky Road ice cream
	return 0;
}
