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

/** FCN script handles difference between Windows/UNIX function 
  * naming conventions (gcc appends "__" to function names).
**/
#ifdef WIN32
	#define FCN(name) name
#else
	#define FCN(name) name##__
#endif

/** This example shows how to use the Fortran/C interface. If calling from
  * Fortran, then directly use the function names (no FCN macro)
  * e.g. in C: FCN(of_initialize)()    in Fortran: of_initialize()
**/
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

	FCN(of_initialize)(); // Setup to use Fortran/C Interface

	// Create the interface that will draw a scene onto a window.
	int winx = 300;
	int winy = 300;
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
	double pos[3] = {turn, 0.0, 0.0}; // Base position for axes
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

// This is only done because we're calling setviewframe() from C, where
// we have to specify string lengths. If calling from Fortran, the
// correct function signature would be automatically chosen and the
// ifdef would be unnecessary.
#ifdef IVF_CALLS
	FCN(ofview_setviewframe)("Artists", "Artists", 7, 7);
#else
	FCN(ofview_setviewframe)("Artists", 7, "Artists", 7);
#endif

	// Reset the view to start out
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

	// Example of polling the window status in a loop
	unsigned int state;
	FCN(ofwin_isrunning)(&state);
	while(state == 1)
	{
	  OpenThreads::Thread::YieldCurrentThread(); // Wait a bit
	  FCN(ofwin_isrunning)(&state); // Check if window is still running
	}

	// Always call the cleanup function after you're done using the
	// OpenFrames Fortran/C interface
	FCN(of_cleanup)();

  } // current turn

	// Go home and eat Rocky Road ice cream
	return 0;
}
