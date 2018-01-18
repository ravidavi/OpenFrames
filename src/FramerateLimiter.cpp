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

#include <OpenFrames/FramerateLimiter.hpp>
#include <math.h>

#ifdef WIN32
  #include <windows.h>
  #define FACTOR 1.0e3  // Windows sleep time in milliseconds
  #define MINSLEEP 1    // Minimum sleep time
  #define SLEEP_FCN Sleep
#else
  #include <unistd.h>
  #define FACTOR 1.0e6  // *nix sleep time in microseconds
  #define MINSLEEP 1000
  #define SLEEP_FCN usleep
#endif
  
namespace OpenFrames{

/** Constructor, defaults to 30fps */
FramerateLimiter::FramerateLimiter()
  	: _timer(*osg::Timer::instance())
{
	setDesiredFramerate(30.0);
}

void FramerateLimiter::setDesiredFramerate(double fps)
{
  if(fps > 0.0)
    _desired_spf = 1.0/fps; // Desired seconds per frame
  else
  {
    // Indicate unlimited framerate
    fps = 0.0;
    _desired_spf = 0.0;
  }

	// Number of frames before checking statistics
	_max_frames = (unsigned int)ceil(fps/2.0);
	if(_max_frames == 0) _max_frames = 128; // Unlimited framerate, so get stats infrequently
	_max_frames_inv = 1.0/(double)_max_frames;

	reset(); // Initialize everything
}

/** This indicates the start of a new frame.  If the new frame is also the first frame of a new group, then compute the statistics for the previous group and update the sleep time for the new group. */
void FramerateLimiter::frame()
{
	  // Collect statistics for the previous set of frames
	if(_framecount == _max_frames)
	{
	    // Calculate the average spf for the previous set of frames
	  _curr_spf = _timer.delta_s(_ref_time, _timer.tick())*_max_frames_inv;

	    // Approximate the extra amount of time that each frame
	    // should take up in order to reach the desired fps.
	  _sleeptime += (int)(FACTOR * (_desired_spf-_curr_spf));

	    // If the sleep time is very small (or negative), then it's not
	    // worth sleeping because the function call could take up more
	    // time than we want to sleep for.
	  if(_sleeptime < MINSLEEP) _sleeptime = 0;

	    // Reset the framecount and reference time to prepare for the
	    // next batch of frames to average.
	  _framecount = 0;
	  _ref_time = _timer.tick();
	}
	
	++_framecount;
	if(_sleeptime > 0) SLEEP_FCN(_sleeptime);
}

void FramerateLimiter::reset()
{
	_framecount = _max_frames;
	_sleeptime = 0;
	_ref_time = _timer.tick();
}

} // !namespace OpenFrames
