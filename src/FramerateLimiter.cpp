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

/** \file FramerateLimiter.cpp
 * Definitions for the FramerateLimiter class.
 */

#include <OpenFrames/FramerateLimiter.hpp>
#include <OpenThreads/Thread>
  
namespace OpenFrames{

/** Constructor */
FramerateLimiter::FramerateLimiter(double fps)
  	: _timer(*osg::Timer::instance()),
    _startTick(0), _endTick(0), _prevStartTick(0)
{
	setDesiredFramerate(fps);
}

void FramerateLimiter::setDesiredFramerate(double fps)
{
  if (fps > 0.0)
    _desiredSPF = 1.0 / fps; // Desired seconds per frame

  else
    _desiredSPF = 0.0; // Indicate unlimited framerate

  _currSPF = _desiredSPF;
}

/** Indicates the start of a new frame */
void FramerateLimiter::frame()
{
  // This frame begins after the previous frame's workload ends
  _endTick = _timer.tick();

  // Compute time taken by workload
  double workTime = _timer.delta_s(_startTick, _endTick);

  // Sleep until the next workload should begin
  double sleepTime = _desiredSPF - workTime;
  if (sleepTime > 0.0) OpenThreads::Thread::microSleep((unsigned int)(sleepTime*1.0e6));

  // Get next workload start time
  _startTick = _timer.tick();

  // Update statistics (considered part of next workload)
  _currSPF = _timer.delta_s(_prevStartTick, _startTick);
  _prevStartTick = _startTick;
}

} // !namespace OpenFrames
