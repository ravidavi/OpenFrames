/***********************************
 Copyright 2019 Ravishankar Mathur
 
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

/** \file FramerateLimiter.hpp
 * Declaration of FramerateLimiter class.
 */

#ifndef _OF_FRAMERATELIMITER_
#define _OF_FRAMERATELIMITER_

#include <OpenFrames/Export.h>
#include <osg/Timer>

namespace OpenFrames
{
  /**
   * \class FramerateLimiter
   *
   * \brief Implements a framerate-limiting algorithm.
   *
   * Implements a simple framerate-limiting algorithm that waits the necessary amount of time
   * between frames, thereby achieving the desired framerate for any given workload.
   */
  class OF_EXPORT FramerateLimiter
  {
  public:
    FramerateLimiter(double fps = 30.0);

    // Set/get the desired framerate in frames/second
    void setDesiredFramerate(double fps);
    double getDesiredFramerate() const
    {
      if (_desiredSPF == 0.0) return 0.0;
      else return 1.0 / _desiredSPF;
    }

    // Get the measured framerate for the last frame
    double getFramerate() const { return 1.0 / _currSPF; }

    // Indicates the start of a frame
    void frame();

  private:
    double _desiredSPF; // Desired seconds/frame (inverse of fps)
    double _currSPF;  // Current measured seconds/frame

    const osg::Timer &_timer; // Timer for framerate control
    osg::Timer_t _startTick, _endTick; // Workload start/end times
    osg::Timer_t _prevStartTick; // Used to measure framerate
  };
  
} // !namespace OpenFrames

#endif // !_OF_FRAMERATELIMITER_
