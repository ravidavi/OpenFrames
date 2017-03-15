/***********************************
 Copyright 2017 Ravishankar Mathur
 
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

#ifndef _OF_FRAMERATELIMITER_
#define _OF_FRAMERATELIMITER_

#include <OpenFrames/Export.h>
#include <osg/Timer>

namespace OpenFrames{
  
  /**************************************
   * Ravi Mathur
   * OpenFrames API, class FramerateLimiter
   * This class implements a simple framerate limiting algorithm.  The method is to wait until a certain number of frames have elapsed then compute the average time per frame.  This is used to compute how much extra time each frame needs to take up in order to achieve the target framerate.  The limiter then applies this extra time to each frame in the next set, and the process is repeated.
   *************************************/
  class OF_EXPORT FramerateLimiter
  {
  public:
    FramerateLimiter();
    
    // Set/get the desired framerate in frames/second
    void setDesiredFramerate(double fps);
    double getDesiredFramerate()
    {
      if(_desired_spf == 0.0) return 0.0;
      else return 1.0/_desired_spf;
    }
    
    // Get the actual framerate for the previous set of frames
    double getFramerate() { return 1.0/_curr_spf; }
    
    // Indicates the start of a frame
    void frame();
    
    // Reset the timer
    void reset();
    
  private:
    double _desired_spf; // Desired seconds/frame (inverse of fps)
    double _curr_spf;  // Current measured seconds/frame
    
    unsigned int _max_frames; // Number of frames to use for averaging
    double _max_frames_inv; // Inverse of _max_frames (for efficiency)
    unsigned int _framecount; // Current number of elapsed frames
    
    int _sleeptime; // Time length to sleep for, in milli/microseconds (depending on system)
    const osg::Timer &_timer; // Timer, duh....
    osg::Timer_t _ref_time;  // Time at start of framerate check
  };
  
} // !namespace OpenFrames

#endif // !_OF_FRAMERATELIMITER_
