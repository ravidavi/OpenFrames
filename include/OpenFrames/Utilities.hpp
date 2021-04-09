/***********************************
 Copyright 2020 Ravishankar Mathur
 
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

/** \file Utilities.hpp
 * Declaration of Utilities class.
 */

#ifndef _OF_UTILITIES_
#define _OF_UTILITIES_

#include <OpenFrames/Export.h>
#include <OpenThreads/Condition>
#include <OpenThreads/Mutex>
#include <osg/Matrixd>
#include <osg/View>

namespace OpenFrames {
  
  /** OpenFrames function that updates a projection matrix with specified near/far plane */
  void updateProjectionMatrix(osg::Matrix& proj, const double &zNear, const double &zfar);
  
  /** Get the osg::View's viewport by searching its master camera then slave cameras */
  osg::Viewport* getMainViewport(osg::View *view);
  
  /** Get the osg::View's graphics context by searching its master camera then slave cameras */
  osg::GraphicsContext* getMainGraphicsContext(osg::View *view);
  
  /** Implements a reader-writer mutex that is biased towards writers.
      Algorithm comes from https://github.com/angrave/SystemProgramming/wiki/Synchronization,-Part-7:-The-Reader-Writer-Problem
   */
  class OF_EXPORT ReadWriteMutex
  {
  public:
    ReadWriteMutex()
    : _writerWaitCount(0), _writerCount(0), _readerCount(0)
    {}
    
    /// Acquire the read lock
    /// This gives preference to waiting writers
    int readLock();
    
    /// Release the read lock
    int readUnlock();
    
    /// Acquire the write lock
    int writeLock();
    
    /// Release the write lock
    int writeUnlock();
    
  protected:
    int _writerWaitCount; // Number of writers waiting to enter the critical section
    int _writerCount;     // Number of writers in the critical section (0 or 1)
    int _readerCount;     // Number of readers in the critical section
    // Note that if _readerCount > 0 then _writerCount must be 0 (and vice versa)
    
    OpenThreads::Condition _turnCond; // Wait for a turn to enter critical section
    OpenThreads::Mutex _countLock;    // Lock while updating counters
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_UTILITIES_
