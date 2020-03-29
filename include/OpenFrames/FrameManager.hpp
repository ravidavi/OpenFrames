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

/** \file FrameManager.hpp
 * Declaration of FrameManager class.
 */

#ifndef _OF_FRAMEMANAGER_
#define _OF_FRAMEMANAGER_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenThreads/Mutex>
#include <osg/Referenced>
#include <osg/ref_ptr>

namespace OpenFrames
{
  /**
   * \class FrameManager
   *
   * \brief This class allows priority access to a ReferenceFrame heirarchy.
   *
   * This class allows multiple clients to synchronize access to a
   * ReferenceFrame heirarchy, but allows high-priority clients
   * access before low-priority clients. This avoids the case where
   * high-fps rendering threads can hog the lock on certain OS's,
   * and significantly delay the user trying to acquire the lock.
   * To use: threads that frequently acquire the lock (e.g.
   * rendering threads) should use low priority, and threads that
   * need to make one-off changes to the scene graph (e.g. the user)
   * should use high priority.
   * NOTE: Implements "triple mutex" approach documented in:
   * https://stackoverflow.com/questions/11666610/how-to-give-priority-to-privileged-thread-in-mutex-locking
   * Approach summary: Data mutex D, Next-access mutex N, low-priority mutex L
   *  Low-priority Thread: lock L -> lock N -> lock D -> unlock N -> (...work...) -> unlock D -> unlock L
   *  High-priority Thread: lock N -> lock D -> unlock N -> (...work...) -> unlock D.
   */
  class OF_EXPORT FrameManager : public osg::Referenced
  {
  public:
	FrameManager(ReferenceFrame *frame = NULL) : _frame(frame) {}
  
  enum Priority
  {
    LOW_PRIORITY, // For frequent users, e.g. rendering threads
    HIGH_PRIORITY // For one-off users, e.g. modifying the scene
  };

  void setFrame(ReferenceFrame *frame)
  {
    lock();
    _frame = frame;
    unlock();
  }

	inline ReferenceFrame* getFrame() {return _frame.get();}
	inline osg::Group* getData() 
	{
	  if(_frame.valid()) return _frame->getGroup();
	  else return NULL;
	}

  inline int lock(Priority priority = HIGH_PRIORITY)
  {
    if(priority == LOW_PRIORITY) _mutexLP.lock();
    _mutexNext.lock();
    int val = _mutexData.lock();
    _mutexNext.unlock();
    return val;
  }
  
  /// Users must specify the same priority as they did for lock/trylock
  inline int unlock(Priority priority = HIGH_PRIORITY)
  {
    int val = _mutexData.unlock();
    if(priority == LOW_PRIORITY) _mutexLP.unlock();
    return val;
  }
  
  inline int trylock(Priority priority = HIGH_PRIORITY)
  {
    if(priority == LOW_PRIORITY) _mutexLP.lock();
    _mutexNext.lock();
    int val = _mutexData.trylock();
    _mutexNext.unlock();
    return val;
  }
  
  protected:
	virtual ~FrameManager() {}

	osg::ref_ptr<ReferenceFrame> _frame;
	OpenThreads::Mutex _mutexData, _mutexNext, _mutexLP;
  };

}

#endif
