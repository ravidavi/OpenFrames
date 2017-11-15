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

#ifndef _OF_FRAMEMANAGER_
#define _OF_FRAMEMANAGER_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenThreads/Mutex>
#include <osg/Referenced>
#include <osg/ref_ptr>

namespace OpenFrames
{

/*******************************************************
 * Ravi Mathur
 * OpenFrames API, class FrameManager
 * This class allows multiple clients to synchronize access to a
 * ReferenceFrame heirarchy.  This is important for cases when
 * only one client should be accessing a ReferenceFrame heirarchy
 * at any time.
*******************************************************/
class OF_EXPORT FrameManager : public osg::Referenced
{
  public:
	FrameManager(ReferenceFrame *frame = NULL) : _frame(frame) {}

	inline void setFrame(ReferenceFrame *frame)
	{
	  _lock.lock();
	  _frame = frame;
	  _lock.unlock();
	}

	inline ReferenceFrame* getFrame() {return _frame.get();}
	inline osg::Group* getData() 
	{
	  if(_frame.valid()) return _frame->getGroup();
	  else return NULL;
	}

	inline int lock() { return _lock.lock(); }
	inline int unlock() { return _lock.unlock(); }
	inline int trylock() { return _lock.trylock(); }
  protected:
	virtual ~FrameManager() {}

	osg::ref_ptr<ReferenceFrame> _frame;
	OpenThreads::Mutex _lock;
};

}

#endif
