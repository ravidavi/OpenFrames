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

/** \file FrameTracker.hpp
 * Declaration of FrameTracker class.
 */

#ifndef _OF_FRAMETRACKER_
#define _OF_FRAMETRACKER_

#include <OpenFrames/Export.h>

#include <osg/ref_ptr>
#include <osg/Referenced>

namespace OpenFrames
{
  class ReferenceFrame;

  /**
   * \class FrameTracker
   *
   * \brief This class tracks children of a reference frame.
   *
   * This abstract base class tracks the children of a particular reference frame. How the tracking is done
   * is up to the deriving subclass.
   */
  class OF_EXPORT FrameTracker : public osg::Referenced
  {
  public:
	FrameTracker();
	FrameTracker( ReferenceFrame* frame );

	  // Called when a frame is added to or removed from a tracked frame
	virtual void childAdded(ReferenceFrame* child, ReferenceFrame* parent) = 0;
	virtual void childRemoved(ReferenceFrame* child, ReferenceFrame* parent) = 0;

	  // Set which frame to track
	virtual void setRoot( ReferenceFrame* frame );
	inline ReferenceFrame* getRoot() {return _root.get();}
	
  protected:
	virtual ~FrameTracker();

	osg::ref_ptr<ReferenceFrame> _root;
  };

} // !namespace OpenFrames

#endif  // !define _OF_FRAMETRACKER_
