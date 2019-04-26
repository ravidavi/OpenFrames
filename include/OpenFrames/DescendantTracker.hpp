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

/** \file DescendantTracker.hpp
 * Declaration of DescendantTracker class.
 */

#ifndef _OF_DESCENDANTTRACKER_
#define _OF_DESCENDANTTRACKER_

#include <OpenFrames/Export.h>
#include <OpenFrames/FrameTracker.hpp>
#include <vector>

namespace OpenFrames {

class ReferenceFrame;
typedef std::vector<ReferenceFrame*> FramePath;

  /**
   * \class DescendantTracker
   *
   * \brief This class stores the path from the tracked frame.
   *
   * This class implements a FrameTracker that stores the path from the tracked frame
   * to a particular descendant frame.
   */
  class OF_EXPORT DescendantTracker : public FrameTracker
  {
  public:
	DescendantTracker();
	DescendantTracker( ReferenceFrame* frame );

	virtual void childAdded( ReferenceFrame* child, ReferenceFrame* parent ) {}
	
	  /** Called when a frame is removed from a tracked frame, because
	      removing a frame *could* change the path to the descendant */
	virtual void childRemoved( ReferenceFrame* child, ReferenceFrame* parent ); 

	  /** Set which frame to track */
	virtual void setRoot( ReferenceFrame* frame );

	  /** Set which descendant of the root to track */
	bool trackDescendant( ReferenceFrame* frame );
	inline ReferenceFrame* getTrackedDescendant()
	{
	  if( _tracking ) return _framePath.back();
	  else return NULL;
	}

	  /** Get the frame path from root to tracked descendant */
	inline const FramePath& getFramePath() { return _framePath; }

	  /** Is the frame tracking a descendant */	
	inline bool isTrackingDescendant() { return _tracking; }

  protected:
	virtual ~DescendantTracker();

	bool _trackDescendant( ReferenceFrame* frame, ReferenceFrame* parent );
	void _clearPath();

	FramePath _framePath;
	bool _tracking;
  }; // !class DescendantTracker

} // !namespace OpenFrames
#endif // !_OF_DESCENDANTTRACKER_
