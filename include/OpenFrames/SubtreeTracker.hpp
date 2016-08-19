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

#ifndef _OF_SUBTREETRACKER_
#define _OF_SUBTREETRACKER_

#include <OpenFrames/Export.h>
#include <OpenFrames/FrameTracker.hpp>
#include <osg/ref_ptr>
#include <map>

namespace OpenFrames {

class ReferenceFrame;

/******************************************
 * Ravi Mathur
 * OpenFrames API, class SubtreeTracker
 * Implements a simple FrameTracker that remembers each frame in the tracked frame's subtree by name.  This allows for quick lookups of frames by their names, without having to traverse the subtree.  Note that if there are multiple frames with the same name, then only the first encountered frame will be remembered.  All other frames with the same name (and their subtrees) will be ignored.
*****************************************/
class OF_EXPORT SubtreeTracker : public FrameTracker {
	typedef std::map<std::string, ReferenceFrame* > FrameMap;

  public:
	SubtreeTracker();
	SubtreeTracker( ReferenceFrame* frame );

	  /** Called when a frame is added/removed from a tracked frame */
	virtual void childAdded( ReferenceFrame* child, ReferenceFrame* parent);
	virtual void childRemoved( ReferenceFrame* child, ReferenceFrame* parent );

	  /** Set which frame to track */
	virtual void setRoot( ReferenceFrame* frame );

	  /** Rescan the root's subtree to check if there are any previously
	      untracked frames that can now be tracked.  This should be called 
	      if a frame is removed from root's subtree, in case there are
	      different frames with the same name in the root's subtree. */
	virtual void rescan();

	  /** Lookup a frame based on its name */
	ReferenceFrame* getFrame( const std::string& name );

	  /** Print a list (to std::cout) of all tracked frames */
	void printFrameList();

  protected:
	virtual ~SubtreeTracker();

	  // Function to add/remove all children of a frame to the frame list
	void _addAllChildren( ReferenceFrame* frame );
	void _removeAllChildren( ReferenceFrame* frame );

	FrameMap _frames;  // All frames being tracked
};

} // !namespace OpenFrames

#endif  // !define _OF_SUBTREETRACKER_
