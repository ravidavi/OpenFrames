/***********************************
   Copyright 2023 Ravishankar Mathur

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

/** \file DescendantTracker.cpp
 * Definitions for the DescendantTracker class.
 */

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/DescendantTracker.hpp>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames
{

DescendantTracker::DescendantTracker() 
	: _tracking(false)
{ 
#ifdef _OF_VERBOSE_
	std::cout<< "DescendantTracker()" << std::endl;
#endif
}

DescendantTracker::DescendantTracker( ReferenceFrame* frame )
	: _tracking(false)
{
#ifdef _OF_VERBOSE_
	std::cout<< "DescendantTracker(";
	if(frame == NULL) std::cout<< "NULL";
	else std::cout<< frame->getName();
	std::cout<< ')' << std::endl;
#endif
	setRoot(frame);
}

DescendantTracker::~DescendantTracker()
{
#ifdef _OF_VERBOSE_
	std::cout<< "~DescendantTracker()" << std::endl;
#endif
	setRoot(NULL);
}

/** If the removed (child,parent) pair occur consecutively in the framePath, 
	then search the root again to see if there is another path from root to 
	the currently tracked descendant */
void DescendantTracker::childRemoved( ReferenceFrame* child,
					ReferenceFrame* parent )
{
	if(!_root) return; // Root has not been specified yet

#ifdef _OF_VERBOSE_
	std::cout<< "DescendantTracker:: Removing child "
		<< child->getName() << " from parent "
		<< parent->getName() << std::endl;
#endif

	if( _framePath.empty() ) return; // Nothing was being tracked anyway

	  // Only iterate to pathSize-1 because the last object in the
	  // path will be the frame currently being tracked and we don't
	  // care if that frame adds/deletes a child.
	FramePath::iterator i;
	for( i = _framePath.begin(); i != _framePath.end() - 1; ++i )
	{
	    // If we find the (child,parent) pair then clear the framepath
	    // and attempt to relocate the currently tracked frame
	  if(*i == parent && *(i+1) == child)
	  { 
	    ReferenceFrame* desc = _framePath.back();
	    _clearPath();
	    _tracking = _trackDescendant(desc, _root.get());
	    return;
	  }
	}
}

/** Set the frame whose descendant should be tracked. */
void DescendantTracker::setRoot( ReferenceFrame* frame )
{
#ifdef _OF_VERBOSE_
	std::cout<< "DescendantTracker::setRoot(";
	if(frame) std::cout<< frame->getName();
	else std::cout << "NULL";
	std::cout << ')' << std::endl;
#endif

	if(_root == frame) return; // Nothing needs to be done
	_tracking = false;
	_clearPath();
	_root = frame;

#ifdef _OF_VERBOSE_
	std::cout<< "Exiting DescendantTracker::setRoot(";
	if(frame) std::cout<< frame->getName();
	else std::cout << "NULL";
	std::cout << ')' << std::endl;
#endif
}

/** Set which frame should be tracked as a descendant */
bool DescendantTracker::trackDescendant( ReferenceFrame* frame )
{
	if(!_root) return false; // Root has not been specified yet

	  // We are already tracking the desired descendant
	if(!_framePath.empty() && (frame == _framePath.back())) 
	  return true;

	_tracking = false;
	_clearPath();

	if(_root == frame) // Just track the root
	{
	  _framePath.push_back(frame);
	  _tracking = true;
	} 
	else if( frame ) // Make sure frame isn't NULL
	{
	  _tracking = _trackDescendant( frame, _root.get() );
	}

	return _tracking;
}

/** Find the desired frame in the child list of the parent */
bool DescendantTracker::_trackDescendant( ReferenceFrame* frame,
					ReferenceFrame* parent )
{
	  // Check if frame is a direct child of the parent
	if( parent->getChildIndex(frame) != -1 )
	{
	  _framePath.push_back(parent); // Add parent and frame to end
	  _framePath.push_back(frame);  // of _framePath and tell parent
	  parent->addTracker(this);    // that it is being tracked.
	  return true;
	    // Note that we don't need to actively track the child because we
	    // don't care if any children are added to or removed from it.
	}

	  // Recursively check all children to see if frame is
	  // one of their descendants
	int num_children = parent->getNumChildren();
	for( int i = 0; i < num_children; ++i )
	{
	  if( _trackDescendant( frame, parent->getChild(i) ) )
	  {
	    _framePath.insert(_framePath.begin(), parent);
	    parent->addTracker(this);
	    return true;
	  }
	}

	return false; // Frame is not a descendant of this frame
}

/** Stop tracking all previously tracked frames */
void DescendantTracker::_clearPath()
{
	if( _framePath.empty() ) return;

	FramePath::iterator i;
	for( i = _framePath.begin(); i != _framePath.end() - 1; ++i )
	  (*i)->removeTracker(this);

	_framePath.clear();
}

} // !namespace OpenFrames
