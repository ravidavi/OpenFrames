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

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/SubtreeTracker.hpp>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames
{

SubtreeTracker::SubtreeTracker() 
{ 
#ifdef _OF_VERBOSE_
	std::cout<< "SubtreeTracker()" << std::endl; 
#endif
}

SubtreeTracker::SubtreeTracker( ReferenceFrame* frame )
{
#ifdef _OF_VERBOSE_
	std::cout<< "SubtreeTracker(frame)" << std::endl;
#endif
	setRoot(frame);
}

SubtreeTracker::~SubtreeTracker()
{
#ifdef _OF_VERBOSE_
	std::cout<< "~SubtreeTracker()" << std::endl;
#endif
	setRoot(NULL);
}

/* Add a child and its subtree to the frame list.  Do not add if we are already tracking a different frame with the same name as the child.  */
void SubtreeTracker::childAdded( ReferenceFrame* child,
				   ReferenceFrame* parent )
{
#ifdef _OF_VERBOSE_
	std::cout<< "In SubtreeTracker::childAdded()" << std::endl;
#endif

	if( _frames[parent->getName()] != parent ) // Should never happen!!
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "SubtreeTracker::childAdded() error: Parent does not exist in frame list!" << std::endl;
#endif
	  return;
	}

  	  // Get the currently tracked frame with the same name
	ReferenceFrame* c = _frames[child->getName()];

	  // If we are not currently tracking a frame with the same name,
	  // then add the child to the frame list.
	if( c == NULL )
	{
	  _frames[child->getName()] = child; // Add child to frame list
	  child->addTracker(this); // Register tracker with child
	  c = child;
	}

	  // If the currently tracked frame is the same as the child, then
	  // make sure that we are also tracking the child's subtree.
	if( c == child )
	  _addAllChildren(child); // Add child's subtree to frame list

	  // At this point, if the currently tracked frame is different
	  // from the child, then the child and its entire subtree
	  // will be ignored.

#ifdef _OF_VERBOSE_
	std::cout<< "Exiting SubtreeTracker::childAdded()" << std::endl;
#endif
}

/* Remove a child and its subtree from the frame list.  Do not remove if the child has other parents that we are still tracking */
void SubtreeTracker::childRemoved( ReferenceFrame* child,
				ReferenceFrame* parent )
{
#ifdef _OF_VERBOSE_
	std::cout<< "In SubtreeTracker::childRemoved()" << std::endl;
#endif
	if( _frames[parent->getName()] != parent )
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "SubtreeTracker::childRemoved() error: Parent does not exist in frame list!" << std::endl;
#endif
	  return;
	}

	  // Child was not being tracked anyway so don't do anything
	if( _frames[child->getName()] != child ) return;

	  /* We should keep tracking the child if it has any other
	     parents that we are currently tracking */
	int num_parents = child->getNumParents();
	for( int i = 0; i < num_parents; ++i )
	{
	  ReferenceFrame* p = child->getParent(i);
	  if( (p != parent) && (p == _frames[p->getName()]) )
	    return; 
	}

	  /* Since the child no longer has parents that are being tracked,
	     we can remove it and its children from the track list */
	_removeAllChildren( child );
	child->removeTracker(this);
	_frames.erase(child->getName());

#ifdef _OF_VERBOSE_
	std::cout<< "Exiting SubtreeTracker::childRemoved()" << std::endl;
#endif
}

/** Choose which frame's subtree should be tracked. */
void SubtreeTracker::setRoot( ReferenceFrame* frame )
{
#ifdef _OF_VERBOSE_
	std::cout<< "In SubtreeTracker::setRoot() with frame ";
	if(frame) std::cout<< frame->getName() << std::endl;
	else std::cout<< "NULL" << std::endl;
#endif

	if( _root == frame ) return;  // Nothing needs to be done

	  // Remove all currently tracked frames
	if( _root != NULL )
	{
	  FrameMap::iterator i;
	  for( i = _frames.begin(); i != _frames.end(); ++i )
	    i->second->removeTracker(this);
	  _frames.clear();
	}

	_root = frame; // Set new frame to be tracked

	  // Scan new tracked frame to discover all of its children
	if( _root != NULL )
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "   SubtreeTracker::setRoot():  Setting _root to " 
	      << frame->getName() << std::endl;
#endif

	  _root->addTracker(this); // Register tracker with the new frame
	  _frames[_root->getName()] = _root.get(); // Add _root to list  
	  rescan(); // Add children of _root to list
	}

#ifdef _OF_VERBOSE_
	std::cout<< "Exiting SubtreeTracker::setRoot()" << std::endl;
#endif
}

void SubtreeTracker::rescan()
{
	if(_root != NULL) _addAllChildren(_root.get());
}

ReferenceFrame* SubtreeTracker::getFrame( const std::string& name )
{
	ReferenceFrame* temp = _frames[name];
	if(!temp)
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "SubtreeTracker ERROR: Frame with name '" << name
	      << "' could not be found." << std::endl;
#endif
	  return NULL;
	}
	return temp;
}

/** Print a list (to std::cout) of all tracked frames */
void SubtreeTracker::printFrameList()
{
#ifdef _OF_VERBOSE_
	FrameMap::iterator i;
	for( i = _frames.begin(); i != _frames.end(); ++i )
	  std::cout<< "->" << i->first;
	std::cout<< std::endl;
#endif
}

/** Recursively add the subtree of the given frame to the frame list */
void SubtreeTracker::_addAllChildren( ReferenceFrame* frame )
{
	int num_children = frame->getNumChildren();
	for( int i = 0; i < num_children; ++i )
	  childAdded( frame->getChild(i), frame );
}

/** Recursively remove the subtree of the given frame from the frame list */
void SubtreeTracker::_removeAllChildren( ReferenceFrame* frame )
{
	int num_children = frame->getNumChildren();
	for( int i = 0; i < num_children; ++i )
	  childRemoved( frame->getChild(i), frame );
}

}
