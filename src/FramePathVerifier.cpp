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

/** \file FramePathVerifier.cpp
 * Definitions for the FramePathVerifier class.
 */

#include <OpenFrames/FramePathVerifier.hpp>
#include <OpenFrames/ReferenceFrame.hpp>

namespace OpenFrames
{

FramePathVerifier::FramePathVerifier()
{
	_valid = false;
}

FramePathVerifier::~FramePathVerifier()
{
	_clearPath();
}

void FramePathVerifier::childRemoved(ReferenceFrame *child, ReferenceFrame *parent)
{
	if(_framePath.empty()) return;

	  // Only iterate to pathSize-1 because we don't care if the last
	  // object in the path adds/deletes a child
	FramePath::iterator i;
	for( i = _framePath.begin(); i != _framePath.end() - 1; ++i )
	{
	    // If we find the (child,parent) pair then check if child is still
	    // another child of parent. If not, then the framePath is no longer
	    // valid.
	  if(*i == parent && *(i+1) == child && parent->getChildIndex(child) == -1)
	  { 
	    _clearPath();
	    return;
	  }
	}
}

void FramePathVerifier::setFramePath(const FramePath &path)
{
	if(_framePath == path) return;
	
	_clearPath(); // Release all tracked frames
	if(path.empty()) return;
	_framePath = path;
	_valid = true;

	  // Iterate through the path to make sure that each frame is its
	  // predecessor's child.
	FramePath::iterator i;
	for(i = _framePath.begin(); i != _framePath.end() - 1; ++i)
	{
	    // If a frame is not its predecessor's child, then the given FramePath
	    // cannot be valid.
	  if((*i)->getChildIndex(*(i+1)) == -1)
	  {
	    _clearPath();
	    return;
	  }
	  else (*i)->addTracker(this);
	}

	/* By this point, if the given FramePath is valid, _valid = true and each
	   frame in the FramePath (except the last frame) will know that we are
	   tracking it. */	
}

/** Stop tracking all previously tracked frames */
void FramePathVerifier::_clearPath()
{
	if( _framePath.empty() ) return;

	FramePath::iterator i;
	for( i = _framePath.begin(); i != _framePath.end() - 1; ++i )
	  (*i)->removeTracker(this);

	_framePath.clear();
	_valid = false;
}

}
