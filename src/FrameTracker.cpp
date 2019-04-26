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

/** \file FrameTracker.cpp
 * Definitions for the FrameTracker class.
 */

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/FrameTracker.hpp>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames{

FrameTracker::FrameTracker() 
{ 
#ifdef _OF_VERBOSE_
	std::cout<< "FrameTracker()" << std::endl; 
#endif
}

FrameTracker::FrameTracker( ReferenceFrame* frame )
{
#ifdef _OF_VERBOSE_
	std::cout<< "FrameTracker(";
	if(frame == NULL) std::cout<< "NULL";
	else std::cout<< frame->getName();
	std::cout<< ')' << std::endl;
#endif
	setRoot(frame);
}

FrameTracker::~FrameTracker()
{
#ifdef _OF_VERBOSE_
	std::cout<< "~FrameTracker()" << std::endl;
#endif
	setRoot(NULL);
}

/** Choose which frame to track. */
void FrameTracker::setRoot( ReferenceFrame* frame )
{
#ifdef _OF_VERBOSE_
	std::cout<< "In FrameTracker::setRoot() with frame ";
	if(frame) std::cout<< frame->getName() << std::endl;
	else std::cout << "NULL" << std::endl;
#endif
	if( _root == frame ) 
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "Exiting FrameTracker::setRoot()" << std::endl;
#endif
	  return; // Nothing needs to be done
	}

	  // Deregister this tracker from the current tracked frame
	if( _root != NULL ) _root->removeTracker(this);

	  // Register this tracker with the new tracked frame
	_root = frame;
	if( _root != NULL ) _root->addTracker(this);

#ifdef _OF_VERBOSE_
	std::cout<< "Exiting FrameTracker::setRoot()" << std::endl;
#endif
}

} // !namespace OpenFrames
