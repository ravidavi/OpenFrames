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

#ifndef _OF_FRAMEPATHVERIFIER_
#define _OF_FRAMEPATHVERIFIER_

#include <OpenFrames/Export.h>
#include <OpenFrames/FrameTracker.hpp>
#include <vector>

namespace OpenFrames
{

class ReferenceFrame;
typedef std::vector<ReferenceFrame*> FramePath;

/*************************************************************
 * Ravi Mathur
 * OpenFrames API, class FramePathVerifier
 * This class keeps track of whether a given FramePath is valid, meaning that all the
 * frames in the FramePath occur in that order in the actual ReferenceFrame tree.
 * For example, if the FramePath is [A, B, C], then this class verifies that C is a
 * direct child of B, which is a direct child of A.
 * This class can be seen as a variation of the OpenFrames::DescendantTracker class,
 * the difference being that this class does not keep a ref_ptr to any root node
 * and does not try to reform the FramePath if it is broken.  This class simply
 * keeps track of whether the given FramePath is valid or not.
************************************************************/
class OF_EXPORT FramePathVerifier : public FrameTracker
{
  public:
	FramePathVerifier();

	  // Called when a frame is added to or removed from a tracked frame
	virtual void childAdded(ReferenceFrame* child, ReferenceFrame* parent) {};
	virtual void childRemoved(ReferenceFrame* child, ReferenceFrame* parent);

	  // Does not apply to this type of FrameTracker
	virtual void setRoot( ReferenceFrame* frame ) {}

	void setFramePath(const FramePath &path);
	inline bool isFramePathValid() {return _valid;}
  protected:
	virtual ~FramePathVerifier();
	void _clearPath();

	FramePath _framePath;
	bool _valid;
};

} // !namespace OpenFrames

#endif
