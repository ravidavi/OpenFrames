/***********************************
   Copyright 2018 Ravishankar Mathur

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

#ifndef _OF_DRAWABLETRAJECTORY_
#define _OF_DRAWABLETRAJECTORY_

#include <OpenFrames/Export.h>
#include <OpenFrames/TrajectoryArtist.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/ref_ptr>

namespace OpenFrames
{

/******************************************
 * Ravi Mathur
 * OpenFrames API, class DrawableTrajectory
 * This class provides a ReferenceFrame that a TrajectoryArtist can hook on to
 * in order to be able to draw its Trajectory.
******************************************/
class OF_EXPORT DrawableTrajectory : public ReferenceFrame
{
  public:
	DrawableTrajectory( const std::string &name );
	DrawableTrajectory( const std::string &name, float r, float g, float b, float a  = 1.0);

	// Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
	// Inherited from ReferenceFrame
	virtual void showContents(bool showContents);
	virtual bool getContentsShown() const;

	/** Manage internal list of TrajectoryArtists. */
	void addArtist(TrajectoryArtist *artist);
	void removeArtist(TrajectoryArtist *artist);
	void removeAllArtists();
	unsigned int getNumArtists() const;
	TrajectoryArtist* getArtist(unsigned int index);

	/** Inherited from ReferenceFrame */
	virtual const osg::BoundingSphere& getBound() const;
	virtual std::string frameInfo() const;

  protected:
	virtual ~DrawableTrajectory();

	void _init();

	osg::ref_ptr<osg::Geode> _geode; // Node to hold the artists
};

} //!namespace OpenFrames

#endif //!define _OF_DRAWABLETRAJECTORY_
