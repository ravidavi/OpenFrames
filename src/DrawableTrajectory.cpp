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

#include <OpenFrames/DrawableTrajectory>
#include <sstream>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames
{

DrawableTrajectory::DrawableTrajectory( const std::string &name ) 
	: ReferenceFrame(name)
{
	_init();
#ifdef _OF_VERBOSE_
	std::cout<< "DrawableTrajectory(" << name << ')' << std::endl;
#endif
}

DrawableTrajectory::DrawableTrajectory( const std::string &name, float r, float g,
                              float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	_init();
#ifdef _OF_VERBOSE_
	std::cout<< "DrawableTrajectory("<< name << ", " << r << ", "
		 << g << ", " << b << ", " << a << ')' << std::endl;
#endif
}

DrawableTrajectory::~DrawableTrajectory() 
{
#ifdef _OF_VERBOSE_
	std::cout<< "~DrawableTrajectory() for " << _name << std::endl;
#endif
}

void DrawableTrajectory::_init()
{
	  // Add this trajectory to this frame's transform
	_geode = new osg::Geode;
	_geode->setName(_name);
	osg::StateSet *stateset = _geode->getOrCreateStateSet();
	stateset->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	_xform->addChild(_geode.get());
}

/** Set the artist which will draw the trajectory */
void DrawableTrajectory::addArtist(TrajectoryArtist *artist)
{
	if(_geode->containsDrawable(artist)) return; // Artist already exists

	_geode->addDrawable(artist); // Add the new artist
}

void DrawableTrajectory::removeArtist(TrajectoryArtist *artist)
{
	_geode->removeDrawable(artist);
}

void DrawableTrajectory::removeAllArtists()
{
	_geode->removeDrawables(0, _geode->getNumDrawables());
}

unsigned int DrawableTrajectory::getNumArtists() const
{
	return _geode->getNumDrawables();
}

TrajectoryArtist* DrawableTrajectory::getArtist(unsigned int index)
{
	osg::Drawable* drawable = _geode->getDrawable(index);
	return static_cast<TrajectoryArtist*>(drawable);
}

const osg::BoundingSphere& DrawableTrajectory::getBound() const
{
	ReferenceFrame::getBound();
	osg::BoundingSphere bs = _geode->getBound();
	bs.expandRadiusBy(_bound);
	_bound = bs;

	return _bound;
}

std::string DrawableTrajectory::frameInfo() const
{
	std::string info = "DrawableTrajectory: ";
	std::stringstream numDrawables;
	numDrawables << _geode->getNumDrawables();
	info = info + "Contains " + numDrawables.str() + " drawables";

	return info;
}

} //!namespace OpenFrames
