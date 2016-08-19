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

#include <OpenFrames/Sphere.hpp>
#include <osg/Vec3d>
#include <osg/Quat>
#include <osg/Geode>
#include <osg/PolygonOffset>
#include <osg/ShapeDrawable>
#include <osg/Shape>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgUtil/CullVisitor>
#include <iostream>

namespace OpenFrames
{

/** A cull callback that sets the detail level of a Sphere based on how far
    away it is from the eye point. */
struct SphereLODCallback : public osg::DrawableCullCallback
{
	SphereLODCallback(const osg::Sphere& sphere, osg::TessellationHints& hints) 
	: _sphere(sphere), _hints(hints), _currentLOD(0) {}

	virtual bool cull(osg::NodeVisitor* nv, osg::Drawable* drawable, osg::RenderInfo* renderInfo) const 
	{
	  osgUtil::CullVisitor *cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	  if(cv)
	  {
	    // Get the eye point in the Sphere's local coordinates
            // Can't use cv->getEyeLocal() since Vec3=Vec3f
	    osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();

	    // Compute the ratio of distance to sphere radius
	    double ratio = (eye - _sphere.getCenter()).length() / _sphere.getRadius();
	    int lod;
	    if(ratio <= 1.0) lod = 50;
	    else if(ratio >= 5.0) lod = 10;
	    else lod = 50.0/ratio;

	    if(_currentLOD != lod)
	    {
	      _hints.setDetailRatio(0.1*(double)lod);
	      drawable->dirtyDisplayList();
	      _currentLOD = lod;
	    }
	  }

	  return false; 
	}

	const osg::Sphere& _sphere;
	osg::TessellationHints& _hints;
	mutable int _currentLOD;
};

Sphere::Sphere(const std::string &name)
	: ReferenceFrame(name)
{
	_init();
}

Sphere::Sphere(const std::string &name, const osg::Vec3 &color)
	: ReferenceFrame(name, color)
{
	_init();
}

Sphere::Sphere(const std::string &name, const osg::Vec4 &color)
	: ReferenceFrame(name, color)
{
	_init();
}

Sphere::Sphere(const std::string &name, float r, float g, float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	_init();
}

Sphere::~Sphere() { }

/** Create the sphere with default radius = 1,
    and color = the color of the sphere's reference frame. */
void Sphere::_init()
{
	osg::ShapeDrawable* sd = new osg::ShapeDrawable;
	osg::Sphere* sphere = new osg::Sphere;
	osg::TessellationHints* hints = new osg::TessellationHints;

	// Set the shape to be drawn
	sd->setShape(sphere);
	sd->setTessellationHints(hints);

	// Don't need very high detail on a simple sphere
	hints->setDetailRatio(2.0);

	// Offset the sphere's polygons backwards so that a decal on the sphere's
	// surface will always show up on top of the sphere.
	osg::PolygonOffset *offset = new osg::PolygonOffset(1, 1);
	sd->getOrCreateStateSet()->setAttributeAndModes(offset);

	// Create the node that contains the Sphere
	_geode = new osg::Geode;
	_geode->setName(_name);
	_geode->addDrawable(sd);
	
	// Add the sphere to the ReferenceFrame
	_xform->addChild(_geode.get());

	// Resize the axes vectors and set the appropriate color
	setRadius(1.0);
	setColor(getColor());
}

void Sphere::setRadius(const double &radius)
{
	osg::Drawable *drawable = _geode->getDrawable(0);
	osg::Sphere *sphere = static_cast<osg::Sphere*>(drawable->getShape());
	sphere->setRadius(radius);
	drawable->dirtyBound();

	moveXAxis(osg::Vec3(radius, 0, 0), 0.5*radius);
	moveYAxis(osg::Vec3(0, radius, 0), 0.5*radius);
	moveZAxis(osg::Vec3(0, 0, radius), 0.5*radius);
}

double Sphere::getRadius()
{
	osg::Drawable *drawable = _geode->getDrawable(0);
	osg::Sphere *sphere = static_cast<osg::Sphere*>(drawable->getShape());
	return sphere->getRadius();
}

bool Sphere::setTextureMap(const std::string &fname, bool force_reload)
{
	if(fname.length() == 0) // Remove existing texture
	{
	  osg::StateSet* stateset = _geode->getStateSet();
	  if(stateset)
	  {
	    stateset->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
	    stateset->removeTextureAttribute(0, osg::StateAttribute::TEXENV);
	  }

	  return false;
	}

	// Check if there is already a texture being used.
	osg::StateSet* stateset = _geode->getOrCreateStateSet();
	osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(stateset->getTextureAttribute(0, osg::StateAttribute::TEXTURE));
	
	// If the current texture has the same filename as the new texture, then reload only if we have to.
	if(texture && (texture->getImage()->getFileName() == fname) && !force_reload) return true;

	osg::Image* image = osgDB::readImageFile(fname);
	if(image)
	{
	  // Create texture using image, and make sure it wraps around the
	  // sphere without a seam at the edges.
	  texture = new osg::Texture2D;
	  texture->setImage(image);
	  texture->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
	  texture->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);

	  // Don't use the sphere's color when mapping the texture.
	  osg::TexEnv* texenv = new osg::TexEnv;
	  texenv->setMode(osg::TexEnv::DECAL);

	  // Set the texture to the sphere
	  stateset->setTextureAttributeAndModes(0, texture);
	  stateset->setTextureAttribute(0, texenv);

	  return true;
	}
	else
	{
	  std::cerr<< "Sphere::setTextureMap ERROR: File \'" << fname 
	           << "\' could not be loaded." << std::endl;
	  return false;
	}
}

void Sphere::setAutoLOD( bool lod )
{
	if(lod)
	{
	  osg::ShapeDrawable *sd = static_cast<osg::ShapeDrawable*>(_geode->getDrawable(0));
	  osg::Sphere *sphere = static_cast<osg::Sphere*>(sd->getShape());
	  osg::TessellationHints *hints = static_cast<osg::TessellationHints*>(sd->getTessellationHints());

	  if(sd->getCullCallback() == NULL)
	    sd->setCullCallback(new SphereLODCallback(*sphere, *hints));
	}
	else
	{
	  _geode->getDrawable(0)->setCullCallback(NULL);
	}
}

void Sphere::setColor( const osg::Vec4 &color )
{
	ReferenceFrame::setColor(color);
	static_cast<osg::ShapeDrawable*>(_geode->getDrawable(0))->setColor(color);
}

const osg::BoundingSphere& Sphere::getBound() const
{
	// Have bounding sphere encompass sphere and axes/labels, but centered
	// on the sphere (since that is the object of interest)
	ReferenceFrame::getBound();
	osg::BoundingSphere bs = _geode->getBound();
	bs.expandRadiusBy(_bound);
	_bound = bs;

	return _bound;
}

} // !namespace OpenFrames

