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

#include <OpenFrames/Sphere.hpp>
#include <osg/Geode>
#include <osg/PolygonOffset>
#include <osg/Shape>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgUtil/CullVisitor>

namespace OpenFrames
{

  /** A cull callback that sets the detail level of a Sphere based on how far
    away it is from the eye point. */
  struct SphereLODCallback : public osg::DrawableCullCallback
  {
    SphereLODCallback()
      : _currentLOD(0) {}

    virtual bool cull(osg::NodeVisitor* nv, osg::Drawable* drawable, osg::RenderInfo* renderInfo) const 
    {
      osgUtil::CullVisitor *cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
      osg::ShapeDrawable *sphereSD = dynamic_cast<osg::ShapeDrawable*>(drawable);
      if(cv && sphereSD)
      {
        osg::Sphere *sphere = static_cast<osg::Sphere*>(sphereSD->getShape());
        osg::TessellationHints *hints = static_cast<osg::TessellationHints*>(sphereSD->getTessellationHints());
          
        // Get the eye point in the Sphere's local coordinates
        // Can't use cv->getEyeLocal() since Vec3=Vec3f
        osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();

        // Compute the ratio of distance to sphere radius
        double ratio = (eye - sphere->getCenter()).length() / sphere->getRadius();
        if(ratio < 1.0) ratio = 1.0;
        const double maxLOD = 50, minLOD = 5;
        double lod = maxLOD / ratio;
        if(lod < minLOD) lod = minLOD;

        if(_currentLOD != (int)lod)
        {
          hints->setDetailRatio(0.1*lod);
          _currentLOD = (int)lod;
          sphereSD->dirtyBound();
          sphereSD->build();
        }
      }

      return false; 
    }

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
    _sphereSD = new osg::ShapeDrawable;
    _sphereSD->setName("SphereDrawable");
    _sphereSD->setUseDisplayList(false);
    _sphereSD->setUseVertexBufferObjects(true);
    osg::Sphere* sphere = new osg::Sphere;
    osg::TessellationHints* hints = new osg::TessellationHints;

    // Set the shape to be drawn
    _sphereSD->setShape(sphere);
    _sphereSD->setTessellationHints(hints);

    // Don't need very high detail on a simple sphere
    hints->setDetailRatio(2.0);

    // Offset the sphere's polygons backwards so that a decal on the sphere's
    // surface will always show up on top of the sphere.
    osg::PolygonOffset *offset = new osg::PolygonOffset(1, 1);
    _sphereSD->getOrCreateStateSet()->setAttributeAndModes(offset);

    // Create the node that contains the Sphere
    _geode = new osg::Geode;
    _geode->setName(_name);
    _geode->addDrawable(_sphereSD);

    // Add the sphere to the ReferenceFrame
    _xform->addChild(_geode.get());

    // Resize the axes vectors and set the appropriate color
    setRadius(1.0);
    setColor(getColor());
  }

  void Sphere::setRadius(const double &radius)
  {
    osg::Sphere *sphere = static_cast<osg::Sphere*>(_sphereSD->getShape());
    sphere->setRadius(radius);
    _sphereSD->dirtyBound();
    _sphereSD->build();

    moveXAxis(osg::Vec3(radius, 0, 0), 0.5*radius);
    moveYAxis(osg::Vec3(0, radius, 0), 0.5*radius);
    moveZAxis(osg::Vec3(0, 0, radius), 0.5*radius);
  }

  double Sphere::getRadius() const
  {
    osg::Sphere *sphere = static_cast<osg::Sphere*>(_sphereSD->getShape());
    return sphere->getRadius();
  }

  bool Sphere::setTextureMap(const std::string &fname, bool force_reload)
  {
    osg::StateSet* stateset = _sphereSD->getStateSet();

    if(fname.length() == 0) // Remove existing texture
    {
      stateset->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
      setColor(getColor()); // Restore sphere color
      return false;
    }

    // Check if there is already a texture being used.
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

      // Set the texture to the sphere
      stateset->setTextureAttributeAndModes(0, texture);
      
      // Set the sphere's color to white to avoid artificially coloring the texture
      // Don't use Sphere::setColor since that will affect the ReferenceFrame color
      _sphereSD->setColor(osg::Vec4(1.0, 1.0, 1.0, 1.0));

      return true;
    }
    else
    {
      OSG_WARN<< "Sphere::setTextureMap ERROR: File \'" << fname
        << "\' could not be loaded." << std::endl;
      return false;
    }
  }

  void Sphere::setAutoLOD( bool lod )
  {
    if(lod)
    {
      if(_sphereSD->getCullCallback() == NULL)
        _sphereSD->setCullCallback(new SphereLODCallback());
    }
    else
    {
      _sphereSD->setCullCallback(NULL);
    }
  }

  void Sphere::setColor( const osg::Vec4 &color )
  {
    ReferenceFrame::setColor(color);
    
    // Only set sphere color if there is no texture, so that an exiting texture
    // doesn't have its color altered by the sphere's color
    osg::StateSet* ss = _sphereSD->getStateSet();
    osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(ss->getTextureAttribute(0, osg::StateAttribute::TEXTURE));
    if(!texture) _sphereSD->setColor(color);
  }
  
  void Sphere::setMaterial( osg::Material *mat )
  {
    if(mat)
      _sphereSD->getStateSet()->setAttributeAndModes(mat);
    else
      _sphereSD->getStateSet()->removeAttribute(osg::StateAttribute::MATERIAL);
  }

  const osg::BoundingSphere& Sphere::getBound() const
  {
    // Normally we would just get the Sphere's bound by computing its
    // Geode's bound. However, Geode's bound is computed by fitting
    // a bounding sphere to its bounding box, which makes the bound
    // bigger than the specified Sphere radius. So we will just use
    // the center from the Geode's bound, and set the radius ourselves.
    osg::BoundingSphere bs = _geode->getBound();
    bs._radius = getRadius();

    // Keep bound center but expand to include axes/labels
    ReferenceFrame::getBound();
    bs.expandRadiusBy(_bound);
    _bound = bs;

    return _bound;
  }

} // !namespace OpenFrames

