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

#ifndef _OF_SPHERE_
#define _OF_SPHERE_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geode>
#include <osg/Material>
#include <string>

namespace OpenFrames
{
  
  /*******************************************************************
   * Ravi Mathur
   * OpenFrames API, class Sphere
   * A Sphere is a ReferenceFrame with a 3D model of a sphere at its origin.
   * The radius of the sphere is adjustable, and a texture can be mapped
   * onto the sphere if desired.
   ******************************************************************/
  class OF_EXPORT Sphere : public ReferenceFrame
  {
  public:
    Sphere( const std::string &name );
    Sphere( const std::string &name, const osg::Vec3 &color );
    Sphere( const std::string &name, const osg::Vec4 &color );
    Sphere( const std::string &name , float r, float g, float b, float a = 1.0 );
    
    /** Needed because osg::ShapeDrawable only sets texture coordinates for unit0.
        This can be removed once the Sphere creates its own geometry.
        Not for use by end user. */
    void restoreTexCoords();
    
    /** Set the radius of the sphere, given wrt the origin of
     the sphere's reference frame */
    void setRadius( const double &radius );
    double getRadius() const;
    
    /** Set the filename of the texture to map onto the sphere. Consult the
     osg documentation to see which image filetypes are supported. */
    bool setTextureMap( const std::string &fname, unsigned int unit = 0, bool force_reload = false );
    
    /** Set the texture environment for the specified texture unit. */
    bool setTexEnv(osg::StateAttribute* texenv, unsigned int unit);
    
    /** Convenience function to specify a night texture map. This is combined with the day
     texture map (assumed to be on previous texture unit) and shows on the dark side of the sphere. */
    bool setNightTextureMap(const std::string &fname, unsigned int unit = 1, bool force_reload = false);
    
    /** Have the sphere automatically adjust its detail level depending on
     how far it is from the current eye point. */
    void setAutoLOD( bool lod );
    
    /** Inherited from ReferenceFrame
     Set the color of the sphere.
     Has no effect if a texture or material are applied to the sphere. */
    virtual void setColor( const osg::Vec4 &color );
    using ReferenceFrame::setColor; // Unhide other setColor() functions
    
    /** Set the material used for sphere lighting.
     See osg::Material and OpenGL documentation for material parameters.
     This overrides color set by setColor(), and affects texture color. */
    void setMaterial( osg::Material *mat );
    osg::Material* getMaterial() const
    { return dynamic_cast<osg::Material*>(_sphereSD->getStateSet()->getAttribute(osg::StateAttribute::MATERIAL)); }
     
    /** Inherited from ReferenceFrame. */
    virtual const osg::BoundingSphere& getBound() const;
    
  protected:
    virtual ~Sphere();
    
    osg::ref_ptr<osg::Geode> _geode; // Node containing the sphere
    osg::ref_ptr<osg::ShapeDrawable> _sphereSD; // The actual sphere
    
  private:
    void _init();
  };
  
} // !namespace OpenFrames

#endif
