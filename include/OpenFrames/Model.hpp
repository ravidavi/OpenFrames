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

#ifndef _OF_MODEL_
#define _OF_MODEL_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Node>
#include <osg/ref_ptr>
#include <string>

namespace OpenFrames
{
  
  /******************************************
   * Ravi Mathur
   * OpenFrames API, class Model
   * A Model is a Reference Frame that also contains a 3D model at the origin.
   * Any model can be loaded as long as the model format is supported by the OSG.
   *****************************************/
  class OF_EXPORT Model : public ReferenceFrame
  {
  public:
    Model( const std::string &name );
    Model( const std::string &name, const osg::Vec3 &color );
    Model( const std::string &name, const osg::Vec4 &color );
    Model( const std::string &name , float r, float g, float b, float a = 1.0 );
    
    /** We will maintain our own osg::Group, because loaded models might have
     ParticleSystems or other things which need to reside in world space.
	    Therefore, our group will contain the ReferenceFrame's transform as
	    well as any ParticleSystems and other such things. */
    virtual osg::Group* getGroup();
    
    /** Set the model that should be displayed */
    bool setModel( const std::string& filename, bool force_reload = false );
    osg::Node* getModel() const { return _model.get(); }
    osg::Geode* getExtras() const { return _extras.get(); }
    
    /** Use a model that has already been loaded by another Model object */
    bool shareModel( const Model* otherModel );
    
    /** Set/get the position of the model wrt the local frame.
	    This is useful e.g. if the model's defined origin is not
	    where you want it to be. */
    void setModelPosition( const double &x, const double &y, const double &z );
    
    void getModelPosition( double &x, double &y, double &z ) const
    {
      _modelXform->getPosition(x, y, z);
    }
    
    /** Set/get the scale of the model wrt the pivot point. */
    void setModelScale( const double &sx, const double &sy, const double &sz);
    
    void getModelScale( double &sx, double &sy, double &sz ) const
    {
      _modelXform->getScale(sx, sy, sz);
    }
    
    /** Set/get the pivot point of the model. This is the point about
	    which all rotations happen, and wrt which all points are moved
	    during a scale. */
    void setModelPivot( const double &px, const double &py, const double &pz)
    {
      _modelXform->setPivot(px, py, pz);
    }
    
    void getModelPivot( double &px, double &py, double &pz ) const
    {
      _modelXform->getPivot(px, py, pz);
    }
    
    /** Inherited function to compute the bounds of the model */
    virtual const osg::BoundingSphere& getBound() const;
    
  protected:
    virtual ~Model();
    
    void init();
    void repositionAxes();
    
    /** The group will hold this frame's transform, as explained above */
    osg::ref_ptr<osg::Group> _group;
    
    /** 3D model, excluding ParticleSystems */
    osg::ref_ptr<osg::Node> _model;
    
    /** Transform that applies only to the model itself */
    osg::ref_ptr<FrameTransform> _modelXform;
    
    /** Any extra drawables which are not contained in the transform,
	    but are still part of this frame. eg. ParticleSystems, etc... */
    osg::ref_ptr<osg::Geode> _extras;
  };
  
} // !namespace OpenFrames

#endif // !define _OF_MODEL_
