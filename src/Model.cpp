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

/** \file Model.cpp
 * Definitions for the Model class.
 */

#include <OpenFrames/Model.hpp>
#include <osg/Geode>
#include <osg/Shape>
#include <osg/ShapeDrawable>
#include <osgParticle/ParticleSystem>
#include <osgDB/ReadFile>
#include <iostream>

namespace OpenFrames
{
  
  Model::Model( const std::string &name)
  : ReferenceFrame(name)
  {
    init();
  }
  
  Model::Model( const std::string &name, const osg::Vec3 &color)
  : ReferenceFrame(name, color)
  {
    init();
  }
  
  Model::Model( const std::string &name, const osg::Vec4 &color)
  : ReferenceFrame(name, color)
  {
    init();
  }
  
  Model::Model( const std::string &name, float r, float g, float b, float a)
  : ReferenceFrame(name, r, g, b, a)
  {
    init();
  }
  
  Model::~Model() { }
  
  void Model::init()
  {
    _group = new osg::Group;
    _extras = new osg::Geode;
    _extras->setName(_name + " extras");
    _group->addChild(_xform.get());
    _modelXform = new FrameTransform;
  }
  
  osg::Group* Model::getGroup() const
  {
    return _group.get();
  }
  
  /********************************************************/
  bool Model::setModel( const std::string& filename, bool force_reload )
  {
    // First check if we are reloading the same model file (assuming it has the same name)
    if(_model.valid() && (_model->getName() == filename) && !force_reload) return true;
    
    // Read the new model file (if a filename is given)
    osg::Node *newModel = NULL;
    if(filename.length() > 0)
    {
      newModel = osgDB::readNodeFile(filename);
    }
    
    // Next, remove existing data if needed
    if((filename.length() == 0) || (newModel != NULL))
    {
      // Remove current model
      _xform->removeChild(_modelXform.get());
      _modelXform->removeChild(_model.get());
      _model = NULL;
      
      // Remove existing ParticleSystems
      _group->removeChild(_extras.get());
      _extras->removeDrawables(0, _extras->getNumDrawables());
      
      // No filename given, so just reset axes and quit
      if(filename.length() == 0)
      {
        moveXAxis(osg::Vec3d(), 1);
        moveYAxis(osg::Vec3d(), 1);
        moveZAxis(osg::Vec3d(), 1);
        return true;
      }
    }
    else
    {
      std::cerr<< "Model ERROR: Model file \'" << filename << "\' could not be loaded!" << std::endl;
      return false; // Model could not be loaded
    }
    
    _model = newModel; // Set new model
    _model->setName(filename);
    
    /* If the model's root node is a group, iterate through its children
     to find any osg::ParticleSystem children.  Then move those children
     to our group so they are not affected by this frame's transform.
     This is done because ParticleSystems need to be in world space. */
    osg::Group *modelGroup = dynamic_cast<osg::Group*>(_model.get());
    if(modelGroup)
    {
      osg::Geode *modelGeode;
      osgParticle::ParticleSystem *ps;
      
      // Find each Geode, since that could contain a ParticleSystem
      for(unsigned int i = 0; i < modelGroup->getNumChildren(); ++i)
      {
        modelGeode = dynamic_cast<osg::Geode*>(modelGroup->getChild(i));
        if(modelGeode)
        {
          // Search through the Geode for a ParticleSystem
          for(unsigned int j = 0; j < modelGeode->getNumDrawables(); ++j)
          {
            ps = dynamic_cast<osgParticle::ParticleSystem*>(modelGeode->getDrawable(j));
            if(ps) // ParticleSystem found
            {
              _extras->addDrawable(ps);
              modelGeode->removeDrawable(ps);
              --j;
            }
          }
          
          // Delete the Geode if it's empty
          if(modelGeode->getNumDrawables() == 0)
          {
            modelGroup->removeChild(modelGeode);
            --i;
          }
        }
      }
      
      // If ParticleSystems exist, add them to the group
      if(_extras->getNumDrawables() > 0)
        _group->addChild(_extras.get());
    }
    
    // Add the new model to this frame
    _modelXform->addChild(_model.get());
    _xform->addChild(_modelXform.get());
    
    // Rescale normals in case we want to scale the model
    _model->getOrCreateStateSet()->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::ON );
    
    // Set the model pivot at its geometric center, so that scales/rotations will make sense.
    osg::Vec3d center = _model->getBound()._center;
    setModelPivot(center[0], center[1], center[2]);
    
    return true;
  }
  
  /********************************************************/
  bool Model::shareModel( const Model* otherModel )
  {
    if(!setModel("")) return false; // Clear any existing data
    
    _model = otherModel->getModel(); // Get model data
    if(!_model.valid()) return true; // No model to share
    
    // Add the new model to this frame
    _modelXform->addChild(_model.get());
    _xform->addChild(_modelXform.get());
    
    // Set the model pivot at its geometric center, so that scales/rotations will make sense.
    osg::Vec3d center = _model->getBound()._center;
    setModelPivot(center[0], center[1], center[2]);

    // Get any extras such as ParticleSystems
    osg::Geode *otherExtras = otherModel->getExtras();
    if(otherExtras->getNumDrawables() > 0)
    {
      // Get all drawables
      for(unsigned int i = 0; i < otherExtras->getNumDrawables(); ++i)
      {
        _extras->addDrawable(otherExtras->getDrawable(i));
      }
      
      // If extras exist, add them to the group
      _group->addChild(_extras.get());
    }
    
    return true;
  }
  
  /** Set the position of the model wrt its own ReferenceFrame. */
  void Model::setModelPosition( const double &x, const double &y, const double &z)
  {
    _modelXform->setPosition(x, y, z);
    
    repositionAxes(); // Reset the x/y/z axes
  }
  
  /** Set the scale of the model wrt the pivot point. */
  void Model::setModelScale( const double &sx, const double &sy, const double &sz)
  {
    if((sx == 0.0) || (sy == 0.0) || (sz == 0.0)) return;
    
    _modelXform->setScale(sx, sy, sz);
    
    repositionAxes(); // Reset the x/y/z axes
  }

  /** Set/get the pivot point of the model. */
  void Model::setModelPivot(const double &px, const double &py, const double &pz)
  {
    _modelXform->setPivot(px, py, pz);

    repositionAxes(); // Reset the x/y/z axes
  }
  
  /** Enable model dragging */
  void Model::addDraggerCallback(osgManipulator::DraggerCallback* callback)
  {
    // Create dragger if needed
    if(!_dragger.valid())
    {
      _draggerXform = new osg::MatrixTransform;
      _xform->addChild(_draggerXform);
      
      _dragger = new osgManipulator::TrackballDragger();
      _dragger->setupDefaultGeometry();
      _dragger->setAxisLineWidth(5.0);
      _xform->addChild(_dragger);
      const osg::BoundingSphere& bound = getBound();
      float scale = bound.radius();
      _dragger->setMatrix(osg::Matrix::scale(scale, scale, scale) *
                          osg::Matrix::translate(-(bound.center())));
      _dragger->setHandleEvents(true);
      _dragger->setActivationModKeyMask(osgGA::GUIEventAdapter::MODKEY_CTRL);
      
      ModelDraggerTransformCallback* draggerCallback = new ModelDraggerTransformCallback(_modelXform.get(), _draggerXform.get());
      _dragger->addDraggerCallback(draggerCallback);
    }
    
    // Add given callback to dragger
    if(callback != nullptr) _dragger->addDraggerCallback(callback);
  }
  
  /** Move the model's x/y/z axes to default positions. */
  void Model::repositionAxes()
  {
    if(_model) // If model exists, move the axes to the its bounds
    {
      // Get the model's transformed bounding sphere
      osg::BoundingSphere bs = _modelXform->getBound();
      
      // Place the axes at the edge of the bounding sphere
      moveXAxis(bs._center + osg::Vec3(bs._radius, 0, 0), 0.5*bs._radius);
      moveYAxis(bs._center + osg::Vec3(0, bs._radius, 0), 0.5*bs._radius);
      moveZAxis(bs._center + osg::Vec3(0, 0, bs._radius), 0.5*bs._radius);
    }
    else // Otherwise move its axes to the origin
    {
      moveXAxis(osg::Vec3(0, 0, 0), 1.0);
      moveYAxis(osg::Vec3(0, 0, 0), 1.0);
      moveZAxis(osg::Vec3(0, 0, 0), 1.0);
    }
  }
  
  const osg::BoundingSphere& Model::getBound() const
  {
    // If model exists, have bounding sphere encompass it and the
    // axes/labels, but keep it centered on the model since that is
    // the object of interest.
    ReferenceFrame::getBound();
    if(_model.valid()) 
    {
      osg::BoundingSphere bs = _modelXform->getBound();
      bs.expandRadiusBy(_bound);
      _bound = bs;
    }
    
    return _bound;
  }
  
  /*****************************************************************/
  ModelDraggerTransformCallback::ModelDraggerTransformCallback(FrameTransform* modelxform, osg::MatrixTransform* draggerxform)
  : osgManipulator::DraggerTransformCallback(draggerxform), _modelXform(modelxform)
  { }
  
  bool ModelDraggerTransformCallback::receive(const osgManipulator::MotionCommand& command)
  {
    if(!osgManipulator::DraggerTransformCallback::receive(command)) return false;
    
    // For a MOVE command, decompose the transformation matrix
    // TODO: Directly compute transformation components without using intermediate MatrixTransform
    if((command.getStage() == osgManipulator::MotionCommand::MOVE) && _modelXform.valid())
    {
      osg::Vec3d trans, scale;
      osg::Quat rot, rotSO;
      _transform->getMatrix().decompose(trans, rot, scale, rotSO);
      _modelXform->setPosition(trans);
      _modelXform->setAttitude(rot);
      _modelXform->setScale(scale[0], scale[1], scale[2]);
    }
    
    return true;
  }
  
} // !namespace OpenFrames
