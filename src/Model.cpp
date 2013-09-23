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

#include <OpenFrames/Model>
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

osg::Group* Model::getGroup()
{
	return _group.get();
}

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
	_modelXform->setPivot(center[0], center[1], center[2]);

	repositionAxes(); // Reset the x/y/z axes

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

/** Move the model's x/y/z axes to default positions. */
void Model::repositionAxes()
{
	if(_model) // If model exists, move its axes to the model's scaled bounds
	{
  	  // Get the center point, radius, and scale of the model
	  double px, py, pz;
	  double sx, sy, sz;
	  double radius, scale;
	  _modelXform->getPosition(px, py, pz);
	  _modelXform->getScale(sx, sy, sz);
	  scale = std::max(std::max(sx, sy), sz); // Get the largest scale
	  radius = _model->getBound()._radius;

	  // We want to place the x/y/z axes around the circle that fits the 
	  // scaled model, so we use the largest scale.
	  moveXAxis(osg::Vec3(scale*radius + px, py, pz), 0.5*scale*radius);
	  moveYAxis(osg::Vec3(px, scale*radius + py, pz), 0.5*scale*radius);
	  moveZAxis(osg::Vec3(px, py, scale*radius + pz), 0.5*scale*radius);
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

} // !namespace OpenFrames
