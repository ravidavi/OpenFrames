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
#include <osgUtil/CullVisitor>
#include <sstream>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames
{

/** A callback that sets the shader uniforms to allow rendering
    relative to the eye using GPU. This method eliminates jittering
    associated with very large OpenGL vertex positions.
    Ref: Cozzi & Ring, "3D Engine Design for Virtual Globes" */
class UniformCallback : public osg::NodeCallback
{
  public:
	UniformCallback(osg::Uniform &mvmat, osg::Uniform &eyeHigh, osg::Uniform &eyeLow) 
        : _mvmat(mvmat), _eyeHigh(eyeHigh), _eyeLow(eyeLow)
	{}

        virtual void operator()(osg::Node *node, osg::NodeVisitor *nv)
	{
	  osgUtil::CullVisitor *cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	  if(cv)
	  {
            // Get ModelView matrix
            osg::Matrixd mvmat = *(cv->getModelViewMatrix());

            // Invert to get eye point
            // Note that we can't use cv->getEyeLocal since Vec3=Vec3f
            osg::Matrixd mvmatinv;
            mvmatinv.invert(mvmat);
            osg::Vec3d eye = mvmatinv.getTrans();

            // Convert eye point to 2 single-precision values
            osg::Vec3f eyeHigh = eye;
            osg::Vec3f eyeLow = eye - osg::Vec3d(eyeHigh);

            // Zero out translation since it will be directly applied
            // in the vertex shader
            mvmat.setTrans(0.0, 0.0, 0.0);

            // Apply new ModelView matrix and eye point shader Uniforms
            _mvmat.set(mvmat);
            _eyeHigh.set(eyeHigh);
            _eyeLow.set(eyeLow);
          }

          traverse(node,nv);
        }

  private:
        osg::Uniform &_mvmat, &_eyeHigh, &_eyeLow;
};

// Implement the shader portion of Rendering Relative to Eye using GPU
static const char *VertSource = {
  "#version 120\n"
  "uniform mat4 osg_ProjectionMatrix;\n"
  "uniform mat4 of_RTEModelViewMatrix;\n"
  "uniform vec3 of_ModelViewEyeHigh;\n"
  "uniform vec3 of_ModelViewEyeLow;\n"
  "attribute vec4 of_VertexLow;\n"
  "void main(void)\n"
  "{\n"
  "  vec3 t1 = of_VertexLow.xyz - of_ModelViewEyeLow;\n"
  "  vec3 e = t1 - of_VertexLow.xyz;\n"
  "  vec3 t2 = ((-of_ModelViewEyeLow - e) + (of_VertexLow.xyz - (t1 - e))) + gl_Vertex.xyz - of_ModelViewEyeHigh;\n"
  "  vec3 diffHigh = t1 + t2;\n"
  "  vec3 diffLow = t2 - (diffHigh - t1);\n"
  "  gl_Position = osg_ProjectionMatrix*of_RTEModelViewMatrix*vec4(diffHigh+diffLow, 1.0);\n"
  "  gl_FrontColor = gl_Color;\n"
  "}\n"
};

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
        // Geode will contain all trajectory artists
	_geode = new osg::Geode;
	_geode->setName(_name);
	osg::StateSet *stateset = _geode->getOrCreateStateSet();
	stateset->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

        // Set shader properties for GPU rendering relative to eye
        osg::Uniform *mvmat = new osg::Uniform(osg::Uniform::FLOAT_MAT4, "of_RTEModelViewMatrix");
        osg::Uniform *eyeHigh = new osg::Uniform(osg::Uniform::FLOAT_VEC3, "of_ModelViewEyeHigh");
        osg::Uniform *eyeLow = new osg::Uniform(osg::Uniform::FLOAT_VEC3, "of_ModelViewEyeLow");
        mvmat->setDataVariance(osg::Object::DYNAMIC);
        eyeHigh->setDataVariance(osg::Object::DYNAMIC);
        eyeLow->setDataVariance(osg::Object::DYNAMIC);
        stateset->addUniform(mvmat);
        stateset->addUniform(eyeHigh);
        stateset->addUniform(eyeLow);
        _geode->setCullCallback(new UniformCallback(*mvmat, *eyeHigh, *eyeLow));
        osg::Program *program = new osg::Program;
        program->setName("OFDrawableTrajectory_Shader");
        program->addShader(new osg::Shader(osg::Shader::VERTEX, VertSource));
        program->addBindAttribLocation("of_VertexLow", 1);
        stateset->setAttributeAndModes(program, osg::StateAttribute::ON);

        // Add contained artists to this frame's transform
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
