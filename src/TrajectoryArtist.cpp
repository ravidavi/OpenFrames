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

#include <OpenFrames/TrajectoryArtist.hpp>
#include <OpenFrames/DoubleSingleUtils.hpp>

namespace OpenFrames
{

// Vertex shader object used by all Trajectory Artists
static osg::ref_ptr<osg::Shader> OFTA_VertShader;

// Implement vertex shader for Rendering Relative to Eye using GPU
static const char *OFTA_VertSource = {
  "#version 120\n"
  "uniform mat4 osg_ProjectionMatrix;\n"

  // ModelView matrix with zero translation component
  "uniform mat4 of_RTEModelViewMatrix;\n"

  // High/low parts of modelview matrix translation
  "uniform vec3 of_ModelViewEyeHigh;\n"
  "uniform vec3 of_ModelViewEyeLow;\n"

  // Low part of current vertex position
  // High part comes in through gl_Vertex
  "attribute vec4 of_VertexLow;\n"

  "void main(void)\n"
  "{\n"
     // Low part of vertex - eye and associated numerical error
  "  vec3 t1 = of_VertexLow.xyz - of_ModelViewEyeLow;\n"
  "  vec3 e = t1 - of_VertexLow.xyz;\n"

     // High part of vertex - eye including numerical error
  "  vec3 t2 = ((-of_ModelViewEyeLow - e) + (of_VertexLow.xyz - (t1 - e))) + gl_Vertex.xyz - of_ModelViewEyeHigh;\n"

     // Sum of low + high parts and associated numerical error
  "  vec3 diffHigh = t1 + t2;\n"
  "  vec3 diffLow = t2 - (diffHigh - t1);\n"

     // Vertex position with low and high parts
  "  gl_Position = osg_ProjectionMatrix*of_RTEModelViewMatrix*vec4(diffHigh+diffLow, 1.0);\n"
  "  gl_FrontColor = gl_Color;\n"
  "  gl_TexCoord[0] = gl_MultiTexCoord0;\n"
  "}\n"
};

TrajectoryArtist::TrajectoryArtist() 
{
        // Create vertex shader if it doesn't already exist
        if(OFTA_VertShader == NULL)
        {
          OFTA_VertShader = new osg::Shader(osg::Shader::VERTEX, OFTA_VertSource);
        }

        // Create vertex program
        _program = new osg::Program;
        _program->setName("OFTrajectoryArtist_ShaderProgram");

        // Add the vertex shader
        _program->addShader(OFTA_VertShader);

        // Create vertex attribute that stores low part of vertex
        // Used by Artists to implement Rendering RTE in GPU
        _program->addBindAttribLocation("of_VertexLow", 1);

        // Set the shader program for this Artist
        getOrCreateStateSet()->setAttribute(_program);
}

// Not using the copy constructor
TrajectoryArtist::TrajectoryArtist( const TrajectoryArtist &ta, const osg::CopyOp& copyop )
{}

TrajectoryArtist::~TrajectoryArtist()
{
	if(_traj.valid()) _traj->removeArtist(this); 
}

void TrajectoryArtist::setTrajectory(const Trajectory *traj)
{
	if(_traj == traj) return;

	// Unregister from the old trajectory
	if(_traj.valid()) _traj->removeArtist(this);

	// Register with the new trajectory
	_traj = traj;
	if(_traj.valid()) _traj->addArtist(this);
}

osg::BoundingBox TrajectoryArtist::computeBoundingBox() const
{
	_boundingBox.init();
	return _boundingBox;
}

void TrajectoryArtist::RTE_glVertex(osg::Vec3d &point, osg::GLExtensions &glext) const
{
        osg::Vec3f high, low;

        // Split input point into high and low portions
        OpenFrames::DS_Split(point, high, low);

        // Submit to OpenGL
        // Note that vertex attribute must be specified BEFORE glVertex
        glext.glVertexAttrib3fv(1, low._v);
        glVertex3fv(high._v);
}

} //!namespace OpenFrames
