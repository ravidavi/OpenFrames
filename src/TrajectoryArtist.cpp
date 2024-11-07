/***********************************
   Copyright 2023 Ravishankar Mathur

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

/** \file TrajectoryArtist.cpp
 * TrajectoryArtist-class function definitions.
 */

#include <OpenFrames/TrajectoryArtist.hpp>
#include <OpenFrames/DoubleSingleUtils.hpp>

namespace OpenFrames
{

// Implement vertex shader for Rendering Relative to Eye using GPU
static const char *OFTA_VertSource = {
  "#version 330 core\n"
  "uniform mat4 osg_ProjectionMatrix;\n"

  // ModelView matrix with zero translation component
  "uniform mat4 of_RTEModelViewMatrix;\n"

  // High/low parts of modelview matrix translation
  "uniform vec3 of_ModelViewEyeHigh;\n"
  "uniform vec3 of_ModelViewEyeLow;\n"

  // Low part of current vertex position
  // High part comes in through vertex position input
  "in vec4 of_VertexLow;\n"
  "in vec3 vertexPosition;\n" // Custom vertex position attribute
  "in vec4 vertexColor;\n"    // Custom vertex color attribute
  "in vec2 texCoord0;\n"      // Custom texture coordinate attribute

  // Output variables for the fragment shader
  "out vec4 fragColor;\n"
  "out vec2 fragTexCoord;\n"

  "void main(void)\n"
  "{\n"
     // Low part of vertex - eye and associated numerical error
  "  vec3 t1 = of_VertexLow.xyz - of_ModelViewEyeLow;\n"
  "  vec3 e = t1 - of_VertexLow.xyz;\n"

     // High part of vertex - eye including numerical error
  "  vec3 t2 = ((-of_ModelViewEyeLow - e) + (of_VertexLow.xyz - (t1 - e))) + vertexPosition - of_ModelViewEyeHigh;\n"

     // Sum of low + high parts and associated numerical error
  "  vec3 diffHigh = t1 + t2;\n"
  "  vec3 diffLow = t2 - (diffHigh - t1);\n"

     // Vertex position with low and high parts
  "  gl_Position = osg_ProjectionMatrix * of_RTEModelViewMatrix * vec4(diffHigh + diffLow, 1.0);\n"

     // Pass the color and texture coordinates to the fragment shader
  "  fragColor = vertexColor;\n"
  "  fragTexCoord = texCoord0;\n"
  "}\n"
};

// Implement vertex shader for Rendering Relative to Eye using GPU
static const char *OFTA_FragSource = {
	"#version 330 core\n"

	// Input variables from the vertex shader
	"in vec4 fragColor;\n"
	"in vec2 fragTexCoord;\n" // Replace usage of gl_PointCoord if necessary

	// Output color
	"out vec4 outColor;\n"

	"void main(void)\n"
	"{\n"
	  // Use fragTexCoord or another mechanism for texture mapping
	  // Color the fragment with the passed-in fragColor
	"  outColor = fragColor;\n"
	"}\n"
};

TrajectoryArtist::TrajectoryArtist() 
{
  // Create vertex shader
  osg::Shader *vertShader = new osg::Shader(osg::Shader::VERTEX, OFTA_VertSource);
  osg::Shader *fragShader = new osg::Shader(osg::Shader::FRAGMENT, OFTA_FragSource);

  // Create vertex program
  _program = new osg::Program;
  _program->setName("OFTrajectoryArtist_ShaderProgram");
  _program->addShader(vertShader);
  _program->addShader(fragShader);

  // Create vertex attribute that stores low part of vertex
  // Used by Artists to implement Rendering RTE in GPU
  _program->addBindAttribLocation("of_VertexLow", OF_VERTEXLOW);

  // Set the shader program for this Artist
  getOrCreateStateSet()->setAttribute(_program);
}

// Not using the copy constructor
TrajectoryArtist::TrajectoryArtist( const TrajectoryArtist &ta, const osg::CopyOp& copyop )
{}

TrajectoryArtist::~TrajectoryArtist()
{
	if(_traj.valid()) _traj->removeSubscriber(this);
}

void TrajectoryArtist::setTrajectory(const Trajectory *traj)
{
	if(_traj == traj) return;

	// Unregister from the old trajectory
	if(_traj.valid()) _traj->removeSubscriber(this);

	// Register with the new trajectory
	_traj = traj;
	if(_traj.valid()) _traj->addSubscriber(this);
}

} //!namespace OpenFrames
