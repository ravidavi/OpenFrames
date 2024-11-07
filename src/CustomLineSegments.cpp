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

/** \file CustomLineSegments.cpp
 * CustomLineSegments-class function definitions.
 */

#include <OpenFrames/CustomLineSegments.hpp>
#include <osg/BlendFunc>
#include <osgDB/FileUtils>

namespace OpenFrames
{

/** Updates a CustomLineSegments's internal geometry using callbacks. */
class CLSUpdateCallback : public osg::Callback
{
public:
  CLSUpdateCallback()
    : mCallback(nullptr)
  {}

  osg::ref_ptr<CustomLineSegments::Callback> mCallback;

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    if(mCallback)
    {
      // Get the arrays that hold vertex data
      // Note that all object types are known, since this callback is only added to a Geode object
      osg::Geode *geode = static_cast<osg::Geode*>(object);
      osg::Geometry *geom = geode->getDrawable(0)->asGeometry();
      osg::Vec3Array *vertices = static_cast<osg::Vec3Array*>(geom->getVertexArray());
      osg::Vec4Array *colors = static_cast<osg::Vec4Array*>(geom->getColorArray());
      osg::DrawArrays *drawArrays = static_cast<osg::DrawArrays*>(geom->getPrimitiveSet(0));

      // Update callback with with latest times
      const osg::FrameStamp* fs = data->asNodeVisitor()->getFrameStamp();
      mCallback->mFrameTime = fs->getReferenceTime();
      mCallback->mSimTime = fs->getSimulationTime();

      // Make sure sensitive callback data is not changed
      mCallback->lockData();

      // Resize arrays in preparation for updating data
      unsigned int numSegments = mCallback->getNumSegments();
      unsigned int numVertices = 2 * numSegments;
      vertices->resize(numVertices);
      colors->resize(numVertices);
      drawArrays->setCount(numVertices);

      // Invoke callback to update array data
      unsigned int iBase, iNext;
      for(unsigned int i = 0; i < numSegments; ++i)
      {
        iBase = 2 * i;
        iNext = iBase + 1;
        osg::Vec3 &posA = (*vertices)[iBase];
        osg::Vec3 &posB = (*vertices)[iNext];
        osg::Vec4 &colorA = (*colors)[iBase];
        osg::Vec4 &colorB = (*colors)[iNext];
        mCallback->getSegmentData(i, posA, colorA, posB, colorB);
      }

      // Allow sensitive callback data to be changed
      mCallback->unlockData();

      // Indicate that arrays have been updated
      vertices->dirty();
      colors->dirty();
      drawArrays->dirty();
      geom->dirtyBound();
    }

    // Continue traversing callbacks
    return traverse(object, data);
  }
};

// Implement vertex shader to pass through vertex id
static const char *CLS_VertSource = {
  "#version 330 core\n"

  "uniform mat4 osg_ModelViewProjectionMatrix;\n"

  // Input attributes
  "in vec3 vertexPosition;\n" // Replacement for gl_Vertex
  "in vec4 vertexColor;\n"    // Replacement for gl_Color

  // Output variables
  "out vec4 fragColor;\n"
  "out float vertexLocation;\n"

  "void main(void)\n"
  "{\n"
  // Compute the position and pass through the color
  "  gl_Position = osg_ModelViewProjectionMatrix * vec4(vertexPosition, 1.0);\n"
  "  fragColor = vertexColor;\n"

  // Use gl_VertexID to compute the vertex location (no extension needed)
  "  vertexLocation = mod(float(gl_VertexID), 2.0);\n"
  "}\n"
};

// Implement frament shader to pass through color (basic shaded line)
static const char *CLS_FragSource = {
  "#version 330 core\n"

  // Input variable from vertex shader
  "in vec4 fragColor;\n"

  // Output color
  "out vec4 outColor;\n"

  "void main(void)\n"
  "{\n"
  // Pass through the fragment color
  "  outColor = fragColor;\n"
  "}\n"
};

  CustomLineSegments::CustomLineSegments(const std::string &name)
    : ReferenceFrame(name)
  {
    _init();
  }

  CustomLineSegments::CustomLineSegments(const std::string &name, const osg::Vec3 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  CustomLineSegments::CustomLineSegments(const std::string &name, const osg::Vec4 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  CustomLineSegments::CustomLineSegments(const std::string &name, float r, float g, float b, float a)
    : ReferenceFrame(name, r, g, b, a)
  {
    _init();
  }

  CustomLineSegments::~CustomLineSegments() { }

  void CustomLineSegments::_init()
  {
    // Create new StateSet to specify star-specific OpenGL properties
    osg::StateSet *ss = new osg::StateSet();

    // Create shaders
    osg::Shader *vertShader = new osg::Shader(osg::Shader::VERTEX, CLS_VertSource);
    vertShader->setName("CLS Vertex Shader");
    _fragShader = new osg::Shader(osg::Shader::FRAGMENT, CLS_FragSource);
    _fragShader->setName("CLS Fragment Shader");
    osg::Program *program = new osg::Program;
    program->setName("CLS Shader Program");
    program->addShader(vertShader);
    program->addShader(_fragShader);
    ss->setAttribute(program);

    // Line width
    _lineWidth = new osg::LineWidth;
    ss->setAttribute(_lineWidth.get());

    // Set up alpha blending
    osg::BlendFunc *fn = new osg::BlendFunc();
    fn->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
    ss->setAttributeAndModes(fn);

    // Create the node that will contain the line segments
    _geode = new osg::Geode;
    _geode->setName(_name);
    _xform->addChild(_geode);

    // Create the geometry and arrays for line segment data
    _segmentGeom = new osg::Geometry;
    _segmentGeom->setName("CLS Geometry");
    _segmentGeom->setStateSet(ss);
    _segmentGeom->setUseDisplayList(false);
    _segmentGeom->setUseVertexBufferObjects(true);
    _segmentGeom->getOrCreateVertexBufferObject()->setUsage(GL_DYNAMIC_DRAW);
    _segmentGeom->setVertexArray(new osg::Vec3Array());
    osg::Vec4Array *colors = new osg::Vec4Array;
    colors->setBinding(osg::Array::BIND_PER_VERTEX);
    _segmentGeom->setColorArray(colors);
    _segmentGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, 0));
    _geode->addDrawable(_segmentGeom);

    _geode->setUpdateCallback(new CLSUpdateCallback());
  }

  void CustomLineSegments::showContents(bool showContents)
  {
    if (showContents) _geode->setNodeMask(0xffffffff);
    else _geode->setNodeMask(0x0);
  }

  bool CustomLineSegments::getContentsShown() const
  {
    return (_geode->getNodeMask() != 0x0);
  }

  void CustomLineSegments::setLineWidth(float width)
  {
    if(width > 0.0) _lineWidth->setWidth(width);
  }

  bool CustomLineSegments::setLineShader(const std::string &fname)
  {
    // Reset shader
    if(fname.length() == 0)
    {
      _fragShader->setShaderSource(CLS_FragSource);
    }
    else
    {
      // Load shader source from file
      std::string fullFile = osgDB::findDataFile(fname);
      bool success = _fragShader->loadShaderSourceFromFile(fullFile);
      if(!success)
      {
        OSG_WARN << "OpenFrames::CustomLineSegments ERROR: Shader file \'" << fname << "\' not properly loaded!" << std::endl;
        return false;
      }
    }

    return true;
  }

  void CustomLineSegments::setLineSegmentCallback(Callback *cb)
  {
    CLSUpdateCallback *clsCallback = static_cast<CLSUpdateCallback*>(_geode->getUpdateCallback());
    clsCallback->mCallback = cb;
  }

  const osg::BoundingSphere& CustomLineSegments::getBound() const
  {
    osg::BoundingSphere bs = _geode->getBound();
   
    // Keep bound center but expand to include axes/labels
    ReferenceFrame::getBound();
    bs.expandRadiusBy(_bound);
    _bound = bs;

    return _bound;
  }

} // !namespace OpenFrames

