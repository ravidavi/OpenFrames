/***********************************
   Copyright 2020 Ravishankar Mathur

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

namespace OpenFrames
{

/** Updates a CustomLineSegments's internal geometry using callbacks. */
class CLSUpdateCallback : public osg::Callback
{
public:
  CLSUpdateCallback()
    : mNumSegments(0)
  {}

  unsigned int mNumSegments;

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    // Get the current time
    osg::NodeVisitor* nv = data->asNodeVisitor();
    double currRefTime = nv->getFrameStamp()->getReferenceTime();
    double currSimTime = nv->getFrameStamp()->getSimulationTime();

    // Get the arrays that hold vertex data
    // Note that all object types are known, since this callback is only added to a Geode object
    osg::Geode *_geode = static_cast<osg::Geode*>(object);
    osg::Geometry *_geom = _geode->getDrawable(0)->asGeometry();
    osg::Vec3Array *_vertices = static_cast<osg::Vec3Array*>(_geom->getVertexArray());
    osg::Vec4Array *_colors = static_cast<osg::Vec4Array*>(_geom->getColorArray());
    osg::DrawArrays *_drawArrays = static_cast<osg::DrawArrays*>(_geom->getPrimitiveSet(0));

    // Resize arrays in preparation for updating data
    unsigned int numVertices = 2 * mNumSegments;
    _vertices->resize(numVertices);
    _colors->resize(numVertices);
    _drawArrays->setCount(numVertices);

    // Update array data
    // TODO: Update this with callback
    for(unsigned int i = 0; i < numVertices; ++i)
    {
      double val = (double)i / (double)numVertices;
      if(i % 2 == 0)
        (*_vertices)[i] = osg::Vec3(cos(val), sin(val), 0.0);
      else
        (*_vertices)[i] = (*_vertices)[i - 1] * 2.0;

      (*_colors)[i] = osg::Vec4(1, 1, 1, 1);
    }

    // Indicate that arrays have been updated
    _vertices->dirty();
    _colors->dirty();
    _drawArrays->dirty();
    _geom->dirtyBound();

    // Continue traversing as needed
    return traverse(object, data);
  }
};

// Implement vertex shader to pass through vertex id
static const char *CLS_VertSource = {
  "#version 120\n"
  "#extension GL_EXT_gpu_shader4 : enable\n"

  "uniform mat4 osg_ModelViewProjectionMatrix;\n"
  "varying float vertexPos;\n"

  "void main(void)\n"
  "{\n"
  // Position and color are just passed through, but vertex position is
  // interpolated between successive pairs of vertices
  "  gl_Position = osg_ModelViewProjectionMatrix*gl_Vertex;\n"
  "  gl_FrontColor = gl_Color;\n"
  "  vertexPos = mod(gl_VertexID, 2);\n"
  "}\n"
};

// Implement frament shader to pass through color (basic shaded line)
static const char *CLS_FragSource = {
  "#version 120\n"

  "varying float vertexPos;\n"

  "void main(void)\n"
  "{\n"
  // Pass through fragment color
  "  gl_FragColor = gl_Color;\n"
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
    osg::Shader *fragShader = new osg::Shader(osg::Shader::FRAGMENT, CLS_FragSource);
    fragShader->setName("CLS Fragment Shader");
    osg::Program *program = new osg::Program;
    program->setName("CLS Shader Program");
    program->addShader(vertShader);
    program->addShader(fragShader);
    ss->setAttribute(program);

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

    // Initialize line properties
    _lineWidth = new osg::LineWidth;
    ss->setAttribute(_lineWidth.get());

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

  void CustomLineSegments::setNumSegments(unsigned int numSegments)
  {
    CLSUpdateCallback *clsCallback = static_cast<CLSUpdateCallback*>(_geode->getUpdateCallback());
    clsCallback->mNumSegments = numSegments;
  }

  unsigned int CustomLineSegments::getNumSegments() const
  {
    CLSUpdateCallback *clsCallback = static_cast<CLSUpdateCallback*>(_geode->getUpdateCallback());
    return clsCallback->mNumSegments;
  }

  void CustomLineSegments::setLineWidth(float width)
  {
    if(width > 0.0) _lineWidth->setWidth(width);
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

