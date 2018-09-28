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

/** \file CoordinateAxes.cpp
 * Implementation of CoordinateAxes' member functions.
 */

#include <OpenFrames/CoordinateAxes.hpp>
#include <osg/BlendFunc>
#include <osg/Point>
#include <osg/PointSprite>
#include <osgDB/ReadFile>
#include <iostream>

namespace OpenFrames
{

// Implement vertex shader to compute tick mark position/color
static const char *OFCoordinateAxes_VertSource = {
	"#version 120\n"
	"uniform mat4 osg_ModelViewProjectionMatrix;\n"

	"void main(void)\n"
	"{\n"
	// Position, color, and texture are just passed through
	"  gl_Position = osg_ModelViewProjectionMatrix*gl_Vertex;\n"
	"  gl_FrontColor = gl_Color;\n"
	"  gl_TexCoord[0] = gl_MultiTexCoord0;\n"
	"}\n"
};

// Fragment shader that draws a texture on a PointSprite
static const char *FragSource_Texture = {
  "#version 120\n"
  "uniform sampler2D tex;\n"

  "void main(void)\n"
  "{\n"
     // Discard fragments with small alpha values
  "  vec4 t2d = texture2D(tex, gl_TexCoord[0].st);\n"
  "  if(t2d.a < 0.05)\n"
  "  {\n"
  "    discard;\n"
  "  }\n"

     // Color the texture with user-specified color
  "  gl_FragColor = t2d*gl_Color;\n"
  "}\n"
};

// Fragment shader that draws a solid disk on a PointSprite
static const char *FragSource_Disk = {
  "#version 120\n"
  "vec2 v;\n"

  "void main(void)\n"
  "{\n"
  // gl_PointCoord has range (x,y) in [0, 1] each, with y-down
  // Move origin to point center, with extents [-0.5, 0.5]
  "  v = gl_PointCoord - vec2(0.5);\n"

  // Throw away fragments outside the disk (radius > 0.5)
  "  if(dot(v, v) > 0.25)\n"
  "  {\n"
  "    discard;\n"
  "  }\n"

  // Remaining fragments get the user-specified color
  "  gl_FragColor = gl_Color;\n"
  "}\n"
};

CoordinateAxes::CoordinateAxes(const std::string &name)
	: ReferenceFrame(name)
{
	_init();
}

CoordinateAxes::CoordinateAxes(const std::string &name, float r, float g,
                             float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	_init();
}

CoordinateAxes::~CoordinateAxes() {}

void CoordinateAxes::showContents(bool showContents)
{
	if (showContents)
	{
	  _axesGeode->setNodeMask(0xffffffff);
	  _tickGeode->setNodeMask(0xffffffff);
	}
	else
	{
	  _axesGeode->setNodeMask(0x0);
	  _tickGeode->setNodeMask(0x0);
	}
}

bool CoordinateAxes::getContentsShown() const
{
	return (_axesGeode->getNodeMask() != 0x0 || _tickGeode->getNodeMask() != 0x0);
}

void CoordinateAxes::setAxisLength(double len)
{
	if((len != _axisLength) && (len >= 0.0))
	{
	  _axisLength = len;
	  _createAxes();

	  moveXAxis(osg::Vec3(len, 0.0, 0.0), 0.5*len);
	  moveYAxis(osg::Vec3(0.0, len, 0.0), 0.5*len);
	  moveZAxis(osg::Vec3(0.0, 0.0, len), 0.5*len);

	  _computeTickAttenuation();
	}
}

void CoordinateAxes::setDrawAxes(unsigned int axes)
{
	if(_drawAxes != axes)
	{
	  _drawAxes = axes;
	  _createAxes();
	}
}

  void CoordinateAxes::setAxisWidth( double width )
  {
    if ( width > 1.0 )
    {
       _lineWidth->setWidth( width );
    }
  }

  double CoordinateAxes::getAxisWidth() const
  {
    return _lineWidth->getWidth();
  }

void CoordinateAxes::setTickSpacing(double major_sp, double minor_sp)
{
	bool create = false;

	if((major_sp != _majorTickSpacing) && (major_sp >= 0.0))
	{
	  _majorTickSpacing = major_sp;
	  create = true;
	}

	if((minor_sp != _minorTickSpacing) && (minor_sp >= 0.0))
	{
	  _minorTickSpacing = minor_sp;
	  create = true;
	}

	if(create) _createAxes();
}

void CoordinateAxes::setTickSize(unsigned int majorSize, unsigned int minorSize)
{
	osg::Point *point;

	if(majorSize > 0)
	{
	  // Get the Point parameter for the major tick marks
	  point = static_cast<osg::Point*>(_majorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	  point->setSize(majorSize);
	  point->setMaxSize(majorSize);
	}

	if(minorSize > 0)
	{
	  // Get the Point parameter for the minor tick marks
	  point = static_cast<osg::Point*>(_minorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	  point->setSize(minorSize);
	  point->setMaxSize(minorSize);
	}
}

bool CoordinateAxes::setTickImage(const std::string &fname)
{
        // Remove any existing tick image
        if(fname.length() == 0)
        {
          resetTickShader();
          return true;
        }

        // Load image from file
	osg::Image *image = osgDB::readImageFile(fname);

	if(image)
	{
          osg::StateSet *ss = _tickGeode->getOrCreateStateSet();

	  // Specify texture to use for point sprite
	  osg::Texture2D *tex = new osg::Texture2D(image);
	  ss->setTextureAttributeAndModes(0, tex);

          // Assume texture may have transparency
          ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

          // Set up alpha blending for texture
          osg::BlendFunc *fn = new osg::BlendFunc();
          fn->setFunction(osg::BlendFunc::SRC_ALPHA,
              osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
          ss->setAttributeAndModes(fn);

          // Fragment shader to draw the marker texture
          _fragShader->setShaderSource(FragSource_Texture);

	  return true;
	}
	else
	{
	  std::cerr<< "OpenFrames::CoordinateAxes ERROR: Image file \'" << fname << "\' could not be found!" << std::endl;
	  return false; // Image was not found
	}
}

bool CoordinateAxes::setTickShader(const std::string &fname)
{
        // Reset shader
        if(fname.length() == 0)
        {
          resetTickShader();
          return true;
        }

        // Load shader source from file
        bool success = _fragShader->loadShaderSourceFromFile(fname);
        if(success)
        {
          osg::StateSet *ss = _tickGeode->getOrCreateStateSet();

          // Remove existing image texture and blend function
          ss->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
          ss->removeAttribute(osg::StateAttribute::BLENDFUNC);

          // Assume opaque marker
          ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);
        }
        else
        {
          std::cerr<< "OpenFrames::MarkerArtist ERROR: Shader file \'" << fname << "\' not properly loaded!" << std::endl;
          return false;
        }

        return true;
}

const osg::BoundingSphere& CoordinateAxes::getBound() const
{
	ReferenceFrame::getBound(); // Get the ReferenceFrame's bounds
	
	// Initialize a BoundingSphere that would just contain the coordinate axes
	osg::BoundingSphere bs;
	bs._radius = _axisLength;

	// Expand to encompass the rest of the reference frame
	bs.expandRadiusBy(_bound);

	// Finally, set this bound to be the frame's BoundingSphere
	_bound = bs;	
	return _bound;
}

void CoordinateAxes::setColor( const osg::Vec4 &color )
{
	// Set new color
	ReferenceFrame::setColor(color);
	(*_colors)[0] = color;
  _colors->dirty();
}

void CoordinateAxes::_init()
{
	// Create geode to hold all the geometries
	_axesGeode = new osg::Geode; // Hold axes lines
	_tickGeode = new osg::Geode; // Hold major/minor tick marks

	_axesGeode->setName(_name + " axes geode");
	_tickGeode->setName(_name + " tick geode");

	// Disable lighting computations
	_axesGeode->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	_tickGeode->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

	// Create the geometry drawables for the actual x/y/z axes, as well
	// as for the major and minor tick marks
	_axesGeom = new osg::Geometry;
	_majorTickGeom = new osg::Geometry;
	_minorTickGeom = new osg::Geometry;
  
  // Use VBOs
  _axesGeom->setUseDisplayList(false);
  _axesGeom->setUseVertexBufferObjects(true);
  _majorTickGeom->setUseDisplayList(false);
  _majorTickGeom->setUseVertexBufferObjects(true);
  _minorTickGeom->setUseDisplayList(false);
  _minorTickGeom->setUseVertexBufferObjects(true);

  // Set initial width of the axis lines and activate that attrib.
  _lineWidth = new osg::LineWidth;
  _lineWidth->setWidth( 1.0 );
  _axesGeom->getOrCreateStateSet()->setAttribute( _lineWidth, osg::StateAttribute::ON );

        // Add the Point parameter to allow major/minor tick mark resizing
        // Note that major/minor Geoms need separate StateSets since they
        // can have different osg::Point sizes
	_majorTickGeom->getOrCreateStateSet()->setAttribute(new osg::Point);
	_minorTickGeom->getOrCreateStateSet()->setAttribute(new osg::Point);

        // Set up point sprite so tick marks can be customized
        // This can be shared between major/minor Geoms
        osg::PointSprite *sprite = new osg::PointSprite();
        _tickGeode->getOrCreateStateSet()->setTextureAttributeAndModes(0, sprite);

        // GLSL shader program
        osg::Program *program = new osg::Program;
        program->setName("OFCoordinateAxes_ShaderProgram");
        _tickGeode->getOrCreateStateSet()->setAttribute(program);
		
		// Vertex shader to draw the tick marks
		osg::Shader *vertShader = new osg::Shader(osg::Shader::VERTEX);
		vertShader->setShaderSource(OFCoordinateAxes_VertSource);
		program->addShader(vertShader);

        // Fragment shader to draw the tick marks
        _fragShader = new osg::Shader(osg::Shader::FRAGMENT);
        program->addShader(_fragShader);
        resetTickShader();

	// Create the arrays for vertex and color data
	_vertices = new osg::Vec3dArray; // Vertices will be added later
	_colors = new osg::Vec4Array(1); // Color will be specified later

	// Attach vertex and color data to the geometries
	_axesGeom->setVertexArray(_vertices.get());
	_axesGeom->setColorArray(_colors.get());
	_axesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_majorTickGeom->setVertexArray(_vertices.get());
	_majorTickGeom->setColorArray(_colors.get());
	_majorTickGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_minorTickGeom->setVertexArray(_vertices.get());
	_minorTickGeom->setColorArray(_colors.get());
	_minorTickGeom->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Add everything to this ReferenceFrame
	_axesGeode->addDrawable(_axesGeom.get());
	_tickGeode->addDrawable(_majorTickGeom.get());
	_tickGeode->addDrawable(_minorTickGeom.get());
	_xform->addChild(_axesGeode.get());
	_xform->addChild(_tickGeode.get());

	// Set up defaults
	_majorTickSpacing = 0.5;
	_minorTickSpacing = 0.25;
	_drawAxes = X_AXIS | Y_AXIS | Z_AXIS;
	(*_colors)[0] = getColor();
	setTickSize(8, 4); // 8-pixel major and 4-pixel minor tick marks
	setAxisLength(1.0);
}

void CoordinateAxes::_computeTickAttenuation()
{
	osg::Point *point;
	float a, b, c; // Coefficients for attenuation

	// The computed point size is based on the specified point size and
	// the attenuation parameters a, b, c. See glPointParameter with the
	// parameter of GL_POINT_DISTANCE_ATTENUATION for more info.
	// The equation is: 
	//   computed size = clamp(specified size * sqrt(1/(a + b*d + c*d*d)))
	//   where d is the eye-distance to the point
	// Here we compute attenuation parameters such that the tick marks
	// will be visible at distances proportional to the size of the axes.
	a = c = 0.0;
	b = 1.0/_axisLength;
	osg::Vec3 attenuation(a, b, c);

	// Get the Point parameter for the major tick marks
	point = static_cast<osg::Point*>(_majorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	point->setDistanceAttenuation(attenuation);

	// Get the Point parameter for the minor tick marks
	point = static_cast<osg::Point*>(_minorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	point->setDistanceAttenuation(attenuation);
}

void CoordinateAxes::_createAxes()
{
	unsigned int start; // Starting index in vertex array
	double mark; // Point to place tick mark
	double val;  // Temp variable

	// Prepare geometries for new vertices that will be computed
	_axesGeom->removePrimitiveSet(0, _axesGeom->getNumPrimitiveSets());
	_majorTickGeom->removePrimitiveSet(0, _majorTickGeom->getNumPrimitiveSets());
	_minorTickGeom->removePrimitiveSet(0, _minorTickGeom->getNumPrimitiveSets());

	// Clear all existing vertices
	_vertices->clear();

	// First add vertices for the axes that need to be drawn
	if(_drawAxes & X_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(_axisLength, 0.0, 0.0));
	}

	if(_drawAxes & Y_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(0.0, _axisLength, 0.0));
	}

	if(_drawAxes & Z_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, _axisLength));
	}

	// Tell the axes geometry to draw data as lines
	_axesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, _vertices->size()));

	start = _vertices->size(); // Start of major tick mark vertices

	// Now add vertices for each major tick mark on each axis
	for(mark = _majorTickSpacing; mark <= _axisLength; mark += _majorTickSpacing)
	{
	  if(_drawAxes & X_AXIS) // On X-axis
	    _vertices->push_back(osg::Vec3d(mark, 0.0, 0.0));

	  if(_drawAxes & Y_AXIS) // On Y-axis
	    _vertices->push_back(osg::Vec3d(0.0, mark, 0.0));

	  if(_drawAxes & Z_AXIS) // On Z-axis
	    _vertices->push_back(osg::Vec3d(0.0, 0.0, mark));
	}

	// Tell the major tick mark geometry to draw data as points
	_majorTickGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS, start, _vertices->size() - start));

	start = _vertices->size(); // Start of minor tick mark vertices

	// Now add vertices for each minor tick mark on each axis
	for(mark = _minorTickSpacing; mark <= _axisLength; mark += _minorTickSpacing)
	{
	  // Make sure the minor tick mark isn't on top of a major tick mark
	  val = mark / _majorTickSpacing;
	  val = val - (int)val;
	  if((val <= 1.0e-6) || (val >= 0.9999)) continue;

	  // Otherwise go ahead and add the minor tick mark
	  if(_drawAxes & X_AXIS) // On X-axis
	    _vertices->push_back(osg::Vec3d(mark, 0.0, 0.0));

	  if(_drawAxes & Y_AXIS) // On Y-axis
	    _vertices->push_back(osg::Vec3d(0.0, mark, 0.0));

	  if(_drawAxes & Z_AXIS) // On Z-axis
	    _vertices->push_back(osg::Vec3d(0.0, 0.0, mark));
	}

	// Tell the minor tick mark geometry to draw data as points
	_minorTickGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS, start, _vertices->size() - start));
  
  // Indicate that data has changed and should be re-rendered
  _vertices->dirty();
}

void CoordinateAxes::resetTickShader()
{
        osg::StateSet *ss = _tickGeode->getOrCreateStateSet();

        // Remove existing image texture and blending
        ss->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
        ss->removeAttribute(osg::StateAttribute::BLENDFUNC);

        // Assume opaque marker
        ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);

        // Default to circular point marker
        _fragShader->setShaderSource(FragSource_Disk);
}

} // !namespace OpenFrames
