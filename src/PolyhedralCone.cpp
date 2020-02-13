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

/** \file PolyhedralCone.cpp
 * PolyhedralCone-class function definitions.
 */

#include <OpenFrames/PolyhedralCone.hpp>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/Geometry>
#include <osg/PolygonOffset>
#include <osg/PositionAttitudeTransform>

namespace OpenFrames
{
  
PolyhedralCone::PolyhedralCone(const std::string &name)
: ReferenceFrame(name)
{
  init();
}

PolyhedralCone::PolyhedralCone(const std::string &name, float r, float g, float b, float a)
: ReferenceFrame(name, r, g, b, a)
{
  init();
}

PolyhedralCone::~PolyhedralCone() {}

void PolyhedralCone::showContents(bool showContents)
{
  if (showContents)
  {
    _coneGeode->setNodeMask(0xffffffff);
  }
  else
  {
    _coneGeode->setNodeMask(0x0);
  }
}

bool PolyhedralCone::getContentsShown() const
{
  return (_coneGeode->getNodeMask() != 0x0);
}

void PolyhedralCone::init()
{
	// Create geode to hold the cone geometries
	_coneGeode = new osg::Geode;
	_coneGeode->setName(_name + " geode");
	osg::StateSet *coneSS = _coneGeode->getOrCreateStateSet();

	// Disable lighting
	coneSS->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

	// Enable standard color blending and transparency
	osg::BlendFunc *bf = new osg::BlendFunc();
	bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
	coneSS->setAttributeAndModes(bf);
	coneSS->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

	// Don't write to the depth buffer so that transparent objects behind
	// this cone will still be drawn properly.
	osg::Depth *depth = new osg::Depth();
	depth->setWriteMask(false);
	coneSS->setAttributeAndModes(depth);

	// Make sure that the cone is always drawn behind overlaid lines by
	// offsetting it slightly. See glPolygonOffset man page.
	osg::PolygonOffset *offset = new osg::PolygonOffset(1, 1);
	coneSS->setAttributeAndModes(offset);

	// Create a geometry drawable for the radial circles & longitudinal
	// lines, and another one for the 0-degree longitude line.
	// Note that the geometry drawables for the actual plane itself
	// will be created and set up on the fly.
	_coneGeom = new osg::Geometry;
  
  // Use VBOs
  _coneGeom->setUseDisplayList(false);
  _coneGeom->setUseVertexBufferObjects(true);

	// Create the arrays for the vertex data
	_coneVertices = new osg::Vec3dArray;

	// Create the color vectors
	_coneColor = new osg::Vec4Array(1);

	// Bind the vertex and color data
	_coneGeom->setVertexArray(_coneVertices.get());
	_coneGeom->setColorArray(_coneColor.get());
	_coneGeom->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Attach the drawables to the main geode. The plane geometries will
	// be added to the main geode on the fly.
  _coneGeode->addDrawable(_coneGeom);

  // Create a transform that will handle scaling the cone
  _coneTransform = new osg::PositionAttitudeTransform;
  
	// Connect everything together
  _coneTransform->addChild(_coneGeode);
	_xform->addChild(_coneTransform);

	// Set default values
  AngleArray clockAngles =
  {
    osg::DegreesToRadians(0.0),
    osg::DegreesToRadians(120.0),
    osg::DegreesToRadians(240.0)
  };
  AngleArray coneAngles =
  {
    osg::DegreesToRadians(45.0),
    osg::DegreesToRadians(45.0),
    osg::DegreesToRadians(45.0)
  };
	setAngles(clockAngles, coneAngles);
	setConeColor(1.0, 1.0, 1.0, 0.2);
}

void PolyhedralCone::setAngles(const AngleArray& clockAngles, const AngleArray& coneAngles)
{
  // Check for duplicate angles
  if((clockAngles == _clockAngles) && (coneAngles == _coneAngles)) return;
  
  // Check for inconsistent angles
  if(clockAngles.size() != coneAngles.size()) return;
  
  _clockAngles = clockAngles;
  _coneAngles = coneAngles;
  createCone();
  
  /*
	if((_radius != radius) && (radius > 0.0))
	{
	  _radius = radius;
	  moveXAxis(osg::Vec3(radius, 0, 0), 0.5*radius);
	  moveYAxis(osg::Vec3(0, radius, 0), 0.5*radius);
	  moveZAxis(osg::Vec3(0, 0, radius), 0.5*radius);
	}
   */
}

void PolyhedralCone::setConeColor(const osg::Vec4 &color)
{
  // Set color of the longitude lines & radial circles
  (*_coneColor)[0] = color;
  _coneColor->dirty();
}

void PolyhedralCone::setConeColor(float r, float g, float b, float a)
{
  setConeColor(osg::Vec4(r, g, b, a));
}

const osg::BoundingSphere& PolyhedralCone::getBound() const
{
  // Have bounding sphere encompass cone and axes/labels, but centered
  // on the plane (since that is the object of interest)
  ReferenceFrame::getBound();
  osg::BoundingSphere bs = _coneGeode->getBoundingBox();
  bs.expandRadiusBy(_bound);
  _bound = bs;

  return _bound;
}

void PolyhedralCone::createCone()
{
	const double epsilon = 1.0e-6; // Precision tolerance
	const double step = osg::PI/180.0; // Step size for radial circles
	double max = 2.0*osg::PI - epsilon;

	// Prepare geometries for new vertices that will be computed
	_coneGeom->removePrimitiveSet(0, _coneGeom->getNumPrimitiveSets());

	// Initialize cone apex vertex
	_coneVertices->clear();
  _coneVertices->push_back(osg::Vec3d(0, 0, 0));

	// Add vertices corresponding to each clock/cone angle
  // Vertices lie alone the z=1 plane, and are scaled with the
  // cone's PositionAttitudeTransform
  for(int i = 0; i < _clockAngles.size(); ++i)
  {
    double clockAngle = _clockAngles[i];
    double coneAngle = _coneAngles[i];
    double z = 1.0;
    double l = z*std::tan(coneAngle);
    double x = l*std::cos(clockAngle);
    double y = l*std::sin(clockAngle);
    _coneVertices->push_back(osg::Vec3d(x, y, z));
  }
  
  _coneGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::TRIANGLE_FAN, 0, _coneVertices->size()));

  // Indicate that data has changed and should be re-rendered
  _coneVertices->dirty();
}

} // !namespace OpenFrames
