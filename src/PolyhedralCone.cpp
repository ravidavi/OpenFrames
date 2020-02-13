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
 * PolyhedralCone class function definitions.
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

  // Rescale normals since we use scale to size the cone
  coneSS->setMode(GL_RESCALE_NORMAL, osg::StateAttribute::ON);

  // Make sure that the cone is always drawn behind overlaid lines by
  // offsetting it slightly. See glPolygonOffset man page.
  osg::PolygonOffset *offset = new osg::PolygonOffset(1, 1);
  coneSS->setAttributeAndModes(offset);

  // Create a geometry for the cone sides, edges, and base
  _sideGeom = new osg::Geometry;
  _edgeGeom = new osg::Geometry;
  _baseGeom = new osg::Geometry;
  _baseOutlineGeom = new osg::Geometry;
  
  // Use VBOs
  _sideGeom->setUseDisplayList(false);
  _sideGeom->setUseVertexBufferObjects(true);
  _edgeGeom->setUseDisplayList(false);
  _edgeGeom->setUseVertexBufferObjects(true);
  _baseGeom->setUseDisplayList(false);
  _baseGeom->setUseVertexBufferObjects(true);
  _baseOutlineGeom->setUseDisplayList(false);
  _baseOutlineGeom->setUseVertexBufferObjects(true);

  // Create the arrays for the vertex data
  _sideVertices = new osg::Vec3dArray;
  _sideNormals = new osg::Vec3dArray;
  _edgeVertices = new osg::Vec3dArray;
  _baseVertices = new osg::Vec3dArray;

  // Create the color vectors
  _coneColor = new osg::Vec4Array(1);
  _lineColor = new osg::Vec4Array(1);

  // Bind the vertex and color data
  _sideGeom->setVertexArray(_sideVertices.get());
  _sideGeom->setNormalArray(_sideNormals.get(), osg::Array::BIND_PER_VERTEX);
  _sideGeom->setColorArray(_coneColor.get(), osg::Array::BIND_OVERALL);
  _edgeGeom->setVertexArray(_edgeVertices.get());
  _edgeGeom->setColorArray(_lineColor.get(), osg::Array::BIND_OVERALL);
  _baseGeom->setVertexArray(_baseVertices.get());
  _baseGeom->setColorArray(_coneColor.get(), osg::Array::BIND_OVERALL);
  _baseOutlineGeom->setVertexArray(_baseVertices.get());
  _baseOutlineGeom->setColorArray(_lineColor.get(), osg::Array::BIND_OVERALL);

  // Attach the drawables to the main geode.
  _coneGeode->addDrawable(_sideGeom);
  _coneGeode->addDrawable(_edgeGeom);
  _coneGeode->addDrawable(_baseGeom);
  _coneGeode->addDrawable(_baseOutlineGeom);

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
  setVertexAngles(clockAngles, coneAngles);

  setConeColor(getColor());
  setLineColor(getColor());
  setDrawMode(DEFAULT);
}

void PolyhedralCone::setVertexAngles(const AngleArray& clockAngles, const AngleArray& coneAngles)
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

void PolyhedralCone::setLineColor(const osg::Vec4 &color)
{
  // Set color of the longitude lines & radial circles
  (*_lineColor)[0] = color;
  _lineColor->dirty();
}

void PolyhedralCone::setConeAxis(const osg::Vec3d& axis)
{
}

void PolyhedralCone::setDrawMode(unsigned int drawMode)
{
  if(drawMode & SIDES) _sideGeom->setNodeMask(~0x0);
  else _sideGeom->setNodeMask(0x0);

  if(drawMode & EDGES) _edgeGeom->setNodeMask(~0x0);
  else _edgeGeom->setNodeMask(0x0);

  if(drawMode & BASE) _baseGeom->setNodeMask(~0x0);
  else _baseGeom->setNodeMask(0x0);

  if(drawMode & BASE_OUTLINE) _baseOutlineGeom->setNodeMask(~0x0);
  else _baseOutlineGeom->setNodeMask(0x0);
}

unsigned int PolyhedralCone::getDrawMode() const
{
  unsigned int nodeMask = NONE;

  if(_sideGeom->getNodeMask()) nodeMask |= SIDES;
  if(_edgeGeom->getNodeMask()) nodeMask |= EDGES;
  if(_baseGeom->getNodeMask()) nodeMask |= BASE;
  if(_baseOutlineGeom->getNodeMask()) nodeMask |= BASE_OUTLINE;

  return nodeMask;
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
	// Prepare geometries for new vertices that will be computed
  _sideGeom->removePrimitiveSet(0, _sideGeom->getNumPrimitiveSets());
  _edgeGeom->removePrimitiveSet(0, _edgeGeom->getNumPrimitiveSets());
  _baseGeom->removePrimitiveSet(0, _baseGeom->getNumPrimitiveSets());
  _baseOutlineGeom->removePrimitiveSet(0, _baseOutlineGeom->getNumPrimitiveSets());

	// Initialize vertices
	_sideVertices->clear();
  _sideNormals->clear();
  _edgeVertices->clear();
  _baseVertices->clear();

  _sideVertices->push_back(osg::Vec3d(0, 0, 0)); // Add cone apex
  _sideNormals->push_back(osg::Vec3d(0, 0, -1)); // Cone apex normal points down

	// Add vertices corresponding to each clock/cone angle
  // Vertices lie alone the z=1 plane, and are scaled with the
  // cone's PositionAttitudeTransform
  osg::Vec3d zVec(0, 0, 1); // z unit vector
  osg::Vec3d vertex(0, 0, 1.0); // Each vertex is unit length in z direction
  osg::Vec3d normal, v_plane;
  for(int i = 0; i < _clockAngles.size(); ++i)
  {
    double clockAngle = _clockAngles[i];
    double coneAngle = _coneAngles[i];

    double l = vertex.z()*std::tan(coneAngle);
    vertex.x() = l*std::cos(clockAngle);
    vertex.y() = l*std::sin(clockAngle);

    // Side vertex
    _sideVertices->push_back(vertex);

    // Side normal
    v_plane = vertex ^ zVec;
    normal = vertex ^ v_plane;
    normal.normalize();
    _sideNormals->push_back(normal);

    // Edge line from apex to vertex
    _edgeVertices->push_back(osg::Vec3d());
    _edgeVertices->push_back(vertex);

    // Base vertex
    _baseVertices->push_back(vertex);
  }

  // Repeat first point to close the cone
  _sideVertices->push_back((*_sideVertices)[1]);
  _sideNormals->push_back((*_sideNormals)[1]);
  
  _sideGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::TRIANGLE_FAN, 0, _sideVertices->size()));
  _edgeGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, _edgeVertices->size()));
  _baseOutlineGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, 0, _baseVertices->size()));
  _baseGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POLYGON, 0, _baseVertices->size()));

  // Indicate that data has changed and should be re-rendered
  _sideVertices->dirty();
  _sideNormals->dirty();
  _edgeVertices->dirty();
  _baseVertices->dirty();
}

} // !namespace OpenFrames
