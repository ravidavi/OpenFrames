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

/** \file SegmentArtist.cpp
 * SegmentArtist-class function definitions.
 */

#include <OpenFrames/DoubleSingleUtils.hpp>
#include <OpenFrames/SegmentArtist.hpp>
#include <osg/Geometry>
#include <climits>
#include <algorithm>

namespace OpenFrames
{

/** Updates a SegmentArtist's internal geometry when its target Trajectory changes. */
class SegmentArtistUpdateCallback : public osg::Callback
{
public:
  SegmentArtistUpdateCallback()
    : _numPoints(0),
    _dataAdded(true),
    _dataCleared(true)
  {}

  void dataAdded() { _dataAdded = true; }
  void dataCleared() { _dataCleared = true; }

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    // Get the arrays that hold vertex data
    // Note that all object types are known, since this callback is only added to a CurveArtist
    // This is why we don't have to use dynamic_cast
    _sa = static_cast<SegmentArtist*>(object);
    _geom = _sa->getDrawable(0)->asGeometry();
    _vertexHigh = static_cast<osg::Vec3Array*>(_geom->getVertexArray());
    _vertexLow = static_cast<osg::Vec3Array*>(_geom->getVertexAttribArray(TrajectoryArtist::OF_VERTEXLOW));
    _drawArrays = static_cast<osg::DrawArrays*>(_geom->getPrimitiveSet(0));

    // Clear data if it is invalid or all points are zero
    if (!_sa->isDataValid() || (_sa->isStartDataZero() && _sa->isEndDataZero()))
    {
      // Clear all points
      clearVertexData();
      dirtyVertexData(0);
    }

    // Otherwise process trajectory points and update vertex data as needed
    else
    {
      // Clear all points if needed
      if (_dataCleared)
      {
        clearVertexData();
        _numPoints = 0;
        _dataCleared = false;
        _dataAdded = true; // Ensure arrays are marked as dirty
      }

      // Process new data if needed
      if (_dataAdded)
      {
        // Get and lock trajectory so its data doesn't move while we're analyzing it
        _dataAdded = false;
        _traj = _sa->getTrajectory();
        _traj->lockData();

        // Process trajectory points
        unsigned int newNumPoints = std::min(_traj->getNumPoints(_sa->getStartDataSource()),
          _traj->getNumPoints(_sa->getEndDataSource()));

        processPoints(newNumPoints);

        // Unlock trajectory
        _traj->unlockData();

        // Mark data as changed
        dirtyVertexData(newNumPoints);
      }
    }

    // Continue traversing as needed
    return traverse(object, data);
  }

private:
  void clearVertexData()
  {
    _vertexHigh->clear();
    _vertexLow->clear();
  }

  void dirtyVertexData(unsigned int newNumPoints)
  {
    _numPoints = newNumPoints;

    // Dirty vertex arrays to indicate they've changed
    _vertexHigh->dirty();
    _vertexLow->dirty();

    // Update number of points to draw
    if (_numPoints == 0) _geom->setNodeMask(0);
    else
    {
      _geom->setNodeMask(~0);
      _drawArrays->setCount(2*_numPoints); // 2 vertices per point
      _drawArrays->dirty();
      _geom->dirtyBound();
    }
  }

  void processPoints(unsigned int newNumPoints)
  {
    osg::Vec3d startVertex, endVertex;  // Start and end vertices for a point
    osg::Vec3f high, low;

    for (unsigned int i = _numPoints; i < newNumPoints; i += _sa->getStride())
    {
      // Get start and end vertices for current point
      _traj->getPoint(i, _sa->getStartDataSource(), startVertex._v);
      _traj->getPoint(i, _sa->getEndDataSource(), endVertex._v);

      // Split start vertex into high and low portions to support GPU-based RTE rendering
      OpenFrames::DS_Split(startVertex, high, low);
      _vertexHigh->push_back(high);
      _vertexLow->push_back(low);

      // Split end vertex into high and low portions to support GPU-based RTE rendering
      OpenFrames::DS_Split(endVertex, high, low);
      _vertexHigh->push_back(high);
      _vertexLow->push_back(low);
    }
  }

  unsigned int _numPoints;
  bool _dataAdded, _dataCleared;

  osg::Geometry* _geom;
  osg::Vec3Array* _vertexHigh;
  osg::Vec3Array* _vertexLow;
  osg::DrawArrays* _drawArrays;
  const Trajectory* _traj;
  SegmentArtist* _sa;
};

SegmentArtist::SegmentArtist(const Trajectory *traj)
: _stride(1), _dataValid(false), _startDataZero(false), _endDataZero(false)
{
  setTrajectory(traj);

  Trajectory::DataSource data; // We can use default values
  setStartXData(data);
  setEndXData(data);
  setStartYData(data);
  setEndYData(data);
  setStartZData(data);
  setEndZData(data);

  // Initialize line properties
  _lineWidth = new osg::LineWidth;
  _linePattern = new osg::LineStipple;
  osg::StateSet* stateset = getOrCreateStateSet();
  stateset->setAttribute(_lineWidth.get());
  stateset->setAttributeAndModes(_linePattern.get());

  // Initialize colors
  // Currently we use one color for the whole trajectory, but this can be
  // changed later for per-vertex colors
  _lineColors = new osg::Vec4Array(1);
  (*_lineColors)[0] = osg::Vec4(1.0, 1.0, 1.0, 1.0);
  _lineColors->setBinding(osg::Array::BIND_OVERALL);

  // Initialize geometry that will draw line segments
  osg::Geometry *geom = new osg::Geometry;
  geom->setDataVariance(osg::Object::DYNAMIC);
  geom->setUseDisplayList(false);
  geom->setUseVertexBufferObjects(true);
  geom->setVertexArray(new osg::Vec3Array());
  geom->setVertexAttribArray(OF_VERTEXLOW, new osg::Vec3Array(), osg::Array::BIND_PER_VERTEX);
  geom->setColorArray(_lineColors);
  geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, 0));
  addDrawable(geom);

  // Add callback that updates our geometry when the Trajectory changes
  addUpdateCallback(new SegmentArtistUpdateCallback());
}

// Not using the copy constructor
SegmentArtist::SegmentArtist( const SegmentArtist &ca, const osg::CopyOp& copyop )
{}

SegmentArtist::~SegmentArtist()
{}

void SegmentArtist::setTrajectory(const Trajectory *traj)
{
	// Do nothing if this artist is already drawing the given Trajectory
	if(_traj == traj) return;

	// Handle default behavior
	TrajectoryArtist::setTrajectory(traj);

	// Make sure that the Trajectory can support the data we want to draw.
	verifyData();
}

bool SegmentArtist::setStartXData(const Trajectory::DataSource &src)
{
	if(_startSource[0] == src) return _dataValid;

	_startSource[0] = src;
	verifyData();

	return _dataValid;
}

bool SegmentArtist::setEndXData(const Trajectory::DataSource &src)
{
	if(_endSource[0] == src) return _dataValid;

	_endSource[0] = src;
	verifyData();

	return _dataValid;
}

bool SegmentArtist::setStartYData(const Trajectory::DataSource &src)
{
	if(_startSource[1] == src) return _dataValid;

	_startSource[1] = src;
	verifyData();

	return _dataValid;
}

bool SegmentArtist::setEndYData(const Trajectory::DataSource &src)
{
	if(_endSource[1] == src) return _dataValid;

	_endSource[1] = src;
	verifyData();

	return _dataValid;
}

bool SegmentArtist::setStartZData(const Trajectory::DataSource &src)
{
	if(_startSource[2] == src) return _dataValid;

	_startSource[2] = src;
	verifyData();

	return _dataValid;
}

bool SegmentArtist::setEndZData(const Trajectory::DataSource &src)
{
	if(_endSource[2] == src) return _dataValid;

	_endSource[2] = src;
	verifyData();

	return _dataValid;
}

void SegmentArtist::setStride(unsigned int stride)
{
	if(_stride == stride) return;

	if(stride == 0) _stride = 1;
	else _stride = stride;

  dataCleared(_traj);
}

void SegmentArtist::setColor( float r, float g, float b)
{
  (*_lineColors)[0].set(r, g, b, 1.0);
  _lineColors->dirty();
}

void SegmentArtist::setWidth( float width )
{
	if(width > 0.0)
	{
	  _lineWidth->setWidth(width);
	}
}

void SegmentArtist::setPattern( GLint factor, GLushort pattern )
{
	_linePattern->setFactor(factor);
	_linePattern->setPattern(pattern);
}

void SegmentArtist::dataCleared(const Trajectory* traj)
{
	verifyData();
  SegmentArtistUpdateCallback *cb = static_cast<SegmentArtistUpdateCallback*>(getUpdateCallback());
  cb->dataCleared();
}

void SegmentArtist::dataAdded(const Trajectory* traj)
{
  SegmentArtistUpdateCallback *cb = static_cast<SegmentArtistUpdateCallback*>(getUpdateCallback());
  cb->dataAdded();
}

void SegmentArtist::verifyData() const
{
	if(_startSource[0]._src == Trajectory::ZERO &&
	   _startSource[1]._src == Trajectory::ZERO &&
	   _startSource[2]._src == Trajectory::ZERO)
	{
	  _startDataZero = true;
	}
	else _startDataZero = false;

	if(_endSource[0]._src == Trajectory::ZERO &&
	   _endSource[1]._src == Trajectory::ZERO &&
	   _endSource[2]._src == Trajectory::ZERO)
	{
	  _endDataZero = true;
	}
	else _endDataZero = false;

	if(_startDataZero && _endDataZero) _dataValid = true;
	else if(_traj.valid())
	{
	  _dataValid = _traj->verifyData(_startSource) &&
	               _traj->verifyData(_endSource);
	}
	else _dataValid = false;
}

}
