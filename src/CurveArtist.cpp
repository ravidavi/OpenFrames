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

/** \file CurveArtist.cpp
 * Definitions for the CurveArtist class.
 */

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DoubleSingleUtils.hpp>
#include <osg/Geometry>
#include <climits>

namespace OpenFrames
{

/** Updates a CurveArtist's internal geometry when its target Trajectory changes. */
class CurveArtistUpdateCallback : public osg::Callback
{
public:
  CurveArtistUpdateCallback()
    : _dataAdded(true), _dataCleared(true), _batchSize(1000), _lastUpdateTime(0.0), _lastRunTime(0.0)
  {}

  void dataAdded() { _dataAdded = true; }
  void dataCleared() { _dataCleared = true; }

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    // Get the current time
    osg::NodeVisitor* nv = data->asNodeVisitor();
    double currTime = nv->getFrameStamp()->getReferenceTime();

    // Process points at fixed rate to avoid performance bottlenecks in high-FPS applications (e.g. VR)
    if (currTime - _lastRunTime < 0.05) return traverse(object, data);
    else _lastRunTime = currTime;

    // Get the arrays that hold vertex data
    // Note that all object types are known, since this callback is only added to a CurveArtist
    // This is why we don't have to use dynamic_cast
    _ca = static_cast<CurveArtist*>(object);
    _geom = _ca->getDrawable(0)->asGeometry();
    _vertexHigh = static_cast<osg::Vec3Array*>(_geom->getVertexArray());
    _vertexLow = static_cast<osg::Vec3Array*>(_geom->getVertexAttribArray(TrajectoryArtist::OF_VERTEXLOW));
    _drawArrays = static_cast<osg::DrawArrays*>(_geom->getPrimitiveSet(0));

    // Clear data if it is invalid or all points are zero
    if (!_ca->isDataValid() || _ca->isDataZero())
    {
      // Clear all points
      clearVertexData();
      dirtyVertexData();
    }

    // Otherwise process trajectory points and update vertex data as needed
    else if (_dataCleared || _dataAdded)
    {
      // Clear all points if needed
      if (_dataCleared)
      {
        clearVertexData();
        _dataCleared = false;
        _dataAdded = true; // Ensure arrays are marked as dirty
      }

      // Process new data if needed
      if (_dataAdded)
      {
        // Get and lock trajectory so its data doesn't move while we're analyzing it
        _dataAdded = false;
        _lastUpdateTime = currTime;
        _traj = _ca->getTrajectory();
        _traj->lockData();

        // Process trajectory points
        unsigned int newNumPoints = _traj->getNumPoints(_ca->getDataSource());
        processPoints(newNumPoints);

        // Unlock trajectory
        _traj->unlockData();

        // Mark data as changed
        dirtyVertexData();
      }
    }

    // If vertex arrays have not been modified in a while, then shink them
    // to fit the current number of points
    else if ((currTime - _lastUpdateTime >= 1.0) && (_vertexHigh->size() > _drawArrays->getCount()))
    {
      _vertexHigh->resize(_drawArrays->getCount());
      _vertexLow->resize(_drawArrays->getCount());
      _vertexHigh->asVector().shrink_to_fit();
      _vertexLow->asVector().shrink_to_fit();
      _vertexHigh->dirty();
      _vertexLow->dirty();
    }

    // Continue traversing as needed
    return traverse(object, data);
  }

private:
  void clearVertexData()
  {
    _vertexHigh->clear();
    _vertexLow->clear();
    _drawArrays->setCount(0);
  }

  void dirtyVertexData()
  {
    // Dirty arrays to indicate they've changed
    _vertexHigh->dirty();
    _vertexLow->dirty();
    _drawArrays->dirty();
    _geom->dirtyBound();
  }

  void processPoints(unsigned int newNumPoints)
  {
    // Make space for new points
    if (newNumPoints > _vertexHigh->size())
    {
      unsigned int newSize = std::ceil((double)newNumPoints / (double)_batchSize);
      newSize *= _batchSize;
      _vertexHigh->resize(newSize);
      _vertexLow->resize(newSize);
    }

    // Process each new point
    osg::Vec3d newPoint;
    osg::Vec3f high, low;
    for (unsigned int i = _drawArrays->getCount(); i < newNumPoints; ++i)
    {
      _traj->getPoint(i, _ca->getDataSource(), newPoint._v); // Get current point

      // Split point into high and low portions to support GPU-based RTE rendering
      OpenFrames::DS_Split(newPoint, high, low);
      (*_vertexHigh)[i] = high;
      (*_vertexLow)[i] = low;
    }
    _drawArrays->setCount(newNumPoints);
  }

  bool _dataAdded, _dataCleared;
  unsigned int _batchSize;
  double _lastUpdateTime, _lastRunTime;

  osg::Geometry* _geom;
  osg::Vec3Array* _vertexHigh;
  osg::Vec3Array* _vertexLow;
  osg::DrawArrays* _drawArrays;
  const Trajectory* _traj;
  CurveArtist* _ca;
};

CurveArtist::CurveArtist(const Trajectory *traj)
: _dataValid(false), _dataZero(false)
{
	setTrajectory(traj); // Set the specified trajectory

	unsigned int dof = 0;
	if(_traj.valid()) dof = _traj->getDOF();

        // By default, the CurveArtist will draw a line between
        // successive points in the trajectory.
	Trajectory::DataSource data;
        data._src = Trajectory::POSOPT;

	if(dof >= 1)
	{
	  data._element = 0;
          setXData(data);
	}

	if(dof >= 2)
	{
	  data._element = 1;
          setYData(data);
	}

	if(dof >= 3)
	{
	  data._element = 2;
          setZData(data);
	}

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

  // Initialize geometry that will draw line strips
  osg::Geometry *geom = new osg::Geometry;
  geom->setDataVariance(osg::Object::DYNAMIC);
  geom->setUseDisplayList(false);
  geom->setUseVertexBufferObjects(true);
  geom->setVertexArray(new osg::Vec3Array());
  geom->setVertexAttribArray(OF_VERTEXLOW, new osg::Vec3Array(), osg::Array::BIND_PER_VERTEX);
  geom->setColorArray(_lineColors);
  geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, 0, 0));
  geom->getOrCreateVertexBufferObject()->setUsage(GL_DYNAMIC_DRAW);
  addDrawable(geom);

  // Add callback that updates our geometry when the Trajectory changes
  addUpdateCallback(new CurveArtistUpdateCallback());
}

// Not using the copy constructor
CurveArtist::CurveArtist( const CurveArtist &ca, const osg::CopyOp& copyop )
{}

CurveArtist::~CurveArtist()
{}

void CurveArtist::setTrajectory(const Trajectory *traj)
{
	// Do nothing if this artist is already drawing the given Trajectory
	if(_traj == traj) return;

	// Handle default behavior
	TrajectoryArtist::setTrajectory(traj);

	verifyData();
}

bool CurveArtist::setXData(const Trajectory::DataSource &src)
{
	if(_dataSource[0] == src) return _dataValid;

	_dataSource[0] = src; // Set new source
	verifyData();

	return _dataValid;
}

bool CurveArtist::setYData(const Trajectory::DataSource &src)
{
	if(_dataSource[1] == src) return _dataValid;

	_dataSource[1] = src; // Set new source
	verifyData();

	return _dataValid;
}

bool CurveArtist::setZData(const Trajectory::DataSource &src)
{
	if(_dataSource[2] == src) return _dataValid;

	_dataSource[2] = src; // Set new source
	verifyData();

	return _dataValid;
}

void CurveArtist::setColor( float r, float g, float b)
{
  (*_lineColors)[0].set(r, g, b, 1.0);
  _lineColors->dirty();
}

void CurveArtist::setWidth( float width )
{
	if(width > 0.0)
	{
	  _lineWidth->setWidth(width);
	}
}

void CurveArtist::setPattern( GLint factor, GLushort pattern )
{
	_linePattern->setFactor(factor);
	_linePattern->setPattern(pattern);
}

void CurveArtist::dataCleared(const Trajectory* traj)
{
	verifyData();
  CurveArtistUpdateCallback *cb = static_cast<CurveArtistUpdateCallback*>(getUpdateCallback());
  cb->dataCleared();
}

void CurveArtist::dataAdded(const Trajectory* traj)
{
  CurveArtistUpdateCallback *cb = static_cast<CurveArtistUpdateCallback*>(getUpdateCallback());
  cb->dataAdded();
}

void CurveArtist::verifyData() const
{
	if(_dataSource[0]._src == Trajectory::ZERO &&
	   _dataSource[1]._src == Trajectory::ZERO &&
	   _dataSource[2]._src == Trajectory::ZERO)
	{
	  _dataValid = true;
	  _dataZero = true;
	}
	else if(_traj.valid()) 
	{
	  _dataValid = _traj->verifyData(_dataSource);
	  _dataZero = false;
	}
	else 
	{
	  _dataValid = false;
	  _dataZero = false;
	}
}

}
