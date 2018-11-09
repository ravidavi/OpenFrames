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
    : _numPoints(0),
    _dataAdded(true),
    _dataCleared(true)
  {}

  virtual bool run(osg::Object* object, osg::Object* data)
  {
    // Get the arrays that hold vertex data
    // Note that all object types are known, since this callback is only added to a CurveArtist
    // This is why we don't have to use dynamic_cast
    CurveArtist* ca = static_cast<CurveArtist*>(object);
    osg::Geometry* geom = ca->getDrawable(0)->asGeometry();
    osg::Vec3Array* vertexHigh = static_cast<osg::Vec3Array*>(geom->getVertexArray());
    osg::Vec3Array* vertexLow = static_cast<osg::Vec3Array*>(geom->getVertexAttribArray(0));

    // Clear data if it is invalid or all points are zero
    if (!ca->isDataValid() || ca->isDataZero())
    {
      // Clear all points
      vertexHigh->clear();
      vertexLow->clear();
      vertexHigh->dirty();
      vertexLow->dirty();
      _numPoints = 0;
    }

    // Otherwise process trajectory points and update vertex data as needed
    else
    {
      // Clear all points if needed
      if (_dataCleared)
      {
        vertexHigh->clear();
        vertexLow->clear();
        _numPoints = 0;
        _dataCleared = false;
        _dataAdded = true; // Ensure arrays are marked as dirty
      }

      // Process new data if needed
      if (_dataAdded)
      {
        // Get and lock trajectory so its data doesn't move while we're analyzing it
        const Trajectory* traj = ca->getTrajectory();
        traj->lockData();

        // Get number of points in trajectory
        unsigned int newNumPoints = traj->getNumPoints(ca->getDataSource());

        // Process each new point
        osg::Vec3d newPoint;
        for (unsigned int i = _numPoints; i < newNumPoints; ++i)
        {
          traj->getPoint(i, ca->getDataSource(), newPoint._v); // Get current point

          // Split point into high and low portions to support GPU-based RTE rendering
          osg::Vec3f high, low;
          OpenFrames::DS_Split(newPoint, high, low);
          vertexHigh->push_back(high);
          vertexLow->push_back(low);
        }

        // Unlock trajectory
        traj->unlockData();

        // Mark data as changed
        vertexHigh->dirty();
        vertexLow->dirty();
        _numPoints = newNumPoints;

        _dataAdded = false;
      }
    }

    // Continue traversing as needed
    return traverse(object, data);
  }

  void dataAdded() { _dataAdded = true; }
  void dataCleared() { _dataCleared = true; }

private:
  unsigned int _numPoints;
  bool _dataAdded, _dataCleared;
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

  // Initialize data arrays that will draw line strips
  osg::Geometry *geom = new osg::Geometry;
  geom->setName("CurveArtist geometry");
  geom->setDataVariance(osg::Object::DYNAMIC);
  geom->setUseDisplayList(false);
  geom->setUseVertexBufferObjects(true);
  geom->setVertexArray(new osg::Vec3Array());
  geom->setVertexAttribArray(OF_VERTEXLOW, new osg::Vec3Array(), osg::Array::BIND_PER_VERTEX);
  geom->setColorArray(_lineColors);
  geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, 0, 0));
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
  (*_lineColors)[0] = osg::Vec4(r, g, b, 1.0);
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

void CurveArtist::dataCleared(Trajectory* traj)
{
	verifyData();
  CurveArtistUpdateCallback *cb = static_cast<CurveArtistUpdateCallback*>(getUpdateCallback());
  cb->dataCleared();
}

void CurveArtist::dataAdded(Trajectory* traj)
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
