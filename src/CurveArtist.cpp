/***********************************
   Copyright 2017 Ravishankar Mathur

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

#include <OpenFrames/CurveArtist.hpp>
#include <climits>

namespace OpenFrames
{

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

  	// Set line width and pattern. Actual values will be specified later
	_lineWidth = new osg::LineWidth;
	_linePattern = new osg::LineStipple;
	osg::StateSet* stateset = getOrCreateStateSet();
	stateset->setAttribute(_lineWidth.get());
	stateset->setAttributeAndModes(_linePattern.get());
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

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();
}

bool CurveArtist::setXData(const Trajectory::DataSource &src)
{
	if(_dataSource[0] == src) return _dataValid;

	_dataSource[0] = src; // Set new source
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool CurveArtist::setYData(const Trajectory::DataSource &src)
{
	if(_dataSource[1] == src) return _dataValid;

	_dataSource[1] = src; // Set new source
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool CurveArtist::setZData(const Trajectory::DataSource &src)
{
	if(_dataSource[2] == src) return _dataValid;

	_dataSource[2] = src; // Set new source
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

void CurveArtist::setColor( float r, float g, float b)
{
	_lineColor[0] = r;
	_lineColor[1] = g;
	_lineColor[2] = b;

	// Indicate that the trajectory has changed
	dirtyDisplayList();
}

void CurveArtist::setWidth( float width )
{
	if(width > 0.0)
	{
	  _lineWidth->setWidth(width);

	  // Indicate that the trajectory has changed
	  dirtyDisplayList();
	}
}

void CurveArtist::setPattern( GLint factor, GLushort pattern )
{
	_linePattern->setFactor(factor);
	_linePattern->setPattern(pattern);

	// Indicate that the trajectory has changed
	dirtyDisplayList();
}

void CurveArtist::drawImplementation(osg::RenderInfo& renderInfo) const
{
	// Make sure trajectory's data is valid
	if(!_dataValid || _dataZero) return;

        unsigned int numPoints; // Number of points to draw
        _traj->lockData();

        // Make sure there are at least 2 drawable points
        numPoints = _traj->getNumPoints(_dataSource);
        if(numPoints < 2 || numPoints == UINT_MAX)
        {
          _traj->unlockData();

          return;
        }

        osg::GLExtensions *glext = renderInfo.getState()->get<osg::GLExtensions>();
        osg::Vec3d currPoint;  // Coordinates to plot

        // Set the drawing color; width & pattern are already set by the State
        glColor3fv(_lineColor); // One color for all lines

	glBegin(GL_LINE_STRIP); // Begin plotting line strips

        // Iterate through each point to be drawn
	for(unsigned int i = 0; i < numPoints; ++i)
	{
          _traj->getPoint(i, _dataSource, currPoint._v); // Get current point
          RTE_glVertex(currPoint, *glext);
	}

	glEnd(); // GL_LINE_STRIP

	_traj->unlockData();
}

void CurveArtist::dataCleared(Trajectory* traj)
{
	verifyData();
	dirtyBound();
	dirtyDisplayList();
}

void CurveArtist::dataAdded(Trajectory* traj)
{
	// Indicate that the trajectory has changed
	dirtyBound();
	dirtyDisplayList();
}

osg::BoundingBox CurveArtist::computeBoundingBox() const
{
	// Set up bounding box
	_boundingBox.init();

	if(_dataZero)
	{
	  _boundingBox.expandBy(0, 0, 0);
	}
	else if(_dataValid)
	{
	  _traj->lockData();

	  unsigned int maxPoints = _traj->getNumPoints(_dataSource);
	  if(maxPoints == UINT_MAX) maxPoints = 1;

	  Trajectory::DataType point[3];

	  // Iterate through each point and expand the bounding box to encompass it
	  for(unsigned int i = 0; i < maxPoints; ++i)
	  {
	    _traj->getPoint(i, _dataSource, point);
	    _boundingBox.expandBy(point[0], point[1], point[2]);
	  }

	  _traj->unlockData();
	}

	return _boundingBox;
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
