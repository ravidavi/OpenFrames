/***********************************
   Copyright 2013 Ravishankar Mathur

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

#include <OpenFrames/CurveArtist>
#include <climits>

namespace OpenFrames
{

CurveArtist::CurveArtist(const Trajectory *traj)
: _dataValid(false), _dataZero(false)
{
  	// Set line width and pattern. Actual values will be specified later
	_lineWidth = new osg::LineWidth;
	_linePattern = new osg::LineStipple;
	osg::StateSet* stateset = getOrCreateStateSet();
	stateset->setAttribute(_lineWidth.get());
	stateset->setAttributeAndModes(_linePattern.get());

	setTrajectory(traj);

	unsigned int dof = 0;
	if(_traj.valid()) dof = _traj->getDOF();

	Trajectory::DataSource data;

	if(dof >= 1)
	{
	  data._src = Trajectory::POSOPT;
	  data._element = 0;
	}
	else data._src = Trajectory::ZERO;
	setXData(data);

	if(dof >= 2)
	{
	  data._src = Trajectory::POSOPT;
	  data._element = 1;
	}
	else data._src = Trajectory::ZERO;
	setYData(data);

	if(dof >= 3)
	{
	  data._src = Trajectory::POSOPT;
	  data._element = 2;
	}
	else data._src = Trajectory::ZERO;
	setZData(data);

	verifyData();
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
	if(!_dataValid) return;

	renderInfo.getState()->checkGLErrors("start of CurveArtist::drawImplementation");

	unsigned int numPoints; // Number of points to draw
	if(_dataZero)
	{
	  numPoints = 1;
	}
	else
	{
	  _traj->lockData();

	  // Compute number of drawable points
	  numPoints = _traj->getNumPoints(_dataSource);
	  if(numPoints == UINT_MAX) numPoints = 1;
	  else if(numPoints == 0) // Make sure something needs to be drawn
	  {
	    _traj->unlockData();
		renderInfo.getState()->checkGLErrors("end of CurveArtist::drawImplementation with no points");

	    return;
	  }
	}

	osg::Vec3d currPoint;  // Coordinates to plot

	GLint size = (_dataSource[2]._src == Trajectory::ZERO)?2:3; // 2D or 3D points

	// Set the drawing color; width & pattern are already set by the State
	glColor3fv(_lineColor);

	glBegin(GL_LINE_STRIP); // Begin plotting line strips

	  // Iterate through each point to be drawn
	for(unsigned int i = 0; i < numPoints; ++i)
	{
	  if(_dataZero) currPoint.set(0, 0, 0);
	  else _traj->getPoint(i, _dataSource, currPoint._v); // Get current point

	  if(size == 3) // 3D trajectory
	    glVertex3dv(currPoint._v);
	  else // 2D trajectory
	    glVertex2dv(currPoint._v);
	}

	glEnd(); // GL_LINE_STRIP

	if(!_dataZero) _traj->unlockData();

	renderInfo.getState()->checkGLErrors("end of CurveArtist::drawImplementation");

}

void CurveArtist::dataCleared()
{
	verifyData();
	dirtyBound();
	dirtyDisplayList();
}

void CurveArtist::dataAdded()
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

	_boundingBoxComputed = true;
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
