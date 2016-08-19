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

#include <OpenFrames/SegmentArtist.hpp>
#include <climits>
#include <algorithm>

namespace OpenFrames
{

SegmentArtist::SegmentArtist(const Trajectory *traj)
: _stride(1), _dataValid(false), _startDataZero(false), _endDataZero(false)
{
    // Set line width and pattern. Actual values will be specified later
	_lineWidth = new osg::LineWidth;
	_linePattern = new osg::LineStipple;
	osg::StateSet* stateset = getOrCreateStateSet();
	stateset->setAttribute(_lineWidth.get());
	stateset->setAttributeAndModes(_linePattern.get());

	setTrajectory(traj);

	Trajectory::DataSource data; // We can use default values
	setStartXData(data);
	setEndXData(data);
	setStartYData(data);
	setEndYData(data);
	setStartZData(data);
	setEndZData(data);

	verifyData(); // Verify that the data is usable
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

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();
}

bool SegmentArtist::setStartXData(const Trajectory::DataSource &src)
{
	if(_startSource[0] == src) return _dataValid;

	_startSource[0] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool SegmentArtist::setEndXData(const Trajectory::DataSource &src)
{
	if(_endSource[0] == src) return _dataValid;

	_endSource[0] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool SegmentArtist::setStartYData(const Trajectory::DataSource &src)
{
	if(_startSource[1] == src) return _dataValid;

	_startSource[1] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool SegmentArtist::setEndYData(const Trajectory::DataSource &src)
{
	if(_endSource[1] == src) return _dataValid;

	_endSource[1] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool SegmentArtist::setStartZData(const Trajectory::DataSource &src)
{
	if(_startSource[2] == src) return _dataValid;

	_startSource[2] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool SegmentArtist::setEndZData(const Trajectory::DataSource &src)
{
	if(_endSource[2] == src) return _dataValid;

	_endSource[2] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

void SegmentArtist::setStride(unsigned int stride)
{
	if(_stride == stride) return;

	if(stride == 0) _stride = 1;
	else _stride = stride;

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();
}

void SegmentArtist::setColor( float r, float g, float b)
{
	_lineColor[0] = r;
	_lineColor[1] = g;
	_lineColor[2] = b;

	// Indicate that the trajectory has changed
	dirtyDisplayList();
}

void SegmentArtist::setWidth( float width )
{
	if(width > 0.0)
	{
	  _lineWidth->setWidth(width);

	  // Indicate that the trajectory has changed
	  dirtyDisplayList();
	}
}

void SegmentArtist::setPattern( GLint factor, GLushort pattern )
{
	_linePattern->setFactor(factor);
	_linePattern->setPattern(pattern);

	// Indicate that the trajectory has changed
	dirtyDisplayList();
}

void SegmentArtist::drawImplementation(osg::RenderInfo& renderInfo) const
{
	// Make sure trajectory's data is valid
	if(!_dataValid) return;

	unsigned int numSegs;  // Number of line segments to be drawn
	if(_startDataZero && _endDataZero) numSegs = 1;
	else
	{
	  _traj->lockData(); // Prevent Trajectory's data from being moved in memory

	  // Compute number of drawable points
	  numSegs = std::min(_traj->getNumPoints(_startSource),
	                     _traj->getNumPoints(_endSource));
	  if(numSegs == UINT_MAX) numSegs = 1;

	  // Make sure something needs to be drawn
	  if(numSegs == 0) 
	  {
	    _traj->unlockData();
	    return;
	  }
	}

        osg::GLExtensions *glext = renderInfo.getState()->get<osg::GLExtensions>();
	osg::Vec3d startPoint, endPoint;  // Coordinates to plot

	// If starting or ending data point is zero, just set it now
	if(_startDataZero) startPoint.set(0, 0, 0);
	if(_endDataZero) endPoint.set(0, 0, 0);

	// Set the drawing color; width & pattern are already set by the State
	glColor3fv(_lineColor);

	glBegin(GL_LINES); // Begin plotting line strips

	for(unsigned int i = 0; i < numSegs; i += _stride)
	{
	  if(!_startDataZero) // Get start point
	    _traj->getPoint(i, _startSource, startPoint._v);

	  if(!_endDataZero) // Get end point
	    _traj->getPoint(i, _endSource, endPoint._v);

	  // Send start/end points of current segment to OpenGL.
          RTE_glVertex(startPoint, *glext);
          RTE_glVertex(endPoint, *glext);
	}

	glEnd(); // GL_LINES

	// Indicate that we are done reading the data
	if(!(_startDataZero && _endDataZero)) _traj->unlockData();
}

void SegmentArtist::dataCleared()
{
	verifyData();
	dirtyBound();
	dirtyDisplayList();
}

void SegmentArtist::dataAdded()
{
	// Indicate that the trajectory has changed
	dirtyBound();
	dirtyDisplayList();
}

osg::BoundingBox SegmentArtist::computeBoundingBox() const
{
	// Set up bounding box
	_boundingBox.init();

	if(_startDataZero && _endDataZero)
	{
	  _boundingBox.expandBy(0, 0, 0);
	}
	else if(_dataValid)
	{
	  _traj->lockData();

	  Trajectory::DataType point[3];
	  unsigned int maxSegs = std::min(_traj->getNumPoints(_startSource),
	                                  _traj->getNumPoints(_endSource));
	  if(maxSegs == UINT_MAX) maxSegs = 1;

	  // Iterate through each point and expand the bounding box to encompass it.
	  for(unsigned int i = 0; i < maxSegs; ++i)
	  {
	    _traj->getPoint(i, _startSource, point);
	    _boundingBox.expandBy(point[0], point[1], point[2]);

	    _traj->getPoint(i, _endSource, point);
	    _boundingBox.expandBy(point[0], point[1], point[2]);
	  }

	  _traj->unlockData();
	}

	return _boundingBox;
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
