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

#include <OpenFrames/MarkerArtist>
#include <osg/AlphaFunc>
#include <osg/BlendFunc>
#include <osg/Point>
#include <osg/PointSprite>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <iostream>

namespace OpenFrames
{

/** The AttenuateUpdater tells a MarkerArtist to recompute its marker
    attenuation parameters during the update traversal. */
struct AttenuateUpdater : public osg::Drawable::UpdateCallback
{
	AttenuateUpdater() {}
	AttenuateUpdater(const AttenuateUpdater&,const osg::CopyOp&) {}

	META_Object(OpenFrames, AttenuateUpdater);

	virtual void update(osg::NodeVisitor *nv, osg::Drawable *drawable)
	{
	  MarkerArtist *ma = static_cast<MarkerArtist*>(drawable);
	  ma->computeAttenuation();
	}
};

MarkerArtist::MarkerArtist(const Trajectory *traj)
: _markers(START | END), _intermediateType(DATA), _intermediateSpacing(1.0),
  _intermediateDirection(START), _dataValid(true), _dataZero(true), _shouldAttenuate(false)
{
	setTrajectory(traj); // Set the specified trajectory

	unsigned int dof = 0;
	if(_traj.valid()) dof = _traj->getDOF();

	// By default, the MarkerArtist will draw a point at the origin. If
	// a trajectory is specified, then it will instead attempt to draw
	// the position components of the trajectory.
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

	// Remove the nearly-transparent pixels from the markers
	osg::AlphaFunc* alphaFunc = new osg::AlphaFunc;
	alphaFunc->setFunction(osg::AlphaFunc::GEQUAL, 0.05f);
	getOrCreateStateSet()->setAttributeAndModes(alphaFunc);
	
	// Add the Point parameter to the marker
	getOrCreateStateSet()->setAttribute(new osg::Point);

	setMarkerColor(_markers, 1, 0, 0);
	setMarkerSize(10);
	
	// Set the auto point size attenuation
	setAutoAttenuate(false);
}

// Not using the copy constructor
MarkerArtist::MarkerArtist( const MarkerArtist &ca, const osg::CopyOp& copyop )
{}

MarkerArtist::~MarkerArtist()
{}

void MarkerArtist::setTrajectory(const Trajectory *traj)
{
	// Do nothing if this artist is already drawing the given Trajectory
	if(_traj == traj) return;

	// Handle default behavior
	TrajectoryArtist::setTrajectory(traj);

	verifyData(); // Verify whether the trajectory data is valid

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();
}

bool MarkerArtist::setXData(const Trajectory::DataSource &src)
{
	if(_dataSource[0] == src) return _dataValid;

	_dataSource[0] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool MarkerArtist::setYData(const Trajectory::DataSource &src)
{
	if(_dataSource[1] == src) return _dataValid;

	_dataSource[1] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

bool MarkerArtist::setZData(const Trajectory::DataSource &src)
{
	if(_dataSource[2] == src) return _dataValid;

	_dataSource[2] = src;
	verifyData();

	// Indicate that the trajectory has changed and should be redrawn
	dirtyBound();
	dirtyDisplayList();

	return _dataValid;
}

void MarkerArtist::setMarkers(unsigned int markers)
{
	if(_markers == markers) return;

	_markers = markers;
	dirtyBound();
	dirtyDisplayList();
}

void MarkerArtist::setMarkerColor(unsigned int markers,
                                  float r, float g, float b)
{
	if(markers & START)
	{
	  _startColor[0] = r;
	  _startColor[1] = g;
	  _startColor[2] = b;
	}

	if(markers & END)
	{
	  _endColor[0] = r;
	  _endColor[1] = g;
	  _endColor[2] = b;
	}

	if(markers & INTERMEDIATE)
	{
	  _intermediateColor[0] = r;
	  _intermediateColor[1] = g;
	  _intermediateColor[2] = b;
	}

	if(markers)
	{
	  dirtyDisplayList();
	}
}

void MarkerArtist::setMarkerSize(unsigned int size)
{
	if(size > 0)
	{
	  osg::Point *point = static_cast<osg::Point*>(getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	  point->setSize(size);
	  point->setMaxSize(size);
	}
}

bool MarkerArtist::setMarkerImage(const std::string &fname, bool force_reload)
{
	osg::StateSet *ss = getOrCreateStateSet();

	if(fname.length() == 0) // Use default OpenGL point as marker
	{
	  ss->removeAttribute(osg::StateAttribute::BLENDFUNC);
	  ss->removeTextureAttribute(0, osg::StateAttribute::POINTSPRITE);
	  ss->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
	  return true;
	}

	// Check if there is already a texture being used.
	osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(ss->getTextureAttribute(0, osg::StateAttribute::TEXTURE));

	// If the current texture has the same filename as the new texture, then reload only if we have to.
	if(texture && (texture->getImage()->getFileName() == fname) && !force_reload) return true;

	osg::Image *image = osgDB::readImageFile(fname); // Load image from file
	if(image)
	{
	  // Set up blending so marker looks nice
	  osg::BlendFunc *fn = new osg::BlendFunc();
	  fn->setFunction(osg::BlendFunc::SRC_ALPHA, 
	                  osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
	  ss->setAttributeAndModes(fn);

	  // Set up point sprite
	  osg::PointSprite *sprite = new osg::PointSprite();
	  ss->setTextureAttributeAndModes(0, sprite);

	  // Specify texture to use for point sprite
	  osg::Texture2D *tex = new osg::Texture2D();
	  tex->setImage(image);
	  ss->setTextureAttributeAndModes(0, tex);

	  return true;
	}
	else
	{
	  std::cerr<< "MarkerArtist ERROR: Image file \'" << fname << "\' could not be found!" << std::endl;
	  return false; // Image was not found
	}
}

void MarkerArtist::setAutoAttenuate(bool attenuate)
{
	if(attenuate)
	{
	  if(osg::Drawable::getUpdateCallback() == NULL) osg::Drawable::setUpdateCallback(new AttenuateUpdater);
	}
	else osg::Drawable::setUpdateCallback(NULL);

	_shouldAttenuate = true;
}

bool MarkerArtist::getAutoAttenuate() const
{
  return (osg::Drawable::getUpdateCallback() != NULL);
}

void MarkerArtist::setIntermediateType(IntermediateType type)
{
	if(_intermediateType != type)
	{
	  _intermediateType = type;
	  dirtyBound();
	  dirtyDisplayList();
	}
}

void MarkerArtist::setIntermediateSpacing(double spacing)
{
	if((_intermediateSpacing != spacing) && (spacing > 0.0)) 
	{
	  _intermediateSpacing = spacing;
	  dirtyBound();
	  dirtyDisplayList();
	}
}

void MarkerArtist::setIntermediateDirection(DrawnMarkers direction)
{
	if(_intermediateDirection != direction)
	{
	  _intermediateDirection = direction;
	  dirtyBound();
	  dirtyDisplayList();
	}
}

void MarkerArtist::drawImplementation(osg::RenderInfo& renderInfo) const
{
	// Make sure data to be drawn is valid
	if(!_dataValid) return;

	unsigned int numPoints; // Number of drawable points
	if(_dataZero)
	{
	  numPoints = 1;
	}
	else
	{
	  _traj->lockData();

	  // Compute number of drawable points
	  numPoints = _traj->getNumPoints(_dataSource);

	  if(numPoints == 0) // No points to draw
	  {
	    _traj->unlockData();
	    return;
	  }
	}

        osg::GLExtensions *glext = renderInfo.getState()->get<osg::GLExtensions>();
	osg::Vec3d currPoint, prevPoint;  // Coordinates to plot

	glBegin(GL_POINTS); // Begin plotting points

	// Draw first point if requested
	if(_markers & START)
	{
	  glColor3fv(_startColor);
	  if(_dataZero) currPoint.set(0, 0, 0);
	  else _traj->getPoint(0, _dataSource, currPoint._v);
	  RTE_glVertex(currPoint, *glext);
	}

	// Draw intermediate points
	if((_markers & INTERMEDIATE) && (numPoints > 2))
	{
	  glColor3fv(_intermediateColor);

	  // Draw points at specified time increments
	  if(_intermediateType == TIME)
	  {
	    // Get the start & end times of the trajectory, depending on
	    // whether we want to plot points from beginning or from end
	    // of the trajectory
	    double start, end;
	    if(_intermediateDirection == START)
	    {
	      _traj->getTimeRange(start, end);
	    }
	    else if(_intermediateDirection == END)
	    {
	      _traj->getTimeRange(end, start);
	    }

	    // Determine whether times are increasing or decreasing
	    int direction = (start <= end)?1:-1;
	    double spacing = direction*_intermediateSpacing;
	    
	    int index; // Used to find index of data to plot

	    for(double t = start + spacing; direction*t < direction*end; t += spacing)
	    {
	      // Get the lower bounding index for the current time
	      _traj->getTimeIndex(t, index);

	      // Compute the distance between the lower & upper bounding times
	      double t_a = _traj->getTimeList()[index];
	      double t_b = _traj->getTimeList()[index+1];

	      // Make sure bounding times are not equal
	      if(t_a == t_b)
	      {
		// Get the data point at the lower time
		_traj->getPoint(index, _dataSource, currPoint._v);
	      }
	      else 
	      {
		double frac = (t - t_a)/(t_b - t_a);

		// Get the data points at the lower and upper times
		_traj->getPoint(index, _dataSource, prevPoint._v);
		_traj->getPoint(index+1, _dataSource, currPoint._v);

		// Extrapolate the data point to be plotted
		currPoint = prevPoint + (currPoint - prevPoint)*frac;
	      }

	      RTE_glVertex(currPoint, *glext); // Plot the interpolated point
	    }
	  }

	  // Draw points at specified distances between points. The distance
	  // here is defined by the arc-length between the two points.
	  else if(_intermediateType == DISTANCE)
	  {
	    double curr_d = 0.0; // Current distance along trajectory
	    double prev_d = 0.0; // Previous distance along trajectory
	    double target_d = _intermediateSpacing; // Target distance
	    double frac;
	    int start, end, direction;

	    // Determine start & end points depending on whether we want to
	    // plot intermediate points from start or end of trajectory.
	    if(_intermediateDirection == START)
	    {
	      start = 0;
	      end = numPoints;
	      direction = 1;
	    }
	    else if(_intermediateDirection == END)
	    {
	      start = numPoints-1;
	      end = -1;
	      direction = -1;
	    }

	    // Iterate over every point, integrating the arc length by
	    // assuming a straight line between points. Draw a marker each
	    // time the arc length reaches the appropriate spacing distance.
	    // Note that if the trajectory's total arc length is less than
	    // the desired marker spacing, then no markers will be drawn.
	    _traj->getPoint(start, _dataSource, prevPoint._v);
	    for(int i = start + direction; i != end; i += direction)
	    {
	      // Get the current point
	      _traj->getPoint(i, _dataSource, currPoint._v);

	      // Add the incremental length from the previous point
	      curr_d += (currPoint - prevPoint).length();

	      // Check if we didn't add any length to the arc, eg if 2
	      // adjacent points in the trajectory are the same.
	      if(curr_d == prev_d) continue;

	      // If we have reached (or overshot) the target distance, then
	      // start plotting points.
	      while(target_d <= curr_d)
	      {
		// Don't plot the last point in the trajectory, since this is
		// handled by the DrawnMarkers::END case.
		if((i == numPoints-1) && (target_d == curr_d)) break;

		frac = (target_d - prev_d)/(curr_d - prev_d);

		// Extrapolate the data point to be plotted
                osg::Vec3d temp = prevPoint + (currPoint - prevPoint)*frac;
                RTE_glVertex(temp, *glext);

		target_d += _intermediateSpacing;
	      }

	      // Prepare for next iteration
	      prev_d = curr_d;
	      prevPoint = currPoint;
	    }
	  }

	  // Draw points at specified data intervals
	  else if(_intermediateType == DATA)
	  {
	    int spacing, start, end, direction;

	    // Make sure requested spacing is valid
	    spacing = (int)_intermediateSpacing;
	    if(spacing == 0) spacing = 1;

	    // Determine start & end points depending on whether we want to
	    // plot intermediate points from start or end of trajectory.
	    if(_intermediateDirection == START)
	    {
	      start = spacing;
	      end = numPoints-1;
	      direction = 1;
	    }
	    else if(_intermediateDirection == END)
	    {
	      start = (numPoints-1) - spacing;
	      end = 0;
	      direction = -1;
	      spacing = -spacing;
	    }

	    // Iterate over the data points, skipping the first one and
	    // stopping before the last one
	    for(int i = start; direction*i < end; i += spacing)
	    {
	      _traj->getPoint(i, _dataSource, currPoint._v);
	      RTE_glVertex(currPoint, *glext);
	    }
	  }
	}

	// Draw last point if requested and available
	if((_markers & END) && (numPoints > 1))
	{
	  glColor3fv(_endColor);
	  _traj->getPoint(numPoints-1, _dataSource, currPoint._v);
	  RTE_glVertex(currPoint, *glext);
	}

	glEnd(); // GL_POINTS

	if(!_dataZero) _traj->unlockData();
}

void MarkerArtist::dataCleared()
{
	verifyData();
	dirtyBound();
	dirtyDisplayList();
}

void MarkerArtist::dataAdded()
{
	dirtyBound();
	dirtyDisplayList();
}

void MarkerArtist::computeAttenuation()
{
	// Make sure we need to recompute the attenuation parameters
	if(!_shouldAttenuate) return;

	// The computed point size is based on the specified point size and
	// the attenuation parameters a, b, c. See glPointParameter with the
	// parameter of GL_POINT_DISTANCE_ATTENUATION for more info.
	// The equation is: 
	//   computed size = clamp(specified size * sqrt(1/(a + b*d + c*d*d)))
	//   where d is the eye-distance to the point
	// Here we compute attenuation parameters such that the tick marks
	// will be visible at distances proportional to the size of the box
	// encompassing all of the markers.
	float a, b, c; // Coefficients for attenuation

	if(_boundingBox.valid())
	{
	  // Get size of the bounding box
	  double length = (_boundingBox._max - _boundingBox._min).length();

	  // If size is too small, then don't auto-attenuate
	  if(length < 1.0e-8)
	  {
	    a = 1.0;
	    b = c = 0.0;
	  }
	  else // Otherwise auto-attenuate normally
	  {
	    a = c = 0.0;
	    b = 1.0/length;
	  }
	}
	else
	{
	  a = 1.0;
	  b = c = 0.0;
	}

	osg::Vec3 attenuation(a, b, c);

	// Get the Point parameter for the major tick marks
	osg::Point *point = static_cast<osg::Point*>(getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	point->setDistanceAttenuation(attenuation);

	_shouldAttenuate = false; // Parameters computed
}

/** Compute the bounding box that encompasses all of the markers */
osg::BoundingBox MarkerArtist::computeBoundingBox() const
{
	// Set up bounding box
	_boundingBox.init();

	if(_dataZero) // Just the zero point
	{
          _boundingBox.expandBy(0.0, 0.0, 0.0);
	}
	else if(_dataValid)
	{
	  _traj->lockData(); // Make sure data doesn't change while in use

	  // Get the maximum number of points supported by the trajectory
	  // with the required data sources
	  unsigned int maxPoints = _traj->getNumPoints(_dataSource);
	  if(maxPoints == UINT_MAX) maxPoints = 1;

	  Trajectory::DataType point[3];

	  if(maxPoints != 0)
	  {
	    // If intermediate markers are not drawn, then just encompass the
	    // start & end markers
	    if(!(_markers & INTERMEDIATE))
	    {
	      if(_markers & START)
	      {
		_traj->getPoint(0, _dataSource, point); // Get first point
		_boundingBox.expandBy(point[0], point[1], point[2]);
	      }

	      if((_markers & END) && (maxPoints > 1))
	      {
		_traj->getPoint(maxPoints-1, _dataSource, point); // Get last point
		_boundingBox.expandBy(point[0], point[1], point[2]);
	      }
	    }

	    // Intermediate markers are drawn, so we *should* check to see
	    // which type of markers are required (TIME, DISTANCE, DATA).
	    // HOWEVER, for ease of computation, we will just create a bounding
	    // box that encompasses the entire trajectory.
	    else
	    {
	      for(unsigned int i = 0; i < maxPoints-1; ++i)
	      {
		_traj->getPoint(i, _dataSource, point);
		_boundingBox.expandBy(point[0], point[1], point[2]);
	      }
	    }
	  }

	  _traj->unlockData();
	}

	// Indiate that we need to recompute attenuation parameters
	_shouldAttenuate = true;

	return _boundingBox;
}

void MarkerArtist::verifyData() const
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
