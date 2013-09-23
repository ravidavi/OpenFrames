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

#include <OpenFrames/FrameTransform>
#include <osgUtil/CullVisitor>
#include <iostream>

namespace OpenFrames
{

FrameTransform::FrameTransform()
{
	reset();
}

FrameTransform::~FrameTransform() {}

void FrameTransform::reset()
{
	_disabled = false;
	_followEye = false;
	setPosition(0.0, 0.0, 0.0);
	setAttitude(0.0, 0.0, 0.0, 1.0);
	setScale(1.0, 1.0, 1.0);
	setPivot(0.0, 0.0, 0.0);
}

void FrameTransform::setPosition(const double &x, const double &y, const double &z)
{
	_position[0] = x;
	_position[1] = y;
	_position[2] = z;
	dirtyBound();
}

void FrameTransform::getPosition(double &x, double &y, double &z) const
{
	if(_disabled) x = y = z = 0.0;
	else
	{
	  x = _position[0];
	  y = _position[1];
	  z = _position[2];
	}
}

void FrameTransform::setAttitude(const double &rx, const double &ry,
				const double &rz, const double &angle)
{
	_attitude._v[0] = rx;
	_attitude._v[1] = ry;
	_attitude._v[2] = rz;
	_attitude._v[3] = angle;
	dirtyBound();
}

void FrameTransform::getAttitude(double &rx, double &ry, double &rz, double &angle) const
{
	if(_disabled)
	{
	  rx = ry = rz = 0.0;
	  angle = 1.0;
	}
	else
	{
	  rx = _attitude._v[0];
	  ry = _attitude._v[1];
	  rz = _attitude._v[2];
	  angle = _attitude._v[3];
	}
}

void FrameTransform::setScale(const double &sx, const double &sy, const double &sz)
{
	_scale[0] = sx;
	_scale[1] = sy;
	_scale[2] = sz;
	dirtyBound();
}

void FrameTransform::getScale(double &sx, double &sy, double &sz)
{
	if(_disabled) sx = sy = sz = 1.0;
	else
	{
	  sx = _scale[0];
	  sy = _scale[1];
	  sz = _scale[2];
	}
}

void FrameTransform::setPivot(const double &px, const double &py, const double &pz)
{
	_pivot[0] = px;
	_pivot[1] = py;
	_pivot[2] = pz;
	dirtyBound();
}

void FrameTransform::getPivot(double &px, double &py, double &pz)
{
	if(_disabled) px = py = pz = 1.0;
	else
	{
	  px = _pivot[0];
	  py = _pivot[1];
	  pz = _pivot[2];
	}
}

/** Compute the matrix that will transform a point in the local frame to a
    point in the world frame. Given a transform consisting of a translation,
    rotation, scale, and pivot, the point is first translated wrt the pivot,
    then scaled in the local frame, then rotated to the world frame, then 
    translated in the world frame. 

    Here, "matrix" is a transform from the parent frame to the world frame,
    so we only need to add (premultiply) the local transformations to it.
*/
bool FrameTransform::computeLocalToWorldMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const
{
	if(_disabled) return false; // Don't do anything

	// Compute the transform relative to the parent node
	if(_referenceFrame == RELATIVE_RF)
	{
	  // If we are following the user's eye (ie for a Sky Sphere), then
	  // first translate for that offset.
	  if(_followEye)
	  {
	    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	    if(cv)
	    {
	      osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();
	      //osg::Vec3d eye = cv->getEyeLocal();
	      matrix.preMultTranslate(eye);
	    }
	  }

	  // "matrix" is the world matrix (from parent to world frame)
	  matrix.preMultTranslate(_position);
	  matrix.preMultRotate(_attitude);
	  matrix.preMultScale(_scale);
	  matrix.preMultTranslate(-_pivot);
	}
	else // ABSOLUTE_RF
	{
	  matrix.makeRotate(_attitude);
	  matrix.postMultTranslate(_position);
	  matrix.preMultScale(_scale);
	  matrix.preMultTranslate(-_pivot);
	}

	return true;
}

/** Compute the matrix that transforms a point in the world frame to a point
    in the local frame. Transforms are applied in the opposite order as in
    the computeLocalToWorldMatrix() function. 

    Here, "matrix" is a transform from the world to the parent frame, so we
    only need to add (postmultiply) local transforms to this.
*/
bool FrameTransform::computeWorldToLocalMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const
{
	if(_disabled) return false; // Don't do anything

	// Any zero scale leads to a singularity in the matrix
	if(_scale[0] == 0.0 || _scale[1] == 0.0 || _scale[2] == 0.0)
	  return false;

	if(_referenceFrame == RELATIVE_RF)
	{
	  if(_followEye)
	  {
	    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	    if(cv)
	    {
	      osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();
	      //osg::Vec3d eye = cv->getEyeLocal();
	      matrix.postMultTranslate(-eye);
	    }
	  }
	  
	  // "matrix" is the local matrix (from world to parent frame)
	  matrix.postMultTranslate(-_position);
	  matrix.postMultRotate(_attitude.inverse());
	  matrix.postMultScale(osg::Vec3d(1.0/_scale[0], 1.0/_scale[1], 1.0/_scale[2]));
	  matrix.postMultTranslate(_pivot);
	}
	else
	{
	  matrix.makeRotate(_attitude.inverse());
	  matrix.preMultTranslate(-_position);
	  matrix.postMultScale(osg::Vec3d(1.0/_scale[0], 1.0/_scale[1], 1.0/_scale[2]));
	  matrix.postMultTranslate(_pivot);
	}

	return true;
}

TrajectoryFollower::TrajectoryFollower(Trajectory *traj)
{
	setFollowTrajectory(traj);

	unsigned int dof = _follow.valid()?_follow->getDOF():0;
	Trajectory::DataSource dataPoint;

	if(dof >= 1) 
	{
	  dataPoint._src = Trajectory::POSOPT;
	  dataPoint._element = 0;
	}
	else dataPoint._src = Trajectory::ZERO;
	setXData(dataPoint);

	if(dof >= 2)
	{
	  dataPoint._src = Trajectory::POSOPT;
	  dataPoint._element = 1;
	}
	else dataPoint._src = Trajectory::ZERO;
	setYData(dataPoint);

	if(dof >= 3)
	{
	  dataPoint._src = Trajectory::POSOPT;
	  dataPoint._element = 2;
	}
	else dataPoint._src = Trajectory::ZERO;
	setZData(dataPoint);

	_mode = LOOP;
	_data = POSITION + ATTITUDE;

	_offsetTime = 0.0;
	_timeScale = 1.0;
	_paused = false;
	_needsUpdate = false;
	_deltaTime = _pauseTime = 0.0;
	_latestTime = DBL_MAX;
}

TrajectoryFollower::~TrajectoryFollower() {}

void TrajectoryFollower::setFollowTrajectory(Trajectory *traj)
{
	if(_follow == traj) return; // Already following specified trajectory
	_dataValid = traj?traj->verifyData(_dataSource):false;
	_follow = traj;
	_needsUpdate = true;
}

bool TrajectoryFollower::setXData(const Trajectory::DataSource &src)
{
	if(_dataSource[0] == src) return _dataValid; // No changes to be made

	_dataSource[0] = src; // Set new source
	_dataValid = _follow.valid()?_follow->verifyData(_dataSource):false;
	_needsUpdate = true;

	return _dataValid;
}

bool TrajectoryFollower::setYData(const Trajectory::DataSource &src)
{
	if(_dataSource[1] == src) return _dataValid;

	_dataSource[1] = src; // Set new source
	_dataValid = _follow.valid()?_follow->verifyData(_dataSource):false;
	_needsUpdate = true;

	return _dataValid;
}

bool TrajectoryFollower::setZData(const Trajectory::DataSource &src)
{
	if(_dataSource[2] == src) return _dataValid;

	_dataSource[2] = src; // Set new source
	_dataValid = _follow.valid()?_follow->verifyData(_dataSource):false;
	_needsUpdate = true;

	return _dataValid;
}

void TrajectoryFollower::setTimeScale(double timeScale)
{
	if(_timeScale != timeScale)
	{
	  // Compute new time offset to account for change of time scale
	  if(_paused) _deltaTime += _pauseTime*(_timeScale - timeScale);
	  else _deltaTime += _latestTime*(_timeScale - timeScale);

	  _timeScale = timeScale;
	}

	_needsUpdate = true;
}

void TrajectoryFollower::setPaused(bool pause)
{
	if(_paused != pause)
	{
	  _paused = pause;

	  if(_paused) _pauseTime = _latestTime;
	  else _deltaTime += _timeScale*(_pauseTime - _latestTime);
	}

	_needsUpdate = true;
}

void TrajectoryFollower::setOffsetTime(double offsetTime)
{
	_offsetTime = offsetTime;
	_needsUpdate = true;
}

void TrajectoryFollower::reset()
{
	// Reset parameters such that the newly computed time will be the user specified time offset
	_deltaTime = -_timeScale*_latestTime;
	_pauseTime = _latestTime;
	_needsUpdate = true;
}

void TrajectoryFollower::operator()(osg::Node *node, osg::NodeVisitor *nv)
{
	if(!_follow.valid()) return; // Make sure trajectory is defined

	double refTime = nv->getFrameStamp()->getReferenceTime();

	if(_latestTime != refTime)
	{ 
	  if(_latestTime == DBL_MAX) // First call, so we need to initialize variables
	  {
	    _latestTime = refTime; // Store the current time
	    reset(); // Initialize times
	  }
	  else _latestTime = refTime; // Just store the current time

	  if(!_paused || _needsUpdate)
	  {
	    // Compute current simulation time as t = _offset + _delta + _tscale*_time
	    double time = _offsetTime + _deltaTime;
	    if(_paused) time += _timeScale*_pauseTime;
	    else time += _timeScale*_latestTime;

	    // Prevent trajectory data from being modified while we are
	    // using it in computations.
	    _follow->lockData();

	    // Apply new position/attitude to the FrameTransform
	    FrameTransform *ft = static_cast<FrameTransform*>(node);

	    if(_dataValid && (_data & POSITION)) 
	    {
	      _updatePosition(time);
	      ft->setPosition(_v1[0], _v1[1], _v1[2]);
	    }

	    if(_data & ATTITUDE) 
	    {
	      _updateAttitude(time);
	      ft->setAttitude(_a1[0], _a1[1], _a1[2], _a1[3]);
	    }

	    _follow->unlockData(); // Free up the trajectory data

	    _needsUpdate = false; // Reset update flag
	  }
	}

	// Call nested callbacks and traverse rest of scene graph
	osg::NodeCallback::traverse(node, nv);
}

void TrajectoryFollower::_updatePosition(double time)
{
	int val, index = 0;
	bool dointerp = false;

	// Get list of times for the followed Trajectory
	const Trajectory::DataArray& times = _follow->getTimeList();

	// Number of position points supported by Trajectory
	unsigned int numPoints = _follow->getNumPoints(_dataSource);
	if(numPoints == UINT_MAX) numPoints = 1;
	else if(numPoints == 0)
	{
	  _v1.set(0.0, 0.0, 0.0);
	  return;
	}

	// Find requested time in the Trajectory
	val = _follow->getTimeIndex(time, index);

	if(val >= 0) dointerp = true; // Time not out of range
	else if(val == -1) // Time out of range
	{
	  if(_mode == LIMIT) // Set frame to either end of the trajectory
	  {
	    if(index < 0) // Requested time is before times in trajectory
	    {
	      _follow->getPoint(0, _dataSource, _v1._v);
	    }
	    else // Requested time is after times in trajectory
	    {
	      _follow->getPoint(numPoints-1, _dataSource, _v1._v);
	    }
	  }
	  else if(_mode == LOOP) // Loop indefinitely
	  {
	    double near, far;

	    // Determine which side of the trajectory's times we are on
	    if(index < 0) { near = times[0]; far = times[numPoints-1]; }
	    else { near = times[numPoints-1]; far = times[0]; }

	    // All points are at the same time, so just use the first point
	    if(fabs(near-far) <= 1.0e-12)
	    {
	      _follow->getPoint(0, _dataSource, _v1._v);
	    }
	    else // Shift time such that it lies within range of the trajectory
	    {
	      time = time + ((int)ceil(fabs((near-time)/(far-near))))*(far-near);
	      val = _follow->getTimeIndex(time, index);
	      if(val >= 0) dointerp = true; // Interpolate time
	      else if(val == -1) 
	      {
		std::cerr<< "FrameTransform::_updatePosition() error: Shifted time is still not within Trajectory's range!" << std::endl;
	      }
	      else if(val == -2)
	      {
		std::cerr<< "FrameTransform::_updatePosition() error: Shifted time cannot be found within a finite number of iterations!" << std::endl;
	      }
	      else
	      {
		std::cerr<< "FrameTransform::_updatePosition() error: Unhandled return value after shifting time!" << std::endl;
	      }
	    }
	  }
	}
	else if(val == -2) // Error in search (endless iterations)
	{
	  std::cerr<< "FrameTransform::_updatePosition() error: Requested time not found in a reasonable number of iterations!" << std::endl;
	}
	else // Unhandled return value from getTimeIndex()
	{
	  std::cerr<< "FrameTransform::_updatePosition() error: Unhandled return value!" << std::endl;
	}

	// Interpolate to find position at adjusted time
	if(dointerp && (index < (int)numPoints))
	{
	  // Get the first position used for interpolation
	  _follow->getPoint(index, _dataSource, _v1._v);

	  // Interpolate only if requested time is fully inside time range and
	  // the two interpolation times are not equal.
	  if((index+1 < (int)numPoints) && (times[index] != times[index+1]))
	  {
	    double frac = (time - times[index])/(times[index+1] - times[index]);

	    // Get the second position used for interpolation
	    _follow->getPoint(index+1, _dataSource, _v2._v);
	    _v1 = _v1 + (_v2-_v1)*frac; // Linear interpolation
	  }
	}
}

void TrajectoryFollower::_updateAttitude(double time)
{
	int val, index = 0;
	bool dointerp = false;

	// Get list of times for the followed Trajectory
	const Trajectory::DataArray& times = _follow->getTimeList();

	// Number of attitude points supported by Trajectory
	unsigned int numPoints = _follow->getNumAtt();
	if(numPoints == 0)
	{
	  _a1.set(0.0, 0.0, 0.0, 1.0);
	  return;
	}

	// Find requested time in the Trajectory
	val = _follow->getTimeIndex(time, index);

	if(val >= 0) dointerp = true; // Time not out of range
	else if(val == -1) // Time out of range
	{
	  if(_mode == LIMIT) // Set frame to either end of the trajectory
	  {
	    if(index < 0) // Requested time is before times in trajectory
	    {
	      _follow->getAttitude(0, _a1[0], _a1[1], _a1[2], _a1[3]);
	    }
	    else // Requested time is after times in trajectory
	    {
	      _follow->getAttitude(numPoints-1, _a1[0], _a1[1], _a1[2], _a1[3]);
	    }
	  }
	  else if(_mode == LOOP) // Loop indefinitely
	  {
	    double near, far;

	    // Determine which side of the trajectory's times we are on
	    if(index < 0) { near = times[0]; far = times[numPoints-1]; }
	    else { near = times[numPoints-1]; far = times[0]; }

	    // All points are at the same time, so just use the first point
	    if(fabs(near-far) <= 1.0e-12)
	    {
	      _follow->getAttitude(0, _a1[0], _a1[1], _a1[2], _a1[3]);
	    }
	    else // Shift time such that it lies within range of the trajectory
	    {
	      time = time + ((int)ceil(fabs((near-time)/(far-near))))*(far-near);
	      val = _follow->getTimeIndex(time, index);
	      if(val >= 0) dointerp = true; // Interpolate time
	      else if(val == -1) 
	      {
		std::cerr<< "FrameTransform::_updateAttitude() error: Shifted time is still not within Trajectory's range!" << std::endl;
	      }
	      else if(val == -2)
	      {
		std::cerr<< "FrameTransform::_updateAttitude() error: Shifted time cannot be found within a finite number of iterations!" << std::endl;
	      }
	      else
	      {
		std::cerr<< "FrameTransform::_updateAttitude() error: Unhandled return value after shifting time!" << std::endl;
	      }
	    }
	  }
	}
	else if(val == -2) // Error in search (endless iterations)
	{
	  std::cerr<< "FrameTransform::_updateAttitude() error: Requested time not found in a reasonable number of iterations!" << std::endl;
	}
	else // Unhandled return value from getTimeIndex()
	{
	  std::cerr<< "FrameTransform::_updateAttitude() error: Unhandled return value!" << std::endl;
	}

	// Interpolate to find attitude at adjusted time
	if(dointerp && (index < (int)numPoints))
	{
	  // Get the first attitude used for interpolation
	  _follow->getAttitude(index, _a1[0], _a1[1], _a1[2], _a1[3]);

	  // Interpolate only if requested time is fully inside time range and
	  // the two interpolation times are not equal.
	  if((index+1 < (int)numPoints) && (times[index] != times[index+1]))
	  {
	    double frac = (time - times[index])/(times[index+1] - times[index]);

	    // Get the second attitude used for interpolation
	    _follow->getAttitude(index+1, _a2[0], _a2[1], _a2[2], _a2[3]);
	    _a1.slerp(frac, _a1, _a2); // Spherical interpolation for attitude
	  }
	}
}

TimeManagementVisitor::TimeManagementVisitor()
{
	setTraversalMode(TRAVERSE_ALL_CHILDREN);

	_pauseState = false;
	_changePauseState = _changeOffsetTime = _changeTimeScale = false;
	_offsetTime = 0.0;
	_timeScale = 1.0;
	_reset = false;
}

TimeManagementVisitor::~TimeManagementVisitor() {}

void TimeManagementVisitor::setPauseState(bool changePauseState, bool pauseState) 
{ 
	_changePauseState = changePauseState;
	_pauseState = pauseState; 
}

void TimeManagementVisitor::setOffsetTime(bool changeOffsetTime, double offsetTime)
{
	_changeOffsetTime = changeOffsetTime;
	_offsetTime = offsetTime;
}

void TimeManagementVisitor::setTimeScale(bool changeTimeScale, double timeScale)
{
	_changeTimeScale = changeTimeScale;
	_timeScale = timeScale;
}

void TimeManagementVisitor::apply(osg::Transform &node)
{
	// Make sure current node is a FrameTransform
	FrameTransform *ft = dynamic_cast<FrameTransform*>(&node);
	if(ft)
	{
	  // Make sure FrameTransform has a TrajectoryFollower callback
	  TrajectoryFollower *tf = dynamic_cast<TrajectoryFollower*>(ft->getUpdateCallback());
	  if(tf) 
	  {
	    if(_changePauseState) tf->setPaused(_pauseState);
	    if(_changeOffsetTime) tf->setOffsetTime(_offsetTime);
	    if(_changeTimeScale) tf->setTimeScale(_timeScale);
	    if(_reset) tf->reset();
	  }
	}

	// Traverse & pause children if needed
	osg::NodeVisitor::traverse(node);
}
} // !namespace OpenFrames
