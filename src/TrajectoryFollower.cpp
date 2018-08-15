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

/** \file TrajectoryFollower.cpp
 * TrajectoryFollower-class function definitions.
 */

#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenThreads/ScopedLock>
#include <osg/NodeVisitor>
#include <climits>
#include <algorithm>

namespace OpenFrames
{

TrajectoryFollower::TrajectoryFollower(Trajectory *traj)
  : _usingDefaultData(true)
{
	setTrajectory(traj);
  
	_mode = LOOP;
	_data = POSITION + ATTITUDE;

  setOffsetTime(0.0);
}

TrajectoryFollower::~TrajectoryFollower()
{
  // Unsubscribe from all followed trajectories
  for(TrajList::iterator i = _trajList.begin(); i != _trajList.end(); ++i)
  {
    (*i)->removeSubscriber(this);
  }
}

void TrajectoryFollower::setTrajectory(Trajectory *traj)
{
  OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mutex);
  
  // Already following specified trajectory
	if((_trajList.size() == 1) && (_trajList[0] == traj)) return;

  // Unsubscribe from all followed trajectories
  for(TrajList::iterator i = _trajList.begin(); i != _trajList.end(); ++i)
  {
    (*i)->removeSubscriber(this);
  }
 
  // Reference new trajectory so that it isn't deleted
  // if it's already being followed
  osg::ref_ptr<Trajectory> tmptraj(traj);
  
  // Clear existing trajectory list and add new trajectory
  _trajList.clear();
  if(traj != NULL)
  {
    _trajList.push_back(traj);
    traj->addSubscriber(this);
  }
  
  // Set default data sources if needed
  if(_usingDefaultData) setDefaultData();
  
  // Check for valid data sources
  else _dataValid = _verifyDataSources();
  
  // Indicate that the follower should update its state
	_needsUpdate = true;
}
  
void TrajectoryFollower::addTrajectory(Trajectory *traj)
{
  if(traj == NULL) return; // Error check

  OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mutex);
  
  // Already following specified trajectory
  if(std::find(_trajList.begin(), _trajList.end(), traj) != _trajList.end()) return;
  
  // Add new trajectory and subscribe to updates
  _trajList.push_back(traj);
  traj->addSubscriber(this);
  
  // Set default data sources if needed
  if(_usingDefaultData) setDefaultData();
  
  // Check for valid data sources
  else _dataValid = _verifyDataSources();
  
  // Indicate that the follower should update its state
  _needsUpdate = true;
}

void TrajectoryFollower::removeTrajectory(Trajectory *traj)
{
  OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mutex);

  // Stop following all trajectories
  if(traj == NULL)
  {
    // Unsubscribe from all followed trajectories
    for(TrajList::iterator i = _trajList.begin(); i != _trajList.end(); ++i)
    {
      (*i)->removeSubscriber(this);
    }
    
    _trajList.clear();
  }
  else // Stop following specified trajectory
  {
    TrajList::iterator i = std::find(_trajList.begin(), _trajList.end(), traj);
    if(i != _trajList.end())
    {
      (*i)->removeSubscriber(this);
      _trajList.erase(i);
    }
  }
  
  // Set default data sources if needed
  if(_usingDefaultData) setDefaultData();
  
  // Check for valid data sources
  else _dataValid = _verifyDataSources();
  
  // Indicate that the follower should update its state
  _needsUpdate = true;
}
  
bool TrajectoryFollower::setXData(const Trajectory::DataSource &src)
{
	if(_dataSource[0] == src) return _dataValid; // No changes to be made

	_dataSource[0] = src; // Set new source
  _dataValid = _verifyDataSources(); // Check data validity
	_needsUpdate = true; // Tell follower to update its state
  _usingDefaultData = false;

	return _dataValid;
}

bool TrajectoryFollower::setYData(const Trajectory::DataSource &src)
{
	if(_dataSource[1] == src) return _dataValid;

	_dataSource[1] = src; // Set new source
  _dataValid = _verifyDataSources(); // Check data validity
  _needsUpdate = true; // Tell follower to update its state
  _usingDefaultData = false;

	return _dataValid;
}

bool TrajectoryFollower::setZData(const Trajectory::DataSource &src)
{
	if(_dataSource[2] == src) return _dataValid;

	_dataSource[2] = src; // Set new source
  _dataValid = _verifyDataSources(); // Check data validity
  _needsUpdate = true; // Tell follower to update its state
  _usingDefaultData = false;

	return _dataValid;
}

void TrajectoryFollower::setDefaultData()
{
  // Get DOF for first followed trajectory
  unsigned int dof = _trajList.empty()?0:_trajList[0]->getDOF();
  Trajectory::DataSource dataPoint;
  
  // Use X data if at least 1 DOF
  if(dof >= 1)
  {
    dataPoint._src = Trajectory::POSOPT;
    dataPoint._element = 0;
  }
  else dataPoint._src = Trajectory::ZERO;
  setXData(dataPoint);
  
  // Use X/Y data if at least 2 DOF
  if(dof >= 2)
  {
    dataPoint._src = Trajectory::POSOPT;
    dataPoint._element = 1;
  }
  else dataPoint._src = Trajectory::ZERO;
  setYData(dataPoint);
  
  // Use X/Y/Z data if at least 3 DOF
  if(dof >= 3)
  {
    dataPoint._src = Trajectory::POSOPT;
    dataPoint._element = 2;
  }
  else dataPoint._src = Trajectory::ZERO;
  setZData(dataPoint);
  
  _usingDefaultData = true;
}

void TrajectoryFollower::setTime(double time)
{
  _timeVal = time;
  _followTime = false;
  _needsUpdate = true;
}
  
void TrajectoryFollower::setOffsetTime(double offsetTime)
{
	_timeVal = offsetTime;
  _followTime = true;
	_needsUpdate = true;
}

bool TrajectoryFollower::run(osg::Object* object, osg::Object* data)
{
  osg::NodeVisitor *nv = data ? data->asNodeVisitor() : 0;
  double simTime = 0.0;
  if(nv) simTime = nv->getFrameStamp()->getSimulationTime();
  
  // Make sure time has changed
  if((_lastSimTime != simTime) || _needsUpdate)
  {
    _lastSimTime = simTime; // Save the current simulation time
    
    // Don't allow followed trajectory list to be modified
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mutex);
    
    // Follow trajectory as needed
    if(!_trajList.empty())
    {
      // Get current time, either constant-time or offset-simulation-time
      double time;
      if(_followTime) time = _lastSimTime + _timeVal;
      else time = _timeVal;
      
      // Prevent trajectories from being modified while reading them
      for(auto traj : _trajList)
      {
        traj->lockData();
      }
      
      // Compute adjusted time based on follow mode
      _lastAdjustedTime = _computeTime(time);
      
      // Choose trajectory based on adjusted time
      _follow = _chooseTrajectory(_lastAdjustedTime);
      
      // Unlock all trajectories except the one being followed
      for(auto traj : _trajList)
      {
        if(traj.get() != _follow.get()) traj->unlockData();
      }
      
      // Apply new position/attitude to the FrameTransform
      FrameTransform *ft = static_cast<FrameTransform*>(object);
      
      if(_dataValid && (_data & POSITION))
      {
        // Apply new position if it can be computed
        if(_updateState(_lastAdjustedTime, POSITION))
          ft->setPosition(_v1[0], _v1[1], _v1[2]);
      }
      
      if(_data & ATTITUDE)
      {
        // Apply new attitude if it can be computed
        if(_updateState(_lastAdjustedTime, ATTITUDE))
          ft->setAttitude(_a1[0], _a1[1], _a1[2], _a1[3]);
      }
      
      _follow->unlockData(); // Unlock followed trajectory
      
      _needsUpdate = false; // Reset update flag
    }
  }
  
  // Call nested callbacks and traverse rest of scene graph
  return osg::Callback::traverse(object, data);
}

double TrajectoryFollower::_computeTime(double time)
{
  // LIMIT mode: don't wrap time
  if(_mode == LIMIT) return time;
  
  // Compute start and end times over all trajectories
  double t0 = DBL_MAX;
  double tf = -DBL_MAX;
  for(auto traj : _trajList)
  {
    // Get current trajectory's start & end time
    double traj_t0, traj_tf;
    if(!traj->getTimeRange(traj_t0, traj_tf)) continue;
    if(traj_t0 > traj_tf) std::swap(traj_t0, traj_tf); // Enforce t0<=tf

    // Update global start & end times
    if(traj_t0 < t0) t0 = traj_t0;
    if(traj_tf > tf) tf = traj_tf;
  }

  // Make sure valid trajectories were found
  if((t0 == DBL_MAX) || (tf == -DBL_MAX)) return time;

  // If [t0, tf] range is too small, then just use t0
  if(tf - t0 <= 8.0*DBL_MIN) return t0;

  // Wrap time to [t0, tf]
  double tnew = time - std::floor((time - t0)/(tf - t0))*(tf - t0);
  return tnew;
}

Trajectory* TrajectoryFollower::_chooseTrajectory(double time)
{
  // If there is only one trajectory in the list, then use it
  if(_trajList.size() == 1) return _trajList[0];

  // If current trajectory contains given time, then continue using it
  if(_follow.valid() && (_follow->getTimeDistance(time) <= 0.0)) return _follow.get();

  // Find first trajectory that contains given time
  double minTimeDistance = DBL_MAX;
  Trajectory* minTimeDistanceTraj = _trajList[0];
  for(auto traj : _trajList)
  {
    // Get distance from given time to current trajectory time range
    double dist = traj->getTimeDistance(time);

    // Use trajectory if it contains given time
    if(dist <= 0.0) return traj;

    // Otherwise save this as a candidate for "closest" trajectory
    else if(dist < minTimeDistance)
    {
      minTimeDistance = dist;
      minTimeDistanceTraj = traj;
    }
  }

  // No trajectories contain given time, so use the closest trajectory
  return minTimeDistanceTraj;
}

bool TrajectoryFollower::_updateState(double time, TrajectoryFollower::FollowData data)
{
  int val, index = 0;

  // Get list of times for the followed Trajectory
  const Trajectory::DataArray& times = _follow->getTimeList();

  // Number of position points supported by Trajectory
  unsigned int numPoints;
  if(data == POSITION)
    numPoints = _follow->getNumPoints(_dataSource);
  else
    numPoints = _follow->getNumAtt();
  
  // Don't update state if trajectory has no points
  if(numPoints == 0) return false;
  
  // Trajectory has UINT_MAX points IFF data is POSITION and source is ZERO
  // In that case set position to origin
  else if(numPoints == UINT_MAX)
  {
    _v1.set(0.0, 0.0, 0.0);
    return true;
  }

  // Find requested time in the Trajectory
  val = _follow->getTimeIndex(time, index);

  if(val >= 0) // Time not out of range, so interpolate
  {
    // Check that time index is less than actual number of points
    if(index < (int)numPoints)
    {
      // Get the first point used for interpolation
      if(data == POSITION)
        _follow->getPoint(index, _dataSource, _v1._v);
      else
        _follow->getAttitude(index, _a1[0], _a1[1], _a1[2], _a1[3]);
      
      // Interpolate if the two times are not equal
      if((index+1 < (int)numPoints) && (times[index] != times[index+1]))
      {
        // Get second interpolation point and do the interpolation
        if(data == POSITION)
        {
          _follow->getPoint(index+1, _dataSource, _v2._v);
          double frac = (time - times[index])/(times[index+1] - times[index]);
          _v1 = _v1 + (_v2-_v1)*frac; // Linear interpolation for position
        }
        else
        {
          _follow->getAttitude(index+1, _a2[0], _a2[1], _a2[2], _a2[3]);
          double frac = (time - times[index])/(times[index+1] - times[index]);
          _a1.slerp(frac, _a1, _a2); // Spherical interpolation for attitude
        }
      }
    }
    else // Otherwise use last available point
    {
      if(data == POSITION)
        _follow->getPoint(numPoints-1, _dataSource, _v1._v);
      else
        _follow->getAttitude(numPoints-1, _a1[0], _a1[1], _a1[2], _a1[3]);
    }
  }
  else if(val == -1) // Time out of range
  {
    if(index < 0) // Requested time before first time
    {
      if(data == POSITION)
        _follow->getPoint(0, _dataSource, _v1._v);
      else
        _follow->getAttitude(0, _a1[0], _a1[1], _a1[2], _a1[3]);
    }
    else // Requested time after last time
    {
      if(data == POSITION)
        _follow->getPoint(numPoints-1, _dataSource, _v1._v);
      else
        _follow->getAttitude(numPoints-1, _a1[0], _a1[1], _a1[2], _a1[3]);
    }
  }
  else if(val == -2) // Error in search (endless iterations)
  {
    OSG_WARN << "TrajectoryFollower::_updateState() error: Requested time not found in a reasonable number of iterations!" << std::endl;
    return false;
  }
  else // Unhandled return value from getTimeIndex()
  {
    OSG_WARN << "TrajectoryFollower::_updateState() error: Unhandled return value!" << std::endl;
    return false;
  }
  
  return true; // State successfully computed
}
  
} // !namespace OpenFrames
