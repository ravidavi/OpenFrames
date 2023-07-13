/***********************************
   Copyright 2023 Ravishankar Mathur

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

/** \file Trajectory.cpp
 * Trajectory-class function definitions.
 */

#include <OpenFrames/Trajectory.hpp>
#include <math.h>
#include <climits>
#include <cfloat>
#include <cstring>
#include <algorithm>
#include <iostream>

namespace OpenFrames {

Trajectory::Trajectory(unsigned int dof, unsigned int nopt )
{
  _autoInformSubscribers = true;
  _dataCleared = true;

	if(dof == 0) dof = 1;
	setDOF(dof);
	setNumOptionals(nopt);
  
  reserveMemory(1024);
}

Trajectory::~Trajectory()
{
  _autoInformSubscribers = true;
	clear();
}
  
void Trajectory::reserveMemory(unsigned int numPoints, bool usePos, bool useAtt)
{
  lockData(WRITE_LOCK);
  _time.reserve(numPoints);                    // 1 element per time
  if(usePos) _posopt.reserve(_base*numPoints); // _base elements per position
  if(useAtt) _att.reserve(4*numPoints);        // 4 elements per attitude
  unlockData(WRITE_LOCK);
}

void Trajectory::setNumOptionals(unsigned int nopt)
{
	if(_nopt == nopt) return;

	_nopt = nopt;
	_base = _dof*(1 + _nopt);

	bool temp = _autoInformSubscribers;
  _autoInformSubscribers = true;
	clear();
  _autoInformSubscribers = temp;
}

void Trajectory::setDOF(unsigned int dof)
{
	if((_dof == dof) || (dof == 0)) return;

	_dof = dof;
	_base = _dof*(1 + _nopt);

  bool temp = _autoInformSubscribers;
  _autoInformSubscribers = true;
	clear();
  _autoInformSubscribers = temp;
}

bool Trajectory::getTimeRange( DataType &begin, DataType &end ) const
{
	// Return first and last time in _time vector.
	// Note that _time is not sorted so these may not be
	// the lowest and highest times.
	if(_time.empty()) return false;	
	begin = _time.front();
	end = _time.back();
	return true;
}

double Trajectory::getTimeDistance(const DataType &t) const
{
  // Empty trajectory
  if(_time.size() == 0) return DBL_MAX;

  // Sort start/end times
  double t0 = _time[0];
  double tf = _time.back();
  if(t0 > tf) std::swap(t0, tf);

  // Compute distance from given time to this trajectory's time range
  // Outside trajectory: distance >= 0 (to nearest start/end time)
  // Inside trajectory: distance < 0 (metric zeros at start/end)
  if(t <= t0) return (t0 - t); // Distance to t0
  else if(t >= tf) return (t - tf); // Distance to tf
  else return (t - t0)*(t - tf); // Quadratic metric always < 0
}

// Get the index such that times[index] and times[index+1] bound the given time
int Trajectory::getTimeIndex(const DataType &t, int &index) const
{
	unsigned int numTimes = _time.size();

	// Time list is empty
	if(numTimes == 0) 
	{
	  index = -1;
	  return -1;
	}

	// Determine if times are increasing or decreasing
	int direction = (_time[0] <= _time.back())?1:-1;
  const DataType tDir = direction*t;

	// Check if requested time is within [t0, tf] range
	double val = getTimeDistance(t);

	// Requested time is out of bounds
	if(val > 0)
	{
	  if(direction*t < direction*_time[0]) index = -1;
	  else index = numTimes;

	  return -1; // Indicate that the requested time is out of bounds
	}

	// Requested time is at beginning or end of trajectory
	else if(val == 0.0)
	{
	  if(t == _time[0]) index = 0;
	  else index = numTimes - 1;

	  return 0; // Indicate that the requested time is at the bounds
	}

	// Requested time is in the trajectory, so search for it using a
	// standard interval bisection method. This assumes that the entire
  // trajectory is either monotonically increasing or decreasing.
	int low = 0;
	int high = numTimes - 1;
  int mid;
	for(int iter = 1; iter < 40; ++iter)
	{
    // Compute midpoint index
    mid = low + (high - low) / 2; // Less likely to overflow for very large trajectories, compared to (low+high)/2

    // Requested time is at or after the midpoint, so bracket the low index
    if (tDir >= direction*_time[mid])
    {
      low = mid;

      // Requested time is in index range [low, low+1)
      if (tDir < direction*_time[low + 1])
      {
        index = low;
        return iter;
      }
    }

    // Otherwise requested time is before the midpoint, so bracket the high index
    else high = mid;
	}

	return -2; // Max iterations reached
}

bool Trajectory::addTime( const DataType &t )
{
  // If time size is at capacity, then adding one more point
  // will resize and reallocate its memory. To prevent readers
  // from potentially accessing old memory, lock the mutex.
  const bool shouldLock = (_time.size() == _time.capacity());
  if(shouldLock) lockData(WRITE_LOCK);
	_time.emplace_back(t); // Add the time
  if(shouldLock) unlockData(WRITE_LOCK);

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addPosition( const DataType &x, const DataType &y,
				const DataType z )
{
	  // Make sure we are not adding too many positions
  unsigned int loc = _posopt.size();
	if(loc == _time.size()*_base) return false;
  
  // If posopt size is at capacity, then adding one more point
  // will resize and reallocate its memory. To prevent readers
  // from potentially accessing old memory, lock the mutex.
  const bool shouldLock = (loc+_base > _posopt.capacity());
  if(shouldLock) lockData(WRITE_LOCK);

	  // Add the position and create dummy optionals to go with it
  _posopt.resize(loc + _base);
  _posopt[loc] = x;
  _posopt[++loc] = y;
  if(_dof == 3) _posopt[++loc] = z;
  ++_numPos;
  
  if(shouldLock) unlockData(WRITE_LOCK);

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addPosition( const DataType* const pos )
{
	  // Make sure we are not adding too many positions
  unsigned int loc = _posopt.size();
	if(loc == _time.size()*_base) return false;

  // If posopt size is at capacity, then adding one more point
  // will resize and reallocate its memory. To prevent readers
  // from potentially accessing old memory, lock the mutex.
  const bool shouldLock = (loc+_base > _posopt.capacity());
  if(shouldLock) lockData(WRITE_LOCK);
  
	  // Add the position and create dummy optionals to go with it
	_posopt.resize(loc + _base);
	std::memcpy(&_posopt[loc], pos, _dof*sizeof(DataType));
	++_numPos;

  if(shouldLock) unlockData(WRITE_LOCK);

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

/** Get the 2D position associated with the nth time. */
bool Trajectory::getPosition( unsigned int n, DataType &x, DataType &y ) const
{
	if(n >= _numPos) return false; 

	unsigned int index = n*_base;
	x = _posopt[index];
	y = _posopt[++index];
	return true;
}

/** Get the 3D position associated with the nth time. */
bool Trajectory::getPosition( unsigned int n, DataType &x, DataType &y, DataType &z ) const
{
	if(n >= _numPos) return false; 

	unsigned int index = n*_base;
	x = _posopt[index];
	y = _posopt[++index];
	if(_dof == 3) z = _posopt[++index];
	else z = 0.0;
	return true;
}

bool Trajectory::addAttitude( const DataType &x, const DataType &y,
				const DataType &z, const DataType &w )
{
	  // Calculate max number of elements allowed in _att.
	  // There is a max of 1 attitude for each time, and each attitude
	  // consists of 4 elements (a quaternion).
	  // Make sure we are not adding too many attitudes
  unsigned int loc = _att.size();
	if(loc == (4*_time.size())) return false;

  // If att size is at capacity, then adding one more point
  // will resize and reallocate its memory. To prevent readers
  // from potentially accessing old memory, lock the mutex.
  const bool shouldLock = (loc+4 > _att.capacity());
  if(shouldLock) lockData(WRITE_LOCK);
  
	  // Add the attitude
  _att.resize(loc + 4);
  _att[loc] = x;
  _att[++loc] = y;
  _att[++loc] = z;
  _att[++loc] = w;
	++_numAtt;

  if(shouldLock) unlockData(WRITE_LOCK);

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addAttitude( const DataType* const att )
{
	  // Calculate max number of elements allowed in _att.
	  // There is a max of 1 attitude for each time, and each attitude
	  // consists of 4 elements (a quaternion).
	  // Make sure we are not adding too many attitudes
  unsigned int loc = _att.size();
	if(loc == (4*_time.size())) return false;

  // If att size is at capacity, then adding one more point
  // will resize and reallocate its memory. To prevent readers
  // from potentially accessing old memory, lock the mutex.
  const bool shouldLock = (loc+4 > _att.capacity());
  if(shouldLock) lockData(WRITE_LOCK);
  
	  // Add the attitude
	_att.resize(loc + 4);
	std::memcpy(&_att[_att.size()-4], att, 4*sizeof(DataType));
	++_numAtt;

	if(shouldLock) unlockData(WRITE_LOCK);

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::getAttitude( unsigned int n, DataType &x, DataType &y, DataType &z, DataType &w ) const
{
	if(n >= _numAtt) return false;

	unsigned int index = 4*n; // 4 elements per attitude (quaternion)
	x = _att[index];
	y = _att[++index];
	z = _att[++index];
	w = _att[++index];
	return true;
}

bool Trajectory::setOptional( unsigned int index, const DataType &x,
				const DataType &y, const DataType z )
{
	  // Make sure we are not adding too many optionals and that
	  // at least one position/optional group has already been added.
	if(index >= _nopt || _posopt.empty()) return false;

	  // Add the optional 
	index = _posopt.size() - _dof*(_nopt - index);
	_posopt[index] = x;
	_posopt[++index] = y;
	if(_dof == 3) _posopt[++index] = z;

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::setOptional( unsigned int index,
				const DataType* const opt )
{
	  // Make sure we are not adding too many optionals and that
	  // at least one position/optional group has already been added.
	if(index >= _nopt || _posopt.empty()) return false;

	  // Add the optional 
	index = _posopt.size() - _dof*(_nopt - index);
	std::memcpy(&_posopt[index], opt, _dof*sizeof(DataType));

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

/** Get optional associated with the nth time. The optional is specified by index */
bool Trajectory::getOptional( unsigned int n, unsigned int index,
				DataType &x, DataType &y ) const
{
	if(n >= _numPos || index >= _nopt) return false;

	unsigned int i = n*_base + (index+1)*_dof;
	x = _posopt[i];
	y = _posopt[++i];
	return true;
}

bool Trajectory::getOptional( unsigned int n, unsigned int index,
				DataType &x, DataType &y, DataType &z ) const
{
	if(n >= _numPos || index >= _nopt) return false;

	unsigned int i = n*_base + (index+1)*_dof;
	x = _posopt[i];
	y = _posopt[++i];
	if(_dof == 3) z = _posopt[++i];
	else z = 0.0;
	return true;
}

void Trajectory::clear()
{
  // Always lock when clearing data
  lockData(WRITE_LOCK);

	_time.clear();
	_posopt.clear();
	_att.clear();
	_numPos = _numAtt = 0;
  _dataCleared = true;

	unlockData(WRITE_LOCK);

	  // Inform subscribers
  if(_autoInformSubscribers) informSubscribers();
}

void Trajectory::getPoint(unsigned int i, const DataSource source[], DataType val[]) const
{
	  // Find (x,y,z) coordinates for each of 3 points
	for(int j = 0; j < 3; ++j)
	{
	  switch(source[j]._src)
	  {
	    case ZERO:
	      val[j] = 0.0;
	      break;

	    case POSOPT:
	      val[j] = source[j]._scale * _posopt[i*_base + source[j]._opt*_dof + source[j]._element];
	      break;

	    case ATTITUDE:
	      val[j] = source[j]._scale * _att[(4*i) + source[j]._element];
	      break;

	    case TIME:
	      val[j] = source[j]._scale * _time[i];
	      break;
	  }
	}
}

bool Trajectory::verifyData(const DataSource source[]) const
{
	// Make sure this trajectory contains data from specified sources
	for(int i = 0; i < 3; ++i)
	{
	  switch(source[i]._src)
	  {
	    case ZERO: continue;
	    case TIME: continue;
	    case ATTITUDE: // Attitude quaternion has 4 elements
	      if(source[i]._element > 3) return false;
	      break;
	    case POSOPT:
	      if(source[i]._element >= _dof || source[i]._opt > _nopt) return false;
	      break;
	  }
	}

	// Tests passed ... this trajectory's data is valid for the given sources
	return true;
}

unsigned int Trajectory::getNumPoints(const DataSource source[]) const
{
	// If all sources are ZERO then assume trajectory has infinite points
	if(source[0]._src == ZERO && 
	   source[1]._src == ZERO && 
	   source[2]._src == ZERO) return UINT_MAX;

	// Max number of points equals number of times
	unsigned int numPoints = _time.size();

        // Actual number of points can be reduced to the least number
        // of elements of any data source
	for(int i = 0; i < 3; ++i)
	{
	  if(source[i]._src == ATTITUDE && numPoints > _numAtt) 
	    numPoints = _numAtt;
	  else if(source[i]._src == POSOPT && numPoints > _numPos) 
	    numPoints = _numPos;
	}

	return numPoints;
}

/** Register the given subscriber with this Trajectory */
void Trajectory::addSubscriber(TrajectorySubscriber* subscriber) const
{
  // Make sure the subscriber is not already registered
  if(std::find(_subscribers.begin(), _subscribers.end(), subscriber) == _subscribers.end())
    _subscribers.emplace_back(subscriber);
}

void Trajectory::removeSubscriber(TrajectorySubscriber* subscriber) const
{
  // Remove subscriber from list of subscribers
  SubscriberArray::iterator i = std::find(_subscribers.begin(), _subscribers.end(), subscriber);
  if(i != _subscribers.end()) _subscribers.erase(i);
}

void Trajectory::informSubscribers()
{
  for(SubscriberArray::iterator i = _subscribers.begin(); i != _subscribers.end(); ++i)
  {
    // Inform artist that data has been cleared
    if(_dataCleared) (*i)->dataCleared(this);

    // Inform artist that data has been added
    else if(!_time.empty()) (*i)->dataAdded(this);
  }

  _dataCleared = false; // Reset flag
}
  
void Trajectory::lockData(DataLockType lockType) const
{
    if (lockType == READ_LOCK) _readWriteMutex.readLock();
    else _readWriteMutex.writeLock();
}

void Trajectory::unlockData(DataLockType lockType) const
{
    if (lockType == READ_LOCK) _readWriteMutex.readUnlock();
    else _readWriteMutex.writeUnlock();
}

} // !namespace OpenFrames
