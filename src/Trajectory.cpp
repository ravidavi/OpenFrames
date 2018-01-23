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

#include <OpenFrames/Trajectory.hpp>
#include <math.h>
#include <climits>
#include <cfloat>
#include <cstring>
#include <algorithm>

namespace OpenFrames {

Trajectory::Trajectory(unsigned int dof, unsigned int nopt )
{
  _autoInformSubscribers = true;
	_safeReadWrite = true;

	if(dof == 0) dof = 1;
	setDOF(dof);
	setNumOptionals(nopt);
}

Trajectory::~Trajectory()
{
  _autoInformSubscribers = true;
	clear();
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
	// modified interval bisection method where we linearly approximate
	// an intermediate index (as opposed to just talking the halfway
	// index for a normal interval bisection method).
	int low = 0;
	int high = numTimes - 1;
	for(int iter = 1; iter < 100; ++iter)
	{
	  // Compute estimated index for the requested time
	  index = low + (int)((t - _time[low])/(_time[high] - _time[low])*(high - low));

	  // In rare instances where t ~= _time[high], precision issues may 
	  // cause the computed index to equal the high bounding index. In
	  // such a case, the actual computed index should be just below
	  // the high index, so we decrement it.
	  if(index == high) --index;

	  // Requested time is between current index and next index.
	  if((t-_time[index])*(t-_time[index+1]) <= 0.0) return iter;

	  // Requested time is before current index, so reset the high bound.
	  if(direction*t < direction*_time[index]) high = index;

	  // Requested time is after current index, so reset the low bound.
	  // Note here that t must also be after the next index, since it
	  // didn't pass the stop condition above.
	  else low = index + 1;
	}

	return -2; // Max iterations reached
}

bool Trajectory::addTime( const DataType &t )
{
	if(_safeReadWrite) _readWriteMutex.writeLock();
	_time.push_back(t); // Add the time
	if(_safeReadWrite) _readWriteMutex.writeUnlock();

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addPosition( const DataType &x, const DataType &y,
				const DataType z )
{
	  // Make sure we are not adding too many positions
	if(_posopt.size() == _time.size()*_base) return false;

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the position and create dummy optionals to go with it
  unsigned int loc = _posopt.size();
  _posopt.insert(_posopt.end(), _base, 0.0);
  _posopt[loc] = x;
  _posopt[++loc] = y;
  if(_dof == 3) _posopt[++loc] = z;
  ++_numPos;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addPosition( const DataType* const pos )
{
	  // Make sure we are not adding too many positions
	if(_posopt.size() == _time.size()*_base) return false;

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the position and create dummy optionals to go with it
  unsigned int loc = _posopt.size();
	_posopt.insert(_posopt.end(), _base, 0.0);
	std::memcpy(&_posopt[loc], pos, _dof*sizeof(DataType));
	++_numPos;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

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
	if(_att.size() == (_time.size() << 2)) return false;

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the attitude
  unsigned int loc = _att.size();
  _att.insert(_att.end(), 4, 0.0);
  _att[loc] = x;
  _att[++loc] = y;
  _att[++loc] = z;
  _att[++loc] = w;
	++_numAtt;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::addAttitude( const DataType* const att )
{
	  // Calculate max number of elements allowed in _att.
	  // There is a max of 1 attitude for each time, and each attitude
	  // consists of 4 elements (a quaternion).
	  // Make sure we are not adding too many attitudes
	if(_att.size() == (_time.size() << 2)) return false;

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the attitude
	_att.insert(_att.end(), 4, 0.0);
	std::memcpy(&_att[_att.size()-4], att, sizeof(DataType) << 2);
	++_numAtt;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::getAttitude( unsigned int n, DataType &x, DataType &y, DataType &z, DataType &w ) const
{
	if(n >= _numAtt) return false;

	unsigned int index = n << 2; // 4 elements per attitude (quaternion)
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

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the optional 
	index = _posopt.size() - _dof*(_nopt - index);
	_posopt[index] = x;
	_posopt[++index] = y;
	if(_dof == 3) _posopt[++index] = z;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

  if(_autoInformSubscribers) informSubscribers();

	return true;
}

bool Trajectory::setOptional( unsigned int index,
				const DataType* const opt )
{
	  // Make sure we are not adding too many optionals and that
	  // at least one position/optional group has already been added.
	if(index >= _nopt || _posopt.empty()) return false;

	if(_safeReadWrite) _readWriteMutex.writeLock();

	  // Add the optional 
	index = _posopt.size() - _dof*(_nopt - index);
	std::memcpy(&_posopt[index], opt, _dof*sizeof(DataType));

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

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
	if(_safeReadWrite) _readWriteMutex.writeLock();

	_time.clear();
	_posopt.clear();
	_att.clear();
	_numPos = _numAtt = 0;

	if(_safeReadWrite) _readWriteMutex.writeUnlock();

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
	      val[j] = source[j]._scale * _att[(i<<2) + source[j]._element];
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
    _subscribers.push_back(subscriber);
}

void Trajectory::removeSubscriber(TrajectorySubscriber* subscriber) const
{
  // Remove subscriber from list of subscribers
  SubscriberArray::iterator i = std::find(_subscribers.begin(), _subscribers.end(), subscriber);
  if(i != _subscribers.end()) _subscribers.erase(i);
}

void Trajectory::informSubscribers()
{
  bool cleared = _time.empty();
  for(SubscriberArray::iterator i = _subscribers.begin(); i != _subscribers.end(); ++i)
  {
    if(cleared) (*i)->dataCleared(this);
    else (*i)->dataAdded(this);
  }
}
  
void Trajectory::lockData() const
{
	_readWriteMutex.readLock();
}

void Trajectory::unlockData() const
{
	_readWriteMutex.readUnlock();
}

} // !namespace OpenFrames
