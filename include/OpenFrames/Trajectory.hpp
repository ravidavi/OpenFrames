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

#ifndef _OF_TRAJECTORY_
#define _OF_TRAJECTORY_

#include <OpenFrames/Export.h>
#include <osg/Referenced>
#include <OpenThreads/ReadWriteMutex>
#include <vector>

namespace OpenFrames {

class TrajectoryArtist; // The class which draws a Trajectory
class TrajectorySubscriber; // Forward-declare trajectory subscriber

/************************************
 * Ravi Mathur  
 * OpenFrames API, class Trajectory
 * A Trajectory holds a collection of times, positions, attitudes, and any number of optional data vectors.  Data is inserted at the end of the trajectory, and no sorting is done during insertion.  This is left to deriving classes.
***********************************/
class OF_EXPORT Trajectory : public osg::Referenced
{
  public:
	/** Type of each data element in the trajectory.  Make sure to change
	    return value of getGLDataType() if you change this! */
	typedef double DataType;
	typedef std::vector<DataType> DataArray;
	typedef std::vector<TrajectoryArtist*> ArtistArray;
  typedef std::vector<TrajectorySubscriber*> SubscriberArray;

	/** SourceType is used to specify where the data for the x/y/z component
	    of every point is to be taken from. */
        enum SourceType
	{
	  ZERO = 0, // Use 0 for the component's value
	  TIME,	    // Use the time value
	  POSOPT,   // Use a position/optional value
	  ATTITUDE  // Use an attitude value
	};

	/** DataSource specifies exactly which element of the source to use.
	    Possible values are:
	    _src        _element     _opt
	    ZERO         N/A         N/A
	    TIME         N/A         N/A
	    POSOPT       0...DOF-1   0...NOPT (0 is for position)
	    ATTITUDE     0...3       N/A
	*/
	struct DataSource
	{
	  DataSource()
	  {
	    _src = ZERO;
	    _element = _opt = 0;
	    _scale = 1.0;
	  }

	  bool operator == (const DataSource &rhs)
	  {
	    if(_src == rhs._src && _element == rhs._element
	       && _opt == rhs._opt && _scale == rhs._scale) return true;
	    else return false;
	  }

	  SourceType   _src;
	  unsigned int _element;
	  unsigned int _opt;
	  double       _scale;
	};

	/** Initialize with degrees of freedom, an id, and how many 
	    optionals are at each point of the trajectory. */
	Trajectory(unsigned int dof = 3, unsigned int nopt = 0 );

	/** Get lists. */
	inline const DataArray& getTimeList() const { return _time; }
	inline const DataArray& getPosOptList() const { return _posopt; }
	inline const DataArray& getAttList() const { return _att; }

	/** Get total number of position/optional groups. This will always be 
	    less then or equal to the number of times in the time list. */
	inline unsigned int getNumPos() const { return _numPos; }

	/** Set/Get number of optionals for each position. Note that the 
	    trajectory will be cleared if the number of optionals is changed. */
	void setNumOptionals(unsigned int nopt);
	inline unsigned int getNumOptionals() const { return _nopt; }

	/** Get the total number of attitude points. This will always be less 
	    than or equal to the number of times in the time list. */
	inline unsigned int getNumAtt() const { return _numAtt; }

	/** Set/Get number of degrees of freedom for each position&optional. 
	    Note that the trajectory will be cleared if its DOF is changed. */
	void setDOF(unsigned int dof);
	inline unsigned int getDOF() const { return _dof; }

	/** Get number of data elements taken up by each position&optional 
	    group.  A data element is of type DataType. */
	inline unsigned int getBase() const { return _base; }

	/** Find the lower bounding index (in the time list) for the given time.
	    Returns -2 if the requested time could not be found after a
	    reasonable number of iterations. In this case, index is the
            last value computed before quitting the search.
	    Returns -1 if the requested time is out of the time list's bounds:
	      index = -1: Requested time before first time 
	      index = numTimes: Requested time after last time 
	    Returns 0 if the requested time is at the list's bounds:
	      index = 0: Requested time is at start of time list
	      index = numTimes-1: Requested time is at end of time list
	    Otherwise returns number of iterations required to find the index
	    such that times[index] <= t < times[index+1]. In this case it is
	    guaranteed that 0 <= index <= numTimes-2.
	**/
	virtual int getTimeIndex( const DataType &t, int &index ) const;

	inline bool isEmpty() const { return _time.empty(); }
	inline unsigned int getNumTimes() const { return _time.size(); }
	virtual bool getTimeRange( DataType &begin, DataType &end ) const;

        /** Get the "distance" from given time to this trajectory's time
            range. Possible return values:
             = DBL_MAX: No times in trajectory
             > 0.0: Out of range, either after or before time range
             = 0.0: At start or end of time range
             < 0.0: Between start and end of time range
        */
        double getTimeDistance(const DataType &t) const;

	/** Add a point with the given time to the end of the time list. */
	virtual bool addTime( const DataType &t );

	/** Add position. For pointer version, the DOF determines how many 
	    elements will be copied. */
	virtual bool addPosition( const DataType &x, const DataType &y, 
			  	const DataType z = 0.0 );
	virtual bool addPosition( const DataType* const pos );
	virtual bool getPosition( unsigned int n, DataType &x, DataType &y ) const;
	virtual bool getPosition( unsigned int n, DataType &x, DataType &y, DataType &z ) const;
	
	/** Add attitude. For pointer version, the pointer must reference 
	    4 elements (for quaternion). */
	virtual bool addAttitude( const DataType &x, const DataType &y,
			  	const DataType &z, const DataType &w );
	virtual bool addAttitude( const DataType* const att );
	virtual bool getAttitude( unsigned int n, DataType &x, DataType &y,
					 DataType &z, DataType &w ) const;

	/** Set the optional with the given index. For pointer version,
	    the DOF determines how many elements will be copied. The index
	    must be in the range [0 ... _nopt-1]. */
	virtual bool setOptional( unsigned int index, const DataType &x,
			  	const DataType &y, const DataType z = 0.0 );
	virtual bool setOptional( unsigned int index, const DataType* const opt );
	virtual bool getOptional( unsigned int n, unsigned int index, DataType &x,
				  DataType &y ) const;
	virtual bool getOptional( unsigned int n, unsigned int index, DataType &x,
				  DataType &y, DataType &z ) const;

	/** Remove all points from this trajectory */
	virtual void clear();

	/** Get data point i from the Trajectory.  The DataSource defines where in
	    in the trajectory the corresponding (x/y/z) data component comes from.  
	    The resulting point is stored in the supplied 'val' 3-element vector. 
	    NOTE: This function does NOT do error checking on it's inputs!
	    You should use verifyData() and getNumPoints() to make sure that
	    the data exists in the trajectory before calling getPoint(). */
	virtual void getPoint(unsigned int i, const DataSource source[], DataType val[]) const;

	/** Verify whether the data specified by the source 3-vector is contained
	    in this trajectory. */
	virtual bool verifyData(const DataSource source[]) const;

	/** Get the maximum number of points that can be obtained by the Trajectory's
	    data using the given sources.  If each of the source's components
	    is ZERO, then this function returns UINT_MAX. */
	virtual unsigned int getNumPoints(const DataSource source[]) const;
		
	/** Register an artist with this trajectory.  The artist will be notified
	    whenever a position, optional, or attitude is added to the trajectory. */
	virtual void addArtist(TrajectoryArtist *artist) const;
	virtual void removeArtist(TrajectoryArtist *artist) const;

	virtual void informArtists();
	inline void autoInformArtists(bool autoinform) { _autoInformArtists = autoinform; }

  /** Register a subscriber with this trajectory. The subscriber will be notified
      whenever the trajectory changes. */
  virtual void addSubscriber(TrajectorySubscriber* subscriber) const;
  virtual void removeSubscriber(TrajectorySubscriber* subscriber) const;
  virtual void informSubscribers();
  inline void autoInformSubscribers(bool autoinform) { _autoInformSubscribers = autoinform; }
  
	/** Synchronization routines which prevent Trajectory's data from being
	    moved around in memory while the data is being read. */
	virtual void lockData() const;   // Block the data from being changed
	virtual void unlockData() const; // Allow the data to be changed

  protected:
	virtual ~Trajectory();

	DataArray _time;   // Times
	DataArray _posopt; // Positions & optionals
	DataArray _att;    // Attitudes

	mutable ArtistArray _artists; // Artists currently drawing this Trajectory
	bool _autoInformArtists;
  
  mutable SubscriberArray _subscribers; // Subscribers of this Trajectory
  bool _autoInformSubscribers;

	unsigned int _nopt; // Number of optionals for each time.
	unsigned int _base; // Number of data elements taken up by each
			    // position/optionals group.  Each data element
			    // is of type DataType, so _base=_dof*(1+_nopt)

	unsigned int _numPos; // Current number of pos/opt groups
	unsigned int _numAtt; // Current number of attitudes

	unsigned int _dof; // Degrees of freedom for position and optionals

	// Synchronization variables
	mutable OpenThreads::ReadWriteMutex _readWriteMutex;
	bool _safeReadWrite;
};

/************************************
 * Ravi Mathur
 * OpenFrames API, class TrajectorySubscriber
 * Pure abstract base class that is informed of changes to Trajectory objects
 ***********************************/
class TrajectorySubscriber
{
public:
  /** Called by a trajectory when its data is cleared. Must be
   implemented by derived classes. */
  virtual void dataCleared(Trajectory *traj) = 0;
  
  /** Called by a trajectory when data is added to it. Must be
   implemented by derived classes. */
  virtual void dataAdded(Trajectory *traj) = 0;
};
  
} // !namespace OpenFrames

#endif // !define _OF_TRAJECTORY_
