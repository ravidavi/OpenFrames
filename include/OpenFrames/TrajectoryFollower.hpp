/***********************************
   Copyright 2020 Ravishankar Mathur

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

/** \file TrajectoryFollower.hpp
 * Declaration of TrajectoryFollower class.
 */

#ifndef _OF_TRAJECTORYFOLLOWER_
#define _OF_TRAJECTORYFOLLOWER_

#include <OpenFrames/Export.h>
#include <OpenFrames/Trajectory.hpp>
#include <osg/Callback>
#include <osg/ref_ptr>
#include <osg/observer_ptr>
#include <osg/Quat>
#include <osg/Vec3d>
#include <OpenThreads/Mutex>

namespace OpenFrames
{
  /**
   * \class TrajectoryFollower
   *
   * \brief Updates FrameTransform objects from Trajectory objects.
   *
   * This class updates a FrameTransform object with the position/attitude of
   * a given set of Trajectory objects. It should be added as an update
   * callback via frameTransformObject->setUpdateCallback(trajectoryFollowerObject).
   * Adjustable parameters are the time scale at which to follow the trajectories,
   * and what to do if the current time is not in the trajectory's time list.
   * If multiple trajectories are specified, then TrajectoryFollower chooses
   * which one to follow based on the current time and follow mode.
   * e.g.: Given trajectory T_i, set of trajectories {T}, and current time t
   *  If t not within time of {T}, then wrap t to time of {T} (based on follow mode)
   *  If t within time of {T} but not within any T_i, then follow closest T_i
   *  If t within time of T_i, then follow T_i.
   */
  class OF_EXPORT TrajectoryFollower : public osg::Callback, public OpenFrames::TrajectorySubscriber
  {
  public:
	/** If a trajectory is being followed, the FollowMode specifies how the
	    transform handles the current time being out of range of the followed
	    trajectory's times.  If the current time is in the trajectory's time
	    list, then the trajectory's position/attitude is used regardless
	    of the FollowMode setting. */
	enum FollowMode 
	{
	  LOOP = 0, // Loop around repeatedly
	  LIMIT     // Limit the frame to the ends of the followed trajectory
	};

	/** Specifies which of position or attitude we want to follow. */
	enum FollowData
	{
	  POSITION = 1,
	  ATTITUDE = 2
	};

	TrajectoryFollower(Trajectory *traj = NULL);

	// Don't allow copying from another TrajectoryFollower
	TrajectoryFollower(const TrajectoryFollower &tf, const osg::CopyOp &copyop) {}

	META_Object(OpenFrames, TrajectoryFollower);

  // Follow ONLY the specified trajectory, and unfollow any others
  // More efficient than unfollowTrajectory(NULL) -> followTrajectory(traj)
  // NULL input: stop following all trajectories (same as unfollowTrajectory(NULL))
	void setTrajectory(Trajectory *traj);
  
  // Add trajectory to be followed
  // Has no effect if trajectory is already being followed
  void addTrajectory(Trajectory *traj);
  
  // Stop following a trajectory
  // NULL input: stop following all trajectories
  void removeTrajectory(Trajectory *traj);

	// Set how trajectories are followed
	inline void setFollowType(unsigned int data, FollowMode mode)
	{
	  _data = data;
	  _mode = mode;
	}
	inline void getFollowType(unsigned int &data, FollowMode &mode) const
	{
	  data = _data;
	  mode = _mode;
	}

	// Set source for each each position component
	bool setXData(const Trajectory::DataSource &src);
	bool setYData(const Trajectory::DataSource &src);
	bool setZData(const Trajectory::DataSource &src);
  const Trajectory::DataSource* getDataSource() const { return _dataSource; }
  
  // Set default sources for all position components
  // This means standard X/Y/Z components for position
  // If following multiple trajectories, then use info for
  // first followed trajectory to determine data availability
  void setDefaultData();
  bool getUsingDefaultData() { return _usingDefaultData; }

  // Time managment functions allow for two types of times
  // - Offset from the global simulation time (set by WindowProxy)
  // - Custom simulation time
  void setTime(double time); // Custom simulation time
  void setOffsetTime(double offsetTime); // Offset from global simulation time
  inline bool isFollowingTime() const
  { return _followTime; } // True for global sim time, false for custom sim time
  
  double getLastTime() const { return _lastAdjustedTime; }
  Trajectory* getLastTrajectory() const { return _follow.get(); }
	
  /** Inherited from osg::Callback, implements the callback. */
  virtual bool run(osg::Object* object, osg::Object* data);
  
  /** Inherited from OpenFrames::TrajectorySubscriber
      Functions that inform about Trajectory changes */
  virtual void dataCleared(const Trajectory* traj) { _needsUpdate = true; }
  virtual void dataAdded(const Trajectory* traj) { _needsUpdate = true; }

  protected:
	virtual ~TrajectoryFollower();

  // Compute adjusted time based on follow mode
  double _computeTime(double time);
  
  // Choose trajectory to follow based on adjusted time
  Trajectory* _chooseTrajectory(double time);
  
  // Update position & orientation based on adjusted time and chosen trajectory
	bool _updateState(double time, FollowData data);
  
  // Check if all followed trajectories support necessary data sources
  bool _verifyDataSources() const
  {
    // Data sources are valid if all followed trajectories support them
    for(auto traj : _trajList)
    {
      if(!traj->verifyData(_dataSource)) return false;
    }
    return true;
  }
  
  typedef std::vector<osg::ref_ptr<Trajectory> > TrajList;

  TrajList _trajList; // All followed trajectories
  osg::observer_ptr<Trajectory> _follow; // Currently followed trajectory
	FollowMode _mode; // Mode in which to follow trajectory
	unsigned int _data; // Whether to follow position and/or attitude

	  // Specifies which data to follow in the trajectory
	Trajectory::DataSource _dataSource[3];
	bool _dataValid; // Test if Trajectory supports needed data
  bool _usingDefaultData; // Whether to use default data sources

  // Time control variables
  bool _needsUpdate, _followTime;
  double _timeVal;  // Time value to use (offset if following time, constant otherwise)
  double _lastSimTime; // Simulation time at most recent update
  double _lastAdjustedTime;
  
  OpenThreads::Mutex _mutex; // For adding/removing followed trajectories

  private:
	osg::Vec3d _v1, _v2; // Used for position interpolation
	osg::Quat _a1, _a2; // Used for attitude interpolation
  };

} // !namespace OpenFrames

#endif // !define _OF_TRAJECTORYFOLLOWER_
