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

#ifndef _OF_FRAMETRANSFORM_
#define _OF_FRAMETRANSFORM_

#include <OpenFrames/Export.h>
#include <OpenFrames/Trajectory.hpp>
#include <osg/Transform>
#include <osg/NodeCallback>
#include <osg/NodeVisitor>
#include <osg/Vec3d>
#include <osg/Vec4d>
#include <osg/ref_ptr>

namespace OpenFrames
{

/************************************
 * Ravi Mathur
 * OpenFrames API, class FrameTransform
 * This class implements a transformation from local to world coordinates and back.  It is a subclass of the OSG class osg::Transform.
************************************/
class OF_EXPORT FrameTransform : public osg::Transform
{
  public:

	FrameTransform();

	// Don't allow copying from another FrameTransform
	FrameTransform(const FrameTransform& xform, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY) {}

	  // Standard OSG Node methods
	META_Node( OpenFrames, FrameTransform );

	void reset();

	// Set whether this frame should follow the viewer's eye point.
	// This is used ie for a SkyBox.
	inline void setFollowEye(bool f) { _followEye = f; }
	inline bool getFollowEye() const { return _followEye; }

	// Set whether this frame applies its transform or not.
	inline void setDisabled(bool d) { _disabled = d; }
	inline bool isDisabled() const { return _disabled; }

	// Set position wrt parent frame
	void setPosition(const double &x, const double &y, const double &z);
  void setPosition(const osg::Vec3d &pos);
	void getPosition(double &x, double &y, double &z) const;
  void getPosition(osg::Vec3d &pos) const;

	// Set attitude (orientation) wrt parent frame. Note that this is
	// a quaternion (qx, qy, qx, qw).
	void setAttitude(const double &rx, const double &ry, const double &rz, const double &angle);
  void setAttitude(const osg::Quat &att);
	void getAttitude(double &rx, double &ry, double &rz, double &angle) const;
  void getAttitude(osg::Quat &att) const;

	// Set scale wrt parent frame
	void setScale(const double &sx, const double &sy, const double &sz);
	void getScale(double &sx, double &sy, double &sz);

	// Set the pivot point about which scales & rotations are computed.
	void setPivot(const double &px, const double &py, const double &pz);
	void getPivot(double &px, double &py, double &pz);

	// Inherited functions to compute transformation matrix
	virtual bool computeLocalToWorldMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const;
	virtual bool computeWorldToLocalMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const;

  protected:
	virtual ~FrameTransform();

	osg::Vec3d _position; // Position relative to parent frame's origin
	osg::Quat _attitude;  // Attitude relative to parent frame
	osg::Vec3d _scale;    // Scale in addition to parent frame's scale
	osg::Vec3d _pivot;    // Pivot point relative to local origin

	// If disabled, this transform will have no effect
	bool _disabled;

	// Allows frame to follow the current eye point.  Only effective if
	// the reference frame is RELATIVE_RF
	bool _followEye;
};

/***************************************************************
 * Ravi Mathur
 * OpenFrames API, class TrajectoryFollower
 * This class updates a FrameTransform object with the position/attitude of a given
 * Trajectory which is to be followed.  It should be added as an update callback
 * via frameTransformObject->setUpdateCallback(trajectoryFollowerObject).
 * Adjustable parameters are the time scale at which to follow the trajectory, and
 * what to do if the current time is not in the trajectory's time list.
***************************************************************/
class OF_EXPORT TrajectoryFollower : public osg::NodeCallback
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

	// Set the trajectory to be followed
	void setFollowTrajectory(Trajectory *traj);

	// Set how the trajectory is followed
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

	// Set where each component of position comes from
	bool setXData(const Trajectory::DataSource &src);
	bool setYData(const Trajectory::DataSource &src);
	bool setZData(const Trajectory::DataSource &src);

	// Time managment functions			 
	void setTimeScale(double timeScale);
	inline double getTimeScale() const { return _timeScale; }
	
	void setPaused(bool pause);
	inline bool getPaused() const { return _paused; }
	
	void setOffsetTime(double offsetTime);
	inline double getOffsetTime() const { return _offsetTime; }
	
	void reset();

	/** Inherited from osg::NodeCallback.  Implements the callback. */
	virtual void operator()(osg::Node *node, osg::NodeVisitor *nv);

  protected:
	virtual ~TrajectoryFollower();

  // Compute adjusted time based on follow mode
  double _computeTime(double time);
  
  // Choose trajectory to follow based on adjusted time
  Trajectory* _chooseTrajectory(double time);
  
  // Update position & orientation based on adjusted time and chosen trajectory
	void _updateState(double time, FollowData data);
  
  // Check if all followed trajectories support necessary data sources
  bool _verifyDataSources() const
  {
    // Data sources are valid if all followed trajectories support them
    bool dataValid = true;
    for(auto traj : _trajList)
    {
      if(!traj->verifyData(_dataSource))
      {
        dataValid = false;
        break;
      }
    }
    return dataValid;
  }

	std::vector<osg::ref_ptr<Trajectory> > _trajList; // All followed trajectories 
  Trajectory* _follow; // Currently followed trajectory
	FollowMode _mode; // Mode in which to follow trajectory
	unsigned int _data; // Whether to follow position and/or attitude

	  // Specifies which data to follow in the trajectory
	Trajectory::DataSource _dataSource[3];
	bool _dataValid; // Test if Trajectory supports needed data

	  // These variables allow for pausing/resuming animation
	bool _paused, _needsUpdate;
	double _pauseTime;  // Time at last pause
	double _latestTime; // Time at most recent update
	double _deltaTime;  // Internally computed time offset
	
	  // User-specified variables for time control
	double _offsetTime; // Constant time offset
	double _timeScale; // Time scale

  private:
	osg::Vec3d _v1, _v2; // Used for position interpolation
	osg::Quat _a1, _a2; // Used for attitude interpolation
};

/***************************************************************
 * Ravi Mathur
 * OpenFrames API, class TimeManagementVisitor
 * This class traverses a nodepath, searching for FrameTransform
 * nodes.  It then pauses and/or sets the offset time of any 
 * TrajectoryFollower callback that transform may have attached to it.
**************************************************************/
class OF_EXPORT TimeManagementVisitor : public osg::NodeVisitor
{
  public:
	TimeManagementVisitor();

	void setPauseState(bool changePauseState, bool pauseState = true);
	inline void getPauseState(bool &changePauseState, bool &pauseState) const
	{
	  changePauseState = _changePauseState;
	  pauseState = _pauseState;
	}
	
	void setOffsetTime(bool changeOffsetTime, double offsetTime = 0.0);
	inline void getOffsetTime(bool &changeOffsetTime, double &offsetTime) const
	{
	  changeOffsetTime = _changeOffsetTime;
	  offsetTime = _offsetTime;
	}
		
	void setTimeScale(bool changeTimeScale, double timeScale = 1.0);
	inline void setTimeScale(bool &changeTimeScale, double &timeScale) const
	{
	  changeTimeScale = _changeTimeScale;
	  timeScale = _timeScale;
	}
	
	inline void setReset(bool reset) { _reset = reset; }
	inline bool getReset() const { return _reset; }
	
	virtual void apply(osg::Transform &node);

  protected:
	virtual ~TimeManagementVisitor();

	bool _changePauseState, _pauseState; // Whether to pause or unpause
	bool _changeOffsetTime; // Whether to set the offset time
	bool _changeTimeScale; // Whether to set the time scale
	bool _reset; // Whether the follower should be reset
	double _offsetTime, _timeScale;
};

} // !namespace OpenFrames

#endif // !define _OF_FRAMETRANSFORM_
