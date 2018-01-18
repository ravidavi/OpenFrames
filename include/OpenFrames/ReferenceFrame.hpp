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

#ifndef _OF_REFERENCEFRAME_
#define _OF_REFERENCEFRAME_

#include <OpenFrames/Export.h>
#include <OpenFrames/FrameTransform.hpp> 
#include <OpenFrames/Vector.hpp>

#include <osg/Geode>
#include <osg/LightSource>
#include <osgText/Text>
#include <osg/Referenced>
#include <osg/ref_ptr>

#include <vector>
#include <string>

namespace OpenFrames {

class FrameTracker;
class Trajectory;

/*******************************************
 * Ravi Mathur
 * OpenFrames API, class ReferenceFrame
******************************************/

/**
* \brief This class defines the standard functions of a classical reference frame. 
* 
* A reference frame can only contain other reference frames,
* so all objects should be derived from this class.
**/
class OF_EXPORT ReferenceFrame : public osg::Referenced {
  public:
    typedef std::vector<ReferenceFrame*> ParentList; /**< Defines a vector of all direct parents of this frame */
    typedef std::vector<osg::ref_ptr<ReferenceFrame> > ChildList; /**< Defines a vector of all direct children of this frame */
    typedef std::vector<FrameTracker*> TrackerList; /**< Defines a vector of all trackers of this frame */

    /**
    * \brief Construct a new ReferenceFrame
    *
    * The color of this frame will be white and 90% opaque.
    *
    * \param name Name of the frame
    **/
    ReferenceFrame(const std::string &name);

    /**
    * \brief Construct a new ReferenceFrame
    *
    * The color of this frame is specified by the constructor arguments and is 90% opaque.
    *
    * \param name  Name of the frame
    * \param color Vector of the colors [red, green, blue]
    **/
    ReferenceFrame(const std::string &name, const osg::Vec3 &color);

    /**
    * \brief Construct a new ReferenceFrame
    *
    * The color of this frame is specified by the constructor arguments.
    *
    * \param name  Name of the frame
    * \param color Vector of the colors [red, green, blue, alpha]
    **/
    ReferenceFrame(const std::string &name, const osg::Vec4 &color);

    /**
    * \brief Construct a new ReferenceFrame
    *
    * The color of this frame is specified by the constructor arguments.
    *
    * \param name Name of the frame
    * \param r    Red color component [0-1]
    * \param g    Green color component [0-1]
    * \param b    Blue color component [0-1]
    * \param a    Alpha (transparancy) component [0-1]
    **/
	ReferenceFrame( const std::string &name , float r, float g, float b, float a = 1.0 );

	/* Set the name of the frame that will be displayed */
	void setName( const std::string &name );

    /**
    * \brief Get the name of the frame
    *
    * \return Name of the frame
    **/
	inline const std::string& getName() const { return _name; }

	/* Set the color of the frame's decorations (axes, name, ...)
	    This method can be overridden by derived classes */
	virtual void setColor( const osg::Vec4 &color );
	virtual void setColor( float r, float g, float b, float a = 1.0 );
	virtual const osg::Vec4& getColor() const;
	virtual void getColor( float &r, float &g, float &b, float &a ) const;

    /**
    * \brief Get the transform corresponding to this ReferenceFrame
    *
    * \return The transform
    **/
	inline FrameTransform* getTransform() {return _xform.get();}

	/* Get the group corresponding to this ReferenceFrame. By default, the
	    frame's group is the same as its transform.  However, subclasses
	    can define a separate group if they wish to. A child frame's group is
	    what is added to a parent frame's transform in addChild(). */
	virtual osg::Group* getGroup();

    /**
    * \brief Set the position of this frame
    *
    * This only applies if the frame is not being auto positioned by a TrajectoryFollower.
    *
    * \param x X position
    * \param y Y position
    * \param z Z position
    **/
	inline void setPosition( const double &x, const double &y, const double &z )
	{ _xform->setPosition(x, y, z); }

  inline void setPosition( const osg::Vec3d &pos )
  { _xform->setPosition(pos); }

    /**
    * \brief Get the position of this frame
    *
    * \param x Returned X position
    * \param y Returned Y position
    * \param z Returned Z position
    **/
	inline void getPosition( double &x, double &y, double &z ) const
	{ _xform->getPosition(x, y, z); }

  inline void getPosition( osg::Vec3d &pos ) const
  { _xform->getPosition(pos); }

    /**
    * \brief Set the orientation of this frame
    *
    * This only applies if the frame is not being auto positioned by a TrajectoryFollower.
    *
    * \param rx    X component of the rotation quaternion
    * \param ry    Y component of the rotation quaternion
    * \param rz    Z component of the rotation quaternion
    * \param angle Angle component of the rotation quaternion
    **/
	inline void setAttitude( const double &rx, const double &ry, const double &rz, const double &angle )
	{ _xform->setAttitude(rx, ry, rz, angle); }

  inline void setAttitude( const osg::Quat &att )
  { _xform->setAttitude(att); }

    /**
    * \brief Get the orientation of this frame
    *
    * \param rx    Returned X component of the rotation quaternion
    * \param ry    Returned Y component of the rotation quaternion
    * \param rz    Returned Z component of the rotation quaternion
    * \param angle Returned angle component of the rotation quaternion
    **/
	inline void getAttitude( double &rx, double &ry, double &rz, double &angle) const
	{ _xform->getAttitude(rx, ry, rz, angle); }

  inline void getAttitude( osg::Quat &att ) const
  { _xform->getAttitude(att); }
	
	/* Get the BoundingSphere encompassing this frame plus all of its
	    decorations. Derived classes should override this method
	    and compute their own local BoundingSphere. */ 
	virtual const osg::BoundingSphere& getBound() const;

	enum AxesType /** Specifies which axes to draw */
	{
	  NO_AXES = 0,
	  X_AXIS = 1,
	  Y_AXIS = 2,
	  Z_AXIS = 4
	};

    // Show/hide the x, y, z axes vectors and labels; see AxesType
    virtual void showAxes(unsigned int axes);
	virtual void showAxesLabels(unsigned int labels);
	virtual void showNameLabel(bool namelabel);

    // Place x/y/z axis vectors at the given location with given length
    void moveXAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;
    void moveYAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;
	void moveZAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;


    /**
    * Set the text displayed for the x-axis label.
    * 
    * The default axis label is 'X'.
    * 
    * \param str String to set as the axis label
    */
    inline void setXLabel(const std::string &str) { _xLabel->setText(str); }

    /**
    * Set the text displayed for the y-axis label.
    *
    * The default axis label is 'Y'.
    *
    * \param str String to set as the axis label
    */
    inline void setYLabel(const std::string &str) { _yLabel->setText(str); }

    /**
    * Set the text displayed for the z-axis label.
    *
    * The default axis label is 'Z'.
    *
    * \param str String to set as the axis label
    */
	inline void setZLabel(const std::string &str) { _zLabel->setText(str); }

	/* Add/remove a frame as a child to this one */
	bool addChild( ReferenceFrame* frame );
	bool removeChild( ReferenceFrame* frame );
  
  /**
   * \brief Set whether this frame's light source is enabled.
   * A light source will be created as needed.
   * A ReferenceFrame's light source disabled by default.
   *
   * \param enable Whether to enable/disable light source
   * \return Enabled light source, or NULL if disabled
   */
  osg::LightSource* setLightSourceEnabled(bool enable);
  
  /**
   * \brief Check whether this frame's light source is enabled.
   */
  bool getLightSourceEnabled() const;

    /**
    * \brief Get the number of children
    *
    * \return The number of children
    **/
    inline int getNumChildren() { return _children.size(); }

    /**
    * \brief Get a child by its index
    *
    * \param i Index of the child to get
    *
    * \return The child at the index
    **/
	inline ReferenceFrame* getChild( int i ) { return _children[i].get(); }

	/* Create a formatted string containing names of all descendants */
 	void createFrameString( std::string& str, std::string prefix = " " ) const;
 	
 	/* Information about this ReferenceFrame that is included in it's
 	    formatted name during a createFrameString() call */
 	virtual std::string frameInfo() const;

	/* Add/remove a frame as a parent of this one.  This is called
	    automatically by addChild, so should not be called manually. */
	void addParent( ReferenceFrame* frame );
	void removeParent( ReferenceFrame* frame );

    /**
    * \brief Get the number of parents
    *
    * \return The number of parents
    **/
    inline int getNumParents() const { return _parents.size(); }

    /**
    * \brief Get a parent by its index
    *
    * \param i Index of the parent to get
    *
    * \return The parent at the index
    **/
	inline ReferenceFrame* getParent( int i ) { return _parents[i]; }

	/* Add/remove a tracker for this frame */
	void addTracker( FrameTracker* t );
	void removeTracker( FrameTracker* t );

    /**
    * \brief Get the number of trackers
    *
    * \return The number of trackers
    **/
    inline int getNumTrackers() const { return _trackers.size(); }

    /**
    * \brief Get a tracker by its index
    *
    * \param i Index of the tracker to get
    *
    * \return The tracker at the index
    **/
	inline FrameTracker* getTracker( int i ) { return _trackers[i]; }

	/* Find the index of the requested child, parent, or tracker.
	    If the requested object does not exist, return -1 */
	int getChildIndex( const ReferenceFrame* frame ) const;
	int getParentIndex( const ReferenceFrame* frame ) const;
	int getTrackerIndex( const FrameTracker* t ) const;

  protected:
	virtual ~ReferenceFrame(); // Must be allocated on heap using 'new'

	std::string _name;  ///< Name of reference frame
    mutable osg::ref_ptr<Vector> _xAxis; ///< Vector of frame's x-axis
    mutable osg::ref_ptr<Vector> _yAxis; ///< Vector of frame's y-axis
    mutable osg::ref_ptr<Vector> _zAxis; ///< Vector of frame's z-axis
    mutable osg::ref_ptr<osgText::Text> _xLabel; ///< X-Axes label
    mutable osg::ref_ptr<osgText::Text> _yLabel; ///< Y-Axes label
    mutable osg::ref_ptr<osgText::Text> _zLabel; ///< Z-Axes label
    mutable osg::ref_ptr<osgText::Text> _nameLabel; ///< Name of reference frame that is displayed
	osg::ref_ptr<osg::Geode> _axes; ///< x,y,z axes together
    osg::ref_ptr<osg::Geode> _labels; ///< axes and name labels

	mutable osg::BoundingSphere _bound; ///< Frame's bounding sphere

	osg::ref_ptr<FrameTransform> _xform; ///< The transform that all contained objects will undergo

  private:
	void _init( const std::string &n, const osg::Vec4& c );

	ParentList _parents;  ///< All direct parents of this frame
	ChildList _children;  ///< All direct children of this frame
	TrackerList _trackers; ///< All trackers of this frame
};

}  // !namespace OpenFrames

#endif  // !define _OF_REFERENCEFRAME_
