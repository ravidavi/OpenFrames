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

#ifndef _OF_REFERENCEFRAME_
#define _OF_REFERENCEFRAME_

#include <OpenFrames/Export.h>
#include <OpenFrames/FrameTransform.hpp> 
#include <OpenFrames/Vector.hpp>

#include <osg/Geode>
#include <osgText/Text>
#include <osg/Referenced>
#include <osg/ref_ptr>

#include <vector>
#include <string>

namespace osg {
  class Vec3f;
  class Vec4f;
  typedef Vec3f Vec3;
  typedef Vec4f Vec4;
}

namespace OpenFrames {

class FrameTracker;
class Trajectory;

/*******************************************
 * Ravi Mathur
 * OpenFrames API, class ReferenceFrame
 * This class defines the standard functions of a classical 
 * reference frame.  A reference frame can only contain other reference
 * frames, so all objects should be derived from this class.
******************************************/
class OF_EXPORT ReferenceFrame : public osg::Referenced {
  public:
	typedef std::vector<ReferenceFrame*> ParentList;
	typedef std::vector<osg::ref_ptr<ReferenceFrame> > ChildList;
	typedef std::vector<FrameTracker*> TrackerList;

	ReferenceFrame( const std::string &name );
	ReferenceFrame( const std::string &name, const osg::Vec3 &color );
	ReferenceFrame( const std::string &name, const osg::Vec4 &color );
	ReferenceFrame( const std::string &name , float r, float g, float b, float a = 1.0 );

	/** Set the name of the frame that will be displayed */
	void setName( const std::string &name );
	inline const std::string& getName() const { return _name; }

	/** Set the color of the frame's decorations (axes, name, ...)
	    This method can be overridden by derived classes */
	virtual void setColor( const osg::Vec4 &color );
	virtual void setColor( float r, float g, float b, float a = 1.0 );
	virtual const osg::Vec4& getColor() const;
	virtual void getColor( float &r, float &g, float &b, float &a ) const;

	/** Get the transform corresponding to this ReferenceFrame */
	inline FrameTransform* getTransform() {return _xform.get();}

	/** Get the group corresponding to this ReferenceFrame. By default, the
	    frame's group is the same as its transform.  However, subclasses
	    can define a separate group if they wish to. A child frame's group is
	    what is added to a parent frame's transform in addChild(). */
	virtual osg::Group* getGroup();

	/** Set the position/orientation of this frame. This only applies if
	    the frame is not being auto positioned by a TrajectoryFollower. */
	inline void setPosition( const double &x, const double &y, const double &z )
	{ _xform->setPosition(x, y, z); }

	inline void getPosition( double &x, double &y, double &z ) const
	{ _xform->getPosition(x, y, z); }

	inline void setAttitude( const double &rx, const double &ry, const double &rz, const double &angle )
	{ _xform->setAttitude(rx, ry, rz, angle); }

	inline void getAttitude( double &rx, double &ry, double &rz, double &angle) const
	{ _xform->getAttitude(rx, ry, rz, angle); }
	
	/** Get the BoundingSphere encompassing this frame plus all of its
	    decorations. Derived classes should override this method
	    and compute their own local BoundingSphere. */ 
	virtual const osg::BoundingSphere& getBound() const;

	enum AxesType // Specifies which axes to draw
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

	/** Place x/y/z axis vectors at the given location with given length */
	void moveXAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;
	void moveYAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;
	void moveZAxis(osg::Vec3d base, double len, double headRatio = 0.3, double bodyRadius = 0.0, double headRadius = 0.0) const;

	/** Set the text displayed for the axes labels.
	    The default is 'X', 'Y', and 'Z' for the respective axes. */
	inline void setXLabel(const std::string &str) { _xLabel->setText(str); }
	inline void setYLabel(const std::string &str) { _yLabel->setText(str); }
	inline void setZLabel(const std::string &str) { _zLabel->setText(str); }

	/** Add/remove a frame as a child to this one */
	bool addChild( ReferenceFrame* frame );
	bool removeChild( ReferenceFrame* frame );

	/** Get a child by its index */
	inline int getNumChildren() { return _children.size(); }
	inline ReferenceFrame* getChild( int i ) { return _children[i].get(); }

	/** Create a formatted string containing names of all descendants */
 	void createFrameString( std::string& str, std::string prefix = " " ) const;
 	
 	/** Information about this ReferenceFrame that is included in it's
 	    formatted name during a createFrameString() call */
 	virtual std::string frameInfo() const;

	/** Add/remove a frame as a parent of this one.  This is called
	    automatically by addChild, so should not be called manually. */
	void addParent( ReferenceFrame* frame );
	void removeParent( ReferenceFrame* frame );

	/** Get a parent by its index */
	inline int getNumParents() const { return _parents.size(); }
	inline ReferenceFrame* getParent( int i ) { return _parents[i]; }

	/** Add/remove a tracker for this frame */
	void addTracker( FrameTracker* t );
	void removeTracker( FrameTracker* t );

	/** Get a tracker by its index */	
	inline int getNumTrackers() const { return _trackers.size(); }
	inline FrameTracker* getTracker( int i ) { return _trackers[i]; }

	/** Find the index of the requested child, parent, or tracker.
	    If the requested object does not exist, return -1 */
	int getChildIndex( const ReferenceFrame* frame ) const;
	int getParentIndex( const ReferenceFrame* frame ) const;
	int getTrackerIndex( const FrameTracker* t ) const;

  protected:
	virtual ~ReferenceFrame(); // Must be allocated on heap using 'new'

	std::string _name;  // Name of reference frame
	mutable osg::ref_ptr<Vector> _xAxis, _yAxis, _zAxis; // Frame's vectors
	mutable osg::ref_ptr<osgText::Text> _xLabel, _yLabel, _zLabel; // Axes labels
	mutable osg::ref_ptr<osgText::Text> _nameLabel;
	osg::ref_ptr<osg::Geode> _axes; // x,y,z axes together

	mutable osg::BoundingSphere _bound; // Frame's bounding sphere

	  // The transform that all contained objects will undergo
	osg::ref_ptr<FrameTransform> _xform;

  private:
	void _init( const std::string &n, const osg::Vec4& c );

	ParentList _parents;  // All direct parents of this frame
	ChildList _children;  // All direct children of this frame
	TrackerList _trackers; // All trackers of this frame
};

}  // !namespace OpenFrames

#endif  // !define _OF_REFERENCEFRAME_
