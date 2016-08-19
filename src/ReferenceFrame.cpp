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

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/DescendantTracker.hpp>
#include <OpenFrames/Vector.hpp>

#include <osg/Geode>
#include <osg/PolygonMode>
#include <osg/ShapeDrawable>
#include <osgText/Text>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames{

ReferenceFrame::ReferenceFrame( const std::string &name ) 
{
	osg::Vec4 color(1, 1, 1, 0.9);
	_init(name, color);
}

ReferenceFrame::ReferenceFrame(const std::string &name, const osg::Vec3& color)
{
	osg::Vec4 newcolor(color, 0.9);
	_init(name, newcolor);
}

ReferenceFrame::ReferenceFrame(const std::string &name, const osg::Vec4& color) 
{ 
	_init(name, color);
}

ReferenceFrame::ReferenceFrame( const std::string &name, float r, float g, float b, float a ) 
{ 
	osg::Vec4 color(r, g, b, a);
	_init(name, color);
}

/** Destructor for ReferenceFrame. */
ReferenceFrame::~ReferenceFrame()
{
#ifdef _OF_VERBOSE_
	std::cout<< "~ReferenceFrame() for " << _name << std::endl;
#endif

	  // Remove each child from our child list
	while( !_children.empty() )
	  removeChild( _children.front().get() );

#ifdef _OF_VERBOSE_
	  // This should never evaluate true, since removeChild() should have told
	  // all trackers about children being removed.
	if( !_trackers.empty() )
	  std::cout<< "~ReferenceFrame() error: There are remaining trackers in frame " << _name << "!" << std::endl;

	std::cout<< "leaving ~ReferenceFrame() for " << _name << std::endl;
#endif
}

void ReferenceFrame::_init( const std::string &n, const osg::Vec4& c )
{
	  // Create the transform for this frame
	_xform = new FrameTransform; 

	  // Create x, y, and z axis vectors
	_xAxis = new Vector(osg::X_AXIS);
	_yAxis = new Vector(osg::Y_AXIS);
	_zAxis = new Vector(osg::Z_AXIS);

	float fontResolution = 30.0; // Resolution to use for all fonts

	// Create x, y, and z axis labels
	_xLabel = new osgText::Text;
	_yLabel = new osgText::Text;
	_zLabel = new osgText::Text;

	// Set the actual label text
	_xLabel->setText("X");
	_yLabel->setText("Y");
	_zLabel->setText("Z");

	// Make sure the labels will always be facing the screen
	_xLabel->setAxisAlignment(osgText::Text::SCREEN);
	_yLabel->setAxisAlignment(osgText::Text::SCREEN);
	_zLabel->setAxisAlignment(osgText::Text::SCREEN);

	// Text size will get smaller as the text gets further away, but will be limited to a maximum
	// size (defined by fontResolution) when the text is close up.
	_xLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);
	_yLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);
	_zLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);

	// This sets how "smooth" the text looks ... larger resolution looks nicer, but takes up more memory
	_xLabel->setFontResolution(fontResolution, fontResolution);
	_yLabel->setFontResolution(fontResolution, fontResolution);
	_zLabel->setFontResolution(fontResolution, fontResolution);

	// Set the text's font
	_xLabel->setFont("arial.ttf");
	_yLabel->setFont("arial.ttf");
	_zLabel->setFont("arial.ttf");

	// Create the name label
	_nameLabel = new osgText::Text;
	_nameLabel->setAutoRotateToScreen(true);
	_nameLabel->setCharacterSizeMode(osgText::Text::SCREEN_COORDS); // Constant font size regarless of distance from viewer
	_nameLabel->setFontResolution(fontResolution, fontResolution);
	_nameLabel->setCharacterSize(30.0);
	_nameLabel->setFont("arial.ttf");

	// Create group to which axes & labels will be added
	_axes = new osg::Geode;
	_xform->addChild(_axes.get());

	setName(n); // Set the name of this ReferenceFrame

	setColor(c); // Set the axes' color

	// Appropriately position the axes
	moveXAxis(osg::Vec3d(), 1.0); // At the origin, with a length of 1.0
	moveYAxis(osg::Vec3d(), 1.0);
	moveZAxis(osg::Vec3d(), 1.0);

	// Show the axes and labels
	showAxes(X_AXIS | Y_AXIS | Z_AXIS); // Show all axes
	showAxesLabels(X_AXIS | Y_AXIS | Z_AXIS); // Show all axes labels
	showNameLabel(true); // Show the frame's name label

	// Disable culling on the axes
	_axes->setCullingActive(false);

	// Rescale axes normals in case we have scales in the scene
	_axes->getOrCreateStateSet()->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::ON );
}

void ReferenceFrame::setName( const std::string& name )
{
	_name = name;
	_nameLabel->setText(name);
	_axes->setName(_name + " axes");
	_xform->setName(_name + " transform");
}

void ReferenceFrame::setColor( const osg::Vec4 &color )
{
	_xAxis->getVector()->setColor(color);
	_yAxis->getVector()->setColor(color);
	_zAxis->getVector()->setColor(color);
	_xLabel->setColor(color);
	_yLabel->setColor(color);
	_zLabel->setColor(color);
	_nameLabel->setColor(color);
}

void ReferenceFrame::setColor( float r, float g, float b, float a )
{
	setColor(osg::Vec4(r, g, b, a));
}

const osg::Vec4& ReferenceFrame::getColor() const
{
	return _xAxis->getVector()->getColor();
}

void ReferenceFrame::getColor(float &r, float &g, float &b, float &a) const
{
	const osg::Vec4& c = getColor();
	r = c[0];
	g = c[1];
	b = c[2];
	a = c[3];
}

osg::Group* ReferenceFrame::getGroup()
{
	return (osg::Group*)_xform.get();
}

const osg::BoundingSphere& ReferenceFrame::getBound() const
{
	_bound.init();
	if(_axes->containsDrawable(_xAxis->getVector()))
	  _bound.expandBy(_xAxis->getVector()->getBound());
	if(_axes->containsDrawable(_yAxis->getVector()))
	  _bound.expandBy(_yAxis->getVector()->getBound());
	if(_axes->containsDrawable(_zAxis->getVector()))
	  _bound.expandBy(_zAxis->getVector()->getBound());

	return _bound;
}

/** Show/hide the frame's x,y,z axes vectors */
void ReferenceFrame::showAxes(unsigned int axes)
{ 
	bool xexists = _axes->containsDrawable(_xAxis->getVector());
	bool yexists = _axes->containsDrawable(_yAxis->getVector());
	bool zexists = _axes->containsDrawable(_zAxis->getVector());

	if((axes & X_AXIS) && !xexists) // Need to add x-axis
	{
		_axes->addDrawable(_xAxis->getVector());
		xexists = true; // Reuse this variable to tell if x-labels need to be repositioned
	}
	else if(!(axes & X_AXIS) && xexists) // Need to remove x-axis
	{
		_axes->removeDrawable(_xAxis->getVector());
		xexists = true;
	}
	else xexists = false;

	if((axes & Y_AXIS) && !yexists) // Need to add y-axis
	{
		_axes->addDrawable(_yAxis->getVector());
		yexists = true; // Reuse this variable to tell if y-labels need to be repositioned
	}
	else if(!(axes & Y_AXIS) && yexists) // Need to remove y-axis
	{
		_axes->removeDrawable(_yAxis->getVector());
		yexists = true;
	}
	else yexists = false;

	if((axes & Z_AXIS) && !zexists) // Need to add z-axis
	{
		_axes->addDrawable(_zAxis->getVector());
		zexists = true; // Reuse this variable to tell if y-labels need to be repositioned
	}
	else if(!(axes & Z_AXIS) && zexists) // Need to remove z-axis
	{
		_axes->removeDrawable(_zAxis->getVector());
		zexists = true;
	}
	else zexists = false;

	// Reposition axes labels if needed
	if(xexists)
	  moveXAxis(_xAxis->getBasePosition(), _xAxis->getTotalLength());
	if(yexists)
	  moveYAxis(_yAxis->getBasePosition(), _yAxis->getTotalLength());
	if(zexists)
	  moveZAxis(_zAxis->getBasePosition(), _zAxis->getTotalLength());
}

void ReferenceFrame::showAxesLabels(unsigned int labels)
{
	bool xexists = _axes->containsDrawable(_xLabel.get());
	bool yexists = _axes->containsDrawable(_yLabel.get());
	bool zexists = _axes->containsDrawable(_zLabel.get());

	// Check if we need to add/remove x-axis label
	if((labels & X_AXIS) && !xexists) _axes->addDrawable(_xLabel.get());
	else if(!(labels & X_AXIS) && xexists) _axes->removeDrawable(_xLabel.get());

	// Check if we need to add/remove y-axis label
	if((labels & Y_AXIS) && !yexists) _axes->addDrawable(_yLabel.get());
	else if(!(labels & Y_AXIS) && yexists) _axes->removeDrawable(_yLabel.get());

	if((labels & Z_AXIS) && !zexists) // Need to add z-axis label
	{
		_axes->addDrawable(_zLabel.get());
		zexists = true; // Reuse this variable to tell if z-labels need to be repositioned
	}
	else if(!(labels & Z_AXIS) && zexists) // Need to remove z-axis label
	{
		_axes->removeDrawable(_zLabel.get());
		zexists = true;
	}
	else zexists = false;

	// Reposition z-axis labels if needed
	if(zexists)
	{
	  double bodyLen, headLen, bodyRadius, headRadius;
	  _zAxis->getLength(bodyLen, headLen);
	  _zAxis->getRadius(bodyRadius, headRadius);
	  double totalLen = bodyLen + headLen;
	  moveZAxis(_zAxis->getBasePosition(), totalLen, headLen/totalLen, bodyRadius, headRadius);
	}
}

void ReferenceFrame::showNameLabel(bool show)
{
	// Check if the name label is already being drawn
	bool exists = _axes->containsDrawable(_nameLabel.get());

	// Add the name label to the list of objects to be drawn
	if(show && !exists) _axes->addDrawable(_nameLabel.get());

	// Remove the name label from the list of objects to be drawn
	else if(!show && exists) _axes->removeDrawable(_nameLabel.get());
}

void ReferenceFrame::moveXAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
{
	bool xexists = _axes->containsDrawable(_xAxis->getVector());

	if(headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
	if(bodyRadius <= 0.0) bodyRadius = 0.05*len;
	if(headRadius <= 0.0) headRadius = 0.1*len;

	_xAxis->setBasePosition(base);
	_xAxis->setLength((1.0-headRatio)*len, headRatio*len);
	_xAxis->setRadius(bodyRadius, headRadius);

	_xLabel->setCharacterSize(0.4*len);
	if(xexists) _xLabel->setPosition(base + osg::Vec3d(len, 0, 0));
	else _xLabel->setPosition(base);
}
	
void ReferenceFrame::moveYAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
{
	bool yexists = _axes->containsDrawable(_yAxis->getVector());

	if(headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
	if(bodyRadius <= 0.0) bodyRadius = 0.05*len;
	if(headRadius <= 0.0) headRadius = 0.1*len;

	_yAxis->setBasePosition(base);
	_yAxis->setLength((1.0-headRatio)*len, headRatio*len);
	_yAxis->setRadius(bodyRadius, headRadius);

	_yLabel->setCharacterSize(0.4*len);
	if(yexists) _yLabel->setPosition(base + osg::Vec3d(0, len, 0));
	else _yLabel->setPosition(base);
}
	
void ReferenceFrame::moveZAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
{
	bool zaxisexists = _axes->containsDrawable(_zAxis->getVector());
	bool zlabelexists = _axes->containsDrawable(_zLabel.get());

	if(headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
	if(bodyRadius <= 0.0) bodyRadius = 0.05*len;
	if(headRadius <= 0.0) headRadius = 0.1*len;

	_zAxis->setBasePosition(base);
	_zAxis->setLength((1.0-headRatio)*len, headRatio*len);
	_zAxis->setRadius(bodyRadius, headRadius);

	_zLabel->setCharacterSize(0.4*len);
	if(zaxisexists)
	{
	  _zLabel->setPosition(base + osg::Vec3d(0, 0, len));
	  if(zlabelexists)
	    _nameLabel->setPosition(base + osg::Vec3d(0, 0, 1.5*len));
	  else
	    _nameLabel->setPosition(base + osg::Vec3d(0, 0, len));
	}
	else 
	{
	  _zLabel->setPosition(base);
	  if(zlabelexists)
	    _nameLabel->setPosition(base + osg::Vec3d(0, 0, 0.5*len));
	  else
	    _nameLabel->setPosition(base);
	}
}

/** Add a ReferenceFrame as a child to this one.  This effectively adds the osg 
  structure of that frame as a child to this frame's transform */
bool ReferenceFrame::addChild( ReferenceFrame* child )
{
	  // Make sure we're not trying to add ourselves as a child
	  // Also make sure child is not NULL
	if(child == this || !child) return false;

	  // Make sure child does not already exist
	if(getChildIndex(child) != -1) return true;

	  // Check to see if we are a descendant of the child.
	  // This case would cause a loop in the tree structure.
	osg::ref_ptr<DescendantTracker> dt = new DescendantTracker(child);
	if(dt->trackDescendant(this)) 
	{
#ifdef _OF_VERBOSE_
	  std::cout<< "ReferenceFrame ERROR: Trying to add child "
	      << child->getName() << " to parent " << getName()
	      << ", but " << getName() << " is already a child of "
	      << child->getName() << "!" << std::endl;
#endif

	    // Remove the child from the tracker without deleting it in case
	    // the child is still being used somewhere else.
	  osg::ref_ptr<ReferenceFrame> temp = child;
	  dt->setRoot(NULL);
	  temp.release(); 

	  return false;
	}

	  // Register this frame as a parent of the child
	child->addParent( this );

	  // Add osg structure of child under the transform of this frame
	_xform->addChild(child->getGroup());

	  // Add child to this frame
	_children.push_back(child);

	  // Tell each tracker that the child was added to this frame.
	int num__trackers = _trackers.size();
	for(int i = 0; i < num__trackers; ++i)
	  _trackers[i]->childAdded(child, this);

	return true;
}

/**
  Remove a ReferenceFrame from the children of this one.  This effectively
  removes the osg structure of that frame from this frame's transform */
bool ReferenceFrame::removeChild( ReferenceFrame* child )
{
	int index = getChildIndex(child);

	  // Make sure the child exists
	if( index == -1 ) return false;

	  // Remove osg structure of child from this frame's transform
	_xform->removeChild(child->getGroup());

	  // Deregister this frame as a parent of the child
	child->removeParent(this);

	  // Remove child frame from this frame's child list, but save
	  // it first in case a tracker needs it.
	osg::ref_ptr<ReferenceFrame> temp = child;
	_children.erase(_children.begin() + index);

	  // Inform _trackers about child's removal
	int num__trackers = _trackers.size();
	for(int i = 0; i < num__trackers; ++i)
	  _trackers[i]->childRemoved(child, this);

	return true;
}

/** Create a formatted string containing names of all child frames */
void ReferenceFrame::createFrameString( std::string& str, std::string prefix ) const
{
	str += prefix + _name + " (" + frameInfo() + ")\n";
	if( prefix[prefix.size() - 1] == '>')
	  prefix[prefix.size() - 1] = '-';
	prefix += "|-->";

	int num__children = _children.size();
	for( int i = 0; i < num__children; ++i )
	  _children[i]->createFrameString( str, prefix );
}

std::string ReferenceFrame::frameInfo() const
{
	return "ReferenceFrame";
}

/** Add/remove a frame as a parent of this one */
void ReferenceFrame::addParent( ReferenceFrame* frame )
{
	if( getParentIndex(frame) == -1 ) _parents.push_back(frame);
}

void ReferenceFrame::removeParent( ReferenceFrame* frame )
{
	int index = getParentIndex(frame);
	if( index != -1 ) _parents.erase(_parents.begin() + index);
}

/** Add/remove a tracker for this frame */
void ReferenceFrame::addTracker( FrameTracker* t )
{
	if( getTrackerIndex(t) == -1 ) _trackers.push_back(t);
}

void ReferenceFrame::removeTracker( FrameTracker* t )
{
	int index = getTrackerIndex(t);
	if( index != -1 ) _trackers.erase(_trackers.begin() + index);
}

/** Find the index of the requested child, parent or tracker.  If the requested object does not exist, then return -1 */
int ReferenceFrame::getChildIndex( const ReferenceFrame* frame ) const
{
	int num__children = _children.size();
	for( int i = 0; i < num__children; ++i )
	  if( _children[i] == frame ) return i;
	
	return -1;
}

int ReferenceFrame::getParentIndex( const ReferenceFrame* frame ) const
{
	int num_parents = _parents.size();
	for( int i = 0; i < num_parents; ++i )
	  if( _parents[i] == frame ) return i;
	
	return -1;
}

int ReferenceFrame::getTrackerIndex( const FrameTracker* frame ) const
{
	int num_trackers = _trackers.size();
	for( int i = 0; i < num_trackers; ++i )
	  if( _trackers[i] == frame ) return i;
	
	return -1;
}

} // !namespace OpenFrames
