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

/** \file ReferenceFrame.cpp
 * ReferenceFrame-class function definitions.
 */

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/DescendantTracker.hpp>
#include <OpenFrames/Vector.hpp>

#include <osg/Geode>
#include <osg/PolygonMode>
#include <osg/ShapeDrawable>
#include <osgDB/FileNameUtils>
#include <osgText/Text>

#ifdef _OF_VERBOSE_
#include <iostream>
#endif

namespace OpenFrames
{

  // Convenience constants for enabling/disabling nodes 
  const osg::Node::NodeMask enabled = 0xffffffff;
  const osg::Node::NodeMask disabled = 0x0;

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

void ReferenceFrame::_init( const std::string &name, const osg::Vec4& c )
{
	  // Create the transform for this frame
	_xform = new FrameTransform; 

	  // Create x, y, and z axis vectors
	_xAxis = new Vector(osg::X_AXIS);
	_yAxis = new Vector(osg::Y_AXIS);
	_zAxis = new Vector(osg::Z_AXIS);
  
	// Create labels
	_xLabel = new osgText::Text;
	_yLabel = new osgText::Text;
	_zLabel = new osgText::Text;
  _nameLabel = new osgText::Text;

	// Make sure the labels will always be facing the screen
	_xLabel->setAxisAlignment(osgText::Text::SCREEN);
	_yLabel->setAxisAlignment(osgText::Text::SCREEN);
	_zLabel->setAxisAlignment(osgText::Text::SCREEN);
  _nameLabel->setAxisAlignment(osgText::Text::SCREEN);

	// X/Y/Z label text size grows as the text gets closer, but is limited to a maximum size (fontResolution)
	_xLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);
	_yLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);
	_zLabel->setCharacterSizeMode(osgText::Text::OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE_CAPPED_BY_FONT_HEIGHT);
  
  // Name label text is constant size regardless of distance from viewer
  _nameLabel->setCharacterSizeMode(osgText::Text::SCREEN_COORDS);

  // Sets how "smooth" the text looks ... larger resolution looks nicer, but takes up more memory
  // Also sets the maximum height of the font when it grows with distance
  setLabelSize(20.0);

  // Set label font
  setLabelFont("arial.ttf");

  // Set label text
  _xLabel->setText("X");
  _yLabel->setText("Y");
  _zLabel->setText("Z");
  _nameLabel->setText(name);

	// Create groups to which axes & labels will be added
	_axes = new osg::Geode;
    _labels = new osg::Geode;
	_xform->addChild(_axes.get());
    _xform->addChild(_labels.get());

    // Add axes and labels to their groups
    _axes->addDrawable(_xAxis->getVector());
    _axes->addDrawable(_yAxis->getVector());
    _axes->addDrawable(_zAxis->getVector());
    _labels->addDrawable(_xLabel);
    _labels->addDrawable(_yLabel);
    _labels->addDrawable(_zLabel);
    _labels->addDrawable(_nameLabel);

	setName(name); // Set the name of this ReferenceFrame

	setColor(c); // Set the axes' color

	// Appropriately position the axes and labels
	moveXAxis(osg::Vec3d(), 1.0); // At the origin, with a length of 1.0
	moveYAxis(osg::Vec3d(), 1.0);
	moveZAxis(osg::Vec3d(), 1.0);

	// Show the axes and labels
	showAxes(X_AXIS | Y_AXIS | Z_AXIS); // Show all axes
	showAxesLabels(X_AXIS | Y_AXIS | Z_AXIS); // Show all axes labels
	showNameLabel(true); // Show the frame's name label

	// Disable culling on the axes and labels
	_axes->setCullingActive(false);
    _labels->setCullingActive(false);

	// Rescale axes normals in case we have scales in the scene
	_axes->getOrCreateStateSet()->setMode( GL_RESCALE_NORMAL, osg::StateAttribute::ON );

    // Disable lighting for labels
    _labels->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
}

  void ReferenceFrame::_resetTextGlyphs()
  {
    // Some graphics drivers have a bug where text can't be properly changed.
    // Get around this by initializing text using all likely characters.
    std::string dummyText("the quick brown fox jumps over the lazy dog"); // Lowercase
    dummyText += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"; // Uppercase
    dummyText += "1234567890";   // Numbers
    dummyText += "~@#$%^&*_|\\"; // Symbols
    dummyText += "[]{}()<>";     // Brackets
    dummyText += ",.!?;:";       // Punctuation
    dummyText += "+-*/=";        // Math
    dummyText += "`'\"";         // Quotes
    
    // Recompute glyphs
    _xLabel->setText(dummyText);
    _yLabel->setText(dummyText);
    _zLabel->setText(dummyText);
    _nameLabel->setText(dummyText);
  }
  
  void ReferenceFrame::setName( const std::string& name )
  {
    _name = name;
    _nameLabel->setText(name);
    _axes->setName(_name + " axes");
    _labels->setName(_name + " labels");
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

  osg::Group* ReferenceFrame::getGroup() const
  {
    if(_shadowedSceneRoot == nullptr) return (osg::Group*)_xform.get();
    else return (osg::Group*)_shadowedSceneRoot.get();
  }

const osg::BoundingSphere& ReferenceFrame::getBound() const
{
	_bound.init();
    if (_xAxis->getVector()->getNodeMask() != 0x0)
	  _bound.expandBy(_xAxis->getVector()->getBound());
    if (_yAxis->getVector()->getNodeMask() != 0x0)
	  _bound.expandBy(_yAxis->getVector()->getBound());
    if (_zAxis->getVector()->getNodeMask() != 0x0)
	  _bound.expandBy(_zAxis->getVector()->getBound());

	return _bound;
}

  void ReferenceFrame::showAxes(unsigned int axes)
  { 
    // Disable entire axes geode if there's nothing to show
    if (axes == NO_AXES) _axes->setNodeMask(disabled);
    else _axes->setNodeMask(enabled);

    if (axes & X_AXIS) // Need to enable x-axis
      _xAxis->getVector()->setNodeMask(enabled);
    else
      _xAxis->getVector()->setNodeMask(disabled);

    if (axes & Y_AXIS) // Need to enable y-axis
      _yAxis->getVector()->setNodeMask(enabled);
    else
      _yAxis->getVector()->setNodeMask(disabled);

    if (axes & Z_AXIS) // Need to enable z-axis
      _zAxis->getVector()->setNodeMask(enabled);
    else
      _zAxis->getVector()->setNodeMask(disabled);
	
    // Reposition axes labels
    moveXAxis(_xAxis->getBasePosition(), _xAxis->getTotalLength());
    moveYAxis(_yAxis->getBasePosition(), _yAxis->getTotalLength());
    moveZAxis(_zAxis->getBasePosition(), _zAxis->getTotalLength());
  }

void ReferenceFrame::showAxesLabels(unsigned int labels)
{
  // Disable entire label geode if there's nothing to show
  if ((labels == NO_AXES) && (_nameLabel->getNodeMask() == disabled)) _labels->setNodeMask(disabled);
  else _labels->setNodeMask(enabled);

  // Enable x-label
  if (labels & X_AXIS) _xLabel->setNodeMask(enabled);
  else _xLabel->setNodeMask(disabled);

  // Enable y-label
  if (labels & Y_AXIS) _yLabel->setNodeMask(enabled);
  else _yLabel->setNodeMask(disabled);

  // Enable z-label
  if (labels & Z_AXIS) _zLabel->setNodeMask(enabled);
  else _zLabel->setNodeMask(disabled);

  // Reposition z-axis label
  double bodyLen, headLen, bodyRadius, headRadius;
  _zAxis->getLength(bodyLen, headLen);
  _zAxis->getRadius(bodyRadius, headRadius);
  double totalLen = bodyLen + headLen;
  moveZAxis(_zAxis->getBasePosition(), totalLen, headLen/totalLen, bodyRadius, headRadius);
}

  void ReferenceFrame::showNameLabel(bool show)
  {
    // Disable entire label geode if there's nothing to show
    if (!show && ((_xLabel->getNodeMask() | _yLabel->getNodeMask() | _zLabel->getNodeMask()) == disabled))
        _labels->setNodeMask(disabled);
    else
        _labels->setNodeMask(enabled);

    // Enable name label
    if (show) _nameLabel->setNodeMask(enabled);
    else _nameLabel->setNodeMask(disabled);
  }

  void ReferenceFrame::moveXAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
  {
    bool xexists = (_xAxis->getVector()->getNodeMask() == enabled);

    if (headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
    if (bodyRadius <= 0.0) bodyRadius = 0.05*len;
    if (headRadius <= 0.0) headRadius = 0.1*len;

    _xAxis->setBasePosition(base);
    _xAxis->setLength((1.0-headRatio)*len, headRatio*len);
    _xAxis->setRadius(bodyRadius, headRadius);

    _xLabel->setCharacterSize(0.4*len);
    if (xexists) _xLabel->setPosition(base + osg::Vec3d(len, 0, 0));
    else _xLabel->setPosition(base);
  }

  void ReferenceFrame::moveYAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
  {
    bool yexists = (_yAxis->getVector()->getNodeMask() == enabled);

    if (headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
    if (bodyRadius <= 0.0) bodyRadius = 0.05*len;
    if (headRadius <= 0.0) headRadius = 0.1*len;

    _yAxis->setBasePosition(base);
    _yAxis->setLength((1.0-headRatio)*len, headRatio*len);
    _yAxis->setRadius(bodyRadius, headRadius);

    _yLabel->setCharacterSize(0.4*len);
    if (yexists) _yLabel->setPosition(base + osg::Vec3d(0, len, 0));
    else _yLabel->setPosition(base);
  }

  void ReferenceFrame::moveZAxis(osg::Vec3d base, double len, double headRatio, double bodyRadius, double headRadius) const
  {
    bool zaxisexists = (_zAxis->getVector()->getNodeMask() == enabled);
    bool zlabelexists = (_zLabel->getNodeMask() == enabled);

    if (headRatio <= 0.0 || headRatio >= 1.0) headRatio = 0.3;
    if (bodyRadius <= 0.0) bodyRadius = 0.05*len;
    if (headRadius <= 0.0) headRadius = 0.1*len;

    _zAxis->setBasePosition(base);
    _zAxis->setLength((1.0 - headRatio)*len, headRatio*len);
    _zAxis->setRadius(bodyRadius, headRadius);

    _zLabel->setCharacterSize(0.4*len);
    if (zaxisexists)
    {
      _zLabel->setPosition(base + osg::Vec3d(0, 0, len));
      if (zlabelexists)
        _nameLabel->setPosition(base + osg::Vec3d(0, 0, 1.5*len));
      else
        _nameLabel->setPosition(base + osg::Vec3d(0, 0, len));
    }
    else
    {
      _zLabel->setPosition(base);
      if (zlabelexists)
        _nameLabel->setPosition(base + osg::Vec3d(0, 0, 0.5*len));
      else
        _nameLabel->setPosition(base);
    }
  }

  void ReferenceFrame::setLabelFont(const std::string &font)
  {
    // Save current label text
    std::string prevXLabel = _xLabel->getText().createUTF8EncodedString();
    std::string prevYLabel = _yLabel->getText().createUTF8EncodedString();
    std::string prevZLabel = _zLabel->getText().createUTF8EncodedString();
    std::string prevNameLabel = _nameLabel->getText().createUTF8EncodedString();

    // Empty out text in preparation for new font
    _xLabel->setText("");
    _yLabel->setText("");
    _zLabel->setText("");
    _nameLabel->setText("");

    // Set the new font
    _xLabel->setFont(font);
    _yLabel->setFont(font);
    _zLabel->setFont(font);
    _nameLabel->setFont(font);
    
    // Initialize text with all printable characters to support older graphics drivers
    _resetTextGlyphs();
    
    // Restore label text
    _xLabel->setText(prevXLabel);
    _yLabel->setText(prevYLabel);
    _zLabel->setText(prevZLabel);
    _nameLabel->setText(prevNameLabel);
  }
  
  std::string ReferenceFrame::getLabelFontName() const
  {
    return osgDB::getSimpleFileName(getLabelFontPath());
  }
  
  std::string ReferenceFrame::getLabelFontPath() const
  {
    const osgText::Font* font = _xLabel->getFont();
    std::string fontFile = font ? font->getFileName() : "default";
    return fontFile;
  }
  
  void ReferenceFrame::setLabelSize(unsigned int size)
  {
    // Set size for axes labels (treated as maximum size)
    _xLabel->setFontResolution(size, size);
    _yLabel->setFontResolution(size, size);
    _zLabel->setFontResolution(size, size);
    
    // Set size for name label (treated as fixed size)
    _nameLabel->setFontResolution(size, size);
    _nameLabel->setCharacterSize(size);
  }
  
bool ReferenceFrame::addChild( ReferenceFrame* child )
{
	  // Make sure we're not trying to add ourselves as a child
	  // Also make sure child is not NULL
	if ((child == this) || (child == nullptr)) return false;

	  // Make sure child does not already exist
	if (getChildIndex(child) != -1) return true;

	  // Check to see if we are a descendant of the child.
	  // This case would cause a loop in the tree structure.
	osg::ref_ptr<DescendantTracker> dt = new DescendantTracker(child);
	if (dt->trackDescendant(this)) 
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
  
/***************************************/
void ReferenceFrame::setLightSourceEnabled(bool enable)
{
  // Find existing LightSource
  osg::LightSource* lightSource = getLightSource();
  if(lightSource)
  {
    // Light source already exists, so just enable/disable it as needed
    if(enable) lightSource->setNodeMask(0xffffffff);
    else lightSource->setNodeMask(0x0);
  }
  
  // Create light source if it doesn't exist but should be enabled
  else if(enable)
  {
    // Create new LightSource at center of this frame
    lightSource = new osg::LightSource;
    lightSource->getLight()->setPosition(osg::Vec4(0.0, 0.0, 0.0, 1.0));
    _xform->addChild(lightSource);
  }
}

/***************************************/
bool ReferenceFrame::getLightSourceEnabled() const
{
  // Find existing LightSource
  osg::LightSource* lightSource = getLightSource();
  if(lightSource) return (lightSource->getNodeMask() != 0x0);
  else return false;
}
  
/***************************************/
osg::LightSource* ReferenceFrame::getLightSource() const
{
  // Find existing LightSource
  for(unsigned int i = 0; i < _xform->getNumChildren(); ++i)
  {
    osg::LightSource* lightSource = dynamic_cast<osg::LightSource*>(_xform->getChild(i));
    if(lightSource) return lightSource;
  }
  return NULL; // Light source doesn't exist
}
  
  void ReferenceFrame::setShadowedSceneRoot(bool isRoot)
  {
    if(!isRoot) _shadowedSceneRoot = nullptr;
    else if(_shadowedSceneRoot == nullptr)
    {
      _shadowedSceneRoot = new osgShadow::ShadowedScene;
      _shadowedSceneRoot->addChild(_xform);
    }
  }
  
  osgShadow::ShadowedScene* ReferenceFrame::getShadowedSceneRoot() const
  {
    return _shadowedSceneRoot;
  }

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

    void ReferenceFrame::addParent( ReferenceFrame* frame )
    {
      if ( getParentIndex(frame) == -1 ) _parents.push_back(frame);
    }

    void ReferenceFrame::removeParent( ReferenceFrame* frame )
    {
      int index = getParentIndex(frame);
      if ( index != -1 ) _parents.erase(_parents.begin() + index);
    }

    void ReferenceFrame::addTracker( FrameTracker* t )
    {
      if ( getTrackerIndex(t) == -1 ) _trackers.push_back(t);
    }

    void ReferenceFrame::removeTracker( FrameTracker* t )
    {
      int index = getTrackerIndex(t);
      if ( index != -1 ) _trackers.erase(_trackers.begin() + index);
    }

    int ReferenceFrame::getChildIndex( const ReferenceFrame* frame ) const
    {
      int num__children = _children.size();
      for( int i = 0; i < num__children; ++i )
      if( _children[i] == frame ) return i;

      return -1;
    }

    int ReferenceFrame::getParentIndex(const ReferenceFrame* frame) const
    {
      int num_parents = _parents.size();
      for (int i=0; i<num_parents; ++i)
      {
        if (_parents[i] == frame)
          return i;
      }

      return -1;
    }

    int ReferenceFrame::getTrackerIndex( const FrameTracker* frame ) const
    {
      int num_trackers = _trackers.size();
      for (int i=0; i<num_trackers; ++i)
      {
        if (_trackers[i] == frame)
          return i;
      }

      return -1;
    }

} // !namespace OpenFrames
