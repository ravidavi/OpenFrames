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

#include <OpenFrames/RenderRectangle.hpp>
#include <osg/Geometry>
#include <osg/Group>
#include <osg/LineWidth>
#include <osg/Depth>
#include <osg/StateSet>

namespace OpenFrames
{
  
  RenderRectangle::RenderRectangle()
  {
    // Create the Camera that will draw the border.
    _border = new osg::Camera;
    
    // Create the auto depth partitioning node
    _depthPartition = new DepthPartitionNode;
    
    // The scene contains the user's scene along with decorations
    _scene = new osg::Group;
    
    // Create the sky sphere but don't draw it yet
    _skySphere = new SkySphere("Sky Sphere");
    _skySphere->setDrawMode(SkySphere::NONE);
    
    // The SceneView is responsible for doing the update, cull, and
    // draw operations for the ReferenceFrame scene.
    _sceneView = new osgViewer::View();
    
    // Create a default view and make it active
    _defaultView = new View;
    _currView = 0;
    _sceneView->setCameraManipulator(getCurrentView()->getTrackball());
    
    // Initialize parameters
    _init();
  }
  
  RenderRectangle::~RenderRectangle() { }
  
  void RenderRectangle::_init()
  {
    // Create and set up the geode that will contain the border
    osg::Geode *geode = new osg::Geode;
    geode->setCullingActive(false);
    osg::StateSet* ss = geode->getOrCreateStateSet();
    ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
    ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
    ss->setAttribute(new osg::LineWidth);
    
    // Specify points that make up the border
    osg::Vec3Array* vertices = new osg::Vec3Array;
    vertices->push_back(osg::Vec3(0, 0, -1));
    vertices->push_back(osg::Vec3(0.999, 0, -1));
    vertices->push_back(osg::Vec3(0.999, 0.999, -1));
    vertices->push_back(osg::Vec3(0, 0.999, -1));
    
    // Create a dummy color for the border
    osg::Vec4Array *colors = new osg::Vec4Array;
    colors->push_back(osg::Vec4(0, 0, 0, 1.0));
    
    // Set up the geometry object for the border
    osg::Geometry *geom = new osg::Geometry;
    geom->setVertexArray(vertices);
    geom->setColorArray(colors);
    geom->setColorBinding(osg::Geometry::BIND_OVERALL);
    geom->addPrimitiveSet(new osg::DrawArrays(GL_LINE_LOOP, 0, 4));
    geode->addDrawable(geom);
    
    // Set up the border itself
    _border->addChild(geode);
    _border->setProjectionMatrix(osg::Matrix::ortho2D(0, 1.0, 0, 1.0));
    _border->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    _border->setViewMatrix(osg::Matrix::identity());
    _border->setClearMask(GL_DEPTH_BUFFER_BIT);
    _border->setRenderOrder(osg::Camera::POST_RENDER);
    
    setSelected(false); // Set the border to look deselected
    
    // Tell the depth partitioner to not clear the color buffer before drawing, so that decorations don't get erased
    _depthPartition->setClearColorBuffer(false);
    
    // Add decorations to the scene
    _scene->addChild(_depthPartition.get());
    _scene->addChild(_border.get());
    _scene->addChild(_skySphere->getGroup());
    
    // Set up the SceneView
    _sceneView->setSceneData(_scene.get());
    _sceneView->getCamera()->setCullingMode(osg::CullSettings::DEFAULT_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
  }
  
  void RenderRectangle::setFrameManager(FrameManager *fm)
  {
    // We're already using the given FrameManager
    if(_frameManager.get() == fm) return;
    
    if(_frameManager.valid()) // Remove the old scene
      _depthPartition->removeChild(_frameManager->getData());
    
    // Set a default view
    if(fm == NULL)
    {
      _defaultView->setViewFrame(NULL, NULL);
      _defaultView->resetTrackball();
    }
    else
    {
      _depthPartition->addChild(fm->getData()); // Set the new scene
      _defaultView->setViewFrame(fm->getFrame(), fm->getFrame());
      _defaultView->resetTrackball();
    }
    
    _frameManager = fm; // Set the new FrameManager
  }
  
  void RenderRectangle::setViewport(int x, int y, int w, int h)
  {
    _sceneView->getCamera()->setViewport(x, y, w, h);
  }
  
  /** Set this RenderRectangle to have a "selected" or "deselected" look */
  void RenderRectangle::setSelected(bool select)
  {
    osg::Geode *geode = dynamic_cast<osg::Geode*>(_border->getChild(0));
    osg::Geometry *geom = dynamic_cast<osg::Geometry*>(geode->getDrawable(0));
    
    osg::Vec4Array *colors = dynamic_cast<osg::Vec4Array*>(geom->getColorArray());
    colors->pop_back(); // Remove the current color
    if(select)
    {
      colors->push_back(osg::Vec4(1.0, 0, 0, 1.0)); // Add a red color
    }
    else
    {
      colors->push_back(osg::Vec4(0, 1.0, 0, 1.0)); // Add a green color
    }
    
    osg::LineWidth *lw = dynamic_cast<osg::LineWidth*>(geode->getStateSet()->getAttribute(osg::StateAttribute::LINEWIDTH));
    if(select)
    {
      lw->setWidth(3.0); // Set a 3.0 line width
    }
    else
    {
      lw->setWidth(1.0); // Set a 1.0 line width
    }
    
    geom->dirtyDisplayList(); // Indicate that the border should be redrawn
  }
  
  /** Get the selected state of this RenderRectangle */
  bool RenderRectangle::getSelected()
  {
    osg::Geode *geode = dynamic_cast<osg::Geode*>(_border->getChild(0));
    osg::LineWidth *lw = dynamic_cast<osg::LineWidth*>(geode->getStateSet()->getAttribute(osg::StateAttribute::LINEWIDTH));
    if(lw->getWidth() == 1.0) return false;
    else return true;
  }
  
  /** Set whether the border rectangle is shown or not */
  void RenderRectangle::setShowBorder(bool show)
  {
    if(show && !_scene->containsNode(_border.get())) _scene->addChild(_border.get());
    else if(!show) _scene->removeChild(_border.get());
  }
  
  void RenderRectangle::setSkySphereTexture(const std::string& fname)
  {
    unsigned int currDrawMode = _skySphere->getDrawMode();
    if(_skySphere->setTextureMap(fname))
      // Add TEXTURE to existing drawmode
      _skySphere->setDrawMode(currDrawMode | SkySphere::TEXTURE);
    else
      // Remove TEXTURE from existing drawmode
      _skySphere->setDrawMode(currDrawMode & ~SkySphere::TEXTURE);
  }
  
  bool RenderRectangle::setSkySphereStarData(
                                             const std::string& catalogName,
                                             float minMag, float maxMag,
                                             unsigned int numStars,
                                             float starScale)
  {
    unsigned int currDrawMode = _skySphere->getDrawMode();
    bool success = _skySphere->setStarData(catalogName, minMag, maxMag, numStars, starScale);
    if(success)
      // Add STARFIELD to existing drawmode
      _skySphere->setDrawMode(currDrawMode | SkySphere::STARFIELD);
    else
      // Remove STARFIELD from existing drawmode
      _skySphere->setDrawMode(currDrawMode & ~SkySphere::STARFIELD);
    
    return success;
  }
  
  void RenderRectangle::setBackgroundColor(float r, float g, float b)
  {
    _sceneView->getCamera()->setClearColor(osg::Vec4(r, g, b, 1.0));
  }
  
  void RenderRectangle::addView(View *view)
  {
    if(view != NULL)
    {
      _views.push_back(view);
      
      // Added first view, so make sure that it is selected
      if(_views.size() == 1)
      {
        _currView = 0;
        selectCurrentView();
      }
    }
  }
  
  void RenderRectangle::removeView(View *view)
  {
    if(view != NULL && !_views.empty())
    {
      osg::ref_ptr<View> cv = getCurrentView(); // Save currently selected view
      osg::ref_ptr<View> v_save = view; // Save to make sure View doesn't get deallocated
      
      // Find the view to be deleted
      ViewList::iterator i = std::find(_views.begin(), _views.end(), view);
      while(i != _views.end())
      {
        // Save this view's trackball then erase it from the list
        (*i)->saveTrackball();
        _views.erase(i);
        
        i = std::find(_views.begin(), _views.end(), view); // Find next instance of view
      }
      
      // We erased the currently selected view, so select the first view
      if(cv == v_save)
      {
        _currView = 0;
        selectCurrentView(); // Will select first view (or default view if no stored views)
      }
      else selectView(cv); // Select what was already selected before
    }
  }
  
  void RenderRectangle::removeAllViews()
  {
    // Save each view's trackball
    for(ViewList::iterator i = _views.begin(); i != _views.end(); ++i)
    {
      (*i)->saveTrackball();
    }
    
    _views.clear();
    selectCurrentView(); // Will select the default view
  }
  
  void RenderRectangle::nextView()
  {
    // Save the current View's trackball
    getCurrentView()->saveTrackball();
    
    // Cycle to next View
    if(!_views.empty())
    {
      if(_currView >= _views.size()-1) _currView = 0;
      else _currView++;
    }
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::previousView()
  {
    // Save the current View's trackball
    getCurrentView()->saveTrackball();
    
    // Cycle to previous View
    if(!_views.empty())
    {
      if(_currView == 0) _currView = _views.size()-1;
      else _currView--;
    }
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::selectView(View *view)
  {
    // Search for the requested view in the list
    for(unsigned int i = 0; i < _views.size(); ++i)
    {
      // If found, apply that view
      if(_views[i] == view)
      {
        // Save the previous View's trackball
        getCurrentView()->saveTrackball();
        
        _currView = i; // Select the new View
        
        // Set the new View's trackball as active
        selectCurrentView();
        return;
      }
    }
  }
  
  void RenderRectangle::selectView(unsigned int newView)
  {
    if(_views.empty() || (newView >= _views.size())) return;
    
    // Save the previous View's trackball
    getCurrentView()->saveTrackball();
    
    _currView = newView; // Select the new View
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::selectCurrentView()
  {
    _sceneView->setCameraManipulator(getCurrentView()->getTrackball());
    applyCurrentPerspective();
  }
  
  View* RenderRectangle::getCurrentView()
  {
    if(_currView >= _views.size()) return _defaultView.get();
    else return _views[_currView].get();
  }
  
  void RenderRectangle::applyCurrentPerspective()
  {
    View *view = getCurrentView();
    
    /** Adjust the perspective projection with the current viewport size */
    if(view->getProjectionType() == View::PERSPECTIVE)
    {
      const osg::Viewport *vp = _sceneView->getCamera()->getViewport();
      double fov, ratio;
      
      view->getPerspective(fov, ratio); // Get current field of view (fov)
      
      // Compute new aspect ratio if the viewport is defined
      if(vp != NULL) ratio = (double)vp->width() / (double)vp->height();
      
      view->setPerspective(fov, ratio); // Set new aspect ratio
    }
    
    _sceneView->getCamera()->setProjectionMatrix(view->getProjectionMatrix());
  }
  
}
