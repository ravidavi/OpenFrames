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

#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/FramerateLimiter.hpp>
#include <osg/GraphicsContext>
#include <osg/PointSprite>
#include <osgGA/GUIEventHandler>
#include <iostream>

namespace OpenFrames
{
  
  /** This is the GraphicsWindow that is used for embedded graphics */
  EmbeddedGraphics::EmbeddedGraphics(int x, int y, int width, int height, WindowProxy *window)
  : _makeCurrent(NULL), _swapBuffers(NULL), _updateContext(NULL), _window(window), _realized(false)
  {
    // Specify traits for this graphics context
    _traits = new GraphicsContext::Traits;
    _traits->x = x;
    _traits->y = y;
    _traits->width = width;
    _traits->height = height;
    
    setState( new osg::State ); // State tracks current OpenGL state
    getState()->setGraphicsContext(this);
    getState()->setContextID(osg::GraphicsContext::createNewContextID());
  }
  
  EmbeddedGraphics::~EmbeddedGraphics() {}
  
  bool EmbeddedGraphics::makeCurrentImplementation()
  {
    if(_makeCurrent == NULL)
    {
      std::cerr<< "EmbeddedGraphics::makeCurrentImplementation() ERROR: Callback function makeCurrent() is not defined." << std::endl;
      return false;
    }
    
    if(_window == NULL)
    {
      std::cerr<< "EmbeddedGraphics::makeCurrentImplementation() ERROR: WindowProxy is not defined." << std::endl;
      return false;
    }
    
    unsigned int winID = _window->getID();
    bool success = false;
    _makeCurrent(&winID, &success);	// Perform the custom MakeCurrent callback, and get its result
    
    return success;
  }
  
  void EmbeddedGraphics::swapBuffersImplementation()
  {
    if(_swapBuffers == NULL)
    {
      std::cerr<< "EmbeddedGraphics::swapBuffersImplementation() ERROR: Callback function swapBuffers() is not defined." << std::endl;
      return;
    }
    
    if(_window == NULL)
    {
      std::cerr<< "EmbeddedGraphics::swapBuffersImplementation() ERROR: WindowProxy is not defined." << std::endl;
      return;
    }
    
    unsigned int winID = _window->getID();
    _swapBuffers(&winID); // Perform the custom SwapBuffers callback
  }
  
  void EmbeddedGraphics::setMakeCurrentFunction(void (*fcn)(unsigned int *winID, bool *success))
  {
    _makeCurrent = fcn;
  }
  
  void EmbeddedGraphics::setUpdateContextFunction(void (*fcn)(unsigned int *winID, bool *success))
  {
    _updateContext = fcn;
  }
  
  bool EmbeddedGraphics::updateContextImplementation()
  {
    if(_updateContext)
    {
      unsigned int winID = _window->getID();
      bool success = false;
      
      // Perform the custom UpdateContext callback, and get its result
      _updateContext(&winID, &success);
      return success;
    }
    else return true;
  }
  
  void EmbeddedGraphics::setSwapBuffersFunction(void (*fcn)(unsigned int *winID))
  {
    _swapBuffers = fcn;
  }
  
  bool EmbeddedGraphics::realizeImplementation()
  {
    _realized = true;
    return _realized;
  }
  
  /** This is the handler for events to a WindowProxy */
  WindowEventHandler::WindowEventHandler(WindowProxy *window)
  : _window(window)
  {
    _currentRow = _currentCol = 0;
    _keyPressCallback = NULL;
    _mouseMotionCallback = NULL;
    _buttonPressCallback = NULL;
    _buttonReleaseCallback = NULL;
  }
  
  WindowEventHandler::~WindowEventHandler() {}
  
  // ea is the event, and aa is the osgViewer::View in which the event occured
  bool WindowEventHandler::handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa)
  {
    // Check for different event types
    switch(ea.getEventType())
    {
        // Mouse button was pushed
      case(osgGA::GUIEventAdapter::PUSH):
      {
        // Get the RenderRectangle at the current mouse location
        unsigned int row, col;
        getRenderRectangle(ea.getX()/ea.getWindowWidth(), ea.getY()/ea.getWindowHeight(), row, col);
        selectRenderRectangle(row, col);
        
        // Call the button press callback
        if(_buttonPressCallback)
        {
          // Get normalized (in range [-1, +1]) x and y coordinates of the mouse, within the current sub-window
          float xnorm = ea.getXnormalized();
          float ynorm = ea.getYnormalized();
          
          unsigned int id = _window->getID(); // Get the window's ID
          unsigned int button = ea.getButton(); // Get the ID of the button that was pressed
          
          // Call the user's button press callback
          _buttonPressCallback(&id, &_currentRow, &_currentCol, &xnorm, &ynorm, &button);
        }
        
        break;
      }
        
        // Mouse button was released
      case(osgGA::GUIEventAdapter::RELEASE):
      {
        // Call the button release callback
        if(_buttonReleaseCallback)
        {
          // Get normalized (in range [-1, +1]) x and y coordinates of the mouse, within
          // the current sub-window
          float xnorm = ea.getXnormalized();
          float ynorm = ea.getYnormalized();
          
          unsigned int id = _window->getID(); // Get the window's ID
          unsigned int button = ea.getButton(); // Get the ID of the button that was pressed
          
          _buttonReleaseCallback(&id, &_currentRow, &_currentCol, &xnorm, &ynorm, &button);
        }
        
        break;
      }
        
        // Mouse was moved
      case(osgGA::GUIEventAdapter::MOVE):
      {
        // Call the mouse move callback
        if(_mouseMotionCallback)
        {
          // Get normalized (in range [-1, +1]) x and y coordinates of the mouse, within
          // the current sub-window
          float xnorm = ea.getXnormalized();
          float ynorm = ea.getYnormalized();
          
          unsigned int id = _window->getID(); // Get the window's ID
          
          _mouseMotionCallback(&id, &_currentRow, &_currentCol, &xnorm, &ynorm);
        }
        
        break;
      }
        
        // A keyboard key was pressed
      case(osgGA::GUIEventAdapter::KEYDOWN):
      {
        int key = ea.getKey();
        
        // 'v' switches to next view
        if(key == 'v')
        {
          _window->getGridPosition(_currentRow, _currentCol)->nextView();
        }
        
        // 'V' switches to previous view
        else if(key == 'V')
        {
          _window->getGridPosition(_currentRow, _currentCol)->previousView();
        }
        
        else if(key == 's')
        {
          std::cout<< "Framerate: " << _window->getDesiredFramerate() << "fps desired, " << _window->getFramerate() << "fps actual" << std::endl;
        }
        
        // Spacebar resets current view
        else if(key == osgGA::GUIEventAdapter::KEY_Space)
          _window->getGridPosition(_currentRow, _currentCol)->getCurrentView()->resetTrackball();
        
        // Call the keypress callback
        unsigned int id = _window->getID();
        if(_keyPressCallback) _keyPressCallback(&id, &_currentRow, &_currentCol, &key);
        
        return true; // Indicate that we've handled the event
        break;
      }
        
        // The window was resized
      case(osgGA::GUIEventAdapter::RESIZE):
      {
        EmbeddedGraphics* eg = dynamic_cast<EmbeddedGraphics*>(aa.asView()->getCamera()->getGraphicsContext());
        
        if(eg)
        {
          bool success = eg->updateContextImplementation();
          if(!success) std::cerr<< "WindowEventHandler::handle WARNING: OpenGL context was not properly updated during RESIZE event. Rendering artifacts may occur." << std::endl;
        }
        
        break;
      }
        
        // The window was closed or application quit
      case(osgGA::GUIEventAdapter::CLOSE_WINDOW):
      case(osgGA::GUIEventAdapter::QUIT_APPLICATION):
      {
        _window->shutdown();
        break;
      }
        
      default:
        break;
    }
    
    return false;  // Indicate that the event still needs further processing
  }
  
  void WindowEventHandler::windowModified()
  {
    unsigned int numRows = _window->getNumRows();
    unsigned int numCols = _window->getNumCols();
    
    // Reset the current RenderRectangle
    if((_currentRow >= numRows) || (_currentCol >= numCols))
    {
      _currentRow = _currentCol = 0;
    }
    
    // Deselect every RenderRectangle
    for(unsigned int i = 0; i < numRows; ++i)
    {
      for(unsigned int j = 0; j < numCols; ++j)
      {
        _window->getGridPosition(i, j)->setSelected(false);
      }
    }
    
    // Now select the current RenderRectangle
    _window->getGridPosition(_currentRow, _currentCol)->setSelected(true);
  }
  
  /** Get the RenderRectangle at the (x,y) coordinates of the window. Note that
   the coordinates must be in the range [0, 1). */
  void WindowEventHandler::getRenderRectangle(float x, float y, unsigned int &row, unsigned int &col)
  {
    unsigned int nRows = _window->getNumRows();
    unsigned int nCols = _window->getNumCols();
    
    // (x, y) are out of range, so return an invalid rectangle position
    if(x < 0.0 || y < 0.0 || x >= 1.0 || y >= 1.0)
    {
      row = nRows;
      col = nCols;
      return;
    }
    
    // Compute the column of the specified coordinates
    col = (unsigned int)(x*nCols);
    
    // Compute the row of the specified coordinates, noting that the first row is at the
    // TOP of the window, whereas y coordinates start at the BOTTOM of the window.
    row = (nRows - 1) - (int)(y*nRows);
  }
  
  void WindowEventHandler::selectRenderRectangle( unsigned int row, unsigned int col )
  {
    // Make sure we clicked in a valid rectangle
    if((row < _window->getNumRows()) && (col < _window->getNumCols()))
    {
      // Make sure we aren't clicking on the current rectangle
      if(row != _currentRow || col != _currentCol)
      {
        // Deselect the old rectangle
        _window->getGridPosition(_currentRow, _currentCol)->setSelected(false);
        
        // Set the new selected rectangle
        _currentRow = row;
        _currentCol = col;
        
        // Select the new rectangle
        _window->getGridPosition(_currentRow, _currentCol)->setSelected(true);
      }
    }
    else
      std::cerr<< "OpenFrames::WindowEventHandler ERROR: invalid grid location (row " << row << ", col " << col << ")" << std::endl;
  }
  
  /** The CheckPrerequisites class checks whether certain necessary OpenGL components exist. If they don't,
   then it warns the user that there may be problems. */
  class CheckPrerequisites : public osg::Operation
  {
  public:
    CheckPrerequisites() {}
    
    /** Do the actual task of this operation.*/
    virtual void operator () (osg::Object* obj)
    {
      // Get the GraphicsContext to be tested
      osg::GraphicsContext *gc = dynamic_cast<osg::GraphicsContext*>(obj);
      osg::State *state = gc->getState();
      osg::GLExtensions *glext = state->get<osg::GLExtensions>();
      
      if(!gc)
      {
        std::cerr<< "OpenFrames::WindowProxy ERROR: GraphicsContext not valid" << std::endl;
        return;
      }
      
      // Report OpenGL version info
      char *glVersionString = (char*)glGetString(GL_VERSION);
      char *glRendererString = (char*)glGetString(GL_RENDERER);
      if(glVersionString && glRendererString)
      {
        std::cout<< "OpenFrames renderer: " << glRendererString << ", version: " << glVersionString;
        if(gc->getTraits()->samples > 0)
          std::cout<< " with " << gc->getTraits()->samples << "x MSAA";
        std::cout<< std::endl;
      }
      else
      {
        std::cerr<< "OpenFrames::WindowProxy ERROR: Could not load a valid OpenGL implementation." << std::endl;
        return;
      }
      
      // Check if PointSprites are supported
      osg::ref_ptr<osg::PointSprite> ps = new osg::PointSprite;
      if(!ps->checkValidityOfAssociatedModes(*(gc->getState())))
      {
        std::cerr<< "OpenFrames::WindowProxy ERROR: OpenGL PointSprite extension not supported." << std::endl;
      }
      
      // Check for glVertexAttrib
      if(!glext->glVertexAttrib3fv)
      {
        std::cerr<< "OpenFrames::WindowProxy ERROR: OpenGL glVertexAttrib3fv not found." << std::endl;
      }
      
      // Check for FrameBuffer Objects (needed for VR support)
      if(!glext->isFrameBufferObjectSupported)
      {
        std::cerr<< "OpenFrames::WindowProxy WARNING: FBOs not supported, VR support disabled." << std::endl;
      }
      
      // Other OpenGL extension checks can go here
    }
    
  protected:
    virtual ~CheckPrerequisites() {}
  };
  
  WindowProxy::WindowProxy( int x, int y, unsigned int width, unsigned int height,
                           unsigned int nrow, unsigned int ncol, bool embedded, bool useVR )
  : _winID(0), _nRow(0), _nCol(0), _isEmbedded(embedded), _pause(false), _isAnimating(false), _useVR(useVR)
  {
    _viewer = new osgViewer::CompositeViewer;
    _embeddedGraphics = new EmbeddedGraphics(x, y, width, height, this);
    _eventHandler = new WindowEventHandler(this);
    
    /** Make sure all rendering is done in one thread */
    _viewer->setThreadingModel(osgViewer::ViewerBase::SingleThreaded);
    
    /** Make sure that OpenGL checks are done when the window is created */
    _viewer->setRealizeOperation(new CheckPrerequisites);
    
    /** We don't want the OpenGL context being made current then released at every frame, because that slows things down and can cause problems with single-context windowing systems such as Winteracter. It's ok to not do this here, because we know that each WindowProxy will only handle one drawing context in its thread. */
    _viewer->setReleaseContextAtEndOfFrameHint(false);
    
    // VR can only be enabled for 1x1 windows
    if(_useVR)
    {
      if((nrow != 1) || (ncol != 1))
      {
        std::cerr<< "OpenFrames::WindowProxy ERROR: VR only available for 1x1 windows. Disabling VR." << std::endl;
        _useVR = false;
      }
      else
        std::cout<< "WindowProxy enabling VR" << std::endl;
    }
    
    // Create the RenderRectangles immediately so that they can be modified as needed
    setGridSize(nrow, ncol);
    
    setDesiredFramerate(30.0); // Framerate is in frames per second
  }
  
  // WindowProxy destructor
  // Stop any animations and wait for the thread to join
  WindowProxy::~WindowProxy()
  {
    shutdown();
    join();
  }
  
  void WindowProxy::cancelCleanup()
  {
    std::cout<< "WindowProxy::cancelCleanup()" << std::endl;
    shutdown();
  }
  
  class DisableSwap : public osg::GraphicsContext::SwapCallback
  {
  public:
    DisableSwap() {}
    virtual void swapBuffersImplementation(osg::GraphicsContext *gc) {}
  };
  
  bool WindowProxy::setupWindow()
  {
    if(_isEmbedded) // For embedded windows, just set the window to our embedded GraphicsContext
    {
      _window = _embeddedGraphics.get();
    }
    else // Otherwise create a new window for OpenGL graphics
    {
      // Get default window traits that are saved in the EmbeddedGraphics object (even if it is not used)
      osg::ref_ptr<osg::GraphicsContext::Traits> traits = new osg::GraphicsContext::Traits;
      *traits = *(_embeddedGraphics->getTraits());
      traits->windowDecoration = true; // We want decorations such as window borders
      traits->doubleBuffer = true; // We want double buffered graphics since we're doing animation
      
      osg::GraphicsContext* gc = osg::GraphicsContext::createGraphicsContext(traits.get());
      _window = dynamic_cast<osgViewer::GraphicsWindow*>(gc);
    }
    
    if(_window.valid())
    {
      // Set the default background color for the window
      _window->setClearColor(osg::Vec4f(0.2f, 0.2f, 0.6f, 1.0f));
      
      // Specify that we want to clear both color & depth buffers at every frame
      _window->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      
#ifdef OF_DEBUG
      // Check GL errors often in debug builds
      _window->getState()->setCheckForGLErrors(osg::State::ONCE_PER_ATTRIBUTE);
#else
      // Check GL errors sparingly in release builds
      _window->getState()->setCheckForGLErrors(osg::State::ONCE_PER_FRAME);
#endif
      
      // Initialize event processing system's input window
      _window->getEventQueue()->syncWindowRectangleWithGraphicsContext();
      
      // Allow shaders to access osg_* uniforms
      _window->getState()->setUseModelViewAndProjectionUniforms(true);
      
      // Disable swap buffers
      //_window->setSwapCallback(new DisableSwap());
    }
    else
    {
      std::cerr<< "WindowProxy ERROR: Graphics Window couldn't be created." << std::endl;
      return false;
    }
    
    // Set the graphics context for each RenderRectangle
    for(unsigned int i = 0; i < _renderList.size(); ++i)
    {
      _renderList[i]->setGraphicsContext(_window.get());
    }
    
    // Compute the positions/sizes of each RenderRectangle
    int width = _embeddedGraphics->getTraits()->width;
    int height = _embeddedGraphics->getTraits()->height;
    setupGrid(width, height);
    return true;
  }
  
  /** Gather all the displayed scenes, each one being one FrameManager */
  void WindowProxy::collectScenes()
  {
    FrameManager* currScene;
    
    // Clear set of scenes
    _scenes.clear();
    
    // Loop over each RenderRectangle
    for(unsigned int row = 0; row < _nRow; ++row)
    {
      for(unsigned int col = 0; col < _nCol; ++col)
      {
        // Add current RenderRectangle's scene to the set (if the scene exists)
        currScene = getScene(row, col);
        if(currScene) _scenes.insert(currScene);
      }
    }
  }
  
  /** Get the width of the displayed window */
  unsigned int WindowProxy::getWindowWidth() const
  {
    if(_window.valid()) return _window->getTraits()->width;
    else return 0;
  }
  
  /** Get the height of the displayed window */
  unsigned int WindowProxy::getWindowHeight() const
  {
    if(_window.valid()) return _window->getTraits()->height;
    else return 0;
  }
  
  /** Create a key pressed event */
  void WindowProxy::keyPress(int key)
  {
    _window->getEventQueue()->keyPress(key);
  }
  
  /** Create a mouse moved event */
  void WindowProxy::mouseMotion(float x, float y)
  {
    _window->getEventQueue()->mouseMotion(x, y);
  }
  
  /** Create a mouse button pressed event */
  void WindowProxy::buttonPress(float x, float y, unsigned int button)
  {
    _window->getEventQueue()->mouseButtonPress(x, y, button);
  }
  
  /** Create a mouse button released event */
  void WindowProxy::buttonRelease(float x, float y, unsigned int button)
  {
    _window->getEventQueue()->mouseButtonRelease(x, y, button);
  }
  
  /** Create a window resized event */
  void WindowProxy::resizeWindow(int x, int y, unsigned int width, unsigned int height)
  {
    _window->resized(x, y, width, height);
    _window->getEventQueue()->windowResize(x, y, width, height);
  }
  
  /** Resize each RenderRectangle's viewport (the on-screen area that it draws to). */
  void WindowProxy::setupGrid(unsigned int width, unsigned int height)
  {
    float rw, rh;
    
    rw = (float)width/(float)_nCol;  // Width of each RenderRect
    rh = (float)height/(float)_nRow; // Height of each RenderRect
    
    // Loop over each RenderRectangle
    RenderRectangle *rect;
    for(unsigned int row = 0; row < _nRow; ++row)
    {
      for(unsigned int col = 0; col < _nCol; ++col)
      {
        // Get the RenderRectangle at the current (row, col) position
        rect = getGridPosition(row, col);
        
        // Set viewport size for this grid position
        rect->setViewport((int)(rw*col), (int)(rh*(_nRow-1-row)), (int)rw, (int)rh);
        
        // Reset the projection matrix
        rect->applyCurrentViewProjection();
      }
    }
  }
  
  /** Pause the window's animation */
  void WindowProxy::pauseAnimation(bool pause)
  {
    if(pause == _pause) return;
    
    _pause = pause;
    if(_pause) _pauseTime = osg::Timer::instance()->tick();
    else
    {
      _frameThrottle.reset(); // Reset the framerate limiter so it doesn't stutter on resume
      _startTime += osg::Timer::instance()->tick() - _pauseTime;
    }
  }
  
  /** Add or remove RenderRectangles to the grid to make it the right size. */
  void WindowProxy::setGridSize(unsigned int row, unsigned int col)
  {
    // Check for invalid grid size
    if(row == 0 || col == 0) return;
    
    _nRow = row;
    _nCol = col;
    
    unsigned int newSize = _nRow*_nCol;
    unsigned int oldSize = _renderList.size();
    
    if(oldSize == newSize) return;
    
    _renderList.resize(newSize); // Resize the RenderRectangle list
    
    // Set up each new RenderRectangle
    osgViewer::View* currView;
    for(unsigned int i = oldSize; i < newSize; ++i)
    {
      // Create the new RenderRectangle
      _renderList[i] = new RenderRectangle(_useVR);
      
      currView = _renderList[i]->getSceneView();
      
      // Set the event handler for this RenderRectangle
      currView->addEventHandler(_eventHandler.get());
      
      // Add the RenderRectangle's camera to the viewer
      _viewer->addView(currView);
    }
    
    // Don't show border if we only have 1 RenderRectangle
    if(newSize == 1) _renderList[0]->setShowBorder(false);
    else _renderList[0]->setShowBorder(true);
    
    // Tell event handler that this WindowProxy was modified
    _eventHandler->windowModified();
    
    collectScenes(); // Make a set containing all unique scenes
  }
  
  /** Set the scene contained in the given FrameManager at the given location. */
  void WindowProxy::setScene(FrameManager *newfm, unsigned int row, unsigned int col)
  {
    // Location out of bounds
    if(row >= _nRow || col >= _nCol)
    {
      std::cerr<< "WindowProxy::setScene ERROR: Grid position (" << row << "," << col << ") out of bounds." << std::endl;
      return;
    }
    
    unsigned int loc = row*_nCol + col;
    FrameManager *oldfm = _renderList[loc]->getFrameManager();
    
    if(newfm == oldfm) return; // The same FrameManager is already there
    
    // Set the new FrameManager for the RenderRectangle. If the old
    // FrameManager is no longer being used, it will automatically be erased.
    _renderList[loc]->setFrameManager(newfm);
    
    collectScenes(); // Make set containing all unique scenes
  }
  
  /** Set the scene contained in the given FrameManager at the given location. */
  FrameManager* WindowProxy::getScene(unsigned int row, unsigned int col)
  {
    if(row >= _nRow || col >= _nCol) return NULL; // Location out of bounds
    return _renderList[row*_nCol + col]->getFrameManager();
  }
  
  /** Get the RenderRectangle used for the given grid position */
  RenderRectangle* WindowProxy::getGridPosition(unsigned int row, unsigned int col)
  {
    if(row >= _nRow || col >= _nCol) return NULL; // Location out of bounds
    return _renderList[row*_nCol + col].get();
  }
  
  void WindowProxy::setMakeCurrentFunction(void (*fcn)(unsigned int *winID, bool *success))
  {
    _embeddedGraphics->setMakeCurrentFunction(fcn);
  }
  
  void WindowProxy::setUpdateContextFunction(void (*fcn)(unsigned int *winID, bool *success))
  {
    _embeddedGraphics->setUpdateContextFunction(fcn);
  }
  
  void WindowProxy::setSwapBuffersFunction(void (*fcn)(unsigned int *winID))
  {
    _embeddedGraphics->setSwapBuffersFunction(fcn);
  }
  
  void WindowProxy::run()
  {
    // Create the window
    if(!setupWindow()) return;
    
    // Set the starting reference time
    _startTime = osg::Timer::instance()->tick();
    
    // Controls the framerate while graphics are paused
    FramerateLimiter _pauseLimiter;
    _pauseLimiter.setDesiredFramerate(5.0); // 5fps while paused
    
    _isAnimating = true; // Indicate that we've started animating
    
    // Loop until the user asks us to quit
    while(!_viewer->done())
    {
      if(_pause)
      {
        _pauseLimiter.frame();
        continue;
      }
      
      // Pause to achieve desired framerate
      _frameThrottle.frame();
      
      // Do one update & event frame
      frame();
    }
    
    // Close the graphics context before exiting this thread. If this is
    // not done, then the graphics context will be released when this
    // WindowProxy is destroyed. This could result in a seg fault if
    // the context is already destroyed before OSG can release it.
    _window->close();
    
    _isAnimating = false; // Indicate that animation has stopped
  }
  
  /** Handle one frame of animation, including event handling */
  void WindowProxy::frame()
  {
    // Iterator to loop through all unique scenes
    std::set<FrameManager*>::iterator sceneIter;
    
    // Lock all scenes so that they aren't modified while being drawn
    for(sceneIter = _scenes.begin(); sceneIter != _scenes.end(); ++sceneIter)
    {
      (*sceneIter)->lock();
    }
    
    // Update, cull, and draw the scene, and process queued events
    _viewer->frame(osg::Timer::instance()->delta_s(_startTime, osg::Timer::instance()->tick()));
    
    // Unlock all scenes so that they can be modified
    for(sceneIter = _scenes.begin(); sceneIter != _scenes.end(); ++sceneIter)
    {
      (*sceneIter)->unlock();
    }
  }
  
  /** Print info about this window to std::cout */
  void WindowProxy::printInfo()
  {
    std::cout<< "WindowProxy info:" << std::endl;
    std::cout<< "\t Window ID: " << _winID << std::endl;
    if(_window.valid()) 
    {
      std::cout<< "\tContext ID: " << _window->getState()->getContextID() << std::endl;
    }
    else
    {
      std::cout<< "No graphics window created yet" << std::endl;
    }
    std::cout<< "\tGrid size: " << _nRow << " rows, " << _nCol << " columns" << std::endl;
    FrameManager *fm;
    for(unsigned int i = 0; i < _renderList.size(); ++i)
    {
      fm = _renderList[i]->getFrameManager();
      if(fm == NULL)
        std::cout<< "\tRenderRectangle " << i << " has no FrameManager" << std::endl;
      else
      {
        std::cout<< "\tRenderRectangle " << i << " has FrameManager " << fm << std::endl;
      }
    }
  }
  
}
