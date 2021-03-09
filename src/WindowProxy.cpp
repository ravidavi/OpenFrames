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

/** \file WindowProxy.cpp
 * EmbeddedGraphics, WindowEventHandler and WindowProxy-class function definitions.
 */

#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/FramerateLimiter.hpp>
#include <OpenFrames/Utilities.hpp>
#include <osg/GraphicsContext>
#include <osg/PointSprite>
#include <osgGA/GUIEventHandler>
#include <iostream>
#include <limits>

namespace OpenFrames
{
  
  /** This is the GraphicsWindow that is used for embedded graphics */
  EmbeddedGraphics::EmbeddedGraphics(int x, int y, int width, int height, WindowProxy *window)
  : _gcCallback(nullptr), _makeCurrent(NULL), _swapBuffers(NULL), _updateContext(NULL), _window(window), _realized(false)
  {
    // Specify traits for this graphics context
    _traits = new GraphicsContext::Traits;
    setGeometry(x, y, width, height);
  }
  
  EmbeddedGraphics::~EmbeddedGraphics() {}
  
  bool EmbeddedGraphics::makeCurrentImplementation()
  {
    if(_gcCallback == nullptr)
    {
      if(_makeCurrent == nullptr)
      {
        OSG_WARN<< "EmbeddedGraphics::makeCurrentImplementation() ERROR: Callback object or makeCurrent() function must be defined." << std::endl;
        return false;
      }
      else if(_window == nullptr)
      {
        OSG_WARN<< "EmbeddedGraphics::makeCurrentImplementation() ERROR: WindowProxy must be defined if makeCurrent() function is used." << std::endl;
        return false;
      }
    }
    
    // Perform the makeCurrent callback
    bool success = false;
    if(_gcCallback != nullptr) success = _gcCallback->makeCurrent();
    else
    {
      unsigned int winID = _window->getID();
      _makeCurrent(&winID, &success);
    }
    
    return success;
  }
  
  void EmbeddedGraphics::swapBuffersImplementation()
  {
    if(_gcCallback == nullptr)
    {
      if(_swapBuffers == nullptr)
      {
        OSG_WARN<< "EmbeddedGraphics::swapBuffersImplementation() ERROR: Callback object or swapBuffers() function must be defined." << std::endl;
        return;
      }
      else if(_window == nullptr)
      {
        OSG_WARN<< "EmbeddedGraphics::swapBuffersImplementation() ERROR: WindowProxy is not defined." << std::endl;
        return;
      }
    }
    
    // Perform the custom SwapBuffers callback
    if(_gcCallback != nullptr) _gcCallback->swapBuffers();
    else
    {
      unsigned int winID = _window->getID();
      _swapBuffers(&winID);
    }
  }

  void EmbeddedGraphics::setGeometry(int x, int y, int width, int height)
  {
    _traits->x = x;
    _traits->y = y;
    _traits->width = width;
    _traits->height = height;
  }

  void EmbeddedGraphics::setGraphicsContextCallback(GraphicsContextCallback *gcCallback)
  {
    _gcCallback = gcCallback;
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
    bool success = false;
    if(_gcCallback != nullptr) success = _gcCallback->updateContext();
    else if(_updateContext)
    {
      unsigned int winID = _window->getID();
      _updateContext(&winID, &success);
    }
    
    return success;
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
    _vrEventCallback = NULL;
  }
  
  WindowEventHandler::~WindowEventHandler() {}
  
  // ea is the event, and aa is the osgViewer::View in which the event occured
  bool WindowEventHandler::handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa)
  {
    bool handled = false;
    
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
          handled = true;
        }
        
        // 'V' switches to previous view
        else if(key == 'V')
        {
          _window->getGridPosition(_currentRow, _currentCol)->previousView();
          handled = true;
        }
        
        // Spacebar resets current view
        else if (key == osgGA::GUIEventAdapter::KEY_Space)
        {
          _window->getGridPosition(_currentRow, _currentCol)->getCurrentView()->resetView();
          handled = true;
        }

        // Scale world if VR is used
        else if (key == osgGA::GUIEventAdapter::KEY_Up)
        {
          if (_window->getUseVR())
          {
            float scale = _window->getWorldUnitsPerMeter();
            scale /= 2.0; // Make world bigger
            _window->setWorldUnitsPerMeter(scale);
            handled = true;
          }
        }

        else if (key == osgGA::GUIEventAdapter::KEY_Down)
        {
          if (_window->getUseVR())
          {
            float scale = _window->getWorldUnitsPerMeter();
            scale *= 2.0; // Make world smaller
            _window->setWorldUnitsPerMeter(scale);
            handled = true;
          }
        }
        
        // Call the keypress callback
        unsigned int id = _window->getID();
        if(_keyPressCallback) _keyPressCallback(&id, &_currentRow, &_currentCol, &key);
        
        break;
      }
        
        // The window was resized
      case(osgGA::GUIEventAdapter::RESIZE):
      {
        // Get the graphics context that was resized
        osg::GraphicsContext *gc = OpenFrames::getMainGraphicsContext(aa.asView());
        
        // If graphics context is EmbeddedGraphics, then call the update context callback
        EmbeddedGraphics* eg = dynamic_cast<EmbeddedGraphics*>(gc);
        if(eg)
        {
          bool success = eg->updateContextImplementation();
          if(!success) std::cerr<< "WindowEventHandler::handle WARNING: OpenGL context was not properly updated during RESIZE event. Rendering artifacts may occur." << std::endl;
        }

        // Make sure OpenFrames::View knows about resize and updates its projection matrix
        _window->setupGrid(ea.getWindowWidth(), ea.getWindowHeight());
        
        break;
      }
        
        // The window was closed or application quit
      case(osgGA::GUIEventAdapter::CLOSE_WINDOW):
      case(osgGA::GUIEventAdapter::QUIT_APPLICATION):
      {
        _window->shutdown();
        break;
      }

      // Check for custom events
      case(osgGA::GUIEventAdapter::USER):
      {
        const OpenVREvent *vrEvent = dynamic_cast<const OpenVREvent*>(&ea);
        if (vrEvent && _vrEventCallback)
        {
          unsigned int id = _window->getID();
          _vrEventCallback(&id, &_currentRow, &_currentCol, vrEvent);
        }
        break;
      }
        
      default:
        break;
    }
    
    return handled;
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
  
  /** 
   Checks whether certain necessary OpenGL and WindowProxy components exist
   */
  class CheckPrerequisites : public osg::Operation
  {
  public:
    CheckPrerequisites(WindowProxy *wp)
    : _windowProxy(wp)
    {}
    
    /** Do the actual task of this operation.*/
    virtual void operator () (osg::Object* obj)
    {
      // Get the GraphicsContext to be tested
      osg::GraphicsContext *gc = dynamic_cast<osg::GraphicsContext*>(obj);
      osg::State *state = gc->getState();
      osg::GLExtensions *glext = state->get<osg::GLExtensions>();
      
      if(!gc)
      {
        OSG_WARN<< "OpenFrames::WindowProxy ERROR: GraphicsContext not valid" << std::endl;
        return;
      }
      
      // Get OpenGL version info
      char *glVersionString = (char*)glGetString(GL_VERSION);
      if(glVersionString == 0)
        OSG_WARN<< "OpenFrames::WindowProxy ERROR in glGetString(GL_VERSION): " << std::endl;

      // Get OpenGL renderer info
      char *glRendererString = (char*)glGetString(GL_RENDERER);
      if(glRendererString == 0)
        OSG_WARN<< "OpenFrames::WindowProxy ERROR in glGetString(GL_RENDERER): " << std::endl;
      
      // Report OpenGL version and renderer info
      if(glVersionString && glRendererString)
      {
        OSG_NOTICE<< "OpenFrames renderer: " << glRendererString << ", version: " << glVersionString;
        if(gc->getTraits()->samples > 0)
          OSG_NOTICE<< " with " << gc->getTraits()->samples << "x MSAA";
        OSG_NOTICE<< std::endl;
      }
      else
      {
        GLenum err = glGetError();
        
        if(err == GL_INVALID_ENUM)
        {
          OSG_WARN<< " GL_INVALID_ENUM, please ensure that a valid OpenGL context is current before starting the WindowProxy animation." << std::endl;
        }
        else if(err == GL_INVALID_OPERATION)
        {
          OSG_WARN<< " GL_INVALID_OPERATION, please ensure that application is not already using OpenGL elsewhere." << std::endl;
        }
        else
        {
          OSG_WARN<< "OpenGL error code " << err << std::endl;
        }
        
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
      
      // Check for multisample support
      if(_windowProxy->getUseVR() && !glext->isRenderbufferMultisampleSupported())
      {
        std::cerr<< "OpenFrames::WindowProxy WARNING: Multisample not supported, VR rendering will not be antialiased." << std::endl;
      }

      // Disable vsync since VR does its own frame synchronization
      if (_windowProxy->getUseVR())
      {
        osgViewer::GraphicsWindow *gw = dynamic_cast<osgViewer::GraphicsWindow*>(gc);
        if (gw) gw->setSyncToVBlank(false);
      }
      
      // Other OpenGL extension checks can go here
    }
    
  protected:
    virtual ~CheckPrerequisites() {}
    
    WindowProxy* _windowProxy;
  };
  
  WindowProxy::WindowProxy( int x, int y, unsigned int width, unsigned int height,
                           unsigned int nrow, unsigned int ncol, bool embedded, bool useVR )
  : _winID(0), _nRow(0), _nCol(0), _isEmbedded(embedded),
  _animationState(IDLE), _pauseAnimation(false),
  _timePaused(false), _useVR(useVR)
  {
    // Input value checks
    if(x < 0) x = 0;
    if(y < 0) y = 0;
    if(width == 0) width = 1;
    if(height == 0) height = 1;
    if(nrow == 0) nrow = 1;
    if(ncol == 0) ncol = 1;
    
    _viewer = new osgViewer::CompositeViewer;
    _viewer->setIncrementalCompileOperation(new osgUtil::IncrementalCompileOperation());
    _embeddedGraphics = new EmbeddedGraphics(x, y, width, height, this);
    _eventHandler = new WindowEventHandler(this);
    _statsHandler = new osgViewer::StatsHandler;
    _screenCaptureHandler = new osgViewer::ScreenCaptureHandler;
    _screenCaptureHandler->setKeyEventToggleContinuousCapture(0); // Disable continuous capture
    _windowSizeHandler = new osgViewer::WindowSizeHandler;
    _windowSizeHandler->setChangeWindowedResolution(false); // Don't change windowed resolution
    
    setWindowName("OpenFrames Window");
    
    /** Make sure that OpenGL checks are done when the window is created */
    _viewer->setRealizeOperation(new CheckPrerequisites(this));
    
    /** We don't want the OpenGL context being made current then released at every frame, because that slows things down and can cause problems with single-context windowing systems such as Winteracter. It's ok to not do this here, because we know that each WindowProxy will only handle one drawing context in its thread. */
    _viewer->setReleaseContextAtEndOfFrameHint(false);
    
    // VR can only be enabled for 1x1 windows
    if(_useVR)
    {
      if(nrow*ncol > 1)
      {
        osg::notify(osg::FATAL)<< "OpenFrames::WindowProxy ERROR: VR only available for 1x1 windows. Disabling VR." << std::endl;
        _useVR = false;
      }
      else
      {
        // Set up OpenVR
        _ovrDevice = new OpenVRDevice(0.001, 0.9144); // 4'6" user height
        if(_ovrDevice->initVR())
        {
          osg::notify(osg::INFO) << "WindowProxy enabling VR" << std::endl;
          int vrWidth, vrHeight;
          _ovrDevice->getRecommendedTextureSize(vrWidth, vrHeight);
          _vrTextureBuffer = new VRTextureBuffer(vrWidth, vrHeight);
          _ovrDevice->setWorldUnitsPerMeterLimits(0.001, 1.0e6);
        }
        else
        {
          osg::notify(osg::FATAL) << "WindowProxy disabling VR" << std::endl;
          _ovrDevice = NULL;
          _useVR = false;
        }
      }
    }
    
    // Create the RenderRectangles immediately so that they can be modified as needed
    setGridSize(nrow, ncol);
    setupGrid(width, height);
    
    // Set time parameters
    setTimeRange(std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max());
    setTime(0.0);
    setTimeScale(1.0);
    
    // Set framerate (in fps) based on whether VR is enabled
    if(_useVR) _frameThrottle.setDesiredFramerate(0.0); // Don't limit framerate
    else _frameThrottle.setDesiredFramerate(30.0);
  }
  
  // WindowProxy destructor
  // Stop any animations and wait for the thread to join
  WindowProxy::~WindowProxy()
  {
    shutdown();
    if (isRunning()) join(); // Only join if thread is running
    if (_useVR) _ovrDevice->shutdownVR();
  }
  
  void WindowProxy::cancelCleanup()
  {
    OSG_NOTICE<< "WindowProxy::cancelCleanup()" << std::endl;
    shutdown();
  }
  
  void WindowProxy::setWindowName(const std::string& name)
  {
    // If window exists, then directly set its name
    if(_window) _window->setWindowName(name);
    
    // Otherwise set the name in the EmbeddedGraphics object, which will be used to initialize
    // the window when it is created
    else _embeddedGraphics->setWindowName(name);
  }
  
  std::string WindowProxy::getWindowName() const
  {
    if(_window) return _window->getWindowName();
    else return _embeddedGraphics->getWindowName();
  }
  
  void WindowProxy::setWindowBackgroundColor(const osg::Vec3& color)
  {
    // If window exists, then directly set its color
    if(_window) _window->setClearColor(osg::Vec4(color, 1.0));
    
    // Otherwise set the color in the EmbeddedGraphics object, which will be used to initialize
    // the window when it is created
    else _embeddedGraphics->setClearColor(osg::Vec4(color, 1.0));
  }
  
  osg::Vec3 WindowProxy::getWindowBackgroundColor() const
  {
    osg::Vec4 clearColor;
    if(_window) clearColor = _window->getClearColor();
    else clearColor = _embeddedGraphics->getClearColor();
    return osg::Vec3(clearColor[0], clearColor[1], clearColor[2]);
  }
  
  bool WindowProxy::setupWindow()
  {
    if(_isEmbedded) // For embedded windows, just set the window to our embedded GraphicsContext
    {
      osg::State* state = new osg::State; // Tracks current OpenGL state
      _embeddedGraphics->setState(state); 
      state->setGraphicsContext(_embeddedGraphics);
      state->setContextID(osg::GraphicsContext::createNewContextID());
      _window = _embeddedGraphics.get();
    }
    else // Otherwise create a new window for OpenGL graphics
    {
      // Get default window traits that are saved in the EmbeddedGraphics object (even if it is not used)
      osg::ref_ptr<osg::GraphicsContext::Traits> traits = new osg::GraphicsContext::Traits;
      *traits = *(_embeddedGraphics->getTraits());
      traits->readDISPLAY(); // Get DISPLAY number for OS's that support it
      traits->windowDecoration = true; // We want decorations such as window borders
      traits->doubleBuffer = true; // We want double buffered graphics since we're doing animation
      if(_useVR)
      {
        traits->vsync = false; // VR rendering handles its own HMD vblank sync
      }
      else
      {
        traits->samples = 4; // Enable 4x MSAA
      }
      
      // Try creating graphics context
      osg::GraphicsContext* gc = osg::GraphicsContext::createGraphicsContext(traits.get());

      // On error, try again without MSAA
      if(!gc) 
      {
        OSG_WARN << "Couldn't create 4x MSAA window, trying again without MSAA..." << std::endl;
        traits->samples = 0; // Disable MSAA
        gc = osg::GraphicsContext::createGraphicsContext(traits.get());
      }

      _window = dynamic_cast<osgViewer::GraphicsWindow*>(gc);
    }
    
    if(_window.valid())
    {
      // Initialize background color for the window
      _window->setClearColor(_embeddedGraphics->getClearColor());
      
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
      if(_useVR) _window->setSwapCallback(new OpenVRSwapBuffers(_ovrDevice, _vrTextureBuffer));
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

  /** Set the key used for fullscreen toggle */
  void WindowProxy::setToggleFullscreenKey(int key)
  {
     if (key == 0)
     {
        _windowSizeHandler->setToggleFullscreen(false);
     }
     else
     {
        _windowSizeHandler->setToggleFullscreen(true);
        _windowSizeHandler->setKeyEventToggleFullscreen(key);
     }
  }
  
  /** Create a key pressed event */
  void WindowProxy::keyPress(int key)
  {
    _window->getEventQueue()->keyPress(key);
  }
  
  /** Create a key released event */
  void WindowProxy::keyRelease(int key)
  {
    _window->getEventQueue()->keyRelease(key);
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
    // Input value checks
    if(x < 0) x = 0;
    if(y < 0) y = 0;
    if(width == 0) width = 1;
    if(height == 0) height = 1;
    
    if(_window)
    {
      // Can this be changed to _window->setWindowRectangle()?
      _window->resized(x, y, width, height);
      _window->getEventQueue()->windowResize(x, y, width, height);
    }
    else
    {
      _embeddedGraphics->setGeometry(x, y, width, height);
      setupGrid(width, height);
    }
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
  
  /** Set the current time */
  void WindowProxy::setTime(double time)
  {
    if(_timeSyncWinProxy.valid()) _timeSyncWinProxy->setTime(time);
    else
    {
      if(time < _minTime) time = _minTime;
      else if(time > _maxTime) time = _maxTime;
      _currTime = _offsetTime = time;
      _Tref = osg::Timer::instance()->tick();
    }
  }
  
  /** Set the time range */
  void WindowProxy::setTimeRange(double tMin, double tMax)
  {
    if(_timeSyncWinProxy.valid()) _timeSyncWinProxy->setTimeRange(tMin, tMax);
    else if(tMax >= tMin)
    {
      _minTime = tMin;
      _maxTime = tMax;
    }
  }
  
  /** Get the time range */
  void WindowProxy::getTimeRange(double& tMin, double& tMax) const
  {
    if(_timeSyncWinProxy.valid()) _timeSyncWinProxy->getTimeRange(tMin, tMax);
    else
    {
      tMin = _minTime;
      tMax = _maxTime;
    }
  }
  
  /** Pause/unpause time */
  void WindowProxy::pauseTime(bool pause)
  {
    if(_timeSyncWinProxy.valid()) _timeSyncWinProxy->pauseTime(pause);
    else
    {
      _timePaused = pause;
      setTime(_currTime);
    }
  }
  
  /** Change time scale */
  void WindowProxy::setTimeScale(double tscale)
  {
    if(_timeSyncWinProxy.valid()) _timeSyncWinProxy->setTimeScale(tscale);
    else
    {
      _timeScale = tscale;
      setTime(_currTime);
    }
  }
  
  /** Synchronize time with specified WindowProxy */
  bool WindowProxy::synchronizeTime(WindowProxy *winproxy)
  {
    // Stop synchronizing time
    if((winproxy == NULL) || (winproxy == this))
    {
      if(_timeSyncWinProxy.valid())
      {
        // Save currently synchronized window's time parameters
        double newTime = _timeSyncWinProxy->getTime();
        _timeScale = _timeSyncWinProxy->getTimeScale();
        _timePaused = _timeSyncWinProxy->isTimePaused();
        _timeSyncWinProxy->getTimeRange(_minTime, _maxTime);
        
        _timeSyncWinProxy = NULL;
        setTime(newTime);
      }
      
      return true;
    }
    
    // Check for circular dependency by traversing full synchronized window chain
    // Note that this should never be an infinite loop since there should be no existing circular dependencies
    for(WindowProxy* syncWP = winproxy->getTimeSyncWindow(); syncWP != NULL; syncWP = syncWP->getTimeSyncWindow())
    {
      // Make sure synchronized window isn't this window
      if(syncWP == this)
      {
        OSG_WARN << "WindowProxy::synchronizeTime circular dependency not allowed." << std::endl;
        return false;
      }
    }
    
    // No circular dependency, so set synchronized window
    _timeSyncWinProxy = winproxy;
    return true;
  }
  
  /** Pause the window's animation */
  void WindowProxy::pauseAnimation(bool pause)
  {
    if(pause == _pauseAnimation) return;
    _pauseAnimation = pause;
    
    // As long as animation is active, wait until animation state matches the desired state
    FramerateLimiter pauseCheck(10.0);
    while(isAnimating() && ((_animationState == PAUSED) != _pauseAnimation)) pauseCheck.frame();
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
      _renderList[i] = new RenderRectangle(_ovrDevice, _vrTextureBuffer);
      
      currView = _renderList[i]->getSceneView();
      
      // Set the event handler for this RenderRectangle
      currView->addEventHandler(_eventHandler.get());

      // Add the stats handler to this RenderRectangle
      currView->addEventHandler(_statsHandler);
      
      // Add the screen capture handler to this RenderRectangle
      currView->addEventHandler(_screenCaptureHandler);

      // Add the fulscreen handler
      if(!_isEmbedded) currView->addEventHandler(_windowSizeHandler);
      
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

  void WindowProxy::setGraphicsContextCallback(GraphicsContextCallback *gcCallback)
  {
    _embeddedGraphics->setGraphicsContextCallback(gcCallback);
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
    _animationState = INITIALIZING;
    
    // Create the window
    if(!setupWindow())
    {
      _animationState = FAILED;
      return;
    }

    // Set up processor affinity for render and database threads
    // First let OSG configure affinity
    _viewer->configureAffinity();
    
    // Next override this rendering thread's affinity so it doesn't
    // suck up time on Proc0
    _viewer->setProcessorAffinity(OpenThreads::Affinity());
    
    // Finally set threading model
    _viewer->setUseConfigureAffinity(false); // Already called configureAffinity
    _viewer->setThreadingModel(osgViewer::ViewerBase::SingleThreaded);
    
    // Controls the framerate while graphics are paused
    FramerateLimiter pauseLimiter(10.0);
    
    // Set the reference time
    _Tref = osg::Timer::instance()->tick();
    
    // Initialize animation state
    _animationState = _pauseAnimation ? PAUSED : ANIMATING;

    // Loop until the user asks us to quit
    while(!_viewer->done())
    {
      if(_pauseAnimation)
      {
        _animationState = PAUSED;
        pauseLimiter.frame();
      }
      else
      {
        _animationState = ANIMATING;

        // Pause to achieve desired framerate
        _frameThrottle.frame();
        
        // Do one frame: check events, update objects, render scene
        frame();
      }
    }
    
    // Close the graphics context before exiting this thread. If this is
    // not done, then the graphics context will be released when this
    // WindowProxy is destroyed. This could result in a seg fault if
    // the context is already destroyed before OSG can release it.
    _window->close();
    
    // Shutdown OpenVR if needed
    if (_useVR) _ovrDevice->shutdownVR();
    
    _animationState = SUCCESS; // Indicate that animation is complete
  }
  
  /** Handle one frame of animation, including event handling */
  void WindowProxy::frame()
  {
    // Use simulation time from synchonized WindowProxy
    if(_timeSyncWinProxy.valid())
    {
      _currTime = _timeSyncWinProxy->getTime();
    }
    
    // Otherwise compute current simulation time if not paused
    else if(!_timePaused)
    {
      double dt = osg::Timer::instance()->delta_s(_Tref, osg::Timer::instance()->tick());
      _currTime = _offsetTime + dt*_timeScale;
      if(_currTime < _minTime) _currTime = _minTime;
      else if(_currTime > _maxTime) _currTime = _maxTime;
    }
    
    // Lock all scenes so that they aren't modified while being drawn
    for(SceneSet::iterator sceneIter = _scenes.begin(); sceneIter != _scenes.end(); ++sceneIter)
    {
      (*sceneIter)->lock(FrameManager::LOW_PRIORITY);
    }
    
    // Process events, then update, cull, and draw all scenes
    _viewer->frame(_currTime);
    
    // Unlock all scenes so that they can be modified
    for(SceneSet::iterator sceneIter = _scenes.begin(); sceneIter != _scenes.end(); ++sceneIter)
    {
      (*sceneIter)->unlock(FrameManager::LOW_PRIORITY);
    }
  }
  
  /** Print info about this window to std::cout */
  void WindowProxy::printInfo()
  {
    OSG_NOTICE<< "WindowProxy info:" << std::endl;
    OSG_NOTICE<< "\t Window ID: " << _winID << std::endl;
    if(_window.valid()) 
    {
      OSG_NOTICE<< "\tContext ID: " << _window->getState()->getContextID() << std::endl;
    }
    else
    {
      OSG_NOTICE<< "No graphics window created yet" << std::endl;
    }
    OSG_NOTICE<< "\tGrid size: " << _nRow << " rows, " << _nCol << " columns" << std::endl;
    FrameManager *fm;
    for(unsigned int i = 0; i < _renderList.size(); ++i)
    {
      fm = _renderList[i]->getFrameManager();
      if(fm == NULL)
      {
        OSG_NOTICE<< "\tRenderRectangle " << i << " has no FrameManager" << std::endl;
      }
      else
      {
        OSG_NOTICE<< "\tRenderRectangle " << i << " has FrameManager " << fm << std::endl;
      }
    }
  }
  
  /** Take a screenshot of this window */
  void WindowProxy::captureWindow(bool waitForCapture)
  {
    _screenCaptureHandler->setFramesToCapture(1);
    _screenCaptureHandler->startCapture();
    
    // Wait for capture to finish
    while(waitForCapture && (_screenCaptureHandler->getFramesToCapture() > 0))
    {
      OpenThreads::Thread::YieldCurrentThread();
    }
  }
  
  /** Set the window capture filename and type */
  void WindowProxy::setWindowCaptureFile(const std::string& fname, const std::string& fext)
  {
    _screenCaptureHandler->setCaptureOperation(new osgViewer::ScreenCaptureHandler::WriteToFile(fname, fext));
  }
  
}
