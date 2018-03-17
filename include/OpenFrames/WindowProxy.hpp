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

#ifndef _OF_WINDOWPROXY_
#define _OF_WINDOWPROXY_

#include <OpenFrames/Export.h>
#include <OpenFrames/RenderRectangle.hpp>
#include <OpenFrames/FramerateLimiter.hpp>
#include <OpenFrames/OpenVRDevice.hpp>
#include <OpenThreads/Thread>
#include <osg/FrameStamp>
#include <osg/Timer>
#include <osg/Referenced>
#include <osg/observer_ptr>
#include <osg/ref_ptr>
#include <osgViewer/CompositeViewer>
#include <osgViewer/GraphicsWindow>
#include <vector>
#include <set>

// Define shortcuts for callback function signatures
#define BASIC_CALLBACK_SIG unsigned int *winID, unsigned int *row, unsigned int *col
#define KEYPRESS_SIG BASIC_CALLBACK_SIG, int *key
#define MOUSEMOTION_SIG BASIC_CALLBACK_SIG, float *x, float *y
#define BUTTON_SIG MOUSEMOTION_SIG, unsigned int *button
#define VR_SIG BASIC_CALLBACK_SIG, const OpenVREvent *vrEvent

namespace OpenFrames
{
  
  class FrameManager;
  class WindowProxy;
  
  /** This is the GraphicsWindow that is used for embedded graphics (if the user provides their own OpenGL window) */
  class EmbeddedGraphics : public osgViewer::GraphicsWindow
  {
  public:
    EmbeddedGraphics(int x, int y, int width, int height, WindowProxy *window);
    
    virtual bool isSameKindAs(const Object* object) const { return dynamic_cast<const EmbeddedGraphics*>(object)!=0; }
    virtual const char* libraryName() const { return "OpenFrames"; }
    virtual const char* className() const { return "EmbeddedGraphics"; }
    
    /** Save a name to this GraphicsWindow's traits
        This is used by WindowProxy when creating a non-embedded window */
    virtual void setWindowName(const std::string& name) { _traits->windowName = name; }
    
    /** Inherited from GraphicsWindow, these functions manage the OpenGL context */
    virtual bool makeCurrentImplementation();
    virtual void swapBuffersImplementation();
    
    /** Callback function for making the OpenGL context current (so it can be drawn on) */
    void setMakeCurrentFunction(void (*fcn)(unsigned int *winID, bool *success));
    
    /** Callback function for updating the OpenGL context after qualifying events (e.g. resize)
        This applies on OSX Cocoa, in which the context must be updated when the window is reshaped */
    void setUpdateContextFunction(void (*fcn)(unsigned int *winID, bool *success));
    bool updateContextImplementation();
    
    /** Callback function for swapping the front/back buffers */
    void setSwapBuffersFunction(void (*fcn)(unsigned int *winID));
    
    /** Callback function for realizing a window (TBD as needed) */
    virtual bool realizeImplementation();
    virtual bool isRealizedImplementation() const  { return _realized; }
    
    /** Dummy implementations, assume that graphics context is *always* current and valid. */
    virtual bool valid() const { return true; }
    virtual bool releaseContextImplementation() { return true; }
    virtual void closeImplementation() {}
    virtual void grabFocus() {}
    virtual void grabFocusIfPointerInWindow() {}
    virtual void raiseWindow() {}
    
  protected:
    virtual ~EmbeddedGraphics();
    
    /** Context management callback functions. */
    void (*_makeCurrent)(unsigned int *winID, bool *success);
    void (*_updateContext)(unsigned int *winID, bool *success);
    void (*_swapBuffers)(unsigned int *winID);
    
    WindowProxy *_window; // Pointer to the WindowProxy that represents the window
    bool _realized; // Whether this context has been realized
  };
  
  /** The WindowEventHandler handles incoming events. */
  class OF_EXPORT WindowEventHandler : public osgGA::GUIEventHandler
  {
  public:
    WindowEventHandler(WindowProxy *window);
    
    // Event handler function
    bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa);
    
    // Indicate that the associated WindowProxy has been modified in some way
    void windowModified();
    
    // Set the callback functions for various events. These will be called when the corresponding
    // event occurs (ie user presses a key, moves the mouse, etc...).
    void setKeyPressCallback(void (*fcn)(KEYPRESS_SIG)) // Keyboard key press callback
    { _keyPressCallback = fcn; }
    
    void setMouseMotionCallback(void (*fcn)(MOUSEMOTION_SIG)) // Mouse moved callback
    { _mouseMotionCallback = fcn; }
    
    void setButtonPressCallback(void (*fcn)(BUTTON_SIG)) // Mouse button pressed callback
    { _buttonPressCallback = fcn; }
    
    void setButtonReleaseCallback(void (*fcn)(BUTTON_SIG)) // Mouse button released callback
    { _buttonReleaseCallback = fcn; }

    void setVREventCallback(void (*fcn)(VR_SIG)) // VR controller event callback
    { _vrEventCallback = fcn; }
    
  protected:
    virtual ~WindowEventHandler();
    
    void getRenderRectangle(float x, float y, unsigned int &row, unsigned int &col);
    void selectRenderRectangle(unsigned int row, unsigned int col);
    
    WindowProxy *_window;
    unsigned int _currentRow, _currentCol;
    
    void (*_keyPressCallback)(KEYPRESS_SIG);
    void (*_mouseMotionCallback)(MOUSEMOTION_SIG);
    void (*_buttonPressCallback)(BUTTON_SIG);
    void (*_buttonReleaseCallback)(BUTTON_SIG);
    void (*_vrEventCallback)(VR_SIG);
  };
  
  /**********************************************************
   * Ravi Mathur
   * OpenFrames API, class WindowProxy
   * This class defines an interface which can draw a scene onto any window,
   * regardless of how that window is created or managed.  This allows the scene to
   * be drawn on a window created by (for example) a GUI management API such as gtk
   * or Winteracter.  Drawing is thread safe and special consideration is given for
   * OpenGL implementations which only allow one current context for all threads.
   **********************************************************/
  class OF_EXPORT WindowProxy : public OpenThreads::Thread, public osg::Referenced
  {
  public:
    // A list of RenderRectangles for each grid position
    typedef std::vector<osg::ref_ptr<RenderRectangle> > RenderList;
    
    WindowProxy(int x, int y,
                unsigned int width, unsigned int height,
                unsigned int nrow, unsigned int ncol,
                bool embedded = false, bool useVR = false);
    
    virtual void cancelCleanup();
    
    /** Set the window's name
     Only applies to non-embedded windows. For embedded windows, the window name
     should be set via the API used to create the window (Qt, wxWidgets, etc.) */
    void setWindowName(const std::string& name);
    
    unsigned int getWindowWidth() const;
    unsigned int getWindowHeight() const;
    
    bool isEmbedded() const { return _isEmbedded; }
    
    /** These functions should be called when keyboard/mouse input is
	    recieved from your own Window Manager, or if you want to simulate
	    user input to the WindowProxy.
	    Note that x and y coordinates should be in the range
	    [0, width] and [0, height] respectively, representing the extents of the window. */
    void keyPress(int key);
    void mouseMotion(float x, float y);
    void buttonPress(float x, float y, unsigned int button);
    void buttonRelease(float x, float y, unsigned int button);
    void resizeWindow(int x, int y, unsigned int width, unsigned int height);
    
    /** Resize each RenderRectangle grid with the specified dimensions */
    void setupGrid(unsigned int width, unsigned int height);
    
    /** Shut down the WindowProxy entirely. */
    void shutdown() { _viewer->setDone(true); }
    
    /** Time control */
    void setTime(double time);
    double getTime() const
    {
      if(_timeSyncWinProxy.valid()) return _timeSyncWinProxy->getTime();
      else return _currTime;
    }
    
    /// Control allowable time range; computed simulation time will always obey this
    void setTimeRange(double tMin, double tMax);
    void getTimeRange(double& tMin, double& tMax) const;
    
    void pauseTime(bool pause);
    bool isTimePaused() const
    {
      if(_timeSyncWinProxy.valid()) return _timeSyncWinProxy->isTimePaused();
      else return _timePaused;
    }
    
    void setTimeScale(double tscale);
    double getTimeScale() const
    {
      if(_timeSyncWinProxy.valid()) return _timeSyncWinProxy->getTimeScale();
      else return _timeScale;
    }
    
    /** Synchronize time with another WindowProxy
        This causes all time control to be forwarded to the other WindowProxy
        Inputs:
          NULL or this: Stop synchronizing time
          Otherwise: Synchronize with specified WindowProxy
        Return: 
          true: success, synchronized with input
          false: circular dependency detected, synchronization unchanged */
    bool synchronizeTime(WindowProxy *winproxy);
    WindowProxy* getTimeSyncWindow() const { return _timeSyncWinProxy.get(); }
    
    /** The state of this WindowProxy's animation loop */
    enum AnimationState
    {
      IDLE = 0,      // Waiting to initiate animation
      INITIALIZING,  // Initializing animation
      ANIMATING,     // Inside animation loop, and actively rendering
      PAUSED,        // Inside animation loop, but rendering is paused
      ERROR,         // Error during initialization
      SUCCESS        // Successfully finished animating
    };
    
    /// Animation loop control (animation means process events, update objects, and render scene)
    /// Set the desired animation pause state. If currently animating, then return after actual
    /// animation state matches the desired animation state. Otherwise return immediately.
    /// NOTE: pauseAnimation should never be called from the render thread, which includes
    /// user-defined callback functions.
    void pauseAnimation(bool pause);
    
    /// Get the current animation state
    AnimationState getAnimationState() const { return _animationState; }
    
    /// Determine whether WindowProxy is in its animation loop
    bool isAnimating() const { return ((_animationState == ANIMATING) || (_animationState == PAUSED)); }
    
    /// Determine whether WindowProxy is done animating
    bool doneAnimating() const { return ((_animationState == ERROR) || (_animationState == SUCCESS)); }
    
    /// If animation is done, then reset the animation state
    void resetAnimationState() { if(doneAnimating()) _animationState = IDLE; }
    
    /** Set the desired framerate at which animation should occur. */
    inline void setDesiredFramerate(const double &fps)
    { _frameThrottle.setDesiredFramerate(fps); }
    
    /** Get the desired framerate at which animation should occur. */
    inline double getDesiredFramerate()
    { return _frameThrottle.getDesiredFramerate(); }
    
    /** Get the actual animation framerate. This is an instantaneous value. */
    inline double getFramerate()
    { return _frameThrottle.getFramerate(); }
    
    osgViewer::CompositeViewer* getViewer() const { return _viewer.get(); }
    
    /** Set the render grid's dimensions. */
    void setGridSize(unsigned int row, unsigned int col);
    inline unsigned int getNumRows() const { return _nRow; }
    inline unsigned int getNumCols() const { return _nCol; }
    
    void setScene(FrameManager *fm, unsigned int row, unsigned int col);
    FrameManager* getScene(unsigned int row, unsigned int col);
    
    /** Get the RenderRectangle used for the given grid position */
    RenderRectangle* getGridPosition(unsigned int row, unsigned int col);
    
    /** Callback function for making the OpenGL context current */
    void setMakeCurrentFunction(void (*fcn)(unsigned int *winID, bool *success));
    
    /** Callback function to update the OpenGL context. Called whenever
	    a renderer or geometry-changing event is handled.
	    e.g. on OSX/Cocoa this is required on each resize event */
    void setUpdateContextFunction(void (*fcn)(unsigned int *winID, bool *success));
    
    /** Callback function for swapping the front/back buffers */
    void setSwapBuffersFunction(void (*fcn)(unsigned int *winID));
    
    /** Set callback functions for events */
    void setKeyPressCallback(void (*fcn)(KEYPRESS_SIG))
    { _eventHandler->setKeyPressCallback(fcn); }
    
    void setMouseMotionCallback(void (*fcn)(MOUSEMOTION_SIG))
    { _eventHandler->setMouseMotionCallback(fcn); }
    
    void setButtonPressCallback(void (*fcn)(BUTTON_SIG))
    { _eventHandler->setButtonPressCallback(fcn); }
    
    void setButtonReleaseCallback(void (*fcn)(BUTTON_SIG))
    { _eventHandler->setButtonReleaseCallback(fcn); }

    void setVREventCallback(void (*fcn)(VR_SIG))
    { _eventHandler->setVREventCallback(fcn); }
    
    /** Set the window id that will be passed into all user-defined
	    callback functions. */
    inline void setID(unsigned int id) { _winID = id; }
    inline unsigned int getID() const { return _winID; }
    
    /** Get whether VR rendering is enabled */
    bool getUseVR() { return _useVR; }

    /** Get the OpenVRDevice object that manages VR rendering */
    const OpenVRDevice* getOpenVRDevice() const { return _ovrDevice; }

    /** Set the ratio of WorldUnits to Meters used for VR rendering */
    void setWorldUnitsPerMeter(double worldUnitsPerMeter)
    {
      if (_useVR) _ovrDevice->setWorldUnitsPerMeter(worldUnitsPerMeter);
    }
    double getWorldUnitsPerMeter()
    {
      if (_useVR) return _ovrDevice->getWorldUnitsPerMeter();
      else return 1.0; // Unused if VR is disabled
    }

    /** Set the limits of the WorldUntis/Meter ratio */
    void setWorldUnitsPerMeterLimits(const double& minWorldUnitsPerMeter, const double& maxWorldUnitsPerMeter)
    {
      if (_useVR) _ovrDevice->setWorldUnitsPerMeterLimits(minWorldUnitsPerMeter, maxWorldUnitsPerMeter);
    }
    void getWorldUnitsPerMeterLimits(double &minWorldUnitsPerMeter, double &maxWorldUnitsPerMeter)
    {
      if (_useVR) _ovrDevice->getWorldUnitsPerMeterLimits(minWorldUnitsPerMeter, maxWorldUnitsPerMeter);
    }

    /** Set the user height in VR */
    void setUserHeight(const double &height)
    {
      if (_useVR) _ovrDevice->setUserHeight(height);
    }

    /** Inherited from OpenThreads::Thread. Called on thread launch. */
    virtual void run();
    
    /** Print info about this window to std::cout. */
    void printInfo();
    
  protected:
    virtual ~WindowProxy();
    
    bool setupWindow();
    void collectScenes();
    void frame();
    
    /** ID of this window. This will be used to identify this window to all
	    user-defined callback functions */
    unsigned int _winID;
    
    unsigned int _nRow, _nCol; // Number of rows/columns in this window's grid
    
    bool _isEmbedded; // True if the user wants to provide their own OpenGL window
    
    RenderList _renderList;  // List of subwindows to be rendered
    
    typedef std::set<FrameManager*> SceneSet;
    SceneSet _scenes; // Set of all unique scenes
    
    /** The CompositeViewer handles drawing several scenes onto a single drawing surface (a window) */
    osg::ref_ptr<osgViewer::CompositeViewer> _viewer;
    
    /** The GraphicsWindow is the actual window that is drawn onto */
    osg::ref_ptr<osgViewer::GraphicsWindow> _window;
    
    /** The EmbeddedGraphics object allows rendering onto a user-provided OpenGL window */
    osg::ref_ptr<EmbeddedGraphics> _embeddedGraphics;
    
    /** The WindowEventHandler handles user events that occur in the window */
    osg::ref_ptr<WindowEventHandler> _eventHandler;
    
    FramerateLimiter _frameThrottle; // Controls animation framerate
    
    /** Time control variables */
    AnimationState _animationState; // Current animation state
    bool _pauseAnimation;           // Indicate that animation should be paused
    bool _timePaused;
    osg::Timer_t _Tref;
    double _currTime, _offsetTime, _timeScale;
    double _minTime, _maxTime;
    osg::observer_ptr<WindowProxy> _timeSyncWinProxy;
    
    bool _useVR; // Whether to use VR rendering
    osg::ref_ptr<OpenVRDevice> _ovrDevice; // OpenVR interface
    osg::ref_ptr<VRTextureBuffer> _vrTextureBuffer; // VR texture buffers
  };
  
}

#endif
