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

#include <OpenFrames/OF_Interface.h>
#include <OpenFrames/CoordinateAxes.hpp>
#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/LatLonGrid.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/RadialPlane.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/SegmentArtist.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/TrajectoryArtist.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <OpenThreads/Thread>
#include <map>
#include <string>
#include <iostream>

#ifdef OF_USE_X11
        #include <X11/Xlib.h>
#endif

#ifdef WIN32
	#include <windows.h>
#else
	#include <unistd.h>
#endif

using namespace OpenFrames;

typedef std::map<unsigned int, osg::ref_ptr<WindowProxy> > WindowMap;
typedef std::map<std::string, osg::ref_ptr<ReferenceFrame> > FrameMap;
typedef std::map<int, osg::ref_ptr<FrameManager> > FMMap;
typedef std::map<std::string, osg::ref_ptr<Trajectory> > TrajectoryMap;
typedef std::map<std::string, osg::ref_ptr<TrajectoryArtist> > ArtistMap;
typedef std::map<std::string, osg::ref_ptr<View> > ViewMap;

/** This class keeps track of all OpenFrames objects created using this
 * interface. It is implemented as a singleton becuase it handles ALL
 * OpenFrames objects and must not have multiple instances. */
class OF_Objects : public osg::Referenced
{
  public:
	// The 'current' object of any type is the one acted upon by functions
	// that operate on that type. e.g. all ReferenceFrame functions
	// will act on _currFrame, all View functions on _currView, etc...
	ReferenceFrame *_currFrame; ///< Current active ReferenceFrame
    WindowProxy *_currWinProxy; ///< Current active WindowProxy
    Trajectory *_currTraj; ///< Current active Trajectory
    FrameManager *_currFM; ///< Current active FrameManager
    TrajectoryArtist *_currArtist; ///< Current active TrajectoryArtist
    View *_currView; ///< Current active View

	WindowMap _winMap;  ///< Map of ID -> WindowProxy
	FrameMap _frameMap; ///< Map of Name -> ReferenceFrame
	FMMap _fmMap;     ///< Map of ID -> FrameManager
	TrajectoryMap _trajMap; ///< Map of ID -> Trajectory
	ArtistMap _artistMap; ///< Map of ID -> TrajectoryArtist
	ViewMap _viewMap; ///< Map of ID -> View

	osg::ref_ptr<TimeManagementVisitor> _tmv; ///< Manages the progression of time

	/// Value returned (if any) by last function call
	int _intVal;

    /**
	* \brief Get an instance of the singleton
    *
	* If this is the first call, then the object will be created
    *
	* \return Instance of the singleton
    **/
	static OF_Objects* instance()
	{
#ifdef OF_USE_X11
          // It's likely that a 3rd-party GUI toolkit (like Winteracter)
          // might use X11, so we need to init X11 threads
          XInitThreads();
#endif

	  static osg::ref_ptr<OF_Objects> _objsref = new OF_Objects;

	  // If cleanup has already happened, then delete this instance of
	  // OF_Objects (release all references) and create a new one
	  if(!_objsref->_needsCleanup)
	  {
	    _objsref = NULL;
	    _objsref = new OF_Objects;
	  }

	  return _objsref.get();
	}

    /**
    * \brief Clean up all OpenFrames objects.
    **/
	void cleanup()
	{
	  // Shut down each WindowProxy's thread
	  for(WindowMap::iterator i = _winMap.begin(); i != _winMap.end(); ++i)
	  {
	    i->second->shutdown();
	  }

	  // Sleep to let threads shut down
#ifdef WIN32
	  Sleep(500);
#else
	  usleep(500000);
#endif

	  // Reset all internal pointers
	  _currFrame = NULL;
	  _currWinProxy = NULL;
	  _currTraj = NULL;
	  _currFM = NULL;
	  _currArtist = NULL;
	  _currView = NULL;

	  // Delete all references to objects
	  // The objects wil be deleted automatically
	  _winMap.clear(); // Each running WindowProxy will be shut down
	  _frameMap.clear();
	  _fmMap.clear();
	  _trajMap.clear();
	  _artistMap.clear();
	  _viewMap.clear();

	  _intVal = 0;

	  _needsCleanup = false; // Indicate that cleanup is complete
	}

  private:
	bool _needsCleanup;

	OF_Objects() 
	: _currFrame(NULL), _currWinProxy(NULL), _currTraj(NULL),
	  _currFM(NULL), _currArtist(NULL), _currView(NULL), _intVal(0),
	  _needsCleanup(true)
	{
	  _tmv = new TimeManagementVisitor;
	}

	~OF_Objects()
	{
	  // Make sure the user called cleanup
	  if(_needsCleanup)
	  {
	    std::cerr<< "OpenFrames WARNING: of_cleanup() must be called before the application exits!" << std::endl;
	  }
	}
};
OF_Objects *_objs = NULL;

/**
* \brief Sets up all internal data. Must be called before using OpenFrames.
**/
void OF_FCN(of_initialize)()
{
	_objs = OF_Objects::instance();
    _objs->_intVal = 0;
}

/**
* \brief Cleans up all internal data. Must be called when done using OpenFrames.
*
* \warning DO NOT call of_getreturnedvalue after calling of_cleanup.
*          This function sets the pointer for _objs to NULL, which would be dereferenced by of_cleanup.
**/
void OF_FCN(of_cleanup)()
{
	if(_objs) 
	{
	  // Clean up all OpenFrames objects
	  _objs->cleanup();

	  // Reset the main OpenFrames object pointer, which will require
	  // a call to of_initialize() before using OpenFrames again
	  _objs = NULL;
	}
	else std::cerr<< "OpenFrames WARNING: Must call of_initialize() before calling of_cleanup()" << std::endl;
}

/**
* \brief Get the return value returned by the most recent API function call.
*
* Functions return zero to indicate success, and non-zero to indicate an error.
* \warning DO NOT call of_getreturnedvalue after calling of_cleanup.
*          of_cleanup sets the pointer for _objs to NULL, which would be dereferenced by this function.
*
* \param val variable to store the return value from the previous API function call
**/
void OF_FCN(of_getreturnedvalue)(int *val)
{
	*val = _objs->_intVal;
}

/***********************************************
	Window Functions
***********************************************/

/**
* \brief Set the active WindowProxy
*
* \param id ID of the window to activate
**/
void OF_FCN(ofwin_activate)(unsigned int *id)
{
	WindowMap::iterator i = _objs->_winMap.find(*id); // Find the WindowProxy with the requested id

	if(i == _objs->_winMap.end()) // Not found
	{
	  _objs->_currWinProxy = NULL; // Deselect the currently selected WindowProxy
	  _objs->_intVal = 1; // Throw an error
	}
	else 
	{
	  _objs->_currWinProxy = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Create a new WindowProxy that will manage drawing onto a window.
*
* This new WindowProxy will also become the current active one.
*
* \param x        X-coordinate (in pixels) of the screen of the upper-right corner of the window
* \param y        Y-coordinate (in pixels) of the screen of the upper-right corner of the window
* \param width    Width of the window (in pixels)
* \param height   Height of the window (in pixels)
* \param nrow     Number of rows in the window grid
* \param ncol     Number of columns in the window grid
* \param embedded True if the user wants to provide their own OpenGL window
* \param id       ID of this window
**/
void OF_FCN(ofwin_createproxy)(int *x, int *y,
                            unsigned int *width, unsigned int *height,
                            unsigned int *nrow, unsigned int *ncol,
                            bool *embedded, unsigned int *id)
{
	// Create the new WindowProxy with the given ID
	WindowProxy* wp = new WindowProxy(*x, *y, *width, *height, *nrow, *ncol, *embedded);
	wp->setID(*id);

	// If a WindowProxy already exists with the same id, stop its thread
	WindowMap::iterator i = _objs->_winMap.find(*id);
	if(i != _objs->_winMap.end())
	{
	  i->second->shutdown();
	  while(i->second->isRunning()){}
	}

	// Add the new WindowProxy to the map, replacing any previous one
	_objs->_winMap[*id] = wp;
    _objs->_currWinProxy = wp;
    _objs->_intVal = 0;
}

/**
* \brief Set the number rows and columns in the grid.
*
* This applies to the current active WindowProxy.
*
* \param nrow     Number of rows in the window grid
* \param ncol     Number of columns in the window grid
**/
OF_EXPORT void OF_FCN(ofwin_setgridsize)(int *nrow, int *ncol)
{
	if(_objs->_currWinProxy)
	{
	  _objs->_currWinProxy->setGridSize(*nrow, *ncol);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function to be called on keypress.
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called on keypress
**/
OF_EXPORT void OF_FCN(ofwin_setkeypresscallback)(void (*fcn)(KEYPRESS_SIG))
{
	if(_objs->_currWinProxy) 
	{
      _objs->_currWinProxy->setKeyPressCallback(fcn);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function to be called on mouse motion.
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called on mouse motion
**/
OF_EXPORT void OF_FCN(ofwin_setmousemotioncallback)(void (*fcn)(MOUSEMOTION_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setMouseMotionCallback(fcn);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function to be called on button press.
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called on button press
**/
OF_EXPORT void OF_FCN(ofwin_setbuttonpresscallback)(void (*fcn)(BUTTON_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setButtonPressCallback(fcn);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function to be called on button release.
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called on button release
**/
OF_EXPORT void OF_FCN(ofwin_setbuttonreleasecallback)(void (*fcn)(BUTTON_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setButtonReleaseCallback(fcn);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Start animation
*
* This applies to the current active WindowProxy.
**/
void OF_FCN(ofwin_start)()
{
	if(_objs->_currWinProxy) 
	{
	  FramerateLimiter waitLimiter;
	  waitLimiter.setDesiredFramerate(5.0); // FPS at which to check

	  // Tell WindowProxy to start animating, then wait until it actually starts
	  _objs->_intVal = _objs->_currWinProxy->startThread();
	  while(!_objs->_currWinProxy->isAnimating()) 
	  {
	    waitLimiter.frame();
	  }
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Force animation to stop
*
* This applies to the current active WindowProxy.
**/
void OF_FCN(ofwin_stop)()
{
	if(_objs->_currWinProxy) 
	{
	  FramerateLimiter waitLimiter;
	  waitLimiter.setDesiredFramerate(5.0); // FPS at which to check

	  // Tell WindowProxy to stop animating, then wait until it actually stops
	  _objs->_currWinProxy->shutdown();
	  while(_objs->_currWinProxy->isRunning()) 
	  {
	    waitLimiter.frame();
	  }
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Wait for user to exit animation
*
* This applies to the current active WindowProxy.
**/
void OF_FCN(ofwin_waitforstop)()
{
	if(_objs->_currWinProxy)
	{
	  FramerateLimiter waitLimiter;
	  waitLimiter.setDesiredFramerate(1.0); // Check once per second

	  // Lazily wait for the WindowProxy to stop animating on its own
	  // This will happen if the user presses 'esc' or closes the window
	  while(_objs->_currWinProxy->isRunning())
	  {
		waitLimiter.frame();
      }
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Pause/unpause the animation
*
* This applies to the current active WindowProxy.
*
* \param pause True to pause the animation, False to unpause the animation
**/
OF_EXPORT void OF_FCN(ofwin_pauseanimation)(bool *pause)
{
	if(_objs->_currWinProxy)
	{
	  _objs->_currWinProxy->pauseAnimation(*pause);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Check if the animation is running
*
* This applies to the current active WindowProxy.
*
* \param state This variable is set to 1 if the animation is running, 0 otherwise
**/
OF_EXPORT void OF_FCN(ofwin_isrunning)(unsigned int *state)
{
	if(_objs->_currWinProxy)
	{
	  *state = _objs->_currWinProxy->isRunning();
      _objs->_intVal = 0;
	}
    else {
      *state = 0;
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the scene at the specified grid position
*
* This applies to the current active WindowProxy.
* The scene is specified by the currently active FrameManager.
*
* \param row Row in the grid to set
* \param col Column in the grid to set
**/
void OF_FCN(ofwin_setscene)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy) {
      _objs->_currWinProxy->setScene(_objs->_currFM, *row, *col);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the 3D stereo mode for the specified grid position
*
* This applies to the current active WindowProxy.
*
* \param row           Row in the grid to set
* \param col           Column in the grid to set
* \param enable        True to enable 3D stereo mode
* \param eyeseparation Set eye separation for 3D stereo
* \param width         Width of the screen
* \param height        Height of the screen
* \param distance      Distance of the screen
**/
void OF_FCN(ofwin_setstereo)(unsigned int *row, unsigned int *col, bool *enable,
                          float *eyeseparation, float *width, float *height, float *distance)
{
	if(_objs->_currWinProxy)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr) 
	  {
		osg::DisplaySettings *ds = rr->getSceneView()->getDisplaySettings();
		if(ds)
		{
		  ds->setStereo(*enable);
		  if(*eyeseparation > 0.0) ds->setEyeSeparation(*eyeseparation);
		  if(*width > 0.0) ds->setScreenWidth(*width);
		  if(*height > 0.0) ds->setScreenHeight(*height);
		  if(*distance > 0.0) ds->setScreenDistance(*distance);
          _objs->_intVal = 0;
        }
        else {
          _objs->_intVal = 1;
        }
      }
      else {
        _objs->_intVal = 2;
      }
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the background color of the specified grid position
*
* This applies to the current active WindowProxy.
*
* \param row Row in the grid to set
* \param col Column in the grid to set
* \param r   Red color component [0-1]
* \param g   Green color component [0-1]
* \param b   Blue color component [0-1]
**/
void OF_FCN(ofwin_setbackgroundcolor)(unsigned int *row, unsigned int *col, 
                                   float *r, float *g, float *b)
{
	if(_objs->_currWinProxy)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
      if (rr) { 
        rr->setBackgroundColor(*r, *g, *b);
        _objs->_intVal = 0;
      }
      else {
        _objs->_intVal = 1;
      }
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the background texture of the specified grid position
*
* This applies to the current active WindowProxy.
*
* \param row   Row in the grid to set
* \param col   Column in the grid to set
* \param fname File containing the background texture
**/
void OF_FCN(ofwin_setbackgroundtexture)(unsigned int *row, unsigned int *col, OF_CHARARG(fname))
{
	if(_objs->_currWinProxy)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr) 
	  {
	    // Convert given character string and length to a proper C string
	    std::string temp(OF_STRING(fname));
        rr->setSkySphereTexture(temp);
        _objs->_intVal = 0;
      }
      else {
        _objs->_intVal = 1;
      }
	}
    else {
      _objs->_intVal = -2;
    }
}


/**
* \brief Set the background star field of the specified grid position
*
* This applies to the current active WindowProxy.
*
* \param row    Row in the grid to set
* \param col    Column in the grid to set
* \param minMag Minimum star magnitude to show
* \param maxMag Maximum star magnitude to show
* \param fname  File containing the background star field catalog data
**/
void OF_FCN(ofwin_setbackgroundstardata)(unsigned int *row, unsigned int *col, float *minMag, float *maxMag, OF_CHARARG(fname))
{
	if(_objs->_currWinProxy)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr) 
	  {
	    // Convert given character string and length to a proper C string
	    std::string temp(OF_STRING(fname));
        rr->setSkySphereStarData(temp, *minMag, *maxMag);
        _objs->_intVal = 0;
      }
      else {
        _objs->_intVal = 1;
      }
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function for swapping the front/back buffers
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called
**/
void OF_FCN(ofwin_setswapbuffersfunction)(void (*fcn)(unsigned int *winID))
{
    if (_objs->_currWinProxy) {
      _objs->_currWinProxy->setSwapBuffersFunction(fcn);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function for making the OpenGL context current (so it can be drawn on)
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called
**/
void OF_FCN(ofwin_setmakecurrentfunction)(void (*fcn)(unsigned int *winID, bool *success))
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->setMakeCurrentFunction(fcn);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set a callback function for updating the OpenGL context after qualifying events
*
* Currently this includes resize
*
* This applies to the current active WindowProxy.
*
* \param fcn Callback function to be called
**/
void OF_FCN(ofwin_setupdatecontextfunction)(void (*fcn)(unsigned int *winID, bool *success))
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->setUpdateContextFunction(fcn);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Resize the window to a new position and size
*
* This applies to the current active WindowProxy.
*
* \param x      X-coordinate (in pixels) of the screen of the upper-right corner of the window
* \param y      Y-coordinate (in pixels) of the screen of the upper-right corner of the window
* \param width  Width of the window (in pixels)
* \param height Height of the window (in pixels)
**/
void OF_FCN(ofwin_resizewindow)(int *x, int *y, unsigned int *width, unsigned int *height)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->resizeWindow(*x, *y, *width, *height);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Create a key pressed event
*
* This applies to the current active WindowProxy.
*
* \param key Key pressed (see osg::GUIEventAdapter::KeySymbol enum)
**/
void OF_FCN(ofwin_keypress)(unsigned int *key)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->keyPress(*key);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Create a mouse button pressed event
*
* This applies to the current active WindowProxy.
*
* \param x      X-coordinate of the mouse in the window
* \param y      Y-coordinate of the mouse in the window
* \param button Mouse button pressed. Button numbering is 1 for left mouse button, 2 for middle, 3 for right.
**/
void OF_FCN(ofwin_buttonpress)(float *x, float *y, unsigned int *button)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->buttonPress(*x, *y, *button);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Create a mouse button released event
*
* This applies to the current active WindowProxy.
*
* \param x      X-coordinate of the mouse in the window
* \param y      Y-coordinate of the mouse in the window
* \param button Mouse button released. Button numbering is 1 for left mouse button, 2 for middle, 3 for right.
**/
void OF_FCN(ofwin_buttonrelease)(float *x, float *y, unsigned int *button)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->buttonRelease(*x, *y, *button);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Create a mouse moved event
*
* This applies to the current active WindowProxy.
*
* \param x X-coordinate of the mouse in the window
* \param y Y-coordinate of the mouse in the window
**/
void OF_FCN(ofwin_mousemotion)(float *x, float *y)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->mouseMotion(*x, *y);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the desired framerate of the window
*
* This applies to the current active WindowProxy.
*
* \param fps Desired framerate value in frames per second
**/
void OF_FCN(ofwin_setdesiredframerate)(double *fps)
{
	if (_objs->_currWinProxy) {
	  _objs->_currWinProxy->setDesiredFramerate(*fps);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Add a view to the window
*
* This applies to the current active WindowProxy.
* This adds the current acive View.
*
* \param row Row to add the view to
* \param col Column to add the view to
**/
void OF_FCN(ofwin_addview)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy && _objs->_currView)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr)
	  {
	    rr->addView(_objs->_currView);
	    _objs->_intVal = 0;
	  }
	  else _objs->_intVal = 1;
    }
    else if (!_objs->_currView) {
      _objs->_intVal = -1;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Remove a view from the window
*
* This applies to the current active WindowProxy.
* This removes the current acive View.
*
* \param row Row to remove the view from
* \param col Column to remove the view from
**/
void OF_FCN(ofwin_removeview)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy && _objs->_currView)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr)
	  {
	    rr->removeView(_objs->_currView);
	    _objs->_intVal = 0;
	  }
	  else _objs->_intVal = 1;
    }
    else if (!_objs->_currView) {
      _objs->_intVal = -1;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Remove all the view(s) from the window
*
* This applies to the current active WindowProxy.
*
* \param row Row to remove the view(s) from
* \param col Column to remove the view(s) from
**/
void OF_FCN(ofwin_removeallviews)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy && _objs->_currView)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr)
	  {
	    rr->removeAllViews();
	    _objs->_intVal = 0;
	  }
	  else _objs->_intVal = 1;
	}
    else if (!_objs->_currView) {
      _objs->_intVal = -1;
    }
    else {
      _objs->_intVal = -2;
    }
}


/**
* \brief Set the view currently displayed in the window
*
* This applies to the current active WindowProxy.
* This selects the current acive View.
*
* \param row Row to set the active view in
* \param col Column to set the active view in
**/
void OF_FCN(ofwin_selectview)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy && _objs->_currView)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr) 
	  {
	    rr->selectView(_objs->_currView);
	    _objs->_intVal = 0;
	  }
	  else _objs->_intVal = 1;
	}
    else if (!_objs->_currView) {
      _objs->_intVal = -1;
    }
    else {
      _objs->_intVal = -2;
    }
}

/*******************************************
	FrameManager Functions
*******************************************/

/**
* \brief Set the currently active FrameManager
*
* \param id ID of the frame manager to activate.
**/
void OF_FCN(offm_activate)(int *id)
{
	FMMap::iterator i = _objs->_fmMap.find(*id);
	if(i == _objs->_fmMap.end())
	{
	  _objs->_currFM = NULL;
	  _objs->_intVal = 1;
	}
	else 
	{
	  _objs->_currFM = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Create a new FrameManager with the given ID
*
* This new FrameManager will also become the current active one.
* 
* \param id ID of the frame manager to create
**/
void OF_FCN(offm_create)(int *id)
{
	_objs->_currFM = new FrameManager;
    _objs->_fmMap[*id] = _objs->_currFM;
    _objs->_intVal = 0;
}

/**
* \brief Assign a ReferenceFrame to the FrameManager
*
* This applies to the current active FrameManager.
* The scene is specified by the currently active ReferenceFrame.
**/
void OF_FCN(offm_setframe)()
{
    if (_objs->_currFM) {
      _objs->_currFM->setFrame(_objs->_currFrame);
      _objs->_intVal = 0;
    }
	else 
	{
	  _objs->_intVal = -2;
	}
}

/**
* \brief Lock the current FrameManager
*
* This should be done before the FrameManager's scene is modified.
* This applies to the current active FrameManager.
**/
void OF_FCN(offm_lock)()
{
	if (_objs->_currFM) {
	  _objs->_intVal = _objs->_currFM->lock();
      _objs->_intVal = 0;
    }
	else 
	{
	  _objs->_intVal = -2;
	}
}

/**
* \brief Unlock the current FrameManager
*
* This should be done after the FrameManager's scene is modified.
* This applies to the current active FrameManager.
**/
void OF_FCN(offm_unlock)()
{
    if (_objs->_currFM) {
	  _objs->_intVal = _objs->_currFM->unlock();
      _objs->_intVal = 0;
    }
	else 
	{
	  _objs->_intVal = -2;
	}
}

/*******************************************
	ReferenceFrame Functions
*******************************************/

/**
* \brief Set the currently active reference frame
*
* \param name Name of the frame to activate
**/
void OF_FCN(offrame_activate)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	// If requested frame does not exist, raise the error flag  
	FrameMap::iterator i = _objs->_frameMap.find(temp);
	if(i == _objs->_frameMap.end()) 
	{
	  _objs->_currFrame = NULL;
	  _objs->_intVal = 1;
	}
	else 
	{
	  _objs->_currFrame = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Create a new ReferenceFrame with the given name
*
* This new ReferenceFrame will also become the current active one.
*
* \param name Name of the frame to create
**/
void OF_FCN(offrame_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));
  
	FrameMap::iterator i = _objs->_frameMap.find(temp);
	if(i == _objs->_frameMap.end()) // Make sure frame doesn't already exist
	{
	  // Create the new ReferenceFrame and make it the active one
	  _objs->_currFrame = new ReferenceFrame(temp);
	  _objs->_frameMap[temp] = _objs->_currFrame;
      _objs->_intVal = 0;
	}
    else {
      _objs->_currFrame = i->second.get();
      _objs->_intVal = 1;
    }
}

/**
* \brief Set the color of the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param r Red color component [0-1]
* \param g Green color component [0-1]
* \param b Blue color component [0-1]
* \param a Alpha (transparancy) component [0-1]
**/
void OF_FCN(offrame_setcolor)(float *r, float *g, float *b, float *a)
{
    if (_objs->_currFrame) {
      _objs->_currFrame->setColor(*r, *g, *b, *a);
      _objs->_intVal = 0;
    }
    else
    {
      _objs->_intVal = -2;
    }
}

/**
* \brief Add a child frame to the current frame
*
* This applies to the current active ReferenceFrame.
* The currently active frame will remain active.
*
* \param name Name of the frame to add as a child to the active frame
**/
void OF_FCN(offrame_addchild)(OF_CHARARG(name))
{
	if(_objs->_currFrame)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(name));

	  // Find desired child frame in frame map
	  FrameMap::iterator i = _objs->_frameMap.find(temp);
	  if(i == _objs->_frameMap.end()) _objs->_intVal = 1; // Not found, so raise error
	  else
	  {
	    _objs->_intVal = 0;
	    _objs->_currFrame->addChild(i->second.get()); // Found, so add it as child
	  }
	}
	else _objs->_intVal = -2; // Current frame isn't defined, so raise error
}

/**
* \brief Remove a child frame from the current frame
*
* This applies to the current active ReferenceFrame.
* The currently active frame will remain active.
*
* \param name Name of the child frame to remove from the active frame
**/
void OF_FCN(offrame_removechild)(OF_CHARARG(name))
{
	if(_objs->_currFrame)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(name));

	  // Find desired child frame in frame map
	  FrameMap::iterator i = _objs->_frameMap.find(temp);
	  if(i == _objs->_frameMap.end()) _objs->_intVal = 1; // Not found, so raise error
	  else
	  {
	    _objs->_intVal = 0;
	    _objs->_currFrame->removeChild(i->second.get()); // Found, so remove it as child
	  }
	}
	else _objs->_intVal = -2;
}

/**
* \brief Remove all child frames from the current frame
*
* This applies to the current active ReferenceFrame.
* The currently active frame will remain active.
**/
void OF_FCN(offrame_removeallchildren)()
{
	if(_objs->_currFrame)
	{
	  int numchildren = _objs->_currFrame->getNumChildren();
	  for(int i = numchildren-1; i >= 0; i--)
	  {
	    _objs->_currFrame->removeChild(_objs->_currFrame->getChild(i));
      }
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the number of child frames in the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param numchildren variable to store the number of child frames to
**/
void OF_FCN(offrame_getnumchildren)(int *numchildren)
{
	if(_objs->_currFrame) // Frame exists, so get number of children
	{
	  *numchildren = _objs->_currFrame->getNumChildren();
	  _objs->_intVal = 0;
	}
	else // Frame doesn't exist, so set error flag
	{
	  *numchildren = 0;
	  _objs->_intVal = 1; // Error
	}
}

/**
* \brief Set the position of the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param x X position
* \param y Y position
* \param z Z position
**/
void OF_FCN(offrame_setposition)(double *x, double *y, double *z)
{
    if (_objs->_currFrame) {
      _objs->_currFrame->setPosition(*x, *y, *z);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the position of the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param x Returned X position
* \param y Returned Y position
* \param z Returned Z position
**/
void OF_FCN(offrame_getposition)(double *x, double *y, double *z)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->getPosition(*x, *y, *z);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the attitude of the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param rx    X component of the rotation quaternion
* \param ry    Y component of the rotation quaternion
* \param rz    Z component of the rotation quaternion
* \param angle Angle component of the rotation quaternion
**/
void OF_FCN(offrame_setattitude)(double *rx, double *ry, double *rz, double *angle)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->setAttitude(*rx, *ry, *rz, *angle);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the attitude of the current frame
*
* This applies to the current active ReferenceFrame.
*
* \param rx    Returned X component of the rotation quaternion
* \param ry    Returned Y component of the rotation quaternion
* \param rz    Returned Z component of the rotation quaternion
* \param angle Returned angle component of the rotation quaternion
**/
void OF_FCN(offrame_getattitude)(double *rx, double *ry, double *rz, double *angle)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->getAttitude(*rx, *ry, *rz, *angle);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Toggle which axis components are displayed
*
* This applies to the current active ReferenceFrame.
* The axis is initially drawn at the origin of the reference frame unless otherwise
* specified by offrame_movexaxis(), offrame_moveyaxis(), or offrame_movezaxis()
*
* To show multiple axis components, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show
*
* \param axes Axis components to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden.
**/
void OF_FCN(offrame_showaxes)(unsigned int *axes)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->showAxes(*axes);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Toggle display of the name label of the frame
*
* This applies to the current active ReferenceFrame.
*
* \param namelabel True to display the label, false to hide it.
**/
void OF_FCN(offrame_shownamelabel)(bool *namelabel)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->showNameLabel(*namelabel);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Toggle which axis labels are displayed
*
* This applies to the current active ReferenceFrame.
* The axis is initially drawn at the origin of the reference frame unless otherwise
* specified by offrame_movexaxis(), offrame_moveyaxis(), or offrame_movezaxis()
*
* To show multiple axis labels, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show
*
* \param labels Axis labels to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden.
**/
void OF_FCN(offrame_showaxeslabels)(unsigned int *labels)
{
	if (_objs->_currFrame) {
	  _objs->_currFrame->showAxesLabels(*labels);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Change the name label for the current ReferenceFrame.
*
* Note that it will still be referred to using the name assigned to it at creation.
*
* This applies to the current active ReferenceFrame.
*
* \param name Name of the label.
**/
void OF_FCN(offrame_setnamelabel)(OF_CHARARG(name))
{
	if(_objs->_currFrame) 
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(name));
	  _objs->_currFrame->setName(temp);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Change the axes labels for the current ReferenceFrame.
*
* This applies to the current active ReferenceFrame.
*
* \param xlabel Name of the x-axis label.
* \param ylabel Name of the y-axis label.
* \param zlabel Name of the z-axis label.
**/
#if defined(IFORT_CALLS)
void OF_FCN(offrame_setaxeslabels)(const char *xlabel,
                                   const char *ylabel,
                                   const char *zlabel, 
                                   unsigned int xlabellen, 
                                   unsigned int ylabellen, 
                                   unsigned int zlabellen)
#else
void OF_FCN(offrame_setaxeslabels)(OF_CHARARG(xlabel),
                                   OF_CHARARG(ylabel),
                                   OF_CHARARG(zlabel))
#endif
{
        std::string _xlabel(OF_STRING(xlabel));
        std::string _ylabel(OF_STRING(ylabel));
        std::string _zlabel(OF_STRING(zlabel));

	if(_objs->_currFrame)
	{
	  _objs->_currFrame->setXLabel(_xlabel);
	  _objs->_currFrame->setYLabel(_ylabel);
	  _objs->_currFrame->setZLabel(_zlabel);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Reposition and resize the x component of the coordinate axis
*
* Make sure that offrame_showaxes() is configured to display this axis.
*
* This applies to the current active ReferenceFrame.
*
* \param pos        Position array of the origin of the x-axis component.
* \param length     Length of the x-axis arrow.
* \param headRatio  Ratio of the arrow head to body.
* \param bodyRadius Radius of the body of the arrow.
* \param headRadius Radius of the head of the arrow.
**/
void OF_FCN(offrame_movexaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveXAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Reposition and resize the y component of the coordinate axis
*
* Make sure that offrame_showaxes() is configured to display this axis.
*
* This applies to the current active ReferenceFrame.
*
* \param pos        Position array of the origin of the y-axis component.
* \param length     Length of the y-axis arrow.
* \param headRatio  Ratio of the arrow head to body.
* \param bodyRadius Radius of the body of the arrow.
* \param headRadius Radius of the head of the arrow.
**/
void OF_FCN(offrame_moveyaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveYAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Reposition and resize the z component of the coordinate axis
*
* Make sure that offrame_showaxes() is configured to display this axis.
*
* This applies to the current active ReferenceFrame.
*
* \param pos        Position array of the origin of the z-axis component.
* \param length     Length of the z-axis arrow.
* \param headRatio  Ratio of the arrow head to body.
* \param bodyRadius Radius of the body of the arrow.
* \param headRadius Radius of the head of the arrow.
**/
void OF_FCN(offrame_movezaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveZAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Have this frame follow the specified trajectory
*
* The name used is the one used in the trajecotries creation in oftraj_create().
*
* This applies to the current active ReferenceFrame.
*
* \param name Name of the trajectory to follow.
**/
void OF_FCN(offrame_followtrajectory)(OF_CHARARG(name))
{
	if(_objs->_currFrame)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(name));

	  // Get the trajectory with the specified name
	  Trajectory *traj;
	  TrajectoryMap::iterator i = _objs->_trajMap.find(temp);

	  // If trajectory not found, then remove any follower that is already
	  // applied on this frame.
	  if(i == _objs->_trajMap.end())
	  {
	    _objs->_currFrame->getTransform()->setUpdateCallback(NULL);
	    _objs->_intVal = 1; // Trajectory not found
	    return;
	  }
	  else traj = i->second.get();

	  // Check if the frame is already following a trajectory
	  TrajectoryFollower *tf = dynamic_cast<TrajectoryFollower*>(_objs->_currFrame->getTransform()->getUpdateCallback());
	    
	  // If not, then create a new TrajectoryFollower for the frame
	  if(tf == NULL)
	  {
	    tf = new TrajectoryFollower(traj);
	    _objs->_currFrame->getTransform()->setUpdateCallback(tf);
	  }
	  else // Otherwise just update the existing TrajectoryFollower
	  {
	    tf->setFollowTrajectory(traj);
	  }
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Follow the trajectory's position, attitude, or both, and set the follow mode
*
* The function offrame_followtrajectory() must be called before this function.
*
* This applies to the current active ReferenceFrame.
*
* \param data Set whether to follow position and/or velocity (see OpenFrames::TrajectoryFollower::FollowData)
* \param mode Set the follow mode to loop repeatedly or to limit to the times added to the trajectory (see OpenFrames::TrajectoryFollower::FollowMode)
**/
OF_EXPORT void OF_FCN(offrame_followtype)(int *data, int *mode)
{
	if(_objs->_currFrame)
	{
	  // Check if the frame is already following a trajectory
	  TrajectoryFollower *tf = dynamic_cast<TrajectoryFollower*>(_objs->_currFrame->getTransform()->getUpdateCallback());
	    
      if (tf == NULL) {
        _objs->_intVal = 1;
      }
      else {
        tf->setFollowType((unsigned int)(*data),
            (TrajectoryFollower::FollowMode)(*mode));
        _objs->_intVal = 0;
      }
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the elements to follow position
*
* Each of src, element, opt, and scale must be 3-element arrays, with one element for each x/y/z source.
*
* The function offrame_followtrajectory() must be called before this function.
*
* This applies to the current active ReferenceFrame.
*
* \param src     Set data source for each axis (see OpenFrames::Trajectory::SourceType)
* \param element Set which element to follow
* \param opt     Set which optional to follow
* \param scale   Set the scale for each axis
**/
OF_EXPORT void OF_FCN(offrame_followposition)(int src[], unsigned int element[],
                                           unsigned int opt[], double scale[])
{
	if(_objs->_currFrame)
	{
	  // Check if the frame is already following a trajectory
	  TrajectoryFollower *tf = dynamic_cast<TrajectoryFollower*>(_objs->_currFrame->getTransform()->getUpdateCallback());
	    
	  if(tf == NULL) _objs->_intVal = 1;
	  else 
	  {
	    // Set data for the parameters of the TrajectoryFollower
	    Trajectory::DataSource data[3];
	    for(unsigned int i = 0; i < 3; ++i)
	    {
	      data[i]._src = (Trajectory::SourceType)(src[i]);
	      data[i]._element = element[i];
	      data[i]._opt = opt[i];
	      data[i]._scale = scale[i];
	    }

	    tf->setXData(data[0]);
	    tf->setYData(data[1]);
        tf->setZData(data[2]);
        _objs->_intVal = 0;
	  }
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Change how this frame follows a trajectory
*
* This applies to the current active ReferenceFrame.
*
* \param affectChildren   True if the children of this frame are also affected by this function, if false, only this frame is affected
* \param reset            Set to true to reset the trajectory to its initial time
* \param changePauseState True if this function call is to set the pauseState, false to ignore the provided value of pauseState
* \param pauseState       Indicate if the playback is to be paused
* \param changeOffsetTime True if this function call is to set the offsetTime, false to ignore the provided value of offsetTime
* \param offsetTime       Shift the playback time by this offset value
* \param changeTimeScale  True if this function call is to set the timeScale, false to ignore the provided value of timeScale
* \param timeScale        Set the timescale for playback
**/
OF_EXPORT void OF_FCN(offrame_managetime)(bool *affectChildren, bool *reset,
				       bool *changePauseState, bool *pauseState,
                                       bool *changeOffsetTime, double *offsetTime,
                                       bool *changeTimeScale, double *timeScale)
{
	if(_objs->_currFrame)
	{
	  // Set traversal mode
	  if(*affectChildren) _objs->_tmv->setTraversalMode(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN);
	  else _objs->_tmv->setTraversalMode(osg::NodeVisitor::TRAVERSE_NONE);

	  // Set pause state, offset time, and time scale
	  _objs->_tmv->setReset(*reset);
	  _objs->_tmv->setPauseState(*changePauseState, *pauseState);
	  _objs->_tmv->setOffsetTime(*changeOffsetTime, *offsetTime);
	  _objs->_tmv->setTimeScale(*changeTimeScale, *timeScale);

	  // Send the visitor to the frame to do it's thing
	  _objs->_currFrame->getTransform()->accept(*(_objs->_tmv.get()));

	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Print (to std::out) a formatted string of the current ReferenceFrame's descendant heirarchy
*
* This applies to the current active ReferenceFrame.
**/
void OF_FCN(offrame_printframestring)()
{
	if(_objs->_currFrame)
	{
	  std::string str;
	  _objs->_currFrame->createFrameString(str);
      std::cout << str;
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/*******************************************
	Sphere Functions
*******************************************/

/**
* \brief Create a new Sphere with the given name
*
* This new Sphere will also become the current active one.
*
* \param name Name of the sphere to create
**/
void OF_FCN(ofsphere_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new Sphere(temp);
    _objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Set the radius of the sphere
*
* This applies to the current active Sphere.
*
* \param radius Radius of the sphere
**/
void OF_FCN(ofsphere_setradius)(double *radius)
{
	  // Make sure that the currently active ReferenceFrame is a Sphere
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
    if (sphere == NULL) {
      _objs->_intVal = 1;
      return;
    }

    sphere->setRadius(*radius);
    _objs->_intVal = 0;
}

/**
* \brief Set the image file used as the texture map for the sphere
*
* See the OpenSceneGraph documentation for supported file types
*
* This applies to the current active Sphere.
*
* \param fname File containing the texture map
**/
void OF_FCN(ofsphere_settexturemap)(OF_CHARARG(fname))
{
	// Make sure that the currently active ReferenceFrame is a Sphere
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
    if (sphere == NULL) {
      _objs->_intVal = 1;
      return;
    }

	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(fname));
    sphere->setTextureMap(temp);
    _objs->_intVal = 0;
}

/**
* \brief Enable/disable auto level of detailing for the sphere
*
* This applies to the current active Sphere.
*
* \param lod True to enable auto level of detailing, false to disable
**/
void OF_FCN(ofsphere_setautolod)(bool *lod)
{
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
	if(sphere)
	{
	  sphere->setAutoLOD(*lod);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/*******************************************
	Model Functions
*******************************************/

/**
* \brief Create a new Model with the given name
*
* This new Model will also become the current active one.
*
* \param name Name of the model to create
**/
void OF_FCN(ofmodel_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new Model(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Set the 3D model to be displayed
*
* See the OpenSceneGraph documentation for supported model types
*
* This applies to the current active Model.
*
* \param fname File containing the 3D model
**/
void OF_FCN(ofmodel_setmodel)(OF_CHARARG(fname))
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(fname));
      _objs->_intVal = model->setModel(temp);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the position wrt the local origin of the current model
*
* This applies to the current active Model.
*
* \param x X position
* \param y Y position
* \param z Z position
**/
void OF_FCN(ofmodel_setmodelposition)(double *x, double *y, double *z)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelPosition(*x, *y, *z);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the position wrt the local origin of the current model
*
* This applies to the current active Model.
*
* \param x Returned X position
* \param y Returned Y position
* \param z Returned Z position
**/
void OF_FCN(ofmodel_getmodelposition)(double *x, double *y, double *z)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelPosition(*x, *y, *z);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the scale wrt the local origin of the current model
*
* This applies to the current active Model.
*
* \param sx Scale along X axis
* \param sy Scale along Y axis
* \param sz Scale along Z axis
**/
void OF_FCN(ofmodel_setmodelscale)(double *sx, double *sy, double *sz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelScale(*sx, *sy, *sz);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the scale wrt the local origin of the current model
*
* This applies to the current active Model.
*
* \param sx Returned scale along X axis
* \param sy Returned scale along Y axis
* \param sz Returned scale along Z axis
**/
void OF_FCN(ofmodel_getmodelscale)(double *sx, double *sy, double *sz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelScale(*sx, *sy, *sz);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the model pivot point wrt the local origin of the current model
*
* This is the point about which all rotations and scales take place.
*
* This applies to the current active Model.
*
* \param px X position of pivot point
* \param py Y position of pivot point
* \param pz Z position of pivot point
**/
void OF_FCN(ofmodel_setmodelpivot)(double *px, double *py, double *pz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelPivot(*px, *py, *pz);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the position wrt the local origin of the current model
*
* This is the point about which all rotations and scales take place.
*
* This applies to the current active Model.
*
* \param px Returned X position of pivot point
* \param py Returned Y position of pivot point
* \param pz Returned Z position of pivot point
**/
void OF_FCN(ofmodel_getmodelpivot)(double *px, double *py, double *pz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelPivot(*px, *py, *pz);
      _objs->_intVal = 0;
	}
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Get the size of the model
*
* This is the radius of the model's bounding sphere
*
* This applies to the current active Model.
*
* \param size Returned size of the model
**/
void OF_FCN(ofmodel_getmodelsize)(double *size)
{
  	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  // Get the node associated with the model
	  const osg::Node *node = model->getModel();

	  // Get the bounding sphere of the model
      if (node) {
        *size = node->getBound()._radius;
        _objs->_intVal = 0;
      }
      else {
        *size = -1.0; // Model not specified
        _objs->_intVal = 1;
      }
    }
    else {
        _objs->_intVal = -2;
    }
}

/***********************************************
	DrawableTrajectory Functions
***********************************************/

/**
* \brief Create a new DrawableTrajectory with the given name
*
* This new DrawableTrajectory will also become the current active one.
*
* \param name Name of the drawable trajectory to create
**/
void OF_FCN(ofdrawtraj_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new DrawableTrajectory(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Allow specified TrajectoryArtist to draw using this DrawableTrajectory
*
* Note that the currently active TrajectoryArtist will NOT be changed.
*
* This applies to the current active DrawableTrajectory.
*
* \param name Name of the trajectory artist to draw this DrawableTrajectory
**/
void OF_FCN(ofdrawtraj_addartist)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	// Make sure that the current ReferenceFrame is a DrawableTrajectory
	DrawableTrajectory *drawtraj = dynamic_cast<DrawableTrajectory*>(_objs->_currFrame);

	// Make sure that requested TrajectoryArtist exists
	ArtistMap::iterator i = _objs->_artistMap.find(temp);

	if(drawtraj && (i != _objs->_artistMap.end())) 
	{
	  drawtraj->addArtist(i->second.get());
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Remove specified artist from the current DrawableTrajectory
*
* Note that the currently active TrajectoryArtist will NOT be changed.
*
* This applies to the current active DrawableTrajectory.
*
* \param name Name of the trajectory artist to be removed from this DrawableTrajectory
**/
OF_EXPORT void OF_FCN(ofdrawtraj_removeartist)(OF_CHARARG(name))
{
  	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	// Make sure that the current ReferenceFrame is a DrawableTrajectory
	DrawableTrajectory *drawtraj = dynamic_cast<DrawableTrajectory*>(_objs->_currFrame);

	// Make sure that requested TrajectoryArtist exists
	ArtistMap::iterator i = _objs->_artistMap.find(temp);

	if(drawtraj && (i != _objs->_artistMap.end())) 
	{
	  drawtraj->removeArtist(i->second.get());
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Remove all artists from the current DrawableTrajectory
*
* This applies to the current active DrawableTrajectory.
**/
OF_EXPORT void OF_FCN(ofdrawtraj_removeallartists)()
{
  	// Make sure that the current ReferenceFrame is a DrawableTrajectory
	DrawableTrajectory *drawtraj = dynamic_cast<DrawableTrajectory*>(_objs->_currFrame);
	if(drawtraj) 
	{
	  drawtraj->removeAllArtists();
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/***********************************************
	CoordinateAxes Functions
***********************************************/

/**
* \brief Create a new CoordinateAxes with the given name
*
* This new CoordinateAxes will also become the current active one.
*
* \param name Name of the coordinate axes to create
**/
void OF_FCN(ofcoordaxes_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new CoordinateAxes(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Sets the length of the axis
*
* This applies to the current active CoordinateAxes.
*
* \param len Axis length
**/
void OF_FCN(ofcoordaxes_setaxislength)(double *len)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setAxisLength(*len);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Sets which axis to draw
*
* To show multiple axis components, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show.
*
* This applies to the current active CoordinateAxes.
*
* \param axes Axis components to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden. 
**/
void OF_FCN(ofcoordaxes_setdrawaxes)(unsigned int *axes)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setDrawAxes(*axes);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Sets the major and minor tick spacing
*
* This applies to the current active CoordinateAxes.
*
* \param major Major tick spacing.
* \param minor Major tick spacing.
**/
void OF_FCN(ofcoordaxes_settickspacing)(double *major, double *minor)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setTickSpacing(*major, *minor);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Sets the major and minor tick size
*
* This applies to the current active CoordinateAxes.
*
* \param major Major tick size.
* \param minor Major tick size.
**/
void OF_FCN(ofcoordaxes_setticksize)(unsigned int *major, unsigned int *minor)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setTickSize(*major, *minor);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Sets an image to be used for the tick, overriding any existing shader
*
* If an empty string is provided, the tick marker is reset.
*
* This applies to the current active CoordinateAxes.
*
* \param fname File containing the image
**/
OF_EXPORT void OF_FCN(ofcoordaxes_settickimage)(OF_CHARARG(fname))
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(fname));
	  _objs->_intVal = !ca->setTickImage(temp);
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set GLSL fragment shader used to draw tick mark, overriding any existing image
*
* If an empty string is provided, the tick marker is reset.
*
* This applies to the current active CoordinateAxes.
*
* \param fname File containing the shader source
**/
OF_EXPORT void OF_FCN(ofcoordaxes_settickshader)(OF_CHARARG(fname))
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(fname));
	  _objs->_intVal = !ca->setTickShader(temp);
	}
	else _objs->_intVal = -2;
}

/***********************************************
	LatLonGrid Functions
***********************************************/

/**
* \brief Create a new LatLonGrid with the given name
*
* This new LatLonGrid will also become the current active one.
*
* \param name Name of the grid to create
**/
void OF_FCN(oflatlongrid_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new LatLonGrid(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Sets the parameters of the LatLonGrid
*
* This applies to the current active LatLonGrid.
*
* \param radius   Radius of the grid.
* \param latSpace Spacing between latitude grid lines in radians.
* \param lonSpace Spacing between longitude grid lines in radians.
**/
void OF_FCN(oflatlongrid_setparameters)(double *radius, double *latSpace, double *lonSpace)
{
	LatLonGrid *llg = dynamic_cast<LatLonGrid*>(_objs->_currFrame);
	if(llg) 
	{
	  llg->setParameters(*radius, *latSpace, *lonSpace);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/***********************************************
	RadialPlane Functions
***********************************************/

/**
* \brief Create a new RadialPlane with the given name
*
* This new RadialPlane will also become the current active one.
*
* \param name Name of the radial plane to create
**/
void OF_FCN(ofradialplane_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new RadialPlane(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

/**
* \brief Sets the parameters of the RadialPlane
*
* This applies to the current active RadialPlane.
*
* \param radius   Radius of the radial plane.
* \param radSpace Spacing between radial grid lines in radians.
* \param lonSpace Spacing between longitude grid lines in radians.
**/
void OF_FCN(ofradialplane_setparameters)(double *radius, double *radSpace, double *lonSpace)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setParameters(*radius, *radSpace, *lonSpace);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set the plane color of the current radial plane
*
* This applies to the current active RadialPlane.
*
* \param r Red color component [0-1]
* \param g Green color component [0-1]
* \param b Blue color component [0-1]
* \param a Alpha (transparancy) component [0-1]
**/
void OF_FCN(ofradialplane_setplanecolor)(float *r, float *g, float *b, float *a)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setPlaneColor(*r, *g, *b, *a);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set the line color of the current radial plane
*
* This applies to the current active RadialPlane.
*
* \param r Red color component [0-1]
* \param g Green color component [0-1]
* \param b Blue color component [0-1]
* \param a Alpha (transparancy) component [0-1]
**/
void OF_FCN(ofradialplane_setlinecolor)(float *r, float *g, float *b, float *a)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setLineColor(*r, *g, *b, *a);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/***********************************************
	Trajectory Functions
***********************************************/

/**
* \brief Set the currently active trajectory
*
* \param name Name of the Trajectory to activate
**/
void OF_FCN(oftraj_activate)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	TrajectoryMap::iterator i = _objs->_trajMap.find(temp);
	if(i == _objs->_trajMap.end())
	{
	  _objs->_currTraj = NULL;
	  _objs->_intVal = 1;
	}
	else 
	{
	  _objs->_currTraj = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Create a new Trajectory with the given name
*
* This new Trajectory will also become the current active one.
*
* This applies to the current active Trajectory.
*
* \param name   Name of the trajectory to create
* \param dof    Number of degrees of freedom this trajectory has
* \param numopt Number of optionals this trajectory has
**/
#if defined(IFORT_CALLS)
void OF_FCN(oftraj_create)(const char *name, unsigned int *dof,
                        unsigned int *numopt, unsigned int namelen)

#else
void OF_FCN(oftraj_create)(OF_CHARARG(name), unsigned int *dof,
                        unsigned int *numopt)

#endif
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));


	_objs->_currTraj = new Trajectory(*dof, *numopt);
	_objs->_trajMap[temp] = _objs->_currTraj;
    _objs->_intVal = 0;
}

/**
* \brief Change the number of optionals for the currently active Trajectory
*
* Each optional has an x/y/z component added.
*
* This applies to the current active Trajectory.
*
* \param nopt Number of optional coordinates to set
**/
void OF_FCN(oftraj_setnumoptionals)(unsigned int *nopt)
{
    if (_objs->_currTraj) {
      _objs->_currTraj->setNumOptionals(*nopt);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Change the degrees of freedom for the currently active Trajectory
*
* This applies to the current active Trajectory.
*
* \param dof Desired number of degrees of freedom
**/
void OF_FCN(oftraj_setdof)(unsigned int *dof)
{
    if (_objs->_currTraj) {
	  _objs->_currTraj->setDOF(*dof);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Add a time to the current trajectory
*
* This applies to the current active Trajectory.
*
* Future positions/attitudes/optionals added to this trajectory will correspond to this time until a new call to oftraj_addtime().
*
* \param t Time
**/
void OF_FCN(oftraj_addtime)(const double *t)
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addTime(*t);
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Add a position to the current trajectory
*
* This applies to the current active Trajectory.
*
* This position corresponds to the most recent time provided by oftraj_addtime().
*
* /warning If multiple positions are added after the previous call to oftraj_addtime(), all but the last position will be overwritten.
*
* \param x X position
* \param y Y position
* \param z Z position
**/
void OF_FCN(oftraj_addposition)(const double *x, const double *y, const double *z)
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addPosition(*x, *y, *z);
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Add a position to the current trajectory
*
* This applies to the current active Trajectory.
*
* This position corresponds to the most recent time provided by oftraj_addtime().
*
* \param pos Position array to add (length 3)
**/
void OF_FCN(oftraj_addpositionvec)(const double pos[])
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addPosition(pos);
    }
    else {
      _objs->_intVal = -2;
    }
}


/**
* \brief Set the attitude of the current trajectory
*
* This applies to the current active Trajectory.
*
* \param x X component of the rotation quaternion
* \param y Y component of the rotation quaternion
* \param z Z component of the rotation quaternion
* \param w Angle component of the rotation quaternion
**/
void OF_FCN(oftraj_addattitude)(const double *x, const double *y,
                             const double *z, const double *w)
{
	if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addAttitude(*x, *y, *z, *w);
    }
    else {
      _objs->_intVal = -2;
    }
}


/**
* \brief Set the attitude of the current trajectory
*
* This applies to the current active Trajectory.
*
* \param att Quaternion array to add (length 4). The vector component of the quaternion precedes the scalar component.
**/
void OF_FCN(oftraj_addattitudevec)(const double att[])
{
	if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addAttitude(att);
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the optional with the given index, for the most recently added position
*
* This applies to the current active Trajectory.
*
* This optional corresponds to the most recent time provided by oftraj_addtime().
*
* \param index index of optional to add values to
* \param x     X component of optional
* \param y     Y component of optional
* \param z     Z component of optional
**/
void OF_FCN(oftraj_setoptional)(unsigned int *index, const double *x,
                             const double *y, const double *z)
{
	if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->setOptional(*index, *x, *y, *z);
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the optional with the given index, for the most recently added position
*
* This applies to the current active Trajectory.
*
* This optional corresponds to the most recent time provided by oftraj_addtime().
*
* \param index index of optional to add values to
* \param opt   Array of values to add to optional (length 3)
**/
void OF_FCN(oftraj_setoptionalvec)(unsigned int *index, const double opt[])
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->setOptional(*index, opt);
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Clear all points from the currently active Trajectory
*
* This applies to the current active Trajectory.
**/
void OF_FCN(oftraj_clear)()
{
    if (_objs->_currTraj) {
	  _objs->_currTraj->clear();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Inform drawable trajecotries to redraw this trajectory
*
* This is only necessary to call if oftraj_autoinformartists() has been set to false. By default, the artists are informed every time data is added
*
* This applies to the current active Trajectory, and all the TrajectoryArtists that are linked to this trajectory
**/
void OF_FCN(oftraj_informartists)()
{
	if (_objs->_currTraj) {
	  _objs->_currTraj->informArtists();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Inform drawable trajecotries to redraw this trajectory
*
* If this function is not called, the default is to automatically inform the artists
*
* If auto-inform is disabled, the artists will still be automatically informed if the data is cleared, or the number of optional coordinates is modified.
*
* This applies to the current active Trajectory, and all the TrajectoryArtists that are linked to this trajectory
*
* \param autoinform True to auto-inform linked artists when data is added to this trajectory.
*                   False to not inform  artists unless oftraj_autoinformartists() is called. 
**/
void OF_FCN(oftraj_autoinformartists)(bool *autoinform)
{
	if (_objs->_currTraj) {
	  _objs->_currTraj->autoInformArtists(*autoinform);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/************************************************
	TrajectoryArtist Functions
************************************************/

/**
* \brief Set the currently active trajectory artist
*
* \param name Name of the TrajectoryArtist to activate
**/
void OF_FCN(oftrajartist_activate)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	ArtistMap::iterator i = _objs->_artistMap.find(temp);
	if(i == _objs->_artistMap.end())
	{
	  _objs->_currArtist = NULL;
	  _objs->_intVal = 1;
	}
	else 
	{
	  _objs->_currArtist = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Tell the active artist to use the active Trajectory
*
* This applies to the current active TrajectoryArtist.
*
* The trajectory is specified by the currently active Trajectory. 
**/
void OF_FCN(oftrajartist_settrajectory)()
{
    if (_objs->_currArtist) {
      if (_objs->_currTraj) {
        _objs->_currArtist->setTrajectory(_objs->_currTraj);
        _objs->_intVal = 0;
      }
      else {
        _objs->_intVal = 1;
      }
    }
    else {
      _objs->_intVal = -2;
    }
}


/************************************************
	CurveArtist Functions
************************************************/

/**
* \brief Create a new CurveArtist with the given name
*
* This new CurveArtist will also become the current active one.
*
* \param name Name of the curve artist to create
**/
void OF_FCN(ofcurveartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new CurveArtist;
	_objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

/**
* \brief Set the data used for X coordinates of each point
*
* This applies to the current active CurveArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position, 
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofcurveartist_setxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setXData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for Y coordinates of each point
*
* This applies to the current active CurveArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofcurveartist_setydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setYData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for Z coordinates of each point
*
* This applies to the current active CurveArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofcurveartist_setzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setZData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the color of the current curve artist
*
* This applies to the current active CurveArtist.
*
* \param r Red color component [0-1]
* \param g Green color component [0-1]
* \param b Blue color component [0-1]
**/
void OF_FCN(ofcurveartist_setcolor)(float *r, float *g, float *b)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
    if (artist) {
      artist->setColor(*r, *g, *b);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the width of the current curve artist
*
* This applies to the current active CurveArtist.
*
* \param width Width of the line 
**/
void OF_FCN(ofcurveartist_setwidth)(float *width)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
    if (artist) {
	  artist->setWidth(*width);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the line pattern of the current curve artist
*
* For valid factors and patterns, see OpenGL::glLineStipple()
*
* This applies to the current active CurveArtist.
*
* \param factor  Specifies scaling factor used to draw the pattern
* \param pattern 16-bit integer which specifies the line pattern
**/
void OF_FCN(ofcurveartist_setpattern)(int *factor, unsigned short *pattern)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
    if (artist) {
	  artist->setPattern(*factor, *pattern);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/************************************************
	SegmentArtist Functions
************************************************/

/**
* \brief Create a new SegmentArtist with the given name
*
* This new SegmentArtist will also become the current active one.
*
* \param name Name of the segment artist to create
**/
void OF_FCN(ofsegmentartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new SegmentArtist;
    _objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

/**
* \brief Set the data used for starting X coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setstartxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setStartXData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for starting Y coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setstartydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setStartYData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for starting Z coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setstartzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setStartZData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for ending X coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setendxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setEndXData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for ending Y coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setendydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setEndYData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for ending Z coordinate of each segment
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofsegmentartist_setendzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setEndZData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the offset between drawn points
*
* This applies to the current active SegmentArtist.
*
* \param stride Minimum offset between sucessive drawn points
**/
void OF_FCN(ofsegmentartist_setstride)(unsigned int *stride)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist)
	{
	  _objs->_intVal = 0;
	  artist->setStride(*stride);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the color of the current segment artist
*
* This applies to the current active SegmentArtist.
*
* \param r Red color component [0-1]
* \param g Green color component [0-1]
* \param b Blue color component [0-1]
**/
void OF_FCN(ofsegmentartist_setcolor)(float *r, float *g, float *b)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
    if (artist) {
	  artist->setColor(*r, *g, *b);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the width of the current segment artist
*
* This applies to the current active SegmentArtist.
*
* \param width Width of the line
**/
void OF_FCN(ofsegmentartist_setwidth)(float *width)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
    if (artist) {
	  artist->setWidth(*width);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the line pattern of the current segment artist
*
* For valid factors and patterns, see OpenGL::glLineStipple()
*
* This applies to the current active SegmentArtist.
*
* \param factor  Specifies scaling factor used to draw the pattern
* \param pattern 16-bit integer which specifies the line pattern
**/
void OF_FCN(ofsegmentartist_setpattern)(int *factor, unsigned short *pattern)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
    if (artist) {
	  artist->setPattern(*factor, *pattern);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/*****************************************************************
	MarkerArtist Functions
*****************************************************************/

/**
* \brief Create a new MarkerArtist with the given name
*
* This new MarkerArtist will also become the current active one.
*
* \param name Name of the marker artist to create
**/
OF_EXPORT void OF_FCN(ofmarkerartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new MarkerArtist;
	_objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

/**
* \brief Set the data used for X coordinates of each point
*
* This applies to the current active MarkerArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofmarkerartist_setxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setXData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for Y coordinates of each point
*
* This applies to the current active MarkerArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofmarkerartist_setydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setYData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Set the data used for Z coordinates of each point
*
* This applies to the current active MarkerArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum)
* \param element Array index of the data indicated in src to plot
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data
**/
void OF_FCN(ofmarkerartist_setzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 2;
	  return;
	}

	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist) 
	{
	  Trajectory::DataSource data;
	  data._src = (Trajectory::SourceType)(*src);
	  data._element = *element;
	  data._opt = *opt;
	  data._scale = *scale;
	  _objs->_intVal = !artist->setZData(data);
	}
	else
	  _objs->_intVal = -2;
}

/**
* \brief Define which markers should be plotted for the current marker artist
*
* This applies to the current active MarkerArtist.
*
* \param markers Indicates which data points should be drawn as markers (see: OpenFrames::MarkerArtist::DrawnMarkers enum)
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkers)( unsigned int *markers )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkers(*markers);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set the color of the current marker artist
*
* This applies to the current active MarkerArtist.
*
* \param markers The markers whose color should be set (see: OpenFrames::MarkerArtist::DrawnMarkers enum)
* \param r       Red color component [0-1]
* \param g       Green color component [0-1]
* \param b       Blue color component [0-1]
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkercolor)( unsigned int *markers, float *r, float *g, float *b )
{
  	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkerColor(*markers, *r, *g, *b);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set image used as marker, overriding any existing shader
*
* If an empty string is given, then use default circular point
*
* This applies to the current active MarkerArtist.
*
* \param fname File containing the image
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkerimage)(OF_CHARARG(fname))
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(fname));
	  _objs->_intVal = !artist->setMarkerImage(temp);
	}
	else _objs->_intVal = -2;
}

/**
* \brief Set GLSL fragment shader used to draw marker, overriding any existing image
*
* If an empty string is given, then use default circular point
*
* This applies to the current active MarkerArtist.
*
* \param fname File containing the shader source
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkershader)(OF_CHARARG(fname))
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(fname));
	  _objs->_intVal = !artist->setMarkerShader(temp);
	}
	else _objs->_intVal = -2;
}

/**
* \brief Specify which type of intermediate markers should be drawn
*
* This applies to the current active MarkerArtist.
*
* \param type Indicates how intermediate marker spacing is determined (see: OpenFrames::MarkerArtist::IntermediateType enum)
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatetype)( unsigned int *type )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateType((MarkerArtist::IntermediateType)(*type));
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Specify the spacing used for intermediate markers
*
* This applies to the current active MarkerArtist.
*
* \param spacing Set spacing for intermediate markers
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatespacing)( double *spacing )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateSpacing(*spacing);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Specify the drawing direction (from start or end) of intermediate markers
*
* This applies to the current active MarkerArtist.
*
* \param direction Set intermediate marker direction (see: OpenFrames::MarkerArtist::DrawnMarkers enum)
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatedirection)( unsigned int *direction )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateDirection((MarkerArtist::DrawnMarkers)(*direction));
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Specify the marker size in pixels
*
* This applies to the current active MarkerArtist.
*
* \param size The marker size
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkersize)( unsigned int *size )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkerSize(*size);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/**
* \brief Specify whether marker size should be automatically attenuated
*
* This applies to the current active MarkerArtist.
*
* \param autoattenuate True to automatically attenuate marker size, False otherwise
**/
OF_EXPORT void OF_FCN(ofmarkerartist_setautoattenuate)( bool *autoattenuate )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setAutoAttenuate(*autoattenuate);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/************************************************
	View Functions
************************************************/

/**
* \brief Set the currently active view
*
* \param name Name of the View to activate
**/
void OF_FCN(ofview_activate)(OF_CHARARG(name))
{
  	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	ViewMap::iterator i = _objs->_viewMap.find(temp);
	if(i == _objs->_viewMap.end())
	{
	  _objs->_currView = NULL;
	  _objs->_intVal = 1;
	}
	else 
	{
	  _objs->_currView = i->second.get();
	  _objs->_intVal = 0;
	}
}

/**
* \brief Create a new View with the given name
*
* This new View will also become the current active one.
*
* \param name Name of the view to create
**/
void OF_FCN(ofview_create)(OF_CHARARG(name))
{
  	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currView = new View;
    _objs->_viewMap[temp] = _objs->_currView;
    _objs->_intVal = 0;
}

/**
* \brief Set an orthographic projection with the given bounds
*
* This applies to the current active View.
*
* \param left   Left coordinate bound of orthographic projection
* \param right  Right coordinate bound of orthographic projection
* \param bottom Bottom coordinate bound of orthographic projection
* \param top    Top coordinate bound of orthographic projection
**/
void OF_FCN(ofview_setorthographic)(double *left, double *right,
                                 double *bottom, double *top)
{
	if (_objs->_currView) {
	  _objs->_currView->setOrthographic(*left, *right, *bottom, *top);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Set the current view to use a symmetric perspective projection
* 
* This applies to the current active View.
*
* \param fov  Vertical field of view (in degrees)
* \param ratio x/y aspect ratio.
**/
void OF_FCN(ofview_setperspective)(double *fov, double *ratio)
{
	if (_objs->_currView) {
	  _objs->_currView->setPerspective(*fov, *ratio);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Tell current view to follow the specified ReferenceFrame
*
* The 'root' input should be set to the root of the ReferenceFrame heirarchy, and the
* 'frame' input should be set to whatever frame you want to view. Note that
* this function does NOT use or modify the currently active ReferenceFrame.
*
* This applies to the current active View.
*
* \param root  Name of the root of the ReferenceFrame heirarchy
* \param frame ReferenceFrame to follow with this view
**/
#if defined(IFORT_CALLS)
void OF_FCN(ofview_setviewframe)(const char *root, const char *frame, 
                                 unsigned int rootlen, 
                                 unsigned int framelen)

#else
void OF_FCN(ofview_setviewframe)(OF_CHARARG(root), OF_CHARARG(frame))

#endif
{
        // Convert inputs to usable strings
        std::string rname(OF_STRING(root));
	std::string fname(OF_STRING(frame));

	if(_objs->_currView)
	{
	  // Find root and viewed frames in the FrameMap
	  FrameMap::iterator i = _objs->_frameMap.find(rname);
	  FrameMap::iterator j = _objs->_frameMap.find(fname);
	  ReferenceFrame *rootFrame, *viewFrame;

	  // Set dummy reference to root frame
	  if(i == _objs->_frameMap.end()) 
          {
            _objs->_intVal = -2;
            return;
          }
	  else rootFrame = i->second.get();

	  // Set dummy reference to view frame
	  if(j == _objs->_frameMap.end()) 
          {
            _objs->_intVal = -3;
            return;
          }
	  else viewFrame = j->second.get();

	  // Tell current view to find and remember the path from the
	  // root -> view frame. If the path doesn't exist, then
	  // the ofview_isvalid() function will raise a flag.
	  _objs->_currView->setViewFrame(rootFrame, viewFrame);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -1;
    }
}

/**
* \brief View from one frame towards another, using a specified frame type and rotation type
*
* The 'root' input should be set to the root of the ReferenceFrame 
* heirarchy, and must contain 'srcframe' and 'dstframe'. Note that 
* this function does NOT use or modify the currently active ReferenceFrame.
*
* This applies to the current active View.
*
* /see OpenFrames::View::ViewFrameType and OpenFrames::View::ViewRotationType
*
* \param root         Name of the root of the ReferenceFrame heirarchy
* \param srcframe     ReferenceFrame this view will look from
* \param dstframe     ReferenceFrame this view will look towards
* \param frameType    Frame type to use (see: OpenFrames::View::ViewFrameType enum)
* \param rotationType Rotation type to use when following dstframe (see: OpenFrames::View::ViewRotationType enum)
**/
#if defined(IFORT_CALLS)
void OF_FCN(ofview_setviewbetweenframes)(const char *root, 
                                 const char *srcframe, 
                                 const char *dstframe,
                                 unsigned int *frameType,
                                 unsigned int *rotationType,
                                 unsigned int rootlen, 
                                 unsigned int srcframelen,
                                 unsigned int dstframelen)

#else
void OF_FCN(ofview_setviewbetweenframes)(OF_CHARARG(root), 
                                         OF_CHARARG(srcframe),
                                         OF_CHARARG(dstframe),
                                         unsigned int *frameType,
                                         unsigned int *rotationType)

#endif
{
        // Convert inputs to usable strings
        std::string rname(OF_STRING(root));
	std::string sfname(OF_STRING(srcframe));
	std::string dfname(OF_STRING(dstframe));

	if(_objs->_currView)
	{
	  // Find root and viewed frames in the FrameMap
	  FrameMap::iterator i = _objs->_frameMap.find(rname);
	  FrameMap::iterator j = _objs->_frameMap.find(sfname);
	  FrameMap::iterator k = _objs->_frameMap.find(dfname);
	  ReferenceFrame *rootFrame, *viewFrame, *lookatFrame;

	  // Set dummy reference to root frame
	  if(i == _objs->_frameMap.end())
          {
            _objs->_intVal = -2;
            return;
          }
	  else rootFrame = i->second.get();

	  // Set dummy reference to view frame
	  if(j == _objs->_frameMap.end())
          {
            _objs->_intVal = -3;
            return;
          }
	  else viewFrame = j->second.get();

	  // Set dummy reference to lookat frame
	  if(k == _objs->_frameMap.end())
          {
            _objs->_intVal = -4;
            return;
          }
	  else lookatFrame = k->second.get();

	  // Tell current view to find and remember the path from the
	  // root -> view frame. If the path doesn't exist, then
	  // the ofview_isvalid() function will raise a flag.
	  _objs->_currView->setViewBetweenFrames(rootFrame, viewFrame, lookatFrame, (View::ViewFrameType)(*frameType), (View::ViewRotationType)(*rotationType));
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -1;
    }
}

/**
* \brief Set the default view distance. 
*
* This applies to the current active View.
*
* \param distance Distance the camera is from the terget point of the reference frame.
*                 A value <= 0.0 means the distance should be auto-computed
**/
void OF_FCN(ofview_setdefaultviewdistance)(double *distance)
{
	if (_objs->_currView) {
	  _objs->_currView->setDefaultViewDistance(*distance);
	  _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/**
* \brief Check if the view frame for the current View is valid
*
* One reason for an invalid view is if the frame to be viewed is not a child of the specified root frame.
*
* This applies to the current active View.
*
* \param valid Returned value if the view is valid
**/
void OF_FCN(ofview_isvalid)(bool *valid)
{
	if(_objs->_currView) 
	{
	  *valid = _objs->_currView->isValid();
	  _objs->_intVal = 0;
	}
	else 
	{
	  *valid = false;
	  _objs->_intVal = -2; // error flag
	}
}

/**
* \brief Reset the view to its default state
*
* This applies to the current active View.
**/
void OF_FCN(ofview_reset)()
{
	if (_objs->_currView) {
      _objs->_currView->resetTrackball();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}
