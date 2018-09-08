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

/** \file OF_Interface.cpp
 * Definitions for the OF_Interface class.
 */

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
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <OpenThreads/Thread>
#include <osg/Notify>
#include <osgDB/Registry>
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

/**
 * \class OF_Objects
 *
 * \brief This class tracks OpenFrames objects created with this interface.
 *
 * This class keeps track of all OpenFrames objects created using this
 * interface. It is implemented as a singleton becuase it handles ALL
 * OpenFrames objects and must not have multiple instances.
 */
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

void OF_FCN(of_getreturnedvalue)(int *val)
{
	*val = _objs->_intVal;
}

OF_EXPORT void OF_FCN(of_adddatafilepath)(OF_CHARARG(newpath))
{
  std::string temp(OF_STRING(newpath)); // Convert to std::string
  osgDB::Registry::instance()->getDataFilePathList().push_front(temp);
}
    
/***********************************************
	Window Functions
***********************************************/

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

void OF_FCN(ofwin_getid)(unsigned int *retid)
{
    if(_objs->_currWinProxy)
    {
      *retid = _objs->_currWinProxy->getID();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

void OF_FCN(ofwin_createproxy)(int *x, int *y,
                            unsigned int *width, unsigned int *height,
                            unsigned int *nrow, unsigned int *ncol,
                            bool *embedded, unsigned int *id,
                            bool *useVR)
{
	// Create the new WindowProxy with the given ID
	WindowProxy* wp = new WindowProxy(*x, *y, *width, *height, *nrow, *ncol, *embedded, *useVR);
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

void OF_FCN(ofwin_setwindowname)(OF_CHARARG(winname))
{
  if(_objs->_currWinProxy)
  {
    std::string temp(OF_STRING(winname));
    _objs->_currWinProxy->setWindowName(temp);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

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
    FramerateLimiter waitLimiter(5.0); // Framerate at which to check for animation
    
    // Tell WindowProxy to start animating, then wait until it actually starts
    _objs->_intVal = _objs->_currWinProxy->startThread();
    if(_objs->_intVal != 0) return;
    
    while(!_objs->_currWinProxy->isAnimating() &&
          !_objs->_currWinProxy->doneAnimating())
    {
      waitLimiter.frame();
    }
    
    // Check for animation error
    if(_objs->_currWinProxy->getAnimationState() == WindowProxy::FAILED)
      _objs->_intVal = -1;
  }
  else {
    _objs->_intVal = -2;
  }
}

/**
* \brief Force animation to stop and wait for the thread to stop
*
* This applies to the current active WindowProxy.
**/
void OF_FCN(ofwin_stop)()
{
  if(_objs->_currWinProxy)
  {
    FramerateLimiter waitLimiter(5.0); // Framerate at which to check for animation
    
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
* \brief Signal animation to stop and return immediately
*
* This applies to the current active WindowProxy.
**/
void OF_FCN(ofwin_signalstop)()
{
	if(_objs->_currWinProxy)
	{
	  // Signal WindowProxy to stop animating
	  _objs->_currWinProxy->shutdown();
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

OF_EXPORT void OF_FCN(ofwin_settime)(double *time)
{
  if(_objs->_currWinProxy)
  {
    _objs->_currWinProxy->setTime(*time);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_gettime)(double *time)
{
  if(_objs->_currWinProxy)
  {
    *time = _objs->_currWinProxy->getTime();
    _objs->_intVal = 0;
  }
  else {
    *time = DBL_MAX;
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_pausetime)(bool *pause)
{
  if(_objs->_currWinProxy)
  {
    _objs->_currWinProxy->pauseTime(*pause);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_istimepaused)(bool *pause)
{
  if(_objs->_currWinProxy)
  {
    *pause = _objs->_currWinProxy->isTimePaused();
    _objs->_intVal = 0;
  }
  else {
    *pause = false;
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_settimescale)(double *tscale)
{
  if(_objs->_currWinProxy)
  {
    _objs->_currWinProxy->setTimeScale(*tscale);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_gettimescale)(double *tscale)
{
  if(_objs->_currWinProxy)
  {
    *tscale = _objs->_currWinProxy->getTimeScale();
    _objs->_intVal = 0;
  }
  else {
    *tscale = DBL_MAX;
    _objs->_intVal = -2;
  }
}

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

void OF_FCN(ofwin_setlightambient)(unsigned int *row, unsigned int *col,
                                   float *r, float *g, float *b)
{
  if(_objs->_currWinProxy)
  {
    RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
    if (rr) {
      osg::Light* globalLight = rr->getSceneView()->getLight();
      globalLight->setAmbient(osg::Vec4(*r, *g, *b, 1.0));
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

void OF_FCN(ofwin_setlightdiffuse)(unsigned int *row, unsigned int *col,
                                   float *r, float *g, float *b)
{
  if(_objs->_currWinProxy)
  {
    RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
    if (rr) {
      osg::Light* globalLight = rr->getSceneView()->getLight();
      globalLight->setDiffuse(osg::Vec4(*r, *g, *b, 1.0));
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
    
void OF_FCN(ofwin_setlightspecular)(unsigned int *row, unsigned int *col,
                                    float *r, float *g, float *b)
{
  if(_objs->_currWinProxy)
  {
    RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
    if (rr) {
      osg::Light* globalLight = rr->getSceneView()->getLight();
      globalLight->setSpecular(osg::Vec4(*r, *g, *b, 1.0));
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

void OF_FCN(ofwin_setlightposition)(unsigned int *row, unsigned int *col,
                                    float *x, float *y, float *z, float *w)
{
  if(_objs->_currWinProxy)
  {
    RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
    if (rr) {
      osg::Light* globalLight = rr->getSceneView()->getLight();
      globalLight->setPosition(osg::Vec4(*x, *y, *z, *w));
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

/** Internal function to find the osg::Geode for the HUD text */
osg::Geode* getOrCreateHUDTextGeode(unsigned int row, unsigned int col)
{
  if(_objs->_currWinProxy)
  {
    RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(row, col);
    if(rr)
    {
      const std::string hudGeodeName = "HUDTextGeode";
      
      // Search all HUD children
      int numChildren = rr->getHUD()->getNumChildren();
      for(int i = 0; i < numChildren; ++i)
      {
        // If child is a HUD text Geode, then return it
        osg::Geode* geode = rr->getHUD()->getChild(i)->asGeode();
        if(geode && (geode->getName() == hudGeodeName))
        {
          _objs->_intVal = 0;
          return geode;
        }
      }
      
      // HUD text doesn't exist, so create and initialize it with defaults
      osg::ref_ptr<osgText::Text> hudText_BottomLeft = new osgText::Text;
      hudText_BottomLeft->setFont("arial.ttf");
      hudText_BottomLeft->setColor(osg::Vec4(1, 1, 0, 1));
      hudText_BottomLeft->setCharacterSizeMode(osgText::Text::SCREEN_COORDS);
      hudText_BottomLeft->setCharacterSize(20.0);    // In screen coordinates (pixels)
      hudText_BottomLeft->setFontResolution(40, 40); // In texels (texture pixels)
      hudText_BottomLeft->setLineSpacing(0.25);
      
      // Position HUD text
      // Screen coordinates go from (0,0) bottom-left to (1,1) top-right
      hudText_BottomLeft->setAlignment(osgText::Text::LEFT_BOTTOM);
      hudText_BottomLeft->setPosition(osg::Vec3(0.0, 0.0, 0.0));
      
      // Some graphics drivers have a bug where text can't be properly changed.
      // Get around this by initializing text using all likely characters.
      std::string dummyText("the quick brown fox jumps over the lazy dog");
      dummyText += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG";
      dummyText += "1234567890";
      dummyText += "[]{}()<>,.;:+-*/_";
      hudText_BottomLeft->setText(dummyText);
      
      // Attach HUD text
      osg::Geode* geode = new osg::Geode;
      geode->setName(hudGeodeName);
      geode->addDrawable(hudText_BottomLeft);
      rr->getHUD()->addChild(geode);
      
      _objs->_intVal = 0;
      return geode;
    }
    else {
      _objs->_intVal = -1;
      return NULL;
    }
  }
  else {
    _objs->_intVal = -2;
    return NULL;
  }
}

OF_EXPORT void OF_FCN(ofwin_enablehudtext)(unsigned int *row, unsigned int *col, bool *enable)
{
  // Get the HUD text osg::Geode
  osg::Geode* hudTextGeode = getOrCreateHUDTextGeode(*row, *col);
  if(!hudTextGeode) return; // Error creating HUD text
  
  // Set HUD text visibility
  if(*enable) hudTextGeode->setNodeMask(0xffffffff);
  else hudTextGeode->setNodeMask(0x0);
}

OF_EXPORT void OF_FCN(ofwin_sethudtextfont)(unsigned int *row, unsigned int *col, OF_CHARARG(fname))
{
  // Get the HUD text osg::Geode
  osg::Geode* hudTextGeode = getOrCreateHUDTextGeode(*row, *col);
  if(!hudTextGeode) return; // Error creating HUD text

  // Convert given character string and length to a proper C string
  std::string temp(OF_STRING(fname));

  // First child of hud geode will be an osgText::Text by design
  osgText::Text* hudText = static_cast<osgText::Text*>(hudTextGeode->getDrawable(0));
  hudText->setFont(temp);
}

OF_EXPORT void OF_FCN(ofwin_sethudtextparameters)(unsigned int *row, unsigned int *col, float *r, float *g, float *b, float *charSize)
{
  // Get the HUD text osg::Geode
  osg::Geode* hudTextGeode = getOrCreateHUDTextGeode(*row, *col);
  if(!hudTextGeode) return; // Error creating HUD text
  
  // First child of hud geode will be an osgText::Text by design
  osgText::Text* hudText = static_cast<osgText::Text*>(hudTextGeode->getDrawable(0));
  hudText->setColor(osg::Vec4(*r, *g, *b, 1.0));
  hudText->setCharacterSize(*charSize);
}

OF_EXPORT void OF_FCN(ofwin_sethudtextposition)(unsigned int *row, unsigned int *col, float *x, float *y, unsigned int *alignment)
{
  // Get the HUD text osg::Geode
  osg::Geode* hudTextGeode = getOrCreateHUDTextGeode(*row, *col);
  if(!hudTextGeode) return; // Error creating HUD text
  
  // First child of hud geode will be an osgText::Text by design
  osgText::Text* hudText = static_cast<osgText::Text*>(hudTextGeode->getDrawable(0));
  hudText->setPosition(osg::Vec3(*x, *y, 0.0));
  hudText->setAlignment((osgText::Text::AlignmentType)*alignment);
}

OF_EXPORT void OF_FCN(ofwin_sethudtext)(unsigned int *row, unsigned int *col, OF_CHARARG(text))
{
  // Get the HUD text osg::Geode
  osg::Geode* hudTextGeode = getOrCreateHUDTextGeode(*row, *col);
  if(!hudTextGeode) return; // Error creating HUD text
  
  // Convert given character string and length to a proper C string
  std::string temp(OF_STRING(text));
  
  // First child of hud geode will be an osgText::Text by design
  osgText::Text* hudText = static_cast<osgText::Text*>(hudTextGeode->getDrawable(0));
  hudText->setText(temp);
}

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

  void OF_FCN(ofwin_keyrelease)(unsigned int *key)
{
  if (_objs->_currWinProxy) {
    _objs->_currWinProxy->keyRelease(*key);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

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

#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(ofwin_setwindowcapturefile)(const char *fname,
                                                  const char *fext,
                                                  unsigned int fnamelen,
                                                  unsigned int fextlen)
#else
OF_EXPORT void OF_FCN(ofwin_setwindowcapturefile)(OF_CHARARG(fname),
                                                  OF_CHARARG(fext))
#endif
{
  if(_objs->_currWinProxy)
  {
    // Convert given character string and length to a proper C string
    std::string fnamestr(OF_STRING(fname));
    std::string fextstr(OF_STRING(fext));
    _objs->_currWinProxy->setWindowCaptureFile(fnamestr, fextstr);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_capturewindow)()
{
  if(_objs->_currWinProxy)
  {
    _objs->_currWinProxy->captureWindow();
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

OF_EXPORT void OF_FCN(ofwin_setwindowcapturekey)(int *key)
{
  if(_objs->_currWinProxy)
  {
    _objs->_currWinProxy->setWindowCaptureKey(*key);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

/*******************************************
	FrameManager Functions
*******************************************/

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

void OF_FCN(offm_create)(int *id)
{
	_objs->_currFM = new FrameManager;
    _objs->_fmMap[*id] = _objs->_currFM;
    _objs->_intVal = 0;
}

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

void OF_FCN(offm_lock)()
{
	if (_objs->_currFM) {
    _objs->_intVal = _objs->_currFM->lock(FrameManager::HIGH_PRIORITY);
      _objs->_intVal = 0;
    }
	else 
	{
	  _objs->_intVal = -2;
	}
}

void OF_FCN(offm_unlock)()
{
    if (_objs->_currFM) {
      _objs->_intVal = _objs->_currFM->unlock(FrameManager::HIGH_PRIORITY);
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

void OF_FCN(offrame_setlightsourceenabled)(bool *enabled)
{
  if(_objs->_currFrame)
  {
    _objs->_currFrame->setLightSourceEnabled(*enabled);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(offrame_getlightsourceenabled)(bool *enabled)
{
  if(_objs->_currFrame)
  {
    *enabled = _objs->_currFrame->getLightSourceEnabled();
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}
  
void OF_FCN(offrame_setlightambient)(float *r, float *g, float *b)
{
  if(_objs->_currFrame)
  {
    osg::LightSource* lightSource = _objs->_currFrame->getLightSource();
    if(lightSource)
    {
      osg::Light* light = lightSource->getLight();
      light->setAmbient(osg::Vec4(*r, *g, *b, 1.0));
      _objs->_intVal = 0;
    }
    else _objs->_intVal = -1;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(offrame_setlightdiffuse)(float *r, float *g, float *b)
{
  if(_objs->_currFrame)
  {
    osg::LightSource* lightSource = _objs->_currFrame->getLightSource();
    if(lightSource)
    {
      osg::Light* light = lightSource->getLight();
      light->setDiffuse(osg::Vec4(*r, *g, *b, 1.0));
      _objs->_intVal = 0;
    }
    else _objs->_intVal = -1;
  }
  else {
    _objs->_intVal = -2;
  }
}
  
void OF_FCN(offrame_setlightspecular)(float *r, float *g, float *b)
{
  if(_objs->_currFrame)
  {
    osg::LightSource* lightSource = _objs->_currFrame->getLightSource();
    if(lightSource)
    {
      osg::Light* light = lightSource->getLight();
      light->setSpecular(osg::Vec4(*r, *g, *b, 1.0));
      _objs->_intVal = 0;
    }
    else _objs->_intVal = -1;
  }
  else {
    _objs->_intVal = -2;
  }
}

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
	    tf->setTrajectory(traj);
	  }
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

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

OF_EXPORT void OF_FCN(offrame_managetime)(bool *affectChildren, bool *reset,
				       bool *changePauseState, bool *pauseState,
                                       bool *changeOffsetTime, double *offsetTime,
                                       bool *changeTimeScale, double *timeScale)
{
  static bool deprecatewarning = true;
  
  if(deprecatewarning)
  {
    OSG_WARN << "OpenFrames::offrame_managetime DEPRECATION WARNING: function will be removed in the future. Please use WindowProxy time management functions instead." << std::endl;
    deprecatewarning = false; // Only warn once
  }
  
  if(*reset) OSG_WARN << "OpenFrames::offrame_managetime WARNING: function no longer supports reset input. Please set time via WindowProxy time management functions instead." << std::endl;
  if(*changePauseState) OF_FCN(ofwin_pausetime)(pauseState);
  if(*changeOffsetTime) OF_FCN(ofwin_settime)(offsetTime);
  if(*changeTimeScale) OF_FCN(ofwin_settimescale)(timeScale);
}

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

void OF_FCN(ofsphere_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new Sphere(temp);
    _objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofsphere_setnighttexturemap)(OF_CHARARG(fname))
{
  // Make sure that the currently active ReferenceFrame is a Sphere
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if (sphere == NULL) {
    _objs->_intVal = 1;
    return;
  }

  // Convert given character string and length to a proper C string
  std::string temp(OF_STRING(fname));
  sphere->setNightTextureMap(temp);
  _objs->_intVal = 0;
}

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

void OF_FCN(ofsphere_setsphereposition)(double *x, double *y, double *z)
{
  // Make sure that the currently active ReferenceFrame is a Sphere
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if (sphere == NULL) {
    _objs->_intVal = 1;
    return;
  }
  
  sphere->setSpherePosition(*x, *y, *z);
  _objs->_intVal = 0;
}

void OF_FCN(ofsphere_setsphereattitude)(double *rx, double *ry, double *rz, double *angle)
{
  // Make sure that the currently active ReferenceFrame is a Sphere
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if (sphere == NULL) {
    _objs->_intVal = 1;
    return;
  }
  
  sphere->setSphereAttitude(osg::Vec4(*rx, *ry, *rz, *angle));
  _objs->_intVal = 0;
}

void OF_FCN(ofsphere_setspherescale)(double *sx, double *sy, double *sz)
{
  // Make sure that the currently active ReferenceFrame is a Sphere
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if (sphere == NULL) {
    _objs->_intVal = 1;
    return;
  }
  
  sphere->setSphereScale(*sx, *sy, *sz);
  _objs->_intVal = 0;
}

void OF_FCN(ofsphere_setmaterialambient)(float *r, float *g, float *b)
{
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if(sphere)
  {
    osg::Material* mat = sphere->getMaterial();
    if(!mat)
    {
      mat = new osg::Material;
      sphere->setMaterial(mat);
    }
    mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(*r, *g, *b, 1.0));
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}
  
void OF_FCN(ofsphere_setmaterialdiffuse)(float *r, float *g, float *b)
{
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if(sphere)
  {
    osg::Material* mat = sphere->getMaterial();
    if(!mat)
    {
      mat = new osg::Material;
      sphere->setMaterial(mat);
    }
    mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(*r, *g, *b, 1.0));
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(ofsphere_setmaterialspecular)(float *r, float *g, float *b)
{
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if(sphere)
  {
    osg::Material* mat = sphere->getMaterial();
    if(!mat)
    {
      mat = new osg::Material;
      sphere->setMaterial(mat);
    }
    mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(*r, *g, *b, 1.0));
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(ofsphere_setmaterialemission)(float *r, float *g, float *b)
{
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if(sphere)
  {
    osg::Material* mat = sphere->getMaterial();
    if(!mat)
    {
      mat = new osg::Material;
      sphere->setMaterial(mat);
    }
    mat->setEmission(osg::Material::FRONT_AND_BACK, osg::Vec4(*r, *g, *b, 1.0));
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(ofsphere_setmaterialshininess)(float *shininess)
{
  Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
  if(sphere)
  {
    osg::Material* mat = sphere->getMaterial();
    if(!mat)
    {
      mat = new osg::Material;
      sphere->setMaterial(mat);
    }
    mat->setShininess(osg::Material::FRONT_AND_BACK, *shininess);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

/*******************************************
	Model Functions
*******************************************/

void OF_FCN(ofmodel_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new Model(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofdrawtraj_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new DrawableTrajectory(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofcoordaxes_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new CoordinateAxes(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

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

void OF_FCN(oflatlongrid_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new LatLonGrid(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

void OF_FCN(oflatlongrid_setparameters)(double *radiusX, double *radiusY, double *radiusZ, double *latSpace, double *lonSpace)
{
	LatLonGrid *llg = dynamic_cast<LatLonGrid*>(_objs->_currFrame);
	if(llg) 
	{
	  llg->setParameters(*radiusX, *radiusY, *radiusZ, *latSpace, *lonSpace);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = -2;
}

/***********************************************
	RadialPlane Functions
***********************************************/

void OF_FCN(ofradialplane_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new RadialPlane(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
    _objs->_intVal = 0;
}

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

void OF_FCN(oftraj_addtime)(const double *t)
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addTime(*t);
    }
    else {
      _objs->_intVal = -2;
    }
}

void OF_FCN(oftraj_addposition)(const double *x, const double *y, const double *z)
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addPosition(*x, *y, *z);
    }
    else {
      _objs->_intVal = -2;
    }
}

void OF_FCN(oftraj_addpositionvec)(const double pos[])
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addPosition(pos);
    }
    else {
      _objs->_intVal = -2;
    }
}

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

void OF_FCN(oftraj_addattitudevec)(const double att[])
{
	if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->addAttitude(att);
    }
    else {
      _objs->_intVal = -2;
    }
}

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

void OF_FCN(oftraj_setoptionalvec)(unsigned int *index, const double opt[])
{
    if (_objs->_currTraj) {
	  _objs->_intVal = !_objs->_currTraj->setOptional(*index, opt);
    }
    else {
      _objs->_intVal = -2;
    }
}

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

void OF_FCN(oftraj_informartists)()
{
	if (_objs->_currTraj) {
	  _objs->_currTraj->informSubscribers();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

void OF_FCN(oftraj_autoinformartists)(bool *autoinform)
{
	if (_objs->_currTraj) {
	  _objs->_currTraj->autoInformSubscribers(*autoinform);
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}

/************************************************
	TrajectoryArtist Functions
************************************************/

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

void OF_FCN(ofcurveartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new CurveArtist;
	_objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofsegmentartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new SegmentArtist;
    _objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

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

OF_EXPORT void OF_FCN(ofmarkerartist_create)(OF_CHARARG(name))
{
	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currArtist = new MarkerArtist;
	_objs->_artistMap[temp] = _objs->_currArtist;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofview_create)(OF_CHARARG(name))
{
  	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(name));

	_objs->_currView = new View;
    _objs->_viewMap[temp] = _objs->_currView;
    _objs->_intVal = 0;
}

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

void OF_FCN(ofview_gettrackball)(double eye[], double center[], double up[])
{
  if (_objs->_currView) {
    osg::Vec3d vEye, vCenter, vUp;
    _objs->_currView->getTrackball()->getTransformation(vEye, vCenter, vUp);
    eye[0] = vEye[0]; eye[1] = vEye[1]; eye[2] = vEye[2];
    center[0] = vCenter[0]; center[1] = vCenter[1]; center[2] = vCenter[2];
    up[0] = vUp[0]; up[1] = vUp[1]; up[2] = vUp[2];
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

void OF_FCN(ofview_settrackball)(double eye[], double center[], double up[])
{
  if (_objs->_currView) {
    osg::Vec3d vEye(eye[0], eye[1], eye[2]);
    osg::Vec3d vCenter(center[0], center[1], center[2]);
    osg::Vec3d vUp(up[0], up[1], up[2]);
    _objs->_currView->getTrackball()->setTransformation(vEye, vCenter, vUp);
    _objs->_intVal = 0;
  }
  else {
    _objs->_intVal = -2;
  }
}

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

void OF_FCN(ofview_reset)()
{
	if (_objs->_currView) {
      _objs->_currView->resetView();
      _objs->_intVal = 0;
    }
    else {
      _objs->_intVal = -2;
    }
}
