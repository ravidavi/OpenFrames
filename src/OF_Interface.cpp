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
	ReferenceFrame *_currFrame;
	WindowProxy *_currWinProxy;
	Trajectory *_currTraj;
	FrameManager *_currFM;
	TrajectoryArtist *_currArtist;
	View *_currView;

	WindowMap _winMap;  // Map of ID -> WindowProxy
	FrameMap _frameMap; // Map of Name -> ReferenceFrame
	FMMap _fmMap;     // Map of ID -> FrameManager
	TrajectoryMap _trajMap; // Map of ID -> Trajectory
	ArtistMap _artistMap; // Map of ID -> TrajectoryArtist
	ViewMap _viewMap; // Map of ID -> View

	osg::ref_ptr<TimeManagementVisitor> _tmv;

	// Value returned (if any) by last function call
	int _intVal;

	// Get an instance of the singleton
	// If this is the first call, then the object will be created
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

void OF_FCN(of_initialize)()
{
	_objs = OF_Objects::instance();
}

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
}

OF_EXPORT void OF_FCN(ofwin_setgridsize)(int *nrow, int *ncol)
{
	if(_objs->_currWinProxy)
	{
	  _objs->_currWinProxy->setGridSize(*nrow, *ncol);
	}
}

OF_EXPORT void OF_FCN(ofwin_setkeypresscallback)(void (*fcn)(KEYPRESS_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setKeyPressCallback(fcn);
	}
}

OF_EXPORT void OF_FCN(ofwin_setmousemotioncallback)(void (*fcn)(MOUSEMOTION_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setMouseMotionCallback(fcn);
	}
}

OF_EXPORT void OF_FCN(ofwin_setbuttonpresscallback)(void (*fcn)(BUTTON_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setButtonPressCallback(fcn);
	}
}

OF_EXPORT void OF_FCN(ofwin_setbuttonreleasecallback)(void (*fcn)(BUTTON_SIG))
{
	if(_objs->_currWinProxy) 
	{
	  _objs->_currWinProxy->setButtonReleaseCallback(fcn);
	}
}

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
}

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
	}
}

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
	}
}

OF_EXPORT void OF_FCN(ofwin_pauseanimation)(bool *pause)
{
	if(_objs->_currWinProxy)
	{
	  _objs->_currWinProxy->pauseAnimation(*pause);
	}
}

OF_EXPORT void OF_FCN(ofwin_isrunning)(unsigned int *state)
{
	if(_objs->_currWinProxy)
	{
	  *state = _objs->_currWinProxy->isRunning();
	}
	else *state = 0;
}

void OF_FCN(ofwin_setscene)(unsigned int *row, unsigned int *col)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->setScene(_objs->_currFM, *row, *col);
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
		}
	  }
	}
}

void OF_FCN(ofwin_setbackgroundcolor)(unsigned int *row, unsigned int *col, 
                                   float *r, float *g, float *b)
{
	if(_objs->_currWinProxy)
	{
	  RenderRectangle *rr = _objs->_currWinProxy->getGridPosition(*row, *col);
	  if(rr) rr->setBackgroundColor(*r, *g, *b);
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
	  }
	}
}

void OF_FCN(ofwin_setswapbuffersfunction)(void (*fcn)(unsigned int *winID))
{
	if(_objs->_currWinProxy) 
	  _objs->_currWinProxy->setSwapBuffersFunction(fcn);
}

void OF_FCN(ofwin_setmakecurrentfunction)(void (*fcn)(unsigned int *winID, bool *success))
{
	if(_objs->_currWinProxy) 
	  _objs->_currWinProxy->setMakeCurrentFunction(fcn);
}

void OF_FCN(ofwin_setupdatecontextfunction)(void (*fcn)(unsigned int *winID, bool *success))
{
	if(_objs->_currWinProxy) 
	  _objs->_currWinProxy->setUpdateContextFunction(fcn);
}

void OF_FCN(ofwin_resizewindow)(int *x, int *y, unsigned int *width, unsigned int *height)
{
	if(_objs->_currWinProxy) 
	  _objs->_currWinProxy->resizeWindow(*x, *y, *width, *height);
}

void OF_FCN(ofwin_keypress)(unsigned int *key)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->keyPress(*key);
}

void OF_FCN(ofwin_buttonpress)(float *x, float *y, unsigned int *button)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->buttonPress(*x, *y, *button);
}
	
void OF_FCN(ofwin_buttonrelease)(float *x, float *y, unsigned int *button)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->buttonRelease(*x, *y, *button);
}

void OF_FCN(ofwin_mousemotion)(float *x, float *y)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->mouseMotion(*x, *y);
}

void OF_FCN(ofwin_setdesiredframerate)(double *fps)
{
	if(_objs->_currWinProxy)
	  _objs->_currWinProxy->setDesiredFramerate(*fps);
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
}

void OF_FCN(offm_setframe)()
{
	if(_objs->_currFM)
	  _objs->_currFM->setFrame(_objs->_currFrame);
}

void OF_FCN(offm_lock)()
{
	if(_objs->_currFM)
	  _objs->_intVal = _objs->_currFM->lock();
}

void OF_FCN(offm_unlock)()
{
	if(_objs->_currFM)
	  _objs->_intVal = _objs->_currFM->unlock();
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
	}
	else _objs->_currFrame = i->second.get();
}

void OF_FCN(offrame_setcolor)(float *r, float *g, float *b, float *a)
{
	if(_objs->_currFrame) 
	  _objs->_currFrame->setColor(*r, *g, *b, *a);
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
	else _objs->_intVal = 1; // Current frame isn't defined, so raise error
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
	else _objs->_intVal = 1;
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
	if(_objs->_currFrame) 
	  _objs->_currFrame->setPosition(*x, *y, *z);
}

void OF_FCN(offrame_getposition)(double *x, double *y, double *z)
{
	if(_objs->_currFrame) 
	  _objs->_currFrame->getPosition(*x, *y, *z);
}

void OF_FCN(offrame_setattitude)(double *rx, double *ry, double *rz, double *angle)
{
	if(_objs->_currFrame) 
	  _objs->_currFrame->setAttitude(*rx, *ry, *rz, *angle);
}

void OF_FCN(offrame_getattitude)(double *rx, double *ry, double *rz, double *angle)
{
	if(_objs->_currFrame) 
	  _objs->_currFrame->setAttitude(*rx, *ry, *rz, *angle);
}

void OF_FCN(offrame_showaxes)(unsigned int *axes)
{
	if(_objs->_currFrame) 
	  _objs->_currFrame->showAxes(*axes);
}

void OF_FCN(offrame_shownamelabel)(bool *namelabel)
{
	if(_objs->_currFrame)
	  _objs->_currFrame->showNameLabel(*namelabel);
}

void OF_FCN(offrame_showaxeslabels)(unsigned int *labels)
{
	if(_objs->_currFrame)
	  _objs->_currFrame->showAxesLabels(*labels);
}

void OF_FCN(offrame_setnamelabel)(OF_CHARARG(name))
{
	if(_objs->_currFrame) 
	{
	  // Convert given character string and length to a proper C string
	  std::string temp(OF_STRING(name));
	  _objs->_currFrame->setName(temp);
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
	}
}

void OF_FCN(offrame_movexaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveXAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
	}
}

void OF_FCN(offrame_moveyaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveYAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
	}
}

void OF_FCN(offrame_movezaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius)
{
	if(_objs->_currFrame)
	{
	  _objs->_currFrame->moveZAxis(osg::Vec3d(pos[0], pos[1], pos[2]), *length, *headRatio, *bodyRadius, *headRadius);
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
	    tf->setFollowTrajectory(traj);
	  }
	}
}

OF_EXPORT void OF_FCN(offrame_followtype)(int *data, int *mode)
{
	if(_objs->_currFrame)
	{
	  // Check if the frame is already following a trajectory
	  TrajectoryFollower *tf = dynamic_cast<TrajectoryFollower*>(_objs->_currFrame->getTransform()->getUpdateCallback());
	    
	  if(tf == NULL) _objs->_intVal = 1;
	  else tf->setFollowType((unsigned int)(*data), 
	                         (TrajectoryFollower::FollowMode)(*mode));
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
	  }
	}
}

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
	else _objs->_intVal = 1;
}

void OF_FCN(offrame_printframestring)()
{
	if(_objs->_currFrame)
	{
	  std::string str;
	  _objs->_currFrame->createFrameString(str);
	  std::cout << str;
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
}

void OF_FCN(ofsphere_setradius)(double *radius)
{
	  // Make sure that the currently active ReferenceFrame is a Sphere
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
	if(sphere == NULL) return;

	sphere->setRadius(*radius);
}

void OF_FCN(ofsphere_settexturemap)(OF_CHARARG(fname))
{
	// Make sure that the currently active ReferenceFrame is a Sphere
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
	if(sphere == NULL) return;

	// Convert given character string and length to a proper C string
	std::string temp(OF_STRING(fname));
	sphere->setTextureMap(temp);
}

void OF_FCN(ofsphere_setautolod)(bool *lod)
{
	Sphere *sphere = dynamic_cast<Sphere*>(_objs->_currFrame);
	if(sphere)
	{
	  sphere->setAutoLOD(*lod);
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
	}
}

void OF_FCN(ofmodel_setmodelposition)(double *x, double *y, double *z)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelPosition(*x, *y, *z);
	}
}

void OF_FCN(ofmodel_getmodelposition)(double *x, double *y, double *z)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelPosition(*x, *y, *z);
	}
}

void OF_FCN(ofmodel_setmodelscale)(double *sx, double *sy, double *sz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelScale(*sx, *sy, *sz);
	}
}

void OF_FCN(ofmodel_getmodelscale)(double *sx, double *sy, double *sz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelScale(*sx, *sy, *sz);
	}
}

void OF_FCN(ofmodel_setmodelpivot)(double *px, double *py, double *pz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->setModelPivot(*px, *py, *pz);
	}
}

void OF_FCN(ofmodel_getmodelpivot)(double *px, double *py, double *pz)
{
	// Make sure that the currently active ReferenceFrame is a Model
	Model *model = dynamic_cast<Model*>(_objs->_currFrame);
	if(model)
	{
	  model->getModelPivot(*px, *py, *pz);
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
	  if(node) *size = node->getBound()._radius;
	  else *size = -1.0; // Model not specified
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
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
}

/***********************************************
	CoordinateAxes Functions
***********************************************/

void OF_FCN(ofcoordaxes_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new CoordinateAxes(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
}

void OF_FCN(ofcoordaxes_setaxislength)(double *len)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setAxisLength(*len);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

void OF_FCN(ofcoordaxes_setdrawaxes)(unsigned int *axes)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setDrawAxes(*axes);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

void OF_FCN(ofcoordaxes_settickspacing)(double *major, double *minor)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setTickSpacing(*major, *minor);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

void OF_FCN(ofcoordaxes_setticksize)(unsigned int *major, unsigned int *minor)
{
	CoordinateAxes *ca = dynamic_cast<CoordinateAxes*>(_objs->_currFrame);
	if(ca) 
	{
	  ca->setTickSize(*major, *minor);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
}

/***********************************************
	LatLonGrid Functions
***********************************************/

void OF_FCN(oflatlongrid_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new LatLonGrid(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
}

void OF_FCN(oflatlongrid_setparameters)(double *radius, double *latSpace, double *lonSpace)
{
	LatLonGrid *llg = dynamic_cast<LatLonGrid*>(_objs->_currFrame);
	if(llg) 
	{
	  llg->setParameters(*radius, *latSpace, *lonSpace);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

/***********************************************
	RadialPlane Functions
***********************************************/

void OF_FCN(ofradialplane_create)(OF_CHARARG(name))
{
	std::string temp(OF_STRING(name));

	_objs->_currFrame = new RadialPlane(temp);
	_objs->_frameMap[temp] = _objs->_currFrame;
}

void OF_FCN(ofradialplane_setparameters)(double *radius, double *radSpace, double *lonSpace)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setParameters(*radius, *radSpace, *lonSpace);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

void OF_FCN(ofradialplane_setplanecolor)(float *r, float *g, float *b, float *a)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setPlaneColor(*r, *g, *b, *a);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

void OF_FCN(ofradialplane_setlinecolor)(float *r, float *g, float *b, float *a)
{
	RadialPlane *rp = dynamic_cast<RadialPlane*>(_objs->_currFrame);
	if(rp) 
	{
	  rp->setLineColor(*r, *g, *b, *a);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
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
}

void OF_FCN(oftraj_setnumoptionals)(unsigned int *nopt)
{
	if(_objs->_currTraj)
	  _objs->_currTraj->setNumOptionals(*nopt);
}

void OF_FCN(oftraj_setdof)(unsigned int *dof)
{
	if(_objs->_currTraj)
	  _objs->_currTraj->setDOF(*dof);
}

void OF_FCN(oftraj_addtime)(const double *t)
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->addTime(*t);
}

void OF_FCN(oftraj_addposition)(const double *x, const double *y, const double *z)
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->addPosition(*x, *y, *z);
}

void OF_FCN(oftraj_addpositionvec)(const double pos[])
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->addPosition(pos);
}

void OF_FCN(oftraj_addattitude)(const double *x, const double *y,
                             const double *z, const double *w)
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->addAttitude(*x, *y, *z, *w);
}

void OF_FCN(oftraj_addattitudevec)(const double att[])
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->addAttitude(att);
}

void OF_FCN(oftraj_setoptional)(unsigned int *index, const double *x,
                             const double *y, const double *z)
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->setOptional(*index, *x, *y, *z);
}

void OF_FCN(oftraj_setoptionalvec)(unsigned int *index, const double opt[])
{
	if(_objs->_currTraj)
	  _objs->_intVal = _objs->_currTraj->setOptional(*index, opt);
}

void OF_FCN(oftraj_clear)()
{
	if(_objs->_currTraj)
	  _objs->_currTraj->clear();
}

void OF_FCN(oftraj_informartists)()
{
	if(_objs->_currTraj)
	  _objs->_currTraj->informArtists();
}

void OF_FCN(oftraj_autoinformartists)(bool *autoinform)
{
	if(_objs->_currTraj)
	  _objs->_currTraj->autoInformArtists(*autoinform);
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
	if(_objs->_currArtist)
	  _objs->_currArtist->setTrajectory(_objs->_currTraj);
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
}

void OF_FCN(ofcurveartist_setxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofcurveartist_setydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofcurveartist_setzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofcurveartist_setcolor)(float *r, float *g, float *b)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setColor(*r, *g, *b);
}

void OF_FCN(ofcurveartist_setwidth)(float *width)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setWidth(*width);
}

void OF_FCN(ofcurveartist_setpattern)(int *factor, unsigned short *pattern)
{
	CurveArtist *artist = dynamic_cast<CurveArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setPattern(*factor, *pattern);
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
}

void OF_FCN(ofsegmentartist_setstartxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setstartydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setstartzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setendxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setendydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setendzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofsegmentartist_setcolor)(float *r, float *g, float *b)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setColor(*r, *g, *b);
}

void OF_FCN(ofsegmentartist_setwidth)(float *width)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setWidth(*width);
}

void OF_FCN(ofsegmentartist_setpattern)(int *factor, unsigned short *pattern)
{
	SegmentArtist *artist = dynamic_cast<SegmentArtist*>(_objs->_currArtist);
	if(artist) 
	  artist->setPattern(*factor, *pattern);
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
}

void OF_FCN(ofmarkerartist_setxdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofmarkerartist_setydata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

void OF_FCN(ofmarkerartist_setzdata)(int *src, unsigned int *element,
                                 unsigned int *opt, double *scale)
{
	// Make sure source is within range (see Trajectory::SourceType enum)
	if(*src < 0 || *src > 3) 
	{
	  _objs->_intVal = 1;
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
	  _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setmarkers)( unsigned int *markers )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkers(*markers);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setmarkercolor)( unsigned int *markers, float *r, float *g, float *b )
{
  	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkerColor(*markers, *r, *g, *b);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
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
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatetype)( unsigned int *type )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateType((MarkerArtist::IntermediateType)(*type));
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatespacing)( double *spacing )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateSpacing(*spacing);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatedirection)( unsigned int *direction )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setIntermediateDirection((MarkerArtist::DrawnMarkers)(*direction));
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setmarkersize)( unsigned int *size )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setMarkerSize(*size);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
}

OF_EXPORT void OF_FCN(ofmarkerartist_setautoattenuate)( bool *autoattenuate )
{
	MarkerArtist *artist = dynamic_cast<MarkerArtist*>(_objs->_currArtist);
	if(artist)
	{
	  artist->setAutoAttenuate(*autoattenuate);
	  _objs->_intVal = 0;
	}
	else _objs->_intVal = 1;
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
}

void OF_FCN(ofview_setorthographic)(double *left, double *right,
                                 double *bottom, double *top)
{
	if(_objs->_currView)
	  _objs->_currView->setOrthographic(*left, *right, *bottom, *top);
}

void OF_FCN(ofview_setperspective)(double *fov, double *ratio)
{
	if(_objs->_currView)
	  _objs->_currView->setPerspective(*fov, *ratio);
}

void OF_FCN(ofview_setaspectmultiplier)(double *mult)
{
	if(_objs->_currView)
	  _objs->_currView->setAspectMultiplier(*mult);
}

#if defined(IFORT_CALLS)
void OF_FCN(ofview_setviewframe)(const char *root, const char *frame, 
                                 unsigned int rootlen, 
                                 unsigned int framelen)

#else
void OF_FCN(ofview_setviewframe)(OF_CHARARG(root), OF_CHARARG(frame))

#endif
{
        std::string rname(OF_STRING(root));
	std::string fname(OF_STRING(frame));

	if(_objs->_currView)
	{

	  // Find root and viewed frames in the FrameMap
	  FrameMap::iterator i = _objs->_frameMap.find(rname);
	  FrameMap::iterator j = _objs->_frameMap.find(fname);
	  ReferenceFrame *rootFrame, *viewFrame;

	  // Set dummy reference to root frame
	  if(i == _objs->_frameMap.end()) rootFrame = NULL;
	  else rootFrame = i->second.get();

	  // Set dummy reference to view frame
	  if(j == _objs->_frameMap.end()) viewFrame = NULL;
	  else viewFrame = j->second.get();

	  // Tell current view to find and remember the path from the
	  // root -> view frame. If the path doesn't exist, then
	  // the ofview_isvalid() function will raise a flag.
	  _objs->_currView->setViewFrame(rootFrame, viewFrame);
	}
}

void OF_FCN(ofview_setdefaultviewdistance)(double *distance)
{
	if(_objs->_currView)
	  _objs->_currView->setDefaultViewDistance(*distance);
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
	  _objs->_intVal = 1; // error flag
	}
}

void OF_FCN(ofview_reset)()
{
	if(_objs->_currView) _objs->_currView->resetTrackball();
}
