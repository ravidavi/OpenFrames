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

#ifndef _OF_FORTRAN_
#define _OF_FORTRAN_

#include <OpenFrames/Export.h>

// Macro to handle compiler function name mangling
#if defined(GFORTRAN_CALLS)
        /// GFortran appends underscores to function names
	#define OF_FCN(name) name##__ 
#else
        /// Function names stay as they are on all other interfaces
        /// This includes Intel Fortran and C compilers
	#define OF_FCN(name) name 
#endif

// Macros to handle Fortran hidden string length arguments

/// \def OF_CHARARG(name)
/// Macro to handle Fortran hidden character length arguments

/// \def OF_STRING(name)
/// Macro to handle Fortran hidden string length arguments

#if defined(GFORTRAN_CALLS) || defined(IFORT_CALLS)
        // Fortran compilers pass hidden char array lengths
        #define OF_CHARARG(name) const char *name, unsigned int name##len
        #define OF_STRING(name) name, name##len
#else
        #define OF_CHARARG(name) const char *name
        #define OF_STRING(name) name
#endif

// Define shortcuts for callback function signatures
/** Shortcut for a basic callback signature */
#define BASIC_CALLBACK_SIG unsigned int *winID, unsigned int *row, unsigned int *col
/** Shortcut for a keypress callback signature */
#define KEYPRESS_SIG BASIC_CALLBACK_SIG, int *key
/** Shortcut for a mouse motion callback signature */
#define MOUSEMOTION_SIG BASIC_CALLBACK_SIG, float *x, float *y
/** Shortcut for a button press callback signature */
#define BUTTON_SIG MOUSEMOTION_SIG, unsigned int *button

// Make sure this header can be used by "pure" C compilers (e.g. Matlab)
#ifdef __cplusplus
extern "C"
{
#endif

/** \file
 * All functions have a prefix indicating what they operate on:
 * - ofwin: Acts on the currently active WindowProxy
 * - offm: Acts on the specified FrameManager.
 * - offrame: Acts on the currently active Reference Frame.
 * - of(type of ReferenceFrame): Acts on the currently active ReferenceFrame,
 *     assuming that it is of the subtype given by the function name.
 * - oftraj: Acts on the currently active Trajectory.
 * - oftrajartist: Acts on the currently active TrajectoryArtist.
 * - of(type of TrajectoryArtist): Acts on the currently active TrajectoryArtist,
 *     assuming that it is of the subtype given by the function name.
 * - ofview: Acts on the currently active View.
 *
 * In addition, some functions produce integer results. These values
 * can be obtained by calling of_getreturnedvalue().
**/

// Must be called before using OpenFrames
// Sets up all internal data
OF_EXPORT void OF_FCN(of_initialize)();

// Must be called when done using OpenFrames
// Cleans up all internal data
OF_EXPORT void OF_FCN(of_cleanup)();

/*
 * \brief Get the return value returned by the most recent API function call.
 *
 * Functions return zero to indicate success, and non-zero to indicate an error.
 * \warning DO NOT call of_getreturnedvalue after calling of_cleanup.
 *          of_cleanup sets the pointer for _objs to NULL, which would be dereferenced by this function.
 *
 * \param val variable to store the return value from the previous API function call.
 */
OF_EXPORT void OF_FCN(of_getreturnedvalue)(int *val);
  
/*
 * \brief Add a search path when OSG tries to load data files. 
 *
 * These can be images, models, or any other data files. This does not affect loading OSG libraries or plugins. The new path is added to the FRONT of the path search list, and will be searched before any existing paths (including previous calls to of_addDataFilePath.
 *
 *
 * \param newpath full path to be searched when loading a file.
 */
OF_EXPORT void OF_FCN(of_adddatafilepath)(OF_CHARARG(newpath));

/***********************************************************************
	Window Functions
***********************************************************************/
// Set the current active WindowProxy

/*
 * \brief Set the active WindowProxy
 *
 * \param[out] id ID of the window to activate.
 */
OF_EXPORT void OF_FCN(ofwin_activate)(unsigned int *id);

/*
 * \brief Get ID of the active WindowProxy
 *
 * \param[out] retid ID of the active window.
 */
OF_EXPORT void OF_FCN(ofwin_getid)(unsigned int *retid);

/*
 * \brief Create a new WindowProxy that will manage drawing onto a window.
 *
 * This new WindowProxy will also become the current active one.
 *
 * \param x        X-coordinate (in pixels) of the screen of the upper-right corner of the window.
 * \param y        Y-coordinate (in pixels) of the screen of the upper-right corner of the window.
 * \param width    Width of the window (in pixels).
 * \param height   Height of the window (in pixels).
 * \param nrow     Number of rows in the window grid.
 * \param ncol     Number of columns in the window grid.
 * \param embedded True if the user wants to provide their own OpenGL window.
 * \param id       ID of this window.
 * \param useVR    Whether to enable rendering in VR (if OpenFrames is built with OpenVR support)
 */
OF_EXPORT void OF_FCN(ofwin_createproxy)(int *x, int *y,
                                      unsigned int *width, unsigned int *height,
                                      unsigned int *nrow, unsigned int *ncol,
                                      bool *embedded, unsigned int *id,
                                      bool *useVR);

/*
 * \brief Set the window name (title). Only applies to non-embedded windows.
 *
 * \param winname The new window name.
 */
OF_EXPORT void OF_FCN(ofwin_setwindowname)(OF_CHARARG(winname));

/*
 * \brief Set the number rows and columns in the grid.
 *
 * This applies to the current active WindowProxy.
 *
 * \param nrow     Number of rows in the window grid.
 * \param ncol     Number of columns in the window grid.
 */
OF_EXPORT void OF_FCN(ofwin_setgridsize)(int *nrow, int *ncol);

// Set event handling callbacks

/*
 * \brief Set a callback function to be called on keypress.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called on keypress.
 */
OF_EXPORT void OF_FCN(ofwin_setkeypresscallback)(void (*fcn)(KEYPRESS_SIG));

/*
 * \brief Set a callback function to be called on mouse motion.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called on mouse motion.
 */
OF_EXPORT void OF_FCN(ofwin_setmousemotioncallback)(void (*fcn)(MOUSEMOTION_SIG));

/*
 * \brief Set a callback function to be called on button press.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called on button press
 */
OF_EXPORT void OF_FCN(ofwin_setbuttonpresscallback)(void (*fcn)(BUTTON_SIG));

/*
 * \brief Set a callback function to be called on button release.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called on button release.
 */
OF_EXPORT void OF_FCN(ofwin_setbuttonreleasecallback)(void (*fcn)(BUTTON_SIG));  

// Start/stop animation
OF_EXPORT void OF_FCN(ofwin_start)(); // Start animation
OF_EXPORT void OF_FCN(ofwin_stop)();  // Force animation to stop and wait for stop
OF_EXPORT void OF_FCN(ofwin_signalstop)();  // Force animation to stop
OF_EXPORT void OF_FCN(ofwin_waitforstop)(); // Wait for user to exit animation

/*
 * \brief Pause/unpause the animation.
 *
 * This applies to the current active WindowProxy.
 *
 * \param pause True to pause the animation, False to unpause the animation.
 */
OF_EXPORT void OF_FCN(ofwin_pauseanimation)(bool *pause);

/*
 * \brief Check if the animation is running.
 *
 * This applies to the current active WindowProxy.
 *
 * \param state This variable is set to 1 if the animation is running, 0 otherwise.
 */
OF_EXPORT void OF_FCN(ofwin_isrunning)(unsigned int *state);

// Time control

/*
 * \brief Set the simulation time.
 *
 * This applies to the current active WindowProxy.
 *
 * \param time New simulation time.
 */
OF_EXPORT void OF_FCN(ofwin_settime)(double *time);

/*
 * \brief Get the simulation time.
 *
 * This applies to the current active WindowProxy.
 *
 * \param time Current simulation time.
 */
OF_EXPORT void OF_FCN(ofwin_gettime)(double *time);

/*
 * \brief Set whether to pause time.
 *
 * This applies to the current active WindowProxy.
 *
 * \param pause Set to 1 to pause the simulation time, 0 to unpause.
 */
OF_EXPORT void OF_FCN(ofwin_pausetime)(bool *pause);

/*
 * \brief Check if time is paused.
 *
 * This applies to the current active WindowProxy.
 *
 * \param pause Set to 1 if simulation time is paused, 0 if unpaused.
 */
OF_EXPORT void OF_FCN(ofwin_istimepaused)(bool *isPaused);

/*
 * \brief Set the simulation time scale.
 *
 * This applies to the current active WindowProxy.
 *
 * \param tscale New simulation time scale.
 */
OF_EXPORT void OF_FCN(ofwin_settimescale)(double *tscale);

/*
 * \brief Get the simulation time scale.
 *
 * This applies to the current active WindowProxy.
 *
 * \param tscale Current simulation time scale.
 */
OF_EXPORT void OF_FCN(ofwin_gettimescale)(double *tscale);

// Default lighting control.
// This can be overridden by enabling light from at least one ReferenceFrame.

/*
 * \brief Set the lighting parameters for the specified grid position.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row  Row in the grid to set.
 * \param col  Column in the grid to set.
 * \param r    Red component of specified light.
 * \param g    Green component of specified light.
 * \param b    Blue component of specified light.
 */
OF_EXPORT void OF_FCN(ofwin_setlightambient)(unsigned int *row, unsigned int *col,
                                             float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofwin_setlightdiffuse)(unsigned int *row, unsigned int *col,
                                             float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofwin_setlightspecular)(unsigned int *row, unsigned int *col,
                                              float *r, float *g, float *b);

/*
 * \brief Set the light position for the specified grid position.
 *
 * This applies to the current active WindowProxy.
 * Light position is defined in eye coordinates (x right, y up, z out of screen).
 *
 * \param row  Row in the grid to set.
 * \param col  Column in the grid to set.
 * \param x    X position in eye space.
 * \param y    Y position in eye space.
 * \param z    Z position in eye space.
 * \param w    If 0 then directional (antiparallel to x,y,z direction).
 *             If 1 then positional  (radiates from x,y,z direction).
 */
OF_EXPORT void OF_FCN(ofwin_setlightposition)(unsigned int *row, unsigned int *col,
                                              float *x, float *y, float *z, float *w);

/*
 * \brief Set the scene at the specified grid position.
 *
 * This applies to the current active WindowProxy.
 * The scene is specified by the currently active FrameManager.
 *
 * \param row Row in the grid to set.
 * \param col Column in the grid to set.
 */
OF_EXPORT void OF_FCN(ofwin_setscene)(unsigned int *row, unsigned int *col);

/*
 * \brief Set the 3D stereo mode for the specified grid position.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row           Row in the grid to set.
 * \param col           Column in the grid to set.
 * \param enable        True to enable 3D stereo mode.
 * \param eyeseparation Set eye separation for 3D stereo.
 * \param width         Width of the screen.
 * \param height        Height of the screen.
 * \param distance      Distance of the screen.
 */
OF_EXPORT void OF_FCN(ofwin_setstereo)(unsigned int *row, unsigned int *col, bool *enable, float *eyeseparation, float *width, float *height, float *distance);

/*
 * \brief Set the background color of the specified grid position.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row Row in the grid to set.
 * \param col Column in the grid to set.
 * \param r   Red color component [0-1].
 * \param g   Green color component [0-1].
 * \param b   Blue color component [0-1].
 */
OF_EXPORT void OF_FCN(ofwin_setbackgroundcolor)(unsigned int *row, unsigned int *col,
                                             float *r, float *g, float *b);

/*
 * \brief Set the background texture of the specified grid position.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row   Row in the grid to set.
 * \param col   Column in the grid to set.
 * \param fname File containing the background texture.
 */
OF_EXPORT void OF_FCN(ofwin_setbackgroundtexture)(unsigned int *row, unsigned int *col, OF_CHARARG(fname));

/*
 * \brief Set the background star field of the specified grid position.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param minMag Minimum star magnitude to show.
 * \param maxMag Maximum star magnitude to show.
 * \param fname  File containing the background star field catalog data.
 */
OF_EXPORT void OF_FCN(ofwin_setbackgroundstardata)(unsigned int *row, unsigned int *col, float *minMag, float *maxMag, OF_CHARARG(fname));

/*
 * \brief Enable/disable the HUD text for the specified grid position. Create placeholder
 *        HUD text if it does not yet exist.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param enable Whether to enable or disable the HUD text.
 */
OF_EXPORT void OF_FCN(ofwin_enablehudtext)(unsigned int *row, unsigned int *col, bool *enable);

/*
 * \brief Set HUD font. Create placeholder HUD text if it does not yet exist.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param fname  Name of font to use, e.g. "arial.ttf".
 */
OF_EXPORT void OF_FCN(ofwin_sethudtextfont)(unsigned int *row, unsigned int *col, OF_CHARARG(fname));

/*
 * \brief Set HUD color and size. Create placeholder HUD text if it does not yet exist.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param r      Red color component.
 * \param g      Green color component.
 * \param b      Blue color component.
 * \param charSize Character size in pixels.
 */
OF_EXPORT void OF_FCN(ofwin_sethudtextparameters)(unsigned int *row, unsigned int *col, float *r, float *g, float *b, float *charSize);

/*
 * \brief Set HUD text position and alignment. Create placeholder HUD text if it does not yet exist.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param x      Text origin x position, in range [0,1] from left to right.
 * \param y      Text origin y position, in range [0,1] from bottom to top.
 * \param alignment Alignment location of text origin, see osgText::AlignmentType enum.
 */
OF_EXPORT void OF_FCN(ofwin_sethudtextposition)(unsigned int *row, unsigned int *col, float *x, float *y, unsigned int *alignment);

/*
 * \brief Set HUD text. Create placeholder HUD text if it does not yet exist.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row    Row in the grid to set.
 * \param col    Column in the grid to set.
 * \param text   The new HUD text.
 */
OF_EXPORT void OF_FCN(ofwin_sethudtext)(unsigned int *row, unsigned int *col, OF_CHARARG(text));

/*
 * \brief Set a callback function for swapping the front/back buffers.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called.
 */
OF_EXPORT void OF_FCN(ofwin_setswapbuffersfunction)(void (*fcn)(unsigned int *winID));

/*
 * \brief Set a callback function for making the OpenGL context current (so it can be drawn on).
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called.
 */
OF_EXPORT void OF_FCN(ofwin_setmakecurrentfunction)(void (*fcn)(unsigned int *winID, bool *success));

/*
 * \brief Set a callback function for updating the OpenGL context after qualifying events.
 *
 * Currently this includes resize.
 *
 * This applies to the current active WindowProxy.
 *
 * \param fcn Callback function to be called.
 */
OF_EXPORT void OF_FCN(ofwin_setupdatecontextfunction)(void (*fcn)(unsigned int *winID, bool *success));

// Inform the WindowProxy that an event occurred.

/*
 * \brief Resize the window to a new position and size.
 *
 * This applies to the current active WindowProxy.
 *
 * \param x      X-coordinate (in pixels) of the screen of the upper-right corner of the window.
 * \param y      Y-coordinate (in pixels) of the screen of the upper-right corner of the window.
 * \param width  Width of the window (in pixels).
 * \param height Height of the window (in pixels).
 */
OF_EXPORT void OF_FCN(ofwin_resizewindow)(int *x, int *y, unsigned int *width, unsigned int *height);

/*
 * \brief Create a key-pressed event.
 *
 * This applies to the current active WindowProxy.
 *
 * \param key Key pressed (see osg::GUIEventAdapter::KeySymbol enum).
 */
OF_EXPORT void OF_FCN(ofwin_keypress)(unsigned int *key);

/*
 * \brief Create a key released event.
 *
 * This applies to the current active WindowProxy.
 *
 * \param key Key released (see osg::GUIEventAdapter::KeySymbol enum).
 */
OF_EXPORT void OF_FCN(ofwin_keyrelease)(unsigned int *key);

/*
 * \brief Create a mouse button pressed event.
 *
 * This applies to the current active WindowProxy.
 *
 * \param x      X-coordinate of the mouse in the window.
 * \param y      Y-coordinate of the mouse in the window.
 * \param button Mouse button pressed. Button numbering is 1 for left mouse button, 2 for middle, 3 for right.
 */
OF_EXPORT void OF_FCN(ofwin_buttonpress)(float *x, float *y, unsigned int *button);

/*
 * \brief Create a mouse button released event.
 *
 * This applies to the current active WindowProxy.
 *
 * \param x      X-coordinate of the mouse in the window.
 * \param y      Y-coordinate of the mouse in the window.
 * \param button Mouse button released. Button numbering is 1 for left mouse button, 2 for middle, 3 for right.
 */
OF_EXPORT void OF_FCN(ofwin_buttonrelease)(float *x, float *y, unsigned int *button);

/*
 * \brief Create a mouse-moved event.
 *
 * This applies to the current active WindowProxy.
 *
 * \param x X-coordinate of the mouse in the window.
 * \param y Y-coordinate of the mouse in the window.
 */
OF_EXPORT void OF_FCN(ofwin_mousemotion)(float *x, float *y);

/*
 * \brief Set the desired framerate of the window (frames/second).
 *
 * This applies to the current active WindowProxy.
 *
 * \param fps Desired framerate value in frames per second.
 */
OF_EXPORT void OF_FCN(ofwin_setdesiredframerate)(double *fps);

// Control views in any given grid position.

/*
 * \brief Add a view to the window.
 *
 * This applies to the current active WindowProxy.
 * This adds the current acive View.
 *
 * \param row Row to add the view to.
 * \param col Column to add the view to.
 */
OF_EXPORT void OF_FCN(ofwin_addview)(unsigned int *row, unsigned int *col);

/*
 * \brief Remove a view from the window.
 *
 * This applies to the current active WindowProxy.
 * This removes the current acive View.
 *
 * \param row Row to remove the view from.
 * \param col Column to remove the view from.
 */
OF_EXPORT void OF_FCN(ofwin_removeview)(unsigned int *row, unsigned int *col);

/*
 * \brief Remove all the view(s) from the window.
 *
 * This applies to the current active WindowProxy.
 *
 * \param row Row to remove the view(s) from.
 * \param col Column to remove the view(s) from.
 */
OF_EXPORT void OF_FCN(ofwin_removeallviews)(unsigned int *row, unsigned int *col);

/*
 * \brief Set the view currently displayed in the window.
 *
 * This applies to the current active WindowProxy.
 * This selects the current acive View.
 *
 * \param row Row to set the active view in.
 * \param col Column to set the active view in.
 */
OF_EXPORT void OF_FCN(ofwin_selectview)(unsigned int *row, unsigned int *col);

// Window capture functions.
// Intel Fortran passes string lengths after all arguments.

/*
 * \brief Set the file name and type that will be used for window captures.
 *
 * This applies to the currently active WindowProxy.
 *
 * \param fname File name (without extension).
 * \param fext  File extension (determines image type).
 */

/*
 * \brief Capture the next rendered frame.
 *
 * This applies to the currently active WindowProxy.
 */
OF_EXPORT void OF_FCN(ofwin_capturewindow)();
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(ofwin_setwindowcapturefile)(const char *fname,
                                                  const char *fext,
                                                  unsigned int fnamelen,
                                                  unsigned int fextlen);

// GFortran passes string lengths after each argument
// C doesn't need string lengths
// Both these cases are handled by the OF_CHARARG() macro
#else
OF_EXPORT void OF_FCN(ofwin_setwindowcapturefile)(OF_CHARARG(fname),
                                                  OF_CHARARG(fext));
#endif

/*
 * \brief Set the key that activates a window capture.
 *
 * This applies to the currently active WindowProxy.
 *
 * \param key Integer representation of key char. Set to 0 (zero) to disable key-based window capture.
 */
OF_EXPORT void OF_FCN(ofwin_setwindowcapturekey)(int *key);

/******************************************************************
	FrameManger Functions
******************************************************************/

/*
 * \brief Set the currently active FrameManager.
 *
 * \param id ID of the frame manager to activate.
 */
OF_EXPORT void OF_FCN(offm_activate)(int *id);

/*
 * \brief Create a new FrameManager with the given ID.
 *
 * This new FrameManager will also become the current active one.
 *
 * \param id ID of the frame manager to create.
 */
OF_EXPORT void OF_FCN(offm_create)(int *id);

/*
 * \brief Assign a ReferenceFrame to the FrameManager.
 *
 * This applies to the current active FrameManager.
 * The scene is specified by the currently active ReferenceFrame.
 */
OF_EXPORT void OF_FCN(offm_setframe)();

/*
 * \brief Lock the current FrameManager.
 *
 * This should be done before the FrameManager's scene is modified.
 * This applies to the current active FrameManager.
 */
OF_EXPORT void OF_FCN(offm_lock)();

/*
 * \brief Unlock the current FrameManager.
 *
 * This should be done after the FrameManager's scene is modified.
 * This applies to the current active FrameManager.
 */
OF_EXPORT void OF_FCN(offm_unlock)();

/******************************************************************
	ReferenceFrame Functions
******************************************************************/

/*
 * \brief Set the currently active reference frame.
 *
 * \param name Name of the frame to activate.
 */
OF_EXPORT void OF_FCN(offrame_activate)(OF_CHARARG(name));

/*
 * \brief Create a new ReferenceFrame with the given name.
 *
 * This new ReferenceFrame will also become the current active one.
 *
 * \param name Name of the frame to create.
 */
OF_EXPORT void OF_FCN(offrame_create)(OF_CHARARG(name));

/*
 * \brief Set the color of the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param r Red color component [0-1].
 * \param g Green color component [0-1].
 * \param b Blue color component [0-1].
 * \param a Alpha (transparancy) component [0-1].
 */
OF_EXPORT void OF_FCN(offrame_setcolor)(float *r, float *g, float *b, float *a);

/**
 * \brief Add a child frame to the current frame.
 *
 * This applies to the current active ReferenceFrame.
 * The currently active frame will remain active.
 *
 * \param name Name of the frame to add as a child to the active frame.
 */
OF_EXPORT void OF_FCN(offrame_addchild)(OF_CHARARG(name));

/*
 * \brief Remove a child frame from the current frame.
 *
 * This applies to the current active ReferenceFrame.
 * The currently active frame will remain active.
 *
 * \param name Name of the child frame to remove from the active frame.
 */
OF_EXPORT void OF_FCN(offrame_removechild)(OF_CHARARG(name));

/*
 * \brief Remove all child frames from the current frame.
 *
 * This applies to the current active ReferenceFrame.
 * The currently active frame will remain active.
 */
OF_EXPORT void OF_FCN(offrame_removeallchildren)();

/*
 * \brief Get the number of child frames in the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param numchildren variable to store the number of child frames to.
 */
OF_EXPORT void OF_FCN(offrame_getnumchildren)(int *numchildren);

/*
 * \brief Set the position of the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param x X position.
 * \param y Y position.
 * \param z Z position.
 */
OF_EXPORT void OF_FCN(offrame_setposition)(double *x, double *y, double *z);

/*
 * \brief Get the position of the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param x Returned X position.
 * \param y Returned Y position.
 * \param z Returned Z position.
 */
OF_EXPORT void OF_FCN(offrame_getposition)(double *x, double *y, double *z);

/*
 * \brief Set the attitude of the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param rx    X component of the rotation quaternion.
 * \param ry    Y component of the rotation quaternion.
 * \param rz    Z component of the rotation quaternion.
 * \param angle Angle component of the rotation quaternion.
 */
OF_EXPORT void OF_FCN(offrame_setattitude)(double *rx, double *ry, double *rz, double *angle);

/*
 * \brief Get the attitude of the current frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param rx    Returned X component of the rotation quaternion.
 * \param ry    Returned Y component of the rotation quaternion.
 * \param rz    Returned Z component of the rotation quaternion.
 * \param angle Returned angle component of the rotation quaternion.
 */
OF_EXPORT void OF_FCN(offrame_getattitude)(double *rx, double *ry, double *rz, double *angle);

/*
 * \brief Toggle which axis components are displayed.
 *
 * This applies to the current active ReferenceFrame.
 * The axis is initially drawn at the origin of the reference frame unless otherwise
 * specified by offrame_movexaxis(), offrame_moveyaxis() or offrame_movezaxis().
 *
 * To show multiple axis components, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show.
 *
 * \param axes Axis components to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden.
 */
OF_EXPORT void OF_FCN(offrame_showaxes)(unsigned int *axes);

/*
 * \brief Toggle display of the name label of the frame.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param namelabel True to display the label, false to hide it.
 */
OF_EXPORT void OF_FCN(offrame_shownamelabel)(bool *namelabel);

/*
 * \brief Toggle which axis labels are displayed.
 *
 * This applies to the current active ReferenceFrame.
 * The axis is initially drawn at the origin of the reference frame unless otherwise
 * specified by offrame_movexaxis(), offrame_moveyaxis() or offrame_movezaxis().
 *
 * To show multiple axis labels, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show.
 *
 * \param labels Axis labels to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden.
 */
OF_EXPORT void OF_FCN(offrame_showaxeslabels)(unsigned int *labels);

/*
 * \brief Change the name label for the current ReferenceFrame.
 *
 * Note that it will still be referred to using the name assigned to it at creation.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param name Name of the label.
 */
OF_EXPORT void OF_FCN(offrame_setnamelabel)(OF_CHARARG(name));

/*
 * \brief Change the axes labels for the current ReferenceFrame.
 * Note- Intel Fortran passes string lengths after all arguments.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param xlabel Name of the x-axis label.
 * \param ylabel Name of the y-axis label.
 * \param zlabel Name of the z-axis label.
 */
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(offrame_setaxeslabels)(const char *xlabel,
                                             const char *ylabel,
                                             const char *zlabel, 
                                             unsigned int xlabellen, 
                                             unsigned int ylabellen, 
                                             unsigned int zlabellen);

// GFortran passes string lengths after each argument
// C doesn't need string lengths
// Both these cases are handled by the OF_CHARARG() macro
#else
OF_EXPORT void OF_FCN(offrame_setaxeslabels)(OF_CHARARG(xlabel),
                                             OF_CHARARG(ylabel),
                                             OF_CHARARG(zlabel));
#endif

// Reposition axes.

/*
 * \brief Reposition and resize the x component of the coordinate axis.
 *
 * Make sure that offrame_showaxes() is configured to display this axis.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param pos        Position array of the origin of the x-axis component.
 * \param length     Length of the x-axis arrow.
 * \param headRatio  Ratio of the arrow head to body. Set to 0.0 to use default.
 * \param bodyRadius Radius of the body of the arrow. Set to 0.0 to use default.
 * \param headRadius Radius of the head of the arrow. Set to 0.0 to use default.
 */
OF_EXPORT void OF_FCN(offrame_movexaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);

/*
 * \brief Reposition and resize the y component of the coordinate axis.
 *
 * Make sure that offrame_showaxes() is configured to display this axis.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param pos        Position array of the origin of the y-axis component.
 * \param length     Length of the y-axis arrow.
 * \param headRatio  Ratio of the arrow head to body. Set to 0.0 to use default.
 * \param bodyRadius Radius of the body of the arrow. Set to 0.0 to use default.
 * \param headRadius Radius of the head of the arrow. Set to 0.0 to use default.
 */
OF_EXPORT void OF_FCN(offrame_moveyaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);

/*
 * \brief Reposition and resize the z component of the coordinate axis.
 *
 * Make sure that offrame_showaxes() is configured to display this axis.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param pos        Position array of the origin of the z-axis component.
 * \param length     Length of the z-axis arrow.
 * \param headRatio  Ratio of the arrow head to body. Set to 0.0 to use default.
 * \param bodyRadius Radius of the body of the arrow. Set to 0.0 to use default.
 * \param headRadius Radius of the head of the arrow. Set to 0.0 to use default.
 */
OF_EXPORT void OF_FCN(offrame_movezaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);

// Enable and manage lighting for the currently active ReferenceFrame.

/*
 * \brief Enable and manage per-frame lighting.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param enabled Whether to enable or disable lighting.
 * \param r Red component of specified light type.
 * \param g Green component of specified light type.
 * \param b Blue component of specified light type.
 */
OF_EXPORT void OF_FCN(offrame_setlightsourceenabled)(bool *enabled);

OF_EXPORT void OF_FCN(offrame_getlightsourceenabled)(bool *enabled);
OF_EXPORT void OF_FCN(offrame_setlightambient)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(offrame_setlightdiffuse)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(offrame_setlightspecular)(float *r, float *g, float *b);

/*
 * \brief Have this frame follow the specified trajectory.
 *
 * The name used is the one used in the trajecotries creation in oftraj_create().
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param name Name of the trajectory to follow.
 */
OF_EXPORT void OF_FCN(offrame_followtrajectory)(OF_CHARARG(name));

/*
 * \brief Follow the trajectory's position, attitude, or both, and set the follow mode.
 *
 * The function offrame_followtrajectory() must be called before this function.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param data Set whether to follow position and/or velocity (see OpenFrames::TrajectoryFollower::FollowData).
 * \param mode Set the follow mode to loop repeatedly or to limit to the times added to the trajectory (see OpenFrames::TrajectoryFollower::FollowMode).
 */
OF_EXPORT void OF_FCN(offrame_followtype)(int *data, int *mode);

/*
 * \brief Set the elements to follow position.
 *
 * Each of src, element, opt, and scale must be 3-element arrays, with one element for each x/y/z source.
 *
 * The function offrame_followtrajectory() must be called before this function.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param src     Set data source for each axis (see OpenFrames::Trajectory::SourceType).
 * \param element Set which element to follow.
 * \param opt     Set which optional to follow.
 * \param scale   Set the scale for each axis.
 */
OF_EXPORT void OF_FCN(offrame_followposition)(int src[], unsigned int element[],
                                           unsigned int opt[], double scale[]);

/*
 * \brief DEPRECATED Change how this frame follows a trajectory.
 *
 * This applies to the current active ReferenceFrame.
 *
 * \param affectChildren   True if the children of this frame are also affected by this function, if false, only this frame is affected.
 * \param reset            UNUSED Set to true to reset the trajectory to its initial time.
 * \param changePauseState True if this function call is to set the pauseState, false to ignore the provided value of pauseState.
 * \param pauseState       Indicate if the playback is to be paused.
 * \param changeOffsetTime True if this function call is to set the offsetTime, false to ignore the provided value of offsetTime.
 * \param offsetTime       Shift the playback time by this offset value.
 * \param changeTimeScale  True if this function call is to set the timeScale, false to ignore the provided value of timeScale.
 * \param timeScale        Set the timescale for playback.
 */
OF_EXPORT void OF_FCN(offrame_managetime)(bool *affectChildren, bool *reset,
                                       bool *changePauseState, bool *pauseState,
                                       bool *changeOffsetTime, double *offsetTime,
                                       bool *changeTimeScale, double *timeScale);

/*
 * \brief Print (to std::out) a formatted string of the current ReferenceFrame's descendant hierarchy.
 *
 * This applies to the current active ReferenceFrame.
 */
OF_EXPORT void OF_FCN(offrame_printframestring)();

/******************************************************************
	Sphere Functions
A Sphere is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to a Sphere. In addition, to operate on a Sphere
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

/*
 * \brief Create a new Sphere with the given name.
 *
 * This new Sphere will also become the current active one.
 *
 * \param name Name of the sphere to create.
 */
OF_EXPORT void OF_FCN(ofsphere_create)(OF_CHARARG(name));

/*
 * \brief Set the radius of the sphere.
 *
 * This applies to the current active Sphere.
 *
 * \param radius Radius of the sphere.
 */
OF_EXPORT void OF_FCN(ofsphere_setradius)(double *radius);

/*
 * \brief Set the image file used as the texture map for the sphere.
 *
 * See the OpenSceneGraph documentation for supported file types.
 *
 * This applies to the current active Sphere.
 *
 * \param fname File containing the texture map.
 */
OF_EXPORT void OF_FCN(ofsphere_settexturemap)(OF_CHARARG(fname));

/*
 * \brief Set the image file used as the night texture map for the sphere.
 *
 * See the OpenSceneGraph documentation for supported file types.
 *
 * This applies to the current active Sphere.
 *
 * \param fname File containing the night texture map.
 */
OF_EXPORT void OF_FCN(ofsphere_setnighttexturemap)(OF_CHARARG(fname));

/*
 * \brief Enable/disable auto level of detailing for the sphere.
 *
 * This applies to the current active Sphere.
 *
 * \param lod True to enable auto level of detailing, false to disable.
 */
OF_EXPORT void OF_FCN(ofsphere_setautolod)(bool *lod);

/*
 * \brief Set position of the sphere (within its own reference frame).
 *
 * This applies to the current active Sphere.
 *
 * \param x X position.
 * \param y Y position.
 * \param z Z position.
 */
OF_EXPORT void OF_FCN(ofsphere_setsphereposition)(double *x, double *y, double *z);

/*
 * \brief Set attitude of the sphere (within its own reference frame).
 *
 * This applies to the current active Sphere.
 *
 * \param rx    X component of the rotation quaternion.
 * \param ry    Y component of the rotation quaternion.
 * \param rz    Z component of the rotation quaternion.
 * \param angle Angle component of the rotation quaternion.
 */
OF_EXPORT void OF_FCN(ofsphere_setsphereattitude)(double *rx, double *ry, double *rz, double *angle);

/*
 * \brief Set the scale of the sphere (to turn sphere into ellipsoid).
 * This applies to the current active Sphere, and can be used to turn a Sphere into an ellipsoid.
 *
 * \param sx X scale.
 * \param sy Y scale.
 * \param sz Z scale.
 */
OF_EXPORT void OF_FCN(ofsphere_setspherescale)(double *sx, double *sy, double *sz);

/*
 * \brief Set material parameters for the sphere.
 *
 * This applies to the current active Sphere.
 *
 * \param r         Red component of reflectivity for given component.
 * \param g         Green component of reflectivity for given component.
 * \param b         Blue component of reflectivity for given component.
 * \param shininess Specular shininess for given component.
 */
OF_EXPORT void OF_FCN(ofsphere_setmaterialambient)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofsphere_setmaterialdiffuse)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofsphere_setmaterialspecular)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofsphere_setmaterialemission)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofsphere_setmaterialshininess)(float *shininess);

/******************************************************************
	Model Functions
A Model displays a 3D model (specified in an external file) in the scene.
See the OpenSceneGraph documentation for allowable model file formats.
A Model is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to it. In addition, to operate on a Model
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

/*
* \brief Create a new Model with the given name.
*
* This new Model will also become the current active one.
*
* \param name Name of the model to create.
**/
OF_EXPORT void OF_FCN(ofmodel_create)(OF_CHARARG(name));

/*
 * \brief Set the 3D model to be displayed.
 *
 * See the OpenSceneGraph documentation for supported model types.
 *
 * This applies to the current active Model.
 *
 * \param fname File containing the 3D model.
 */
OF_EXPORT void OF_FCN(ofmodel_setmodel)(OF_CHARARG(fname));

/*
 * \brief Set the position wrt the local origin of the current model.
 *
 * This applies to the current active Model.
 *
 * \param x X position.
 * \param y Y position.
 * \param z Z position.
 */
OF_EXPORT void OF_FCN(ofmodel_setmodelposition)(double *x, double *y, double *z);

/*
 * \brief Get the position wrt the local origin of the current model.
 *
 * This applies to the current active Model.
 *
 * \param x Returned X position.
 * \param y Returned Y position.
 * \param z Returned Z position.
 */
OF_EXPORT void OF_FCN(ofmodel_getmodelposition)(double *x, double *y, double *z);

/*
 * \brief Set the scale wrt the local origin of the current model.
 *
 * This applies to the current active Model.
 *
 * \param sx Scale along X axis.
 * \param sy Scale along Y axis.
 * \param sz Scale along Z axis.
 */
OF_EXPORT void OF_FCN(ofmodel_setmodelscale)(double *sx, double *sy, double *sz);

/*
 * \brief Get the scale wrt the local origin of the current model.
 *
 * This applies to the current active Model.
 *
 * \param sx Returned scale along X axis.
 * \param sy Returned scale along Y axis.
 * \param sz Returned scale along Z axis.
 */
OF_EXPORT void OF_FCN(ofmodel_getmodelscale)(double *sx, double *sy, double *sz);

/*
 * \brief Set the model pivot point wrt the local origin of the current model.
 *
 * This is the point about which all rotations and scales take place.
 *
 * This applies to the current active Model.
 *
 * \param px X position of pivot point.
 * \param py Y position of pivot point.
 * \param pz Z position of pivot point.
 */
OF_EXPORT void OF_FCN(ofmodel_setmodelpivot)(double *px, double *py, double *pz);

/*
 * \brief Get the position wrt the local origin of the current model.
 *
 * This is the point about which all rotations and scales take place.
 *
 * This applies to the current active Model.
 *
 * \param px Returned X position of pivot point.
 * \param py Returned Y position of pivot point.
 * \param pz Returned Z position of pivot point.
 */
OF_EXPORT void OF_FCN(ofmodel_getmodelpivot)(double *px, double *py, double *pz);

/*
 * \brief Get the size of the model.
 *
 * This is the radius of the model's bounding sphere.
 *
 * This applies to the current active Model.
 *
 * \param size Returned size of the model.
 */
OF_EXPORT void OF_FCN(ofmodel_getmodelsize)(double *size);

/******************************************************************
	DrawableTrajectory Functions
A DrawableTrajectory allows a TrajectoryArtist to do its drawing.
A DrawableTrajectory is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to it. In addition, to operate on a DrawableTrajectory
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

/*
 * \brief Create a new DrawableTrajectory with the given name.
 *
 * This new DrawableTrajectory will also become the current active one.
 *
 * \param name Name of the drawable trajectory to create.
 */
OF_EXPORT void OF_FCN(ofdrawtraj_create)(OF_CHARARG(name));

/*
 * \brief Allow specified TrajectoryArtist to draw using this DrawableTrajectory.
 *
 * Note that the currently active TrajectoryArtist will NOT be changed.
 *
 * This applies to the current active DrawableTrajectory.
 *
 * \param name Name of the trajectory artist to draw this DrawableTrajectory.
 */
OF_EXPORT void OF_FCN(ofdrawtraj_addartist)(OF_CHARARG(name));

/*
 * \brief Remove specified artist from the current DrawableTrajectory.
 *
 * Note that the currently active TrajectoryArtist will NOT be changed.
 *
 * This applies to the current active DrawableTrajectory.
 *
 * \param name Name of the trajectory artist to be removed from this DrawableTrajectory.
 */
OF_EXPORT void OF_FCN(ofdrawtraj_removeartist)(OF_CHARARG(name));

/*
 * \brief Remove all artists from the current DrawableTrajectory.
 *
 * This applies to the current active DrawableTrajectory.
 */
OF_EXPORT void OF_FCN(ofdrawtraj_removeallartists)();


/******************************************************************
	CoordinateAxes Functions
A CoordinateAxis is a ReferenceFrame that draws x/y/z axes at its origin,
and allows for variably spaced major and minor tick marks.
******************************************************************/

/*
 * \brief Create a new CoordinateAxes with the given name.
 *
 * This new CoordinateAxes will also become the current active one.
 *
 * \param name Name of the coordinate axes to create.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_create)(OF_CHARARG(name));

/*
 * \brief Sets the length of the axis.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param len Axis length.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_setaxislength)(double *len);

/*
 * \brief Sets which axis to draw.
 *
 * To show multiple axis components, sum the enumerations of OpenFrames::ReferenceFrame::AxesType you want to show.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param axes Axis components to show specified by OpenFrames::ReferenceFrame::AxesType, others will be hidden.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_setdrawaxes)(unsigned int *axes);

/*
 * \brief Sets the major and minor tick spacing.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param major Major tick spacing.
 * \param minor Major tick spacing.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_settickspacing)(double *major, double *minor);

/*
 * \brief Sets the major and minor tick size.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param major Major tick size.
 * \param minor Major tick size.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_setticksize)(unsigned int *major, unsigned int *minor);

/*
 * \brief Sets an image to be used for the tick, overriding any existing shader.
 *
 * If an empty string is provided, the tick marker is reset.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param fname File containing the image.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_settickimage)(OF_CHARARG(fname));

/*
 * \brief Set GLSL fragment shader used to draw tick mark, overriding any existing image.
 *
 * If an empty string is provided, the tick marker is reset.
 *
 * This applies to the current active CoordinateAxes.
 *
 * \param fname File containing the shader source.
 */
OF_EXPORT void OF_FCN(ofcoordaxes_settickshader)(OF_CHARARG(fname));

/******************************************************************
	LatLonGrid Functions
A LatLonGrid is a ReferenceFrame that draws a spherical latitude/longitude
grid with specified radius and line spacings.
******************************************************************/

/*
 * \brief Create a new LatLonGrid with the given name.
 *
 * This new LatLonGrid will also become the current active one.
 *
 * \param name Name of the grid to create.
 */
OF_EXPORT void OF_FCN(oflatlongrid_create)(OF_CHARARG(name));

/*
 * \brief Sets the parameters of the LatLonGrid.
 *
 * This applies to the current active LatLonGrid.
 *
 * \param radiusX  Radius of the grid in the X direction.
 * \param radiusY  Radius of the grid in the Y direction.
 * \param radiusZ  Radius of the grid in the Z direction.
 * \param latSpace Spacing between latitude grid lines in radians.
 * \param lonSpace Spacing between longitude grid lines in radians.
 */
OF_EXPORT void OF_FCN(oflatlongrid_setparameters)(double *radiusX,
                                                  double *radiusY,
                                                  double *radiusZ,
                                                  double *latSpace,
                                                  double *lonSpace);

/******************************************************************
	RadialPlane Functions
A RadialPlane is a ReferenceFrame that draws a circular X/Y plane with
specified radius, radial circle distance, and longitude line spacing.
******************************************************************/

/*
 * \brief Create a new RadialPlane with the given name.
 *
 * This new RadialPlane will also become the current active one.
 *
 * \param name Name of the radial plane to create.
 */
OF_EXPORT void OF_FCN(ofradialplane_create)(OF_CHARARG(name));

/*
 * \brief Sets the parameters of the RadialPlane.
 *
 * This applies to the current active RadialPlane.
 *
 * \param radius   Radius of the radial plane.
 * \param radSpace Spacing between radial grid lines in radians.
 * \param lonSpace Spacing between longitude grid lines in radians.
 */
OF_EXPORT void OF_FCN(ofradialplane_setparameters)(double *radius,
                                                   double *radSpace,
                                                   double *lonSpace);

/*
 * \brief Set the plane color of the current radial plane.
 *
 * This applies to the current active RadialPlane.
 *
 * \param r Red color component [0-1].
 * \param g Green color component [0-1].
 * \param b Blue color component [0-1].
 * \param a Alpha (transparancy) component [0-1].
 */
OF_EXPORT void OF_FCN(ofradialplane_setplanecolor)(float *r, float *g,
                                                   float *b, float *a);

/*
 * \brief Set the line color of the current radial plane.
 *
 * This applies to the current active RadialPlane.
 *
 * \param r Red color component [0-1].
 * \param g Green color component [0-1].
 * \param b Blue color component [0-1].
 * \param a Alpha (transparancy) component [0-1].
 */
OF_EXPORT void OF_FCN(ofradialplane_setlinecolor)(float *r, float *g,
                                                  float *b, float *a);

/******************************************************************
	Trajectory Functions
******************************************************************/

/*
 * \brief Set the currently-active trajectory.
 *
 * \param name Name of the Trajectory to activate.
 */
OF_EXPORT void OF_FCN(oftraj_activate)(OF_CHARARG(name));

/*
 * \brief Create a new Trajectory with the given name.
 *
 * This new Trajectory will also become the current active one.
 *
 * This applies to the current active Trajectory.
 *
 * \param name   Name of the trajectory to create.
 * \param dof    Number of degrees of freedom this trajectory has.
 * \param numopt Number of optionals this trajectory has.
 */
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(oftraj_create)(const char *name, unsigned int *dof,
                                     unsigned int *numopt, 
                                     unsigned int namelen); 
#else
OF_EXPORT void OF_FCN(oftraj_create)(OF_CHARARG(name), unsigned int *dof,
                                     unsigned int *numopt);
#endif                                  

/*
 * \brief Change the number of optionals for the currently active Trajectory.
 *
 * Each optional has an x/y/z component added.
 *
 * This applies to the current active Trajectory.
 *
 * \param nopt Number of optional coordinates to set.
 */
OF_EXPORT void OF_FCN(oftraj_setnumoptionals)(unsigned int *nopt);

/*
 * \brief Change the degrees of freedom for the currently-active Trajectory.
 *
 * This applies to the current active Trajectory.
 *
 * \param dof Desired number of degrees of freedom.
 */
OF_EXPORT void OF_FCN(oftraj_setdof)(unsigned int *dof);

/*
 * \brief Add a time to the current trajectory.
 *
 * This applies to the current active Trajectory.
 *
 * Future positions/attitudes/optionals added to this trajectory will correspond to this time until a new call to oftraj_addtime().
 *
 * \param t Time.
 */
OF_EXPORT void OF_FCN(oftraj_addtime)(const double *t);

/*
 * \brief Add a position to the current trajectory.
 *
 * Add a position as long as the new number of positions will not exceed
 *  the current number of times. Note that for 2D trajectories, the z
 * component will be ignored.
 *
 * This position corresponds to the most recent time provided by oftraj_addtime().
 *
 * /warning If multiple positions are added after the previous call to oftraj_addtime(), all but the last position will be overwritten.
 *
 * \param x X position.
 * \param y Y position.
 * \param z Z position.
 */
OF_EXPORT void OF_FCN(oftraj_addposition)(const double *x, const double *y,
                                          const double *z);

/*
 * \brief Add a position to the current trajectory.
 *
 * This applies to the current active Trajectory.
 *
 * This position corresponds to the most recent time provided by oftraj_addtime().
 *
 * \param pos Position array to add (length 3).
 */
OF_EXPORT void OF_FCN(oftraj_addpositionvec)(const double pos[]);

/*
 * \brief Add an attitude to the current trajectory.
 *
 * This applies to the current active Trajectory. It is ignored if the new
 * number of attitudes will exceed the current number of times.
 * The attitude is given as a 4-element quaternion.
 *
 * \param x X component of the rotation quaternion.
 * \param y Y component of the rotation quaternion.
 * \param z Z component of the rotation quaternion.
 * \param w Angle component of the rotation quaternion.
 */
OF_EXPORT void OF_FCN(oftraj_addattitude)(const double *x, const double *y,
                                          const double *z, const double *w);

/*
 * \brief Set the attitude of the current trajectory.
 *
 * This applies to the current active Trajectory.
 *
 * \param att Quaternion array to add (length 4). The vector component of the quaternion precedes the scalar component.
 */
OF_EXPORT void OF_FCN(oftraj_addattitudevec)(const double att[]);

/*
 * \brief Set the optional with the given index, for the most recently-added position.
 *
 * This applies to the current active Trajectory.
 * The index must be in the range [0, num optionals - 1].
 *
 * This optional corresponds to the most recent time provided by oftraj_addtime().
 *
 * \param index index of optional to add values to.
 * \param x     X component of optional.
 * \param y     Y component of optional.
 * \param z     Z component of optional.
 */
OF_EXPORT void OF_FCN(oftraj_setoptional)(unsigned int *index, const double *x,
                                          const double *y, const double *z);

/*
 * \brief Set the optional with the given index, for the most recently added position.
 *
 * This applies to the current active Trajectory.
 * Here the optional is given as a 2 or 3 element vector.
 *
 * This optional corresponds to the most recent time provided by oftraj_addtime().
 *
 * \param index index of optional to add values to.
 * \param opt   Array of values to add to optional (length 3).
 */
OF_EXPORT void OF_FCN(oftraj_setoptionalvec)(unsigned int *index, const double opt[]);

/*
 * \brief Clear all points from the currently active Trajectory.
 *
 * This applies to the current active Trajectory.
 */
OF_EXPORT void OF_FCN(oftraj_clear)();

/*
 * \brief Inform drawable trajectories to redraw this trajectory.
 *
 * This is only necessary to call if oftraj_autoinformartists() has been set to false. By default, the artists are informed every time data is added.
 *
 * This applies to the current active Trajectory and all the TrajectoryArtists that are linked to it.
 */
OF_EXPORT void OF_FCN(oftraj_informartists)();

/*
 * \brief Inform drawable trajecotries to redraw this trajectory.
 *
 * If this function is not called, the default is to automatically inform the artists.
 *
 * If auto-inform is disabled, the artists will still be automatically informed if the data is cleared, or the number of optional coordinates is modified.
 *
 * This applies to the current active Trajectory and all the TrajectoryArtists that are linked to it.
 *
 * \param autoinform True to auto-inform linked artists when data is added to this trajectory.
 *                   False to not inform  artists unless oftraj_autoinformartists() is called.
 */
OF_EXPORT void OF_FCN(oftraj_autoinformartists)(bool *autoinform);

/******************************************************************
	TrajectoryArtist Functions
A TrajectoryArtist graphically interprets the data contained in a
Trajectory. Since it is not a ReferenceFrame, it must be attached
to a DrawableTrajectory before it can be added to a scene. Note that
you cannot create a TrajectoryArtist by itself. You must create one
of its derived types (eg CurveArtist etc...).
******************************************************************/

/*
 * \brief Set the currently active trajectory artist.
 *
 * \param name Name of the TrajectoryArtist to activate.
 */
OF_EXPORT void OF_FCN(oftrajartist_activate)(OF_CHARARG(name));

/*
 * \brief Set the currently-active trajectory artist.
 *
 * \param name Name of the TrajectoryArtist to activate.
 */
OF_EXPORT void OF_FCN(oftrajartist_settrajectory)();

/*****************************************************************
	CurveArtist Functions
A CurveArtist is a type of TrajectoryArtist that allows arbitrary
Trajectory data to be used for plotting (x,y,z) points. Since it
is a type of Trajectory, a CurveArtist has all Trajectory functions
available to it.
*****************************************************************/

/*
 * \brief Create a new CurveArtist with the given name.
 *
 * This new CurveArtist will also become the current active one.
 *
 * \param name Name of the curve artist to create.
 */
OF_EXPORT void OF_FCN(ofcurveartist_create)(OF_CHARARG(name));

/*
 * \brief Set the data used for X coordinates of each point.
 *
 * This applies to the current active CurveArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofcurveartist_setxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

/*
 * \brief Set the data used for Y coordinates of each point.
 *
 * This applies to the current active CurveArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofcurveartist_setydata)(int *src, unsigned int *element,
                                              unsigned int *opt, double *scale);

/*
 * \brief Set the data used for Z coordinates of each point.
 *
 * This applies to the current active CurveArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofcurveartist_setzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

/*
 * \brief Set the color of the current curve artist.
 *
 * This applies to the current active CurveArtist.
 *
 * \param r Red color component [0-1].
 * \param g Green color component [0-1].
 * \param b Blue color component [0-1].
 */
OF_EXPORT void OF_FCN(ofcurveartist_setcolor)(float *r, float *g, float *b);

/*
 * \brief Set the width of the current curve artist.
 *
 * This applies to the current active CurveArtist.
 *
 * \param width Width of the line.
 */
OF_EXPORT void OF_FCN(ofcurveartist_setwidth)(float *width);

/*
* \brief Set the line pattern of the current curve artist.
*
* This applies to the current active CurveArtist.
*
* For valid factors and patterns, see OpenGL::glLineStipple().
*
* \param factor  Specifies scaling factor used to draw the pattern.
* \param pattern 16-bit integer which specifies the line pattern.
*/
OF_EXPORT void OF_FCN(ofcurveartist_setpattern)(int *factor, unsigned short *pattern);

/*****************************************************************
	SegmentArtist Functions
A SegmentArtist is a type of TrajectoryArtist that allows arbitrary
Trajectory data to be used for plotting line segments. Since it
is a type of Trajectory, a SegmentArtist has all Trajectory functions
available to it.
*****************************************************************/

/*
 * \brief Create a new SegmentArtist with the given name.
 *
 * This new SegmentArtist will also become the current active one.
 *
 * \param name Name of the segment artist to create.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_create)(OF_CHARARG(name));

/*
 * \brief Set the data used for starting X coordinate of each segment.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setstartxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

/*
 * \brief Set the data used for starting Y coordinate of each segment.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setstartydata)(int *src, unsigned int *element,
                                                     unsigned int *opt, double *scale);

/*
 * \brief Set the data used for starting Z coordinate of each segment.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setstartzdata)(int *src, unsigned int *element,
                                                     unsigned int *opt, double *scale);

/*
* \brief Set the data used for ending X coordinate of each segment.
*
* This applies to the current active SegmentArtist.
*
* \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
* \param element Array index of the data indicated in src to plot.
* \param opt     Indicate if a position or optional is plotted. 0 is for position,
*                other values indicate the index of the optional to use. Only used if src = POSOPT.
* \param scale   Scale factor to apply to drawn trajectory data.
**/
OF_EXPORT void OF_FCN(ofsegmentartist_setendxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

/*
 * \brief Set the data used for ending Y coordinate of each segment.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setendydata)(int *src, unsigned int *element,
                                                   unsigned int *opt, double *scale);

/*
 * \brief Set the data used for ending Z coordinate of each segment.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setendzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

/*
 * \brief Set the offset between drawn points.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param stride Minimum offset between sucessive drawn points.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setstride)(unsigned int *stride);

// Set line attributes

/*
 * \brief Set the color of the current segment artist.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param r Red color component [0-1].
 * \param g Green color component [0-1].
 * \param b Blue color component [0-1].
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setcolor)(float *r, float *g, float *b);

/*
 * \brief Set the width of the current segment artist.
 *
 * This applies to the current active SegmentArtist.
 *
 * \param width Width of the line.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setwidth)(float *width);

/*
 * \brief Set the line pattern of the current segment artist.
 *
 * For valid factors and patterns, see OpenGL::glLineStipple().
 *
 * This applies to the current active SegmentArtist.
 *
 * \param factor  Specifies scaling factor used to draw the pattern.
 * \param pattern 16-bit integer which specifies the line pattern.
 */
OF_EXPORT void OF_FCN(ofsegmentartist_setpattern)(int *factor, unsigned short *pattern);

/*****************************************************************
	MarkerArtist Functions
A MarkerArtist is a type of TrajectoryArtist that plots markers at the
start/end of a trajectory. The marker style can be customized.
*****************************************************************/

/*
 * \brief Create a new MarkerArtist with the given name.
 *
 * This new MarkerArtist will also become the current active one.
 *
 * \param name Name of the marker artist to create.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_create)(OF_CHARARG(name));

/*
 * \brief Set the data used for X coordinates of each point.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setxdata)(int *src, unsigned int *element,
                                               unsigned int *opt, double *scale);

/*
 * \brief Set the data used for Y coordinates of each point.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setydata)(int *src, unsigned int *element,
                                               unsigned int *opt, double *scale);

/*
 * \brief Set the data used for Z coordinates of each point.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param src     Type of data source to draw (see OpenFrames::Trajectory::SourceType enum).
 * \param element Array index of the data indicated in src to plot.
 * \param opt     Indicate if a position or optional is plotted. 0 is for position,
 *                other values indicate the index of the optional to use. Only used if src = POSOPT.
 * \param scale   Scale factor to apply to drawn trajectory data.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setzdata)(int *src, unsigned int *element,
                                               unsigned int *opt, double *scale);

/*
 * \brief Define which markers should be plotted for the current marker artist.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param markers Indicates which data points should be drawn as markers (see: OpenFrames::MarkerArtist::DrawnMarkers enum).
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkers)( unsigned int *markers );

/*
 * \brief Set the color of the current marker artist.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param markers The markers whose color should be set (see: OpenFrames::MarkerArtist::DrawnMarkers enum).
 * \param r       Red color component [0-1].
 * \param g       Green color component [0-1].
 * \param b       Blue color component [0-1].
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkercolor)( unsigned int *markers, float *r, float *g, float *b );

/*
 * \brief Set image used as marker, overriding any existing shader.
 *
 * If an empty string is given, then use default circular point.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param fname File containing the image.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkerimage)(OF_CHARARG(fname));

/*
 * \brief Set GLSL fragment shader used to draw marker, overriding any existing image.
 *
 * If an empty string is given, then use default circular point.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param fname File containing the shader source.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkershader)(OF_CHARARG(fname));

/*
 * \brief Specify which type of intermediate markers should be drawn.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param type Indicates how intermediate marker spacing is determined (see: OpenFrames::MarkerArtist::IntermediateType enum).
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatetype)( unsigned int *type );

/*
 * \brief Specify the spacing used for intermediate markers.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param spacing Set spacing for intermediate markers.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatespacing)( double *spacing );

/*
 * \brief Specify the drawing direction (from start or end) of intermediate markers.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param direction Set intermediate marker direction (see: OpenFrames::MarkerArtist::DrawnMarkers enum).
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatedirection)( unsigned int *direction );

/*
 * \brief Specify the marker size in pixels.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param size The marker size.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkersize)( unsigned int *size );

/*
 * \brief Specify whether marker size should be automatically attenuated.
 *
 * This applies to the current active MarkerArtist.
 *
 * \param autoattenuate True to automatically attenuate marker size, False otherwise.
 */
OF_EXPORT void OF_FCN(ofmarkerartist_setautoattenuate)( bool *autoattenuate );

/*****************************************************************
	View Functions
A View represents the "camera" that looks at a scene. It controls the
projection (like the lens of the camera) and modelview (like the position
of the camera) matrices. Views are added to grid positions in a WindowProxy,
and multiple Views are allowed for each grid position.
*****************************************************************/

/*
 * \brief Set the currently active view.
 *
 * \param name Name of the View to activate.
 */
OF_EXPORT void OF_FCN(ofview_activate)(OF_CHARARG(name));

/*
 * \brief Create a new View with the given name.
 *
 * This new View will also become the current active one.
 *
 * \param name Name of the view to create
 */
OF_EXPORT void OF_FCN(ofview_create)(OF_CHARARG(name));

/*
 * \brief Set an orthographic projection with the given bounds.
 *
 * This applies to the current active View.
 *
 * \param left   Left coordinate bound of orthographic projection.
 * \param right  Right coordinate bound of orthographic projection.
 * \param bottom Bottom coordinate bound of orthographic projection.
 * \param top    Top coordinate bound of orthographic projection.
 */
OF_EXPORT void OF_FCN(ofview_setorthographic)(double *left, double *right,
                                              double *bottom, double *top);

/*
 * \brief Set the current view to use a symmetric perspective projection.
 *
 * This applies to the current active View.
 *
 * \param fov  Vertical field of view (in degrees).
 * \param ratio x/y aspect ratio.
 */
OF_EXPORT void OF_FCN(ofview_setperspective)(double *fov, double *ratio);

/*
 * \brief Tell current view to follow the specified ReferenceFrame.
 *
 * The 'root' input should be set to the root of the ReferenceFrame heirarchy, and the
 * 'frame' input should be set to whatever frame you want to view. Note that
 * this function does NOT use or modify the currently active ReferenceFrame.
 *
 * This applies to the current active View.
 *
 * \param root  Name of the root of the ReferenceFrame heirarchy.
 * \param frame ReferenceFrame to follow with this view.
 */
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(ofview_setviewframe)(const char *root, const char *frame, unsigned int rootlen, unsigned int framelen);
#else
OF_EXPORT void OF_FCN(ofview_setviewframe)(OF_CHARARG(root),
                                           OF_CHARARG(frame));
#endif

/*
 * \brief View from one frame towards another, using a specified frame type and rotation type.
 *
 * The 'root' input should be set to the root of the ReferenceFrame
 * heirarchy, and must contain 'srcframe' and 'dstframe'. Note that
 * this function does NOT use or modify the currently active ReferenceFrame.
 *
 * This applies to the current active View.
 *
 * /see OpenFrames::View::ViewFrameType and OpenFrames::View::ViewRotationType.
 *
 * \param root         Name of the root of the ReferenceFrame heirarchy. Note: Must contain 'srcframe' and 'dstframe'.
 * \param srcframe     ReferenceFrame this view will look from.
 * \param dstframe     ReferenceFrame this view will look towards.
 * \param frameType    Frame type to use (see: OpenFrames::View::ViewFrameType enum)
 * \param rotationType Rotation type to use when following dstframe (see: OpenFrames::View::ViewRotationType enum).
 */
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(ofview_setviewbetweenframes)(const char *root, const char *srcframe, const char *dstframe, unsigned int *frameType, unsigned int *rotationType, unsigned int rootlen, unsigned int srcframelen, unsigned int dstframelen);
#else
OF_EXPORT void OF_FCN(ofview_setviewbetweenframes)(OF_CHARARG(root), OF_CHARARG(srcframe), OF_CHARARG(dstframe), unsigned int *frameType, unsigned int *rotationType);
#endif

/*
 * \brief Set the default view distance.
 *
 * This applies to the current active View.
 *
 * \param distance Distance the camera is from the terget point of the reference frame.
 *                 A value <= 0.0 means the distance should be auto-computed.
 */
OF_EXPORT void OF_FCN(ofview_setdefaultviewdistance)(double *distance);

/*
 * \brief Get the trackball's view matrix.
 *
 * This applies to the current active View.
 *
 * \param eye    3-vector eye position.
 * \param center 3-vector look-at position.
 * \param up     3-vector up vector.
 */
OF_EXPORT void OF_FCN(ofview_gettrackball)(double eye[], double center[], double up[]);

/*
 * \brief Set the trackball view matrix.
 *
 * This applies to the current active View.
 *
 * \param eye    3-vector eye position.
 * \param center 3-vector look-at position.
 * \param up     3-vector up vector.
 */
OF_EXPORT void OF_FCN(ofview_settrackball)(double eye[], double center[], double up[]);

/*
 * \brief Check if the view frame for the current View is valid.
 *
 * One reason for an invalid view is if the frame to be viewed is not a child of the specified root frame.
 *
 * This applies to the current active View.
 *
 * \param valid Returned value if the view is valid.
 */
OF_EXPORT void OF_FCN(ofview_isvalid)(bool *valid);

/*
 * \brief Reset the view to its default state.
 *
 * This applies to the current active View.
 */
OF_EXPORT void OF_FCN(ofview_reset)();

#ifdef __cplusplus
}
#endif

#endif
