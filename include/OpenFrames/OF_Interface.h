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

#ifndef _OF_FORTRAN_
#define _OF_FORTRAN_

#include <OpenFrames/Export.h>

// Macro to handle compiler function name mangling
#if defined(GFORTRAN_CALLS)
        // GFortran appends underscores to function names
	#define OF_FCN(name) name##__ 
#else
        // Function names stay as they are on all other interfaces
        // This includes Intel Fortran and C compilers
	#define OF_FCN(name) name 
#endif

// Macros to handle Fortran hidden string length arguments
#if defined(GFORTRAN_CALLS) || defined(IFORT_CALLS)
        // Fortran compilers pass hidden char array lengths
        #define OF_CHARARG(name) const char *name, unsigned int name##len
        #define OF_STRING(name) name, name##len
#else
        #define OF_CHARARG(name) const char *name
        #define OF_STRING(name) name
#endif

// Define shortcuts for callback function signatures
#define BASIC_CALLBACK_SIG unsigned int *winID, unsigned int *row, unsigned int *col
#define KEYPRESS_SIG BASIC_CALLBACK_SIG, int *key
#define MOUSEMOTION_SIG BASIC_CALLBACK_SIG, float *x, float *y
#define BUTTON_SIG MOUSEMOTION_SIG, unsigned int *button

// Make sure this header can be used by "pure" C compilers (e.g. Matlab)
#ifdef __cplusplus
extern "C"
{
#endif

/** All functions have a prefix indicating what they operate on:
 * ofwin: Acts on the currently active WindowProxy
 * offm: Acts on the specified FrameManager.
 * offrame: Acts on the currently active Reference Frame.
 * of(type of ReferenceFrame): Acts on the currently active ReferenceFrame,
 *   assuming that it is of the subtype given by the function name.
 * oftraj: Acts on the currently active Trajectory.
 * oftrajartist: Acts on the currently active TrajectoryArtist.
 * of(type of TrajectoryArtist): Acts on the currently active TrajectoryArtist,
 *  assuming that it is of the subtype given by the function name.
 * ofview: Acts on the currently active View.
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

// Get the values returned by functions.
OF_EXPORT void OF_FCN(of_getreturnedvalue)(int *val);

/***********************************************************************
	Window Functions
***********************************************************************/
// Set the current active WindowProxy
OF_EXPORT void OF_FCN(ofwin_activate)(unsigned int *id);

// Create a new WindowProxy that will manage drawing onto a window.
// This new WindowProxy will also become the current active one.
OF_EXPORT void OF_FCN(ofwin_createproxy)(int *x, int *y,
                                      unsigned int *width, unsigned int *height,
                                      unsigned int *nrow, unsigned int *ncol,
                                      bool *embedded, unsigned int *id);

OF_EXPORT void OF_FCN(ofwin_setgridsize)(int *nrow, int *ncol);

// Set event handling callbacks
OF_EXPORT void OF_FCN(ofwin_setkeypresscallback)(void (*fcn)(KEYPRESS_SIG));
OF_EXPORT void OF_FCN(ofwin_setmousemotioncallback)(void (*fcn)(MOUSEMOTION_SIG));
OF_EXPORT void OF_FCN(ofwin_setbuttonpresscallback)(void (*fcn)(BUTTON_SIG));
OF_EXPORT void OF_FCN(ofwin_setbuttonreleasecallback)(void (*fcn)(BUTTON_SIG));  

// Start/stop animation
OF_EXPORT void OF_FCN(ofwin_start)(); // Start animation
OF_EXPORT void OF_FCN(ofwin_stop)();  // Force animation to stop
OF_EXPORT void OF_FCN(ofwin_waitforstop)(); // Wait for user to exit animation
OF_EXPORT void OF_FCN(ofwin_pauseanimation)(bool *pause);
OF_EXPORT void OF_FCN(ofwin_isrunning)(unsigned int *state);

// Set the scene at the specified grid position
// A scene is specified by the currently active FrameManager
OF_EXPORT void OF_FCN(ofwin_setscene)(unsigned int *row, unsigned int *col);

// Set the 3D stereo mode for the specified grid position
OF_EXPORT void OF_FCN(ofwin_setstereo)(unsigned int *row, unsigned int *col, bool *enable, float *eyeseparation, float *width, float *height, float *distance);

// Set the background color of the specified grid position
OF_EXPORT void OF_FCN(ofwin_setbackgroundcolor)(unsigned int *row, unsigned int *col,
                                             float *r, float *g, float *b);

// Set the background texture of the specified grid position
OF_EXPORT void OF_FCN(ofwin_setbackgroundtexture)(unsigned int *row, unsigned int *col, OF_CHARARG(fname));
                                               
// Set the callback functions for swapping buffers and making a context current
OF_EXPORT void OF_FCN(ofwin_setswapbuffersfunction)(void (*fcn)(unsigned int *winID));
OF_EXPORT void OF_FCN(ofwin_setmakecurrentfunction)(void (*fcn)(unsigned int *winID, bool *success));
OF_EXPORT void OF_FCN(ofwin_setupdatecontextfunction)(void (*fcn)(unsigned int *winID, bool *success));

// Inform the WindowProxy that an event occured
OF_EXPORT void OF_FCN(ofwin_resizewindow)(int *x, int *y, unsigned int *width, unsigned int *height);
OF_EXPORT void OF_FCN(ofwin_keypress)(unsigned int *key);
OF_EXPORT void OF_FCN(ofwin_buttonpress)(float *x, float *y, unsigned int *button);
OF_EXPORT void OF_FCN(ofwin_buttonrelease)(float *x, float *y, unsigned int *button);
OF_EXPORT void OF_FCN(ofwin_mousemotion)(float *x, float *y);

// Set the desired framerate (in frames/second)
OF_EXPORT void OF_FCN(ofwin_setdesiredframerate)(double *fps);

// Control views in any given grid position
OF_EXPORT void OF_FCN(ofwin_addview)(unsigned int *row, unsigned int *col);
OF_EXPORT void OF_FCN(ofwin_removeview)(unsigned int *row, unsigned int *col);
OF_EXPORT void OF_FCN(ofwin_removeallviews)(unsigned int *row, unsigned int *col);
OF_EXPORT void OF_FCN(ofwin_selectview)(unsigned int *row, unsigned int *col);

/******************************************************************
	FrameManger Functions
******************************************************************/

// Set the currently active FrameManager
OF_EXPORT void OF_FCN(offm_activate)(int *id);

// Create a new FrameManager with the given ID
OF_EXPORT void OF_FCN(offm_create)(int *id);

// Assign the currently active ReferenceFrame to the currently active FrameManager
OF_EXPORT void OF_FCN(offm_setframe)();

// Lock the current FrameManager. This should be done before the
// FrameManager's scene is modified.
OF_EXPORT void OF_FCN(offm_lock)();

// Unlock the current FrameManager. This should be done after the
// FrameManager's scene is modified.
OF_EXPORT void OF_FCN(offm_unlock)();

/******************************************************************
	ReferenceFrame Functions
******************************************************************/

// Set the currently active reference frame
OF_EXPORT void OF_FCN(offrame_activate)(OF_CHARARG(name));

// Create a new ReferenceFrame with the given name and activate it
OF_EXPORT void OF_FCN(offrame_create)(OF_CHARARG(name));

// Set the color of the current frame
OF_EXPORT void OF_FCN(offrame_setcolor)(float *r, float *g, float *b, float *a);

// Add/remove the given frame as a child to/from the currently active frame. 
// The currently active frame will remain active.
OF_EXPORT void OF_FCN(offrame_addchild)(OF_CHARARG(name));
OF_EXPORT void OF_FCN(offrame_removechild)(OF_CHARARG(name));

// Remove all children from the currently active frame
OF_EXPORT void OF_FCN(offrame_removeallchildren)();
OF_EXPORT void OF_FCN(offrame_getnumchildren)(int *numchildren);

// Set/get the current frame's position (3 element vector)
OF_EXPORT void OF_FCN(offrame_setposition)(double *x, double *y, double *z);
OF_EXPORT void OF_FCN(offrame_getposition)(double *x, double *y, double *z);

// Set/get the current frame's attitude (4 element quaternion)
OF_EXPORT void OF_FCN(offrame_setattitude)(double *rx, double *ry, double *rz, double *angle);
OF_EXPORT void OF_FCN(offrame_getattitude)(double *rx, double *ry, double *rz, double *angle);

// Show/hide the current frame's coordinate axes or labels
OF_EXPORT void OF_FCN(offrame_showaxes)(unsigned int *axes);
OF_EXPORT void OF_FCN(offrame_shownamelabel)(bool *namelabel);
OF_EXPORT void OF_FCN(offrame_showaxeslabels)(unsigned int *labels);

// Change the name label for the current ReferenceFrame. Note that it will
// still be referred to using the name assigned to it at creation.
OF_EXPORT void OF_FCN(offrame_setnamelabel)(OF_CHARARG(name));

// Change axis labels
// Intel Fortran passes string lengths after all arguments
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

// Reposition axes
OF_EXPORT void OF_FCN(offrame_movexaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);
OF_EXPORT void OF_FCN(offrame_moveyaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);
OF_EXPORT void OF_FCN(offrame_movezaxis)(double pos[], double *length, double *headRatio, double *bodyRadius, double *headRadius);

// Have this frame follow the specified trajectory
OF_EXPORT void OF_FCN(offrame_followtrajectory)(OF_CHARARG(name));

// Follow the trajectory's position, attitude, or both
OF_EXPORT void OF_FCN(offrame_followtype)(int *data, int *mode);

// Set the elements to follow position. Each of src, element, opt, and scale
// must be 3-element arrays, with one element for each x/y/z source.
OF_EXPORT void OF_FCN(offrame_followposition)(int src[], unsigned int element[],
                                           unsigned int opt[], double scale[]);

// Change how this frame follows a trajectory
OF_EXPORT void OF_FCN(offrame_managetime)(bool *affectChildren, bool *reset,
                                       bool *changePauseState, bool *pauseState,
                                       bool *changeOffsetTime, double *offsetTime,
                                       bool *changeTimeScale, double *timeScale);

// Print (to std::out) a formatted string of the current ReferenceFrame's descendant heirarchy.
OF_EXPORT void OF_FCN(offrame_printframestring)();

/******************************************************************
	Sphere Functions
A Sphere is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to a Sphere. In addition, to operate on a Sphere
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

// Create a new Sphere, and make it the currently active ReferenceFrame
OF_EXPORT void OF_FCN(ofsphere_create)(OF_CHARARG(name));

// Set the radius of the sphere
OF_EXPORT void OF_FCN(ofsphere_setradius)(double *radius);
  
// Set the image file used as the texture map for the sphere. See
// the OpenSceneGraph documentation for supported file types.
OF_EXPORT void OF_FCN(ofsphere_settexturemap)(OF_CHARARG(fname));

// Enable/disable auto level of detailing for the sphere.
OF_EXPORT void OF_FCN(ofsphere_setautolod)(bool *lod);

/******************************************************************
	Model Functions
A Model displays a 3D model (specified in an external file) in the scene.
See the OpenSceneGraph documentation for allowable model file formats.
A Model is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to it. In addition, to operate on a Model
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

// Create a new Model, and make it the currently active ReferenceFrame
OF_EXPORT void OF_FCN(ofmodel_create)(OF_CHARARG(name));

// Set the 3D model to be displayed. See the OpenSceneGraph documentation
// for supported model types.
OF_EXPORT void OF_FCN(ofmodel_setmodel)(OF_CHARARG(fname));

// Set/get the model position wrt the local origin
OF_EXPORT void OF_FCN(ofmodel_setmodelposition)(double *x, double *y, double *z);
OF_EXPORT void OF_FCN(ofmodel_getmodelposition)(double *x, double *y, double *z);

// Set/get the model scale wrt the pivot point
OF_EXPORT void OF_FCN(ofmodel_setmodelscale)(double *sx, double *sy, double *sz);
OF_EXPORT void OF_FCN(ofmodel_getmodelscale)(double *sx, double *sy, double *sz);

// Set/get the model pivot point wrt the local origin. This is the point
// about which all rotations and scales take place.
OF_EXPORT void OF_FCN(ofmodel_setmodelpivot)(double *px, double *py, double *pz);
OF_EXPORT void OF_FCN(ofmodel_getmodelpivot)(double *px, double *py, double *pz);

// Get the size of the model. This is the radius of the model's bounding sphere
OF_EXPORT void OF_FCN(ofmodel_getmodelsize)(double *size);

/******************************************************************
	DrawableTrajectory Functions
A DrawableTrajectory allows a TrajectoryArtist to do its drawing.
A DrawableTrajectory is a type of ReferenceFrame, so all the above ReferenceFrame
functions also apply to it. In addition, to operate on a DrawableTrajectory
you must first set it as the currently active ReferenceFrame by using
offrame_activate() (just like for any other ReferenceFrame).
******************************************************************/

// Create a new DrawableTrajectory, and make it the active ReferenceFrame.
OF_EXPORT void OF_FCN(ofdrawtraj_create)(OF_CHARARG(name));

// Allow specified TrajectoryArtist to draw using this DrawableTrajectory.
// Note that the currently active TrajectoryArtist will NOT be changed.
OF_EXPORT void OF_FCN(ofdrawtraj_addartist)(OF_CHARARG(name));

// Remove specified artist from the current DrawableTrajectory
OF_EXPORT void OF_FCN(ofdrawtraj_removeartist)(OF_CHARARG(name));

// Remove all artists from the current DrawableTrajectory
OF_EXPORT void OF_FCN(ofdrawtraj_removeallartists)();


/******************************************************************
	CoordinateAxes Functions
A CoordinateAxis is a ReferenceFrame that draws x/y/z axes at its origin,
and allows for variably spaced major and minor tick marks.
******************************************************************/

// Create a new CoordinateAxes, and make it the active ReferenceFrame.
OF_EXPORT void OF_FCN(ofcoordaxes_create)(OF_CHARARG(name));

OF_EXPORT void OF_FCN(ofcoordaxes_setaxislength)(double *len);
OF_EXPORT void OF_FCN(ofcoordaxes_setdrawaxes)(unsigned int *axes);
OF_EXPORT void OF_FCN(ofcoordaxes_settickspacing)(double *major, double *minor);
OF_EXPORT void OF_FCN(ofcoordaxes_setticksize)(unsigned int *major, unsigned int *minor);
OF_EXPORT void OF_FCN(ofcoordaxes_settickimage)(OF_CHARARG(fname));
OF_EXPORT void OF_FCN(ofcoordaxes_settickshader)(OF_CHARARG(fname));

/******************************************************************
	LatLonGrid Functions
A LatLonGrid is a ReferenceFrame that draws a spherical latitude/longitude
grid with specified radius and line spacings.
******************************************************************/

// Create a new LatLonGrid, and make it the active ReferenceFrame.
OF_EXPORT void OF_FCN(oflatlongrid_create)(OF_CHARARG(name));

OF_EXPORT void OF_FCN(oflatlongrid_setparameters)(double *radius,
                                               double *latSpace,
                                               double *lonSpace);

/******************************************************************
	RadialPlane Functions
A RadialPlane is a ReferenceFrame that draws a circular X/Y plane with
specified radius, radial circle distance, and longitude line spacing.
******************************************************************/

// Creat a new RadialPlane, and make it the active ReferenceFrame
OF_EXPORT void OF_FCN(ofradialplane_create)(OF_CHARARG(name));

OF_EXPORT void OF_FCN(ofradialplane_setparameters)(double *radius,
                                                double *radSpace,
                                                double *lonSpace);

OF_EXPORT void OF_FCN(ofradialplane_setplanecolor)(float *r, float *g,
                                                float *b, float *a);

OF_EXPORT void OF_FCN(ofradialplane_setlinecolor)(float *r, float *g,
                                               float *b, float *a);

/******************************************************************
	Trajectory Functions
******************************************************************/

// Make the specified Trajectory the currently active one
OF_EXPORT void OF_FCN(oftraj_activate)(OF_CHARARG(name));

// Create a new Trajectory, and make it the currently active one
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(oftraj_create)(const char *name, unsigned int *dof,
                                     unsigned int *numopt, 
                                     unsigned int namelen); 
#else
OF_EXPORT void OF_FCN(oftraj_create)(OF_CHARARG(name), unsigned int *dof,
                                     unsigned int *numopt);
#endif                                  
                                  
// Change the number of optionals for the currently active Trajectory.
OF_EXPORT void OF_FCN(oftraj_setnumoptionals)(unsigned int *nopt);

// Change the degrees of freedom for the currently active Trajectory.
OF_EXPORT void OF_FCN(oftraj_setdof)(unsigned int *dof);

// Add a time to the end of the currently active Trajectory.
OF_EXPORT void OF_FCN(oftraj_addtime)(const double *t);

// Add a position as long as the new number of positions will not exceed
// the current number of times. Note that for 2D trajectories, the z
// component will be ignored.
OF_EXPORT void OF_FCN(oftraj_addposition)(const double *x, const double *y,
                                       const double *z);

// Same as above, but the position is given as a 2 or 3 element vector
OF_EXPORT void OF_FCN(oftraj_addpositionvec)(const double pos[]);

// Add an attitude to the currently active Trajectory. Ignored if the new
// number of attitudes will exceed the current number of times.
// The attitude is given as a 4-element quaternion.
OF_EXPORT void OF_FCN(oftraj_addattitude)(const double *x, const double *y,
                                       const double *z, const double *w);

// Same as above, but the attitude is given as a 4 element vector
OF_EXPORT void OF_FCN(oftraj_addattitudevec)(const double att[]);

// Set the optional with the given index, for the most recently added position.
// The index must be in the range [0, num optionals - 1]
OF_EXPORT void OF_FCN(oftraj_setoptional)(unsigned int *index, const double *x,
                                       const double *y, const double *z);

// Same as above, but the optional is given as a 2 or 3 element vector
OF_EXPORT void OF_FCN(oftraj_setoptionalvec)(unsigned int *index, const double opt[]);

// Clear all points from the currently active Trajectory
OF_EXPORT void OF_FCN(oftraj_clear)();

OF_EXPORT void OF_FCN(oftraj_informartists)();
OF_EXPORT void OF_FCN(oftraj_autoinformartists)(bool *autoinform);

/******************************************************************
	TrajectoryArtist Functions
A TrajectoryArtist graphically interprets the data contained in a
Trajectory. Since it is not a ReferenceFrame, it must be attached
to a DrawableTrajectory before it can be added to a scene. Note that
you cannot create a TrajectoryArtist by itself. You must create one
of its derived types (eg CurveArtist etc...).
******************************************************************/

// Make the specified TrajectoryArtist active.
OF_EXPORT void OF_FCN(oftrajartist_activate)(OF_CHARARG(name));

// Tell the active artist to draw the active Trajectory.
OF_EXPORT void OF_FCN(oftrajartist_settrajectory)();

/*****************************************************************
	CurveArtist Functions
A CurveArtist is a type of TrajectoryArtist that allows arbitrary
Trajectory data to be used for plotting (x,y,z) points. Since it
is a type of Trajectory, a CurveArtist has all Trajectory functions
available to it.
*****************************************************************/

// Create a CurveArtist with the specified ID.
OF_EXPORT void OF_FCN(ofcurveartist_create)(OF_CHARARG(name));

// Set the data used for X coordinates of each point
OF_EXPORT void OF_FCN(ofcurveartist_setxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for Y coordinates of each point
OF_EXPORT void OF_FCN(ofcurveartist_setydata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for Z coordinates of each point
OF_EXPORT void OF_FCN(ofcurveartist_setzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set line attributes
OF_EXPORT void OF_FCN(ofcurveartist_setcolor)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofcurveartist_setwidth)(float *width);
OF_EXPORT void OF_FCN(ofcurveartist_setpattern)(int *factor, unsigned short *pattern);

/*****************************************************************
	SegmentArtist Functions
A SegmentArtist is a type of TrajectoryArtist that allows arbitrary
Trajectory data to be used for plotting line segments. Since it
is a type of Trajectory, a SegmentArtist has all Trajectory functions
available to it.
*****************************************************************/

// Create a SegmentArtist with the specified ID.
OF_EXPORT void OF_FCN(ofsegmentartist_create)(OF_CHARARG(name));

// Set the data used for starting X coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setstartxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for starting Y coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setstartydata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for starting Z coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setstartzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for ending X coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setendxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for ending Y coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setendydata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for ending Z coordinate of each segment
OF_EXPORT void OF_FCN(ofsegmentartist_setendzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the offset between drawn points
OF_EXPORT void OF_FCN(ofsegmentartist_setstride)(unsigned int *stride);

// Set line attributes
OF_EXPORT void OF_FCN(ofsegmentartist_setcolor)(float *r, float *g, float *b);
OF_EXPORT void OF_FCN(ofsegmentartist_setwidth)(float *width);
OF_EXPORT void OF_FCN(ofsegmentartist_setpattern)(int *factor, unsigned short *pattern);

/*****************************************************************
	MarkerArtist Functions
A MarkerArtist is a type of TrajectoryArtist that plots markers at the
start/end of a trajectory. The marker style can be customized.
*****************************************************************/

// Create a MarkerArtist with the specified ID
OF_EXPORT void OF_FCN(ofmarkerartist_create)(OF_CHARARG(name));

// Set the data used for X coordinates of each point
OF_EXPORT void OF_FCN(ofmarkerartist_setxdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Set the data used for Y coordinates of each point
OF_EXPORT void OF_FCN(ofmarkerartist_setydata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);
                                           
// Set the data used for Z coordinates of each point
OF_EXPORT void OF_FCN(ofmarkerartist_setzdata)(int *src, unsigned int *element,
                                           unsigned int *opt, double *scale);

// Define which markers should be plotted
// See DrawnMarkers enum in MarkerArtist class
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkers)( unsigned int *markers );

// Set color for markers
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkercolor)( unsigned int *markers, float *r, float *g, float *b );

// Set image used as marker, overriding any existing shader
// If an empty string is given, then use default circular point
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkerimage)(OF_CHARARG(fname));
  
// Set GLSL fragment shader used to draw marker, overriding any existing image
// If an empty string is given, then use default circular point
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkershader)(OF_CHARARG(fname));
  
// Specify which type of intermediate markers should be drawn
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatetype)( unsigned int *type );

// Specify the spacing used for intermediate markers
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatespacing)( double *spacing );

// Specify the drawing direction (from start or end) of intermediate markers
OF_EXPORT void OF_FCN(ofmarkerartist_setintermediatedirection)( unsigned int *direction );

// Specify the marker size in pixels
OF_EXPORT void OF_FCN(ofmarkerartist_setmarkersize)( unsigned int *size );

// Specify whether marker size should be automatically attenuated
OF_EXPORT void OF_FCN(ofmarkerartist_setautoattenuate)( bool *autoattenuate );

/*****************************************************************
	View Functions
A View represents the "camera" that looks at a scene. It controls the
projection (like the lens of the camera) and modelview (like the position
of the camera) matrices. Views are added to grid positions in a WindowProxy,
and multiple Views are allowed for each grid position.
*****************************************************************/

// Activate the specified View
OF_EXPORT void OF_FCN(ofview_activate)(OF_CHARARG(name));

// Create a new View and make it the currently active one
OF_EXPORT void OF_FCN(ofview_create)(OF_CHARARG(name));

// Set the current view to use an orthographic projection with the given bounds.
OF_EXPORT void OF_FCN(ofview_setorthographic)(double *left, double *right,
                                           double *bottom, double *top);

// Set the current view to use a symmetric perspective projection
// with the given vertical field of view (in degrees) and x/y aspect ratio.
OF_EXPORT void OF_FCN(ofview_setperspective)(double *fov, double *ratio);

// Set the constant multiplier that all aspect ratios are multiplied by
// before a perspective view is created. This can be used to stretch/squeeze
// the image by a constant amount.
OF_EXPORT void OF_FCN(ofview_setaspectmultiplier)(double *mult);

// Set the default view that the current transform will show. The 'root'
// input should be set to the root of the ReferenceFrame heirarchy, and the
// 'frame' input should be set to whatever frame you want to view. Note that
// this function does NOT use or modify the currently active ReferenceFrame.
#if defined(IFORT_CALLS)
OF_EXPORT void OF_FCN(ofview_setviewframe)(const char *root, const char *frame, unsigned int rootlen, unsigned int framelen);
#else
OF_EXPORT void OF_FCN(ofview_setviewframe)(OF_CHARARG(root),
                                           OF_CHARARG(frame));
#endif

// Set the default view distance. A value <= 0.0 means the distance should be auto-computed
OF_EXPORT void OF_FCN(ofview_setdefaultviewdistance)(double *distance);

// Check if the view frame for the current View is valid. One reason for an
// invalid view is if the frame to be viewed is not a child of the specified
// root frame.
OF_EXPORT void OF_FCN(ofview_isvalid)(bool *valid);

// Reset the view to its default state
OF_EXPORT void OF_FCN(ofview_reset)();

#ifdef __cplusplus
}
#endif

#endif
