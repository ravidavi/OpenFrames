!  Copyright 2018 Ravishankar Mathur
!
!  Licensed under the Apache License, Version 2.0 (the "License")
!  you may not use this file except in compliance with the License.
!  You may obtain a copy of the License at
!
!      http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software
!  distributed under the License is distributed on an "AS IS" BASIS,
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!  See the License for the specific language governing permissions and
!  limitations under the License.
	
!   This module defines the FORTRAN 90 bindings for the OpenFrames API.
!   Note that lines starting with "!DEC$" are compiler directives for
!   the Visual Fortran family of compilers, and should not be messed with.
	
	MODULE OpenFrames

! Constants used to tell an Artist where to get data for a point.
	INTEGER, PARAMETER :: OF_ZERO = 0
	INTEGER, PARAMETER :: OF_TIME = 1
	INTEGER, PARAMETER :: OF_POSOPT = 2
	INTEGER, PARAMETER :: OF_ATTITUDE = 3

! Constants used to tell an Artist what part of the data to use
! when plotting a point.
	INTEGER, PARAMETER :: OF_X = 0 ! X position or quaternion 1 element
	INTEGER, PARAMETER :: OF_Y = 1 ! Y position or quaternion 2 element
	INTEGER, PARAMETER :: OF_Z = 2 ! Z position or quaternion 3 element
	INTEGER, PARAMETER :: OF_W = 3 ! quaternion 4 element (angle)

! Constants used to tell a MarkerArtist which markers to draw.
	INTEGER, PARAMETER :: OFMA_START = 1 ! Draw start marker
	INTEGER, PARAMETER :: OFMA_INTERMEDIATE = 2 ! Draw intermediate markers
	INTEGER, PARAMETER :: OFMA_END = 4   ! Draw end marker

! Constants used to tell a MarkerArtist which intermediate markers to draw
	INTEGER, PARAMETER :: OFMA_TIME = 1 ! Time increments
	INTEGER, PARAMETER :: OFMA_DISTANCE = 2 ! Distance increments
	INTEGER, PARAMETER :: OFMA_DATA = 3 ! Data point increments

! Constants that determine how a frame following a trajectory handles when
! the current time is out of the trajectory's data bounds
	INTEGER, PARAMETER :: OFFOLLOW_LOOP = 0
	INTEGER, PARAMETER :: OFFOLLOW_LIMIT = 1

! Constants that specify whether a frame follows a trajectory's position,
! attitude, or both
	INTEGER, PARAMETER :: OFFOLLOW_POSITION = 1
	INTEGER, PARAMETER :: OFFOLLOW_ATTITUDE = 2

! Constants that specify which axes to use
	INTEGER, PARAMETER :: OF_NOAXES = 0 ! Don't use any axes
	INTEGER, PARAMETER :: OF_XAXIS = 1 ! Use X axis
	INTEGER, PARAMETER :: OF_YAXIS = 2 ! Use Y axis 
	INTEGER, PARAMETER :: OF_ZAXIS = 4 ! Use Z axis

! Constants that specify relative view base reference frame
	INTEGER, PARAMETER :: OFVIEW_ABSOLUTE = 0 ! Global reference frame
	INTEGER, PARAMETER :: OFVIEW_RELATIVE = 1 ! Body-fixed frame

! Constants that specify relative view rotation between frames
	INTEGER, PARAMETER :: OFVIEW_DIRECT = 0 ! Direct rotation
	INTEGER, PARAMETER :: OFVIEW_AZEL = 1 ! Azimuth-Elevation rotation

	INTERFACE

! Sets up all internal OpenFrames Fortran/C interface variables
! Must be called before other OpenFrames calls

	SUBROUTINE of_initialize()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: of_initialize
	END SUBROUTINE

! Cleans up all internal OpenFrames Fortran/C interface variables
! Must be called when done using OpenFrames
! Afterwards, the only way to continue using OpenFrames is to
! first make another call to of_initialize() 

	SUBROUTINE of_cleanup()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: of_cleanup
	END SUBROUTINE

! Retrieve the result of the last function call

	SUBROUTINE of_getreturnedvalue(val)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: of_getreturnedvalue
	INTEGER, INTENT(OUT) :: val
	END SUBROUTINE

! Add a path to the start of the OSG search path list
  SUBROUTINE of_adddatafilepath(newpath)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: of_adddatafilepath
  CHARACTER(LEN=*), INTENT(IN) :: newpath
  END SUBROUTINE

! WindowProxy functions

	SUBROUTINE ofwin_activate(id)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_activate
	INTEGER, INTENT(IN) :: id
	END SUBROUTINE
	
	SUBROUTINE ofwin_createproxy(x, y, width, height, nrow, ncol, embedded, id, useVR)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_createproxy
	INTEGER, INTENT(IN) :: x, y, width, height, nrow, ncol, id
	LOGICAL, INTENT(IN) :: embedded, useVR
	END SUBROUTINE
	
	SUBROUTINE ofwin_setwindowname(winname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setwindowname
	CHARACTER(LEN=*), INTENT(IN) :: winname
	END SUBROUTINE

	SUBROUTINE ofwin_setgridsize(nrow, ncol)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setgridsize
	INTEGER, INTENT(IN) :: nrow, ncol
	END SUBROUTINE

	SUBROUTINE ofwin_setkeypresscallback(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setkeypresscallback
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_setmousemotioncallback(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setmousemotioncallback
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_setbuttonpresscallback(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setbuttonpresscallback
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_setbuttonreleasecallback(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setbuttonreleasecallback
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_start()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_start
	END SUBROUTINE

	SUBROUTINE ofwin_stop()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_stop
	END SUBROUTINE

	SUBROUTINE ofwin_waitforstop()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_waitforstop
	END SUBROUTINE

	SUBROUTINE ofwin_pauseanimation(pause)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_pauseanimation
	LOGICAL, INTENT(IN) :: pause
	END SUBROUTINE

	SUBROUTINE ofwin_isrunning(state)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_isrunning
	INTEGER, INTENT(OUT) :: state
	END SUBROUTINE

	SUBROUTINE ofwin_setscene(row, col)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setscene
	INTEGER, INTENT(IN) :: row, col
	END SUBROUTINE

  SUBROUTINE ofwin_settime(time)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_settime
  REAL(8), INTENT(IN) :: time
  END SUBROUTINE

  SUBROUTINE ofwin_gettime(time)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_gettime
  REAL(8), INTENT(OUT) :: time
  END SUBROUTINE

  SUBROUTINE ofwin_pausetime(pause)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_pausetime
  LOGICAL, INTENT(IN) :: pause
  END SUBROUTINE

  SUBROUTINE ofwin_istimepaused(ispaused)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_istimepaused
  LOGICAL, INTENT(OUT) :: ispaused
  END SUBROUTINE

  SUBROUTINE ofwin_settimescale(tscale)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_settimescale
  REAL(8), INTENT(IN) :: tscale
  END SUBROUTINE

  SUBROUTINE ofwin_gettimescale(tscale)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_gettimescale
  REAL(8), INTENT(OUT) :: tscale
  END SUBROUTINE

  SUBROUTINE ofwin_setlightambient(row, col, r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setlightambient
  INTEGER, INTENT(IN) :: row, col
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofwin_setlightdiffuse(row, col, r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setlightdiffuse
  INTEGER, INTENT(IN) :: row, col
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofwin_setlightspecular(row, col, r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setlightspecular
  INTEGER, INTENT(IN) :: row, col
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofwin_setlightposition(row, col, x, y, z, w)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setlightposition
  INTEGER, INTENT(IN) :: row, col
  REAL, INTENT(IN) :: x, y, z, w
  END SUBROUTINE

	SUBROUTINE ofwin_setstereo(row, col, enable, eyeseparation, width, height, distance)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setstereo
	INTEGER, INTENT(IN) :: row, col
	LOGICAL, INTENT(IN) :: enable
	REAL, INTENT(IN) :: eyeseparation, width, height, distance
	END SUBROUTINE

	SUBROUTINE ofwin_setbackgroundcolor(row, col, r, g, b)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setbackgroundcolor
	INTEGER, INTENT(IN) :: row, col
	REAL, INTENT(IN) :: r, g, b
	END SUBROUTINE

	SUBROUTINE ofwin_setbackgroundtexture(row, col, fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setbackgroundtexture
	INTEGER, INTENT(IN) :: row, col
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofwin_setbackgroundstardata(row, col, minmag, maxmag, fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setbackgroundstardata
	INTEGER, INTENT(IN) :: row, col
        REAL, INTENT(IN) :: minmag, maxmag
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

  SUBROUTINE ofwin_enablehudtext(row, col, enable)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_enablehudtext
  INTEGER, INTENT(IN) :: row, col
  LOGICAL, INTENT(IN) :: enable
  END SUBROUTINE

  SUBROUTINE ofwin_sethudtextfont(row, col, fname)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_sethudtextfont
  INTEGER, INTENT(IN) :: row, col
  CHARACTER(LEN=*), INTENT(IN) :: fname
  END SUBROUTINE

  SUBROUTINE ofwin_sethudtextparameters(row, col, r, g, b, charSize)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_sethudtextparameters
  INTEGER, INTENT(IN) :: row, col
  REAL, INTENT(IN) :: r, g, b, charSize
  END SUBROUTINE

  SUBROUTINE ofwin_sethudtextposition(row, col, x, y, alignment)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_sethudtextposition
  INTEGER, INTENT(IN) :: row, col, alignment
  REAL, INTENT(IN) :: x, y
  END SUBROUTINE

  SUBROUTINE ofwin_sethudtext(row, col, text)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_sethudtext
  INTEGER, INTENT(IN) :: row, col
  CHARACTER(LEN=*), INTENT(IN) :: text
  END SUBROUTINE

	SUBROUTINE ofwin_setdesiredframerate(fps)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setdesiredframerate
	REAL(8), INTENT(IN) :: fps
	END SUBROUTINE

	SUBROUTINE ofwin_addview(row, col)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_addview
	INTEGER, INTENT(IN) :: row, col
	END SUBROUTINE

	SUBROUTINE ofwin_removeview(row, col)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_removeview
	INTEGER, INTENT(IN) :: row, col
	END SUBROUTINE

	SUBROUTINE ofwin_removeallviews(row, col)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_removeallviews
	INTEGER, INTENT(IN) :: row, col
	END SUBROUTINE

	SUBROUTINE ofwin_selectview(row, col)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_selectview
	INTEGER, INTENT(IN) :: row, col
	END SUBROUTINE

	SUBROUTINE ofwin_setswapbuffersfunction(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setswapbuffersfunction
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_setmakecurrentfunction(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setmakecurrentfunction
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_setupdatecontextfunction(fcn)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setupdatecontextfunction
	EXTERNAL :: fcn
	END SUBROUTINE

	SUBROUTINE ofwin_resizewindow(x, y, width, height)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_resizewindow
	INTEGER, INTENT(IN) :: x, y, width, height
	END SUBROUTINE

	SUBROUTINE ofwin_keypress(key)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_keypress
	INTEGER, INTENT(IN) :: key
	END SUBROUTINE

   SUBROUTINE ofwin_keyrelease(key)
   !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_keyrelease
   INTEGER, INTENT(IN) :: key
   END SUBROUTINE

	SUBROUTINE ofwin_buttonpress(x, y, button)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_buttonpress
	REAL, INTENT(IN) :: x, y
	INTEGER, INTENT(IN) :: button
	END SUBROUTINE

	SUBROUTINE ofwin_buttonrelease(x, y, button)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_buttonrelease
	REAL, INTENT(IN) :: x, y
	INTEGER, INTENT(IN) :: button
	END SUBROUTINE

	SUBROUTINE ofwin_mousemotion(x, y)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_mousemotion
	REAL, INTENT(IN) :: x, y
	END SUBROUTINE

  SUBROUTINE ofwin_capturewindow()
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_capturewindow
  END SUBROUTINE

  SUBROUTINE ofwin_setwindowcapturefile(fname, fext)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setwindowcapturefile
  CHARACTER(LEN=*), INTENT(IN) :: fname, fext
  END SUBROUTINE

  SUBROUTINE ofwin_setwindowcapturekey(key)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofwin_setwindowcapturekey
  INTEGER, INTENT(IN) :: key
  END SUBROUTINE

! FrameManager functions

	SUBROUTINE offm_activate(id)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offm_activate
	INTEGER, INTENT(IN) :: id
	END SUBROUTINE

	SUBROUTINE offm_create(id)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offm_create
	INTEGER, INTENT(IN) :: id
	END SUBROUTINE

	SUBROUTINE offm_setframe()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offm_setframe
	END SUBROUTINE

	SUBROUTINE offm_lock()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offm_lock
	END SUBROUTINE
    
	SUBROUTINE offm_unlock()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offm_unlock
	END SUBROUTINE

! ReferenceFrame functions

	SUBROUTINE offrame_activate(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_activate
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_setcolor(r, g, b, a)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setcolor
	REAL, INTENT(IN) :: r, g, b, a
	END SUBROUTINE

	SUBROUTINE offrame_addchild(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_addchild
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_removechild(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_removechild
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_removeallchildren()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_removeallchildren
	END SUBROUTINE

	SUBROUTINE offrame_getnumchildren(numchildren)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_getnumchildren
	INTEGER, INTENT(OUT) :: numchildren
	END SUBROUTINE
		
	SUBROUTINE offrame_setposition(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setposition
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE

	SUBROUTINE offrame_getposition(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_getposition
	REAL(8), INTENT(OUT) :: x, y, z
	END SUBROUTINE

	SUBROUTINE offrame_setattitude(x, y, z, angle)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setattitude
	REAL(8), INTENT(IN) :: x, y, z, angle
	END SUBROUTINE

	SUBROUTINE offrame_getattitude(x, y, z, angle)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_getattitude
	REAL(8), INTENT(OUT) :: x, y, z, angle
	END SUBROUTINE

	SUBROUTINE offrame_showaxes(axes)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_showaxes
	INTEGER, INTENT(IN) :: axes
	END SUBROUTINE

	SUBROUTINE offrame_shownamelabel(namelabel)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_shownamelabel
	LOGICAL, INTENT(IN) :: namelabel
	END SUBROUTINE

	SUBROUTINE offrame_showaxeslabels(labels)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_showaxeslabels
	INTEGER, INTENT(IN) :: labels
	END SUBROUTINE

	SUBROUTINE offrame_setnamelabel(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setnamelabel
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_setaxeslabels(xlabel, ylabel, zlabel)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setaxeslabels
	CHARACTER(LEN=*), INTENT(IN) :: xlabel, ylabel, zlabel
	END SUBROUTINE

	SUBROUTINE offrame_movexaxis(pos, length, headRatio, bodyRadius, headRadius)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_movexaxis
	REAL(8), INTENT(IN) :: pos(3), length, headRatio, bodyRadius, headRadius
	END SUBROUTINE

	SUBROUTINE offrame_moveyaxis(pos, length, headRatio, bodyRadius, headRadius)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_moveyaxis
	REAL(8), INTENT(IN) :: pos(3), length, headRatio, bodyRadius, headRadius
	END SUBROUTINE

	SUBROUTINE offrame_movezaxis(pos, length, headRatio, bodyRadius, headRadius)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_movezaxis
	REAL(8), INTENT(IN) :: pos(3), length, headRatio, bodyRadius, headRadius
	END SUBROUTINE

  SUBROUTINE offrame_setlightsourceenabled(enabled)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setlightsourceenabled
  LOGICAL, INTENT(IN) :: enabled
  END SUBROUTINE

  SUBROUTINE offrame_getlightsourceenabled(enabled)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_getlightsourceenabled
  LOGICAL, INTENT(OUT) :: enabled
  END SUBROUTINE

  SUBROUTINE offrame_setlightambient(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setlightambient
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE offrame_setlightdiffuse(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setlightdiffuse
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE offrame_setlightspecular(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_setlightspecular
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

	SUBROUTINE offrame_followtrajectory(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_followtrajectory
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE offrame_followtype(data, mode)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_followtype
	INTEGER, INTENT(IN) :: data, mode
	END SUBROUTINE

	SUBROUTINE offrame_followposition(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_followposition
	INTEGER, INTENT(IN) :: src(3), element(3), opt(3)
	REAL(8), INTENT(IN) :: scale(3)
	END SUBROUTINE

  ! offrame_managetime DEPRECATED: use ofwin_*time functions instead
	SUBROUTINE offrame_managetime(affectChildren, reset, &
	                              changePauseState, pauseState, &
	                              changeOffsetTime, offsetTime, &
				      changeTimeScale, timeScale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_managetime
	LOGICAL, INTENT(IN) :: affectChildren, reset, changePauseState, pauseState, &
	                       changeOffsetTime, changeTimeScale
	REAL(8), INTENT(IN) :: offsetTime, timeScale
	END SUBROUTINE
				   	                              
	SUBROUTINE offrame_printframestring()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: offrame_printframestring
	END SUBROUTINE

! Sphere functions
! A Sphere is a type of ReferenceFrame, so all the above ReferenceFrame
! functions also apply to a Sphere.  In addition, to operate on a Sphere
! you must first set it as the currently active ReferenceFrame by using
! offrame_activate() (just like for any other ReferenceFrame).

	SUBROUTINE ofsphere_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofsphere_setradius(radius)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setradius
	REAL(8), INTENT(IN) :: radius
	END SUBROUTINE

	SUBROUTINE ofsphere_settexturemap(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_settexturemap
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofsphere_setnighttexturemap(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setnighttexturemap
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofsphere_setautolod(lod)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setautolod
	LOGICAL, INTENT(IN) :: lod
	END SUBROUTINE

  SUBROUTINE ofsphere_setsphereposition(x, y, z)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setsphereposition
  REAL(8), INTENT(IN) :: x, y, z
  END SUBROUTINE

  SUBROUTINE ofsphere_setsphereattitude(rx, ry, rz, angle)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setsphereattitude
  REAL(8), INTENT(IN) :: rx, ry, rz, angle
  END SUBROUTINE

  SUBROUTINE ofsphere_setspherescale(sx, sy, sz)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setspherescale
  REAL(8), INTENT(IN) :: sx, sy, sz
  END SUBROUTINE

  SUBROUTINE ofsphere_setmaterialambient(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setmaterialambient
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofsphere_setmaterialdiffuse(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setmaterialdiffuse
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofsphere_setmaterialspecular(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setmaterialspecular
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofsphere_setmaterialemission(r, g, b)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setmaterialemission
  REAL, INTENT(IN) :: r, g, b
  END SUBROUTINE

  SUBROUTINE ofsphere_setmaterialshininess(shininess)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsphere_setmaterialshininess
  REAL, INTENT(IN) :: shininess
  END SUBROUTINE

! Model Functions
! A Model is a type of ReferenceFrame, so all the above ReferenceFrame
! functions also apply to it.  In addition, to operate on a Model
! you must first set it as the currently active ReferenceFrame by using
! offrame_activate() (just like for any other ReferenceFrame).

	SUBROUTINE ofmodel_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofmodel_setmodel(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_setmodel
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE
	
	SUBROUTINE ofmodel_setmodelposition(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_setmodelposition
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE
	
	SUBROUTINE ofmodel_getmodelposition(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_getmodelposition
	REAL(8), INTENT(OUT) :: x, y, z
	END SUBROUTINE
	
	SUBROUTINE ofmodel_setmodelscale(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_setmodelscale
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE
	
	SUBROUTINE ofmodel_getmodelscale(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_getmodelscale
	REAL(8), INTENT(OUT) :: x, y, z
	END SUBROUTINE
	
	SUBROUTINE ofmodel_setmodelpivot(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_setmodelpivot
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE
	
	SUBROUTINE ofmodel_getmodelpivot(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_getmodelpivot
	REAL(8), INTENT(OUT) :: x, y, z
	END SUBROUTINE

	SUBROUTINE ofmodel_getmodelsize(size)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmodel_getmodelsize
	REAL(8), INTENT(OUT) :: size
	END SUBROUTINE

! DrawableTrajectory functions
! A DrawableTrajectory allows a TrajectoryArtist to do its drawing.
! A DrawableTrajectory is a type of ReferenceFrame, so all the above ReferenceFrame
! functions also apply to it.  In addition, to operate on a DrawableTrajectory
! you must first set it as the currently active ReferenceFrame by using
! offrame_activate() (just like for any other ReferenceFrame).

	SUBROUTINE ofdrawtraj_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofdrawtraj_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofdrawtraj_addartist(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofdrawtraj_addartist
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofdrawtraj_removeartist(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofdrawtraj_removeartist
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofdrawtraj_removeallartists()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofdrawtraj_removeallartists
	END SUBROUTINE

! CoordinateAxes functions

	SUBROUTINE ofcoordaxes_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_setaxislength(len)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_setaxislength
	REAL(8), INTENT(IN) :: len
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_setdrawaxes(axes)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_setdrawaxes
	INTEGER, INTENT(IN) :: axes
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_settickspacing(major, minor)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_settickspacing
	REAL(8), INTENT(IN) :: major, minor
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_setticksize(major, minor)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_setticksize
	INTEGER, INTENT(IN) :: major, minor
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_settickimage(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_settickimage
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofcoordaxes_settickshader(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcoordaxes_settickshader
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

! LatLonGrid functions

	SUBROUTINE oflatlongrid_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oflatlongrid_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE oflatlongrid_setparameters(radiusX, radiusY, radiusZ, latSpace, lonSpace)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oflatlongrid_setparameters
	REAL(8), INTENT(IN) :: radiusX, radiusY, radiusZ, latSpace, lonSpace
	END SUBROUTINE

! RadialPlane functions

	SUBROUTINE ofradialplane_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofradialplane_create
	CHARACTER(LEN=*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofradialplane_setparameters(radius, radSpace, lonSpace)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofradialplane_setparameters
	REAL(8), INTENT(IN) :: radius, radSpace, lonSpace
	END SUBROUTINE

	SUBROUTINE ofradialplane_setplanecolor(r, g, b, a)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofradialplane_setplanecolor
	REAL, INTENT(IN) :: r, g, b, a
	END SUBROUTINE

	SUBROUTINE ofradialplane_setlinecolor(r, g, b, a)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofradialplane_setlinecolor
	REAL, INTENT(IN) :: r, g, b, a
	END SUBROUTINE

! Trajectory functions

	SUBROUTINE oftraj_activate(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_activate
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE oftraj_create(name, dof, numopt)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_create
	CHARACTER(*), INTENT(IN) :: name
	INTEGER, INTENT(IN) :: dof, numopt
	END SUBROUTINE

	SUBROUTINE oftraj_setnumoptionals(nopt)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_setnumoptionals
	INTEGER, INTENT(IN) :: nopt
	END SUBROUTINE

	SUBROUTINE oftraj_setdof(dof)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_setdof
	INTEGER, INTENT(IN) :: dof
	END SUBROUTINE

	SUBROUTINE oftraj_addtime(t)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_addtime
	REAL(8), INTENT(IN) :: t
	END SUBROUTINE

	SUBROUTINE oftraj_addposition(x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_addposition
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE

	SUBROUTINE oftraj_addpositionvec(pos)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_addpositionvec
	REAL(8), INTENT(IN) :: pos(*)
	END SUBROUTINE

	SUBROUTINE oftraj_addattitude(x, y, z, w)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_addattitude
	REAL(8), INTENT(IN) :: x, y, z, w
	END SUBROUTINE

	SUBROUTINE oftraj_addattitudevec(att)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_addattitudevec
	REAL(8), INTENT(IN) :: att(4)
	END SUBROUTINE

	SUBROUTINE oftraj_setoptional(index, x, y, z)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_setoptional
	INTEGER, INTENT(IN) :: index
	REAL(8), INTENT(IN) :: x, y, z
	END SUBROUTINE

	SUBROUTINE oftraj_setoptionalvec(index, opt)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_setoptionalvec
	INTEGER, INTENT(IN) :: index
	REAL(8), INTENT(IN) :: opt(*)
	END SUBROUTINE

	SUBROUTINE oftraj_clear()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_clear
	END SUBROUTINE

	SUBROUTINE oftraj_informartists()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_informartists
	END SUBROUTINE

	SUBROUTINE oftraj_autoinformartists(autoinform)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftraj_autoinformartists
	LOGICAL, INTENT(IN) :: autoinform
	END SUBROUTINE

! TrajectoryArtist functions
! A TrajectoryArtist graphically interprets the data contained in a
! Trajectory.  Since it is not a ReferenceFrame, it must be attached
! to a DrawableTrajectory before it can be added to a scene.  Note that
! you cannot create a TrajectoryArtist by itself.  You must create one
! of its derived types (eg CurveArtist etc...).

	SUBROUTINE oftrajartist_activate(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftrajartist_activate
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE oftrajartist_settrajectory()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: oftrajartist_settrajectory
	END SUBROUTINE

! CurveArtist functions

	SUBROUTINE ofcurveartist_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_create
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setxdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setxdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setydata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setydata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setzdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setzdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setcolor(r, g, b)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setcolor
	REAL, INTENT(IN) :: r, g, b
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setwidth(width)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setwidth
	REAL, INTENT(IN) :: width
	END SUBROUTINE

	SUBROUTINE ofcurveartist_setpattern(factor, pattern)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofcurveartist_setpattern
	INTEGER, INTENT(IN) :: factor
	INTEGER(2), INTENT(IN) :: pattern
	END SUBROUTINE

! SegmentArtist functions

	SUBROUTINE ofsegmentartist_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_create
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setstartxdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setstartxdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setstartydata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setstartydata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setstartzdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setstartzdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setendxdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setendxdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setendydata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setendydata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setendzdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setendzdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setstride(stride)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setstride
	INTEGER, INTENT(IN) :: stride
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setcolor(r, g, b)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setcolor
	REAL, INTENT(IN) :: r, g, b
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setwidth(width)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setwidth
	REAL, INTENT(IN) :: width
	END SUBROUTINE

	SUBROUTINE ofsegmentartist_setpattern(factor, pattern)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofsegmentartist_setpattern
	INTEGER, INTENT(IN) :: factor
	INTEGER(2), INTENT(IN) :: pattern
	END SUBROUTINE

! MarkerArtist functions

	SUBROUTINE ofmarkerartist_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_create
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setxdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setxdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setydata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setydata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setzdata(src, element, opt, scale)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setzdata
	INTEGER, INTENT(IN) :: src, element, opt
	REAL(8), INTENT(IN) :: scale
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setmarkers(markers)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setmarkers
	INTEGER, INTENT(IN) :: markers
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setmarkercolor(markers, r, g, b)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setmarkercolor
	INTEGER, INTENT(IN) :: markers
	REAL, INTENT(IN) :: r, g, b
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setmarkerimage(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setmarkerimage
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setmarkershader(fname)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setmarkershader
	CHARACTER(LEN=*), INTENT(IN) :: fname
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setintermediatetype(type)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setintermediatetype
	INTEGER, INTENT(IN) :: type
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setintermediatespacing(spacing)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setintermediatespacing
	REAL(8), INTENT(IN) :: spacing
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setintermediatedirection(direction)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setintermediatedirection
	INTEGER, INTENT(IN) :: direction
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setmarkersize(size)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setmarkersize
	INTEGER, INTENT(IN) :: size
	END SUBROUTINE

	SUBROUTINE ofmarkerartist_setautoattenuate(autoattenuate)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofmarkerartist_setautoattenuate
	LOGICAL, INTENT(IN) :: autoattenuate
	END SUBROUTINE

! View functions

	SUBROUTINE ofview_activate(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_activate
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofview_create(name)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_create
	CHARACTER(*), INTENT(IN) :: name
	END SUBROUTINE

	SUBROUTINE ofview_setorthographic(left, right, bottom, top)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_setorthographic
	REAL(8), INTENT(IN) :: left, right, bottom, top
	END SUBROUTINE

	SUBROUTINE ofview_setperspective(fov, ratio)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_setperspective
	REAL(8), INTENT(IN) :: fov, ratio
	END SUBROUTINE

	SUBROUTINE ofview_setviewframe(root, frame)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_setviewframe
	CHARACTER(*), INTENT(IN) :: root, frame
	END SUBROUTINE

	SUBROUTINE ofview_setviewbetweenframes(root, srcframe, dstframe, frameType, rotationType)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_setviewbetweenframes
	CHARACTER(*), INTENT(IN) :: root, srcframe, dstframe
	INTEGER, INTENT(IN) :: frameType, rotationType
	END SUBROUTINE

	SUBROUTINE ofview_setdefaultviewdistance(distance)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_setdefaultviewdistance
	REAL(8), INTENT(IN) :: distance
	END SUBROUTINE

  SUBROUTINE ofview_gettrackball(eye, center, up)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_gettrackball
  REAL(8), INTENT(OUT) :: eye(3), center(3), up(3)
  END SUBROUTINE

  SUBROUTINE ofview_settrackball(eye, center, up)
  !DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_settrackball
  REAL(8), INTENT(IN) :: eye(3), center(3), up(3)
  END SUBROUTINE

	SUBROUTINE ofview_isvalid(valid)
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_isvalid
	LOGICAL, INTENT(OUT) :: valid
	END SUBROUTINE

	SUBROUTINE ofview_reset()
	!DEC$ ATTRIBUTES DLLIMPORT,C,REFERENCE :: ofview_reset
	END SUBROUTINE

	END INTERFACE

	END MODULE ! OpenFrames
