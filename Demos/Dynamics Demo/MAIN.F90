!------------------------------------------------------------------------------------------------------------------
	PROGRAM OPENGL_GRAPHICS_EXAMPLE
!------------------------------------------------------------------------------------------------------------------

	USE WINTERACTER
	USE OPENFRAMES
	USE DYNAMIC_VARIABLES
	USE RESOURCE

	IMPLICIT NONE

	TYPE(WIN_MESSAGE) :: MESSAGE
	INTEGER           :: ITYPE
	INTEGER		  :: IWIDTH, IHEIGHT
	REAL		  :: MX, MY

! Initialise Winteracter and OpenFrames
	CALL WInitialise()
	CALL OF_Initialize()

! Create the central and orbiting bodies
	CALL OFSphere_Create("CB")	
	CALL OFFrame_SetColor(0, 0, 1, 1)
	CALL OFSphere_SetTextureMap("../Images/EarthTexture.bmp");

	CALL OFSphere_Create("Orbiter")
	CALL OFSphere_SetTextureMap("../Images/MoonTexture.bmp")
	CALL OFFrame_SetColor(1, 1, 1, 1)
	CALL OFSphere_SetRadius(0.1);

! Create the ReferenceFrame heirarchy
	CALL OFFrame_Activate("CB")
	CALL OFFrame_AddChild("Orbiter") ! Orbiter's position is now given wrt CB's origin

! Create a FrameManager to handle access to the ReferenceFrame heirarchy
	CALL OFFM_Create(0)
	CALL OFFM_SetFrame()

! Create a window and retrieve its width & height
	CALL WindowOpen(FLAGS=SYSMENUON+MINBUTTON+MAXBUTTON+MAXWINDOW, MENUID=IDR_MENU1,TITLE='OPENGL Graphics Template')
	IWIDTH	= WInfoWindow(WindowWidth)
	IHEIGHT	= WInfoWindow(WindowHeight)

! Create a WindowProxy which will draw onto the window.
! The ID will be 0 since we want to use the main window just created.
	CALL OFWin_CreateProxy(IWIDTH, IHEIGHT, 1, 1, 0)
	CALL OFWin_SetSwapBuffersFunction(SwapBuffers)
	CALL OFWin_SetMakeCurrentFunction(MakeCurrent)

! Set the scene to be drawn
	CALL OFWin_SetScene(0, 0)
	CALL OFFrame_Activate("Orbiter")

! Start the WindowProxy's animations
	CALL OFWin_Start()

! INITIALIZE DYNAMICS
	R_KM(1)		= 20000.0D0
	R_KM(2)		= 0.0D0
	R_KM(3)		= 0.0D0
	V_KMPS(1)	= 0.0D0
	V_KMPS(2)	= 4.0D0
	V_KMPS(3)	= 0.0D0
	CALL RESET_DYNAMICS()

!  Main message loop
	CALL WMessageEnable(MouseButUp,1)
	DO
          CALL WMessagePeek(ITYPE, MESSAGE)
          SELECT CASE (ITYPE)
	    CASE(-1)
	      CALL IDLE()
	    CASE(KeyDown)
	      IF(MESSAGE%VALUE1 == KeyEscape) THEN
		CALL OFWin_Stop()
		EXIT
	      ELSE
		CALL OFWin_KeyPress(MESSAGE%VALUE1)
	      ENDIF
	    CASE(MouseButDown)
	      CALL WINT_TO_OF_POINT(MESSAGE%X, MESSAGE%Y, MX, MY)
	      CALL OFWin_ButtonPress(MX, MY, MESSAGE%VALUE1)
	      CALL WMessageEnable(MouseMove, 1)
	    CASE(MouseButUp)
	      CALL WINT_TO_OF_POINT(MESSAGE%X, MESSAGE%Y, MX, MY)
	      CALL OFWin_ButtonRelease(MX, MY, MESSAGE%VALUE1)
	      CALL WMessageEnable(MouseMove, 0) 
	    CASE(MouseMove)
	      CALL WINT_TO_OF_POINT(MESSAGE%X, MESSAGE%Y, MX, MY)
	      CALL OFWin_MouseMotion(MX, MY)
	    CASE(MenuSelect)
	      IF (MESSAGE%VALUE1 == ID_EXIT) THEN
		CALL OFWin_Stop()
		EXIT
	      ELSE
		CALL ProcessMenu(MESSAGE%VALUE1)
	      END IF
	    CASE(Expose)
	    CASE(Resize)
	      IWIDTH  = MESSAGE%VALUE1	! Get new window size
	      IHEIGHT = MESSAGE%VALUE2
	      CALL OFWin_ResizeWindow(IWIDTH, IHEIGHT)
	    CASE(CloseRequest)
	      CALL OFWin_Stop()
	      EXIT                   ! Exit program (e.g. Alt/F4)
          END SELECT
	END DO

	CALL WglSelect(0)
	CALL WindowClose()                 ! Remove program window

	STOP

	CONTAINS

!	This subroutine converts a point (OldX, OldY) in Winteracter coordinates
!	to a point (NewX, NewY) in coordinates requied by OpenFrames.
!	Winteracter coordinates range from [0, 9999], whereas
!	OpenFrames coordinates range from [-1, 1]

	SUBROUTINE WINT_TO_OF_POINT(OldX, OldY, NewX, NewY)
	implicit none
	integer OldX, OldY
	real NewX, NewY

	NewX = 2.0*OldX/9999.0 - 1.0
	NewY = 2.0*(9999 - OldY)/9999.0 - 1.0

	END SUBROUTINE WINT_TO_OF_POINT

	END PROGRAM OPENGL_GRAPHICS_EXAMPLE

!------------------------------------------------------------------------------------------------------------------
	SUBROUTINE ProcessMenu(IDENT)
!------------------------------------------------------------------------------------------------------------------
!  Process menu selection messages
!
	USE DYNAMIC_VARIABLES
	USE RESOURCE
	USE WINTERACTER
!
	IMPLICIT NONE
	INTEGER, INTENT(IN) :: IDENT
!
	INTEGER             :: IOPT
!
	SELECT CASE (IDENT)
          CASE (ID_LEFT_ROTATE:ID_LEFT_Z)
              DO IOPT = ID_LEFT_ROTATE,ID_LEFT_Z
                  CALL WMenuSetState(IOPT,ItemChecked,WintOff)
              END DO
              CALL WMenuSetState(IDENT,ItemChecked,WintOn)
          CASE (ID_RIGHT_ROTATE:ID_RIGHT_Z)
              DO IOPT = ID_RIGHT_ROTATE,ID_RIGHT_Z
                  CALL WMenuSetState(IOPT,ItemChecked,WintOff)
              END DO
              CALL WMenuSetState(IDENT,ItemChecked,WintOn)
          CASE (ID_CURSOR_ROTATE:ID_CURSOR_Z)
              DO IOPT = ID_CURSOR_ROTATE,ID_CURSOR_Z
                  CALL WMenuSetState(IOPT,ItemChecked,WintOff)
              END DO
              CALL WMenuSetState(IDENT,ItemChecked,WintOn)
          CASE (ID_VIEW_SOLID,ID_VIEW_WIREFRAME,ID_VIEW_POINTS)
              CALL WMenuSetState(ID_VIEW_SOLID,    ItemChecked,WintOff)
              CALL WMenuSetState(ID_VIEW_WIREFRAME,ItemChecked,WintOff)
              CALL WMenuSetState(ID_VIEW_POINTS,   ItemChecked,WintOff)
              CALL WMenuSetState(IDENT,ItemChecked,WintOn)
          CASE (ID_VIEW_AXES)
          CASE (ID_VIEW_RESET)
 !             CALL RESET_TO_INIT()
	  CASE (ID_DYNAMIC_MODEL_PARAMETERS)

			CALL WDIALOGLOAD(IDD_DIALOG002)

			CALL WDialogFieldOptions(IDF_GM,EDITFIELDCHANGED,ENABLED)

			CALL WDialogPutDouble(IDF_GM,GM,'(F20.3)')

			CALL WDIALOGSHOW(IXPOS=100,IYPOS=100,ITYPE=MODAL)

			CALL WDialogGETDouble(IDF_GM,GM)

			CALL RESET_DYNAMICS()

		  CASE (ID_INITIAL_CONDITIONS)

			CALL WDIALOGLOAD(IDD_DIALOG001)

			CALL WDialogFieldOptions(IDF_TIME_T0,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_TIME_TF,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_X_T0	,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_Y_T0	,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_Z_T0	,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_VX_T0	,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_VY_T0	,EDITFIELDCHANGED,ENABLED)
			CALL WDialogFieldOptions(IDF_VZ_T0	,EDITFIELDCHANGED,ENABLED)

			CALL WDialogPutDouble(IDF_TIME_T0,TIME_0,'(F20.3)')
			CALL WDialogPutDouble(IDF_TIME_TF,TIME_F,'(F20.3)')
			CALL WDialogPutDouble(IDF_X_T0,R_KM(1),'(F20.3)')
			CALL WDialogPutDouble(IDF_Y_T0,R_KM(2),'(F20.3)')
			CALL WDialogPutDouble(IDF_Z_T0,R_KM(3),'(F20.3)')
			CALL WDialogPutDouble(IDF_VX_T0,V_KMPS(1),'(F20.6)')
			CALL WDialogPutDouble(IDF_VY_T0,V_KMPS(2),'(F20.6)')
			CALL WDialogPutDouble(IDF_VZ_T0,V_KMPS(3),'(F20.6)')

			CALL WDIALOGSHOW(IXPOS=100,IYPOS=100,ITYPE=MODAL)

			CALL WDialogGETDouble(IDF_TIME_T0,TIME_0)
			CALL WDialogGETDouble(IDF_TIME_TF,TIME_F)
			CALL WDialogGETDouble(IDF_X_T0,R_KM(1))
			CALL WDialogGETDouble(IDF_Y_T0,R_KM(2))
			CALL WDialogGETDouble(IDF_Z_T0,R_KM(3))
			CALL WDialogGETDouble(IDF_VX_T0,V_KMPS(1))
			CALL WDialogGETDouble(IDF_VY_T0,V_KMPS(2))
			CALL WDialogGETDouble(IDF_VZ_T0,V_KMPS(3))

			CALL RESET_DYNAMICS()

		  CASE(ID_RESET_DYNAMICS)
			CALL RESET_DYNAMICS()
      END SELECT
!
      RETURN
      END SUBROUTINE ProcessMenu

!------------------------------------------------------------------------------------------------------------------
      SUBROUTINE RESET_DYNAMICS()
!------------------------------------------------------------------------------------------------------------------
	  USE DYNAMIC_VARIABLES

      IMPLICIT NONE

	  TIME_CURR = TIME_0
	  X_DIM(1:3) = R_KM(1:3)
	  X_DIM(4:6) = V_KMPS(1:3)

      RETURN
      END SUBROUTINE RESET_DYNAMICS
      
!------------------------------------------------------------------------------------------------------------------
	SUBROUTINE IDLE()
!------------------------------------------------------------------------------------------------------------------
	USE DYNAMIC_VARIABLES
	USE OpenFrames

	IMPLICIT NONE

	INTEGER,PARAMETER:: NQ=6,INTEGMETHOD=3,GRAPHSTATE=0,WRITESTATE=0,FILE=0
	DOUBLE PRECISION,PARAMETER::REL_TOL = 1.D-6,ABS_TOL=1.D-6,SIM_T0_DAY=0.0D0,SEG_T0_DAY=0.0D0

	DOUBLE PRECISION	T1,T2

	DOUBLE PRECISION,PARAMETER:: TS_VAL = TIME_DELTA,TS_OUT=TIME_DELTA

	EXTERNAL			DERIVS,SUB_OUTPUT


	IF(TIME_CURR > TIME_F) THEN
		RETURN
	ELSE

		T1 = TIME_CURR
		T2 = T1 + TIME_DELTA

		CALL INTEG(X_DIM,X_DIM,NQ,DERIVS,T1,T2,INTEGMETHOD,TS_VAL,REL_TOL,ABS_TOL,TS_OUT,
     +                 GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

		TIME_CURR = TIME_CURR + TIME_DELTA		!INCREMENT TIME

		R_GU(1:3) = X_DIM(1:3)/MOD2GRAF

		CALL OFFrame_SetPosition(R_GU(1), R_GU(2), R_GU(3))
	ENDIF

	END
!------------------------------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DERIVS(N,T,X,DXDT)
!------------------------------------------------------------------------------------------------------------------
	USE DYNAMIC_VARIABLES	!TO ACCESS GM
	IMPLICIT NONE
	
	INTEGER N
	DOUBLE PRECISION T,X(N),DXDT(N)
	DOUBLE PRECISION R(3),V(3),RMAG

	R(1:3) = X(1:3)
	V(1:3) = X(4:6)
	RMAG   = DSQRT(R(1)**2+R(2)**2+R(3)**2)

	DXDT(1:3) = V(1:3)
	DXDT(4:6) = -GM/(RMAG**3)*R(1:3)
	

	END SUBROUTINE DERIVS

!------------------------------------------------------------------------------------------------------------------
	SUBROUTINE SUB_OUTPUT(T,VEC,GRAPH,WRITE,FILE,SIM_T0_DAY,SEG_T0_DAY,MODE)
!------------------------------------------------------------------------------------------------------------------
	IMPLICIT NONE

	DOUBLE PRECISION T,VEC(*),SIM_T0_DAY,SEG_T0_DAY
	INTEGER			 GRAPH,WRITE,FILE
	CHARACTER(LEN=*) MODE


	RETURN

	END SUBROUTINE SUB_OUTPUT
!------------------------------------------------------------------------------------------------------------------

!-------------------------------------------
	SUBROUTINE MakeCurrent(id)
	USE WINTERACTER
	IMPLICIT NONE
	!DEC$ ATTRIBUTES C,REFERENCE :: MakeCurrent
	integer, intent(in) :: id

	CALL WglSelect(DRAWWIN, id, WGLDOUBLEBUFFER)

	END SUBROUTINE MakeCurrent

!-------------------------------------------
	SUBROUTINE SwapBuffers(id)
	USE WINTERACTER
	IMPLICIT NONE
	!DEC$ ATTRIBUTES C,REFERENCE :: SwapBuffers
	integer, intent(in) :: id

	CALL WglSwapBuffers()

	END SUBROUTINE SwapBuffers

!--------------------------------------------