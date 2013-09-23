!-------------------------------------------
	SUBROUTINE MakeCurrent(id, success)
	USE WINTERACTER
	IMPLICIT NONE
	!DEC$ ATTRIBUTES C,REFERENCE :: MakeCurrent
	integer, intent(in) :: id
	logical(1), intent(out) :: success

	CALL WglSelect(DRAWWIN, 0, WGLDOUBLEBUFFER)
	success = .true.
	
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

	PROGRAM TRIVIAL

	USE WINTERACTER
	USE OpenFrames

	IMPLICIT NONE

	TYPE(WIN_MESSAGE)   :: MESSAGE
	INTEGER		    :: ITYPE
	INTEGER    :: IWIDTH, IHEIGHT
	REAL		    :: MX, MY
	DOUBLE PRECISION :: X, Y, Z, T
	DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846
	INTEGER :: src(3), element(3), opt(3)
	REAL(8) :: scale(3)
	src = (/OF_POSOPT, OF_POSOPT, OF_POSOPT/) ! Get X/Y/Z positions from the POSOPT list
	opt = (/0, 0, 0/) ! Use position group for X/Y/Z position
	element = (/OF_X, OF_Y, OF_Z/) ! Use first/second/third elements from position group for X/Y/Z position
	scale = (/1.0, 1.0, 1.0/) ! Unity scaling for positions

! Initialise Winteracter and OpenFrames
	CALL WInitialise()
	CALL OF_Initialize()

! Create a WindowProxy which will draw onto the window
	CALL OFWin_CreateProxy(0, 0, 800, 600, 1, 1, .TRUE., 0)
	
	CALL OFTraj_Create("Trajectory", 3, 0)
	
	CALL OFCurveArtist_Create("CurveArtist")
	CALL OFTrajArtist_SetTrajectory()
	CALL OFTrajArtist_SetColor(1.0, 0.0, 0.0, 1.0)
	CALL OFCurveArtist_SetXData(OF_POSOPT, OF_X, 0, 1.0)
	CALL OFCurveArtist_SetYData(OF_POSOPT, OF_Y, 0, 1.0)
	CALL OFCurveArtist_SetZData(OF_POSOPT, OF_Z, 0, 1.0)
	
! Create a ReferenceFrame that will follow along the trajectory
	CALL OFFrame_Create("Follower")
	CALL OFFrame_FollowTrajectory(OFFOLLOW_LOOP, src, element, opt, scale)

! Create a DrawableTrajectory that will contain the artists as well as the following frame
	CALL OFDrawTraj_Create("DrawTraj")
	CALL OFDrawTraj_AddArtist("CurveArtist")
	CALL OFFrame_AddChild("Follower")
	
! Pause time at the beginning
	!CALL OFFrame_ManageTime(.true., .false., .true., .true., .true., 0.0d0, .false., 1.0D0)
	
! Create a FrameManager which will handle a ReferenceFrame heirarchy
	CALL OFFM_Create(0)
	CALL OFFM_SetFrame()

	!DEC$ ATTRIBUTES C,REFERENCE :: MakeCurrent
	!DEC$ ATTRIBUTES C,REFERENCE :: SwapBuffers
	CALL OFWin_SetSwapBuffersFunction(SwapBuffers)
	CALL OFWin_SetMakeCurrentFunction(MakeCurrent)
		
	!CALL OFWin_SetBackgroundTexture(0, 0, "../Images/StarMap.tif")

	CALL OFWin_SetScene(0, 0)
	
	! Create a Winteracter window
	IWIDTH = 800
	IHEIGHT = 600
	CALL WindowOpen(X=20, Y=20, WIDTH=IWIDTH, HEIGHT=IHEIGHT, TITLE='Trivial OpenGL Example')
		
! Start animation
	CALL OFWin_Start()

!  Main message loop
	CALL WMessageEnable(MouseButUp,1)
	CALL WMessageEnable(MouseMove, 1)
	DO
	    CALL WMessagePeek(ITYPE, MESSAGE)
	    SELECT CASE (ITYPE)
		CASE (MouseButDown)
			MX = REAL(MESSAGE%X)/9999.0*REAL(IWIDTH)
			MY = REAL(MESSAGE%Y)/9999.0*REAL(IHEIGHT)
		    CALL OFWin_ButtonPress(MX, MY, MESSAGE%VALUE1)
		CASE (MouseButUp)
			MX = REAL(MESSAGE%X)/9999.0*REAL(IWIDTH)
			MY = REAL(MESSAGE%Y)/9999.0*REAL(IHEIGHT)
		    CALL OFWin_ButtonRelease(MX, MY, MESSAGE%VALUE1)
		CASE (MouseMove)
			MX = REAL(MESSAGE%X)/9999.0*REAL(IWIDTH)
			MY = REAL(MESSAGE%Y)/9999.0*REAL(IHEIGHT)
		    CALL OFWin_MouseMotion(MX, MY)
		CASE (KeyDown)
		    IF(MESSAGE%VALUE1 == KeyEscape) THEN
			CALL OFWin_Stop()
			EXIT
		    ELSE
			CALL OFWin_KeyPress(MESSAGE%VALUE1)
		    ENDIF
		CASE (Resize)
		    IWIDTH  = MESSAGE%VALUE1             ! Get new window size
		    IHEIGHT = MESSAGE%VALUE2
		    CALL OFWin_ResizeWindow(0, 0, IWIDTH, IHEIGHT)
		CASE (CloseRequest)
		    CALL OFWin_Stop()
		    EXIT
	    END SELECT
	    	    
	    CALL OFTraj_Clear()
	    
	    T = 0.0D0
	    DO WHILE(T < 2.0D0*PI)
		  X = DSIN(T)
		  Y = DCOS(T)
		  Z = 0.0D0
		  
		  CALL OFTraj_AddTime(T)
		  CALL OFTraj_AddPosition(X, Y, Z)
		  
		  T = T + PI/360.0D0
	    END DO
	    
	END DO
!
!  Terminate Open-GL and Winteracter
!
	CALL WglSelect(0)
	CALL WindowClose()
!
	STOP

	END PROGRAM TRIVIAL