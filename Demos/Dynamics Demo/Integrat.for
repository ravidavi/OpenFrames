!------------------------------------------------------------------------------
!	INTEG: DRIVER SUBROUTINE FOR ALL NUMERICAL INTEGRATORS
!------------------------------------------------------------------------------
      SUBROUTINE INTEG(VEC1,VEC2,NQ,FUNC,T1,T2,INTEGMETHOD,TS_VAL,REL_TOL,ABS_TOL,TS_OUT,
     +                 GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

      IMPLICIT NONE

      INTEGER				INTEGMETHOD,NQ
      INTEGER				GRAPHSTATE,WRITESTATE,FILE
      DOUBLE PRECISION	VEC(NQ),DVEC(NQ)
      DOUBLE PRECISION	T1,T2,VEC1(NQ),VEC2(NQ)
      DOUBLE PRECISION	TS_VAL,REL_TOL,ABS_TOL,TS_OUT
	DOUBLE PRECISION	SIM_T0_DAY,SEG_T0_DAY	!EPOCH TIMES SPECIFIC TO COPERNICUS
      EXTERNAL			FUNC,SUB_OUTPUT
      INTEGER I

!     VARIABLES ASSOCIATED WITH ODEINT-RKQS,ODEINT-BSSTEP----------------------------------------------------------------
      INTEGER NRHS
      DOUBLE PRECISION,PARAMETER::DTSAV=0.0D0,H1=0.01D0,HMIN=0.0D0
      COMMON NRHS
      EXTERNAL RKQS,BSSTEP

!	VARIABLES SPECIFIC TO THE USE OF DLSODE/DVODE FROM ODEPACK---------------------------------------------------------------
	INTEGER				ISTATE								!INOUT IN DLSODE =1 FOR FIRST CALL =2 FOR SUBSEQUENT CALL
															!OUT =1 IF NOTHING WAS DONE TOUT=T 2:SUCCESSFULL
															!IF ISTATE < 0 THEN THESE ARE ERROR COMMENTS (SEE DLSODE DOC)

	INTEGER,PARAMETER::MF=10								!MF = 10 : NONSTIFF ADAMS METHOD; NO JACOBIAN
	INTEGER,PARAMETER::ITOL	=	1							!SIMPLE ERROR CONTROL; REL_TOL,ABS_TOL WILL BE SCALARS; ABS_TOL NEED NOT BE DIMENSIONED
	INTEGER,PARAMETER::IOPT	=	1							!NO OPTIONAL INPUTS WILL BE USED; 0=NO 1=YES
	INTEGER			   ITASK       							!NORMAL COMPUTATION OF OUTPUT VALUES (SEE DLSODE DOC)
	DOUBLE PRECISION			 RPAR						!USER DP ARRAY OR SCALAR THAT CAN BE USED; NEEDED BY DVODE	
	INTEGER						 IPAR						!USER INT ARRAY OR SCALAR THAT CAN BE USED; NEEDED BY DVODE	
															!THE CURRENT SETTING ASSUMES PURE RELATIVE ERROR CONTROL SO ABS_TOL=0 AND REL_TOL IN INPUT
	INTEGER							LRW,LIW					!INTEGER DIMENSIONS FOR RWORK AND IWORK
															!FOR MF 10, LRW = 20+16*NEQ  LIW = 20
	INTEGER,ALLOCATABLE::			IWORK(:)
	DOUBLE PRECISION,ALLOCATABLE::	RWORK(:)
	DOUBLE PRECISION				TS,T,TOUT,T0I
	EXTERNAL						DLSODE_JAC				!DUMMY VARIABLE SINCE JACOBIAN WILL NOT BE USED
															!IF MF = 21 OR 24, JAC MUST EXIST
	EXTERNAL						DVODE_JAC				!DUMMARY JAC ROUTINE FOR DVODE

!	OPTIONAL PARAMETERS
!     DOUBLE PRECISION,PARAMETER::	H0=						!SET IN RWORK(5)   Step size to be attempted on the first step.
															!The default value is determined by the solver.
!     DOUBLE PRECISION,PARAMETER::	HMAX=					!SET IN RWORK(6)   Maximum absolute step size allowed.  The
															!default value is infinite.
!     DOUBLE PRECISION,PARAMETER::	HMIN=					!SET IN RWORK(7)   Minimum absolute step size allowed.  The
															!default value is 0.  (This lower bound is not
															!enforced on the final step before reaching
															!TCRIT when ITASK = 4 or 5.)
!	INTEGER,PARAMETER::				MAXORD=					!SET IN IWORK(5) Maximum order to be allowed.  The default value
															!is 12 if METH = 1, and 5 if METH = 2. (See the
															!MF description above for METH.)  If MAXORD
															!exceeds the default value, it will be reduced
															!to the default value.  If MAXORD is changed
															!during the problem, it may cause the current
															!order to be reduced.
	INTEGER,PARAMETER::				MXSTEP=	50000			!SET IN IWORK(6) Maximum number of (internally defined) steps
															!allowed during one call to the solver.  The
															!default value is 500.
!	INTEGER,PARAMETER::				MXHNIL=	10				!SET IN IWORK(7)Maximum number of messages printed (per
															!problem) warning that T + H = T on a step
															!(H = step size).  This must be positive to
															!result in a nondefault value.  The default
															!value is 10.

!	FOR MF = 10 (NONSTIFF (ADAMS) METHOD, NO JACOBIAN USE:
!	INTEGER,PARAMETER:: MF=10
!	FOR MF = 22 (STIFF METHOD, INTERNALLY GENERATED JACOBIAN USE:
!	INTEGER,PARAMETER:: MF=22

!	END: DLSODE/DVODE SPECIFIC VARIABLES-------------------------------------------------------------------------------------

!	VARIABLES SPECIFIC TO KEPLER BASED PROPAGATOR
	DOUBLE PRECISION COE(1:6),R(3),V(3)

!	VARIABLES ASSOCIATED WITH USING RKSUITE (SEE RKSUITE DOCUMENTATION)
	DOUBLE PRECISION DMACH
	INTEGER			 OUTCH
	DOUBLE PRECISION MCHEPS,DWARF
	INTEGER			 METHOD
	CHARACTER(LEN=1) TASK
	LOGICAL,PARAMETER::	 ERRASS=.FALSE.,MESSAGE=.FALSE.
	DOUBLE PRECISION,PARAMETER:: HSTART=0.0D0
	INTEGER			 LENWRK
	DOUBLE PRECISION,ALLOCATABLE:: WORK(:)
	DOUBLE PRECISION THRES(NQ),TWANT,TGOT,YGOT(NQ),YPGOT(NQ),YMAX(NQ)
	DOUBLE PRECISION TNOW,YNOW(NQ),YPNOW(NQ)
	INTEGER			 UFLAG,CFLAG
!	END: RKSUITE SPECIFIC VARIABLES-------------------------------------------------------------------------------------

	VEC(1:NQ) = VEC1(1:NQ)

	IF(T1==T2) THEN
		VEC2(1:NQ) = VEC1(1:NQ)
		RETURN
	ENDIF

	SELECT CASE(INTEGMETHOD)
	CASE(1)
		CALL RK4(VEC,NQ,FUNC,T1,T2,TS_VAL,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)
	CASE(2)
		CALL RKF(VEC,NQ,FUNC,T1,T2,REL_TOL,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)
	CASE(3)		!ODEINT FROM NUMRICAL RECIPES
		CALL ODEINT(VEC,NQ,T1,T2,REL_TOL,H1,HMIN,DTSAV,FUNC,RKQS,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)
	CASE(4)		!BULIRSCH STOER EXTRAPOLATION METHOD FROM NUMRICAL RECIPES
		CALL ODEINT(VEC,NQ,T1,T2,REL_TOL,H1,HMIN,DTSAV,FUNC,BSSTEP,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

	CASE(6)		!ODEINT WITH FIX STEP SIZE REPORTING
		ISTATE	= 1				!THIS IS THE FIRST CALL TO DLSODE

		LRW		= 20+16*NQ		!VALUE FOR MF = 10 (SEE ODEPACK DOC)
		LIW		= 20			!IF MF = 10 (SEE ODEPACK DOC)

!		THE FOLLOWING STIFF METHOD WILL WORK BETTER WHEN THE JACOBIAN IS AVAILABLE
!		MF		= 22			!STIFF METHOD
!		LRW		= 20+16*NQ+NQ**2!VALUE FOR MF = 10 (SEE ODEPACK DOC)
!		LIW		= 20+NQ			!IF MF = 10 (SEE ODEPACK DOC)

!		CALL SEQUENCE TO DLSODE IS ONE OF TWO WAYS
		IF(GRAPHSTATE==0.AND.WRITESTATE==0) THEN
!			IF NO INTERMEDIATE STEPS ARE NEEDED, AS WHEN EVALUATING PARTIALS USE:
			CALL ODEINT(VEC,NQ,T1,T2,REL_TOL,H1,HMIN,DTSAV,FUNC,RKQS,0,0,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)
		ENDIF
		IF(GRAPHSTATE.NE.0 .OR. WRITESTATE==1) THEN
!			IF INTERMEDIATE STEPS ARE REQUIRED THEN USE DLSODE IN A LOOP.  OPERATION IN THIS MODE SLOWS DOWN PROCESS
			TS = DABS(TS_OUT)
C			CHOOSE THE CORRECT SIGN FOR THE TIME STEP TS
			IF(T1 .LT. T2) TS =  TS
			IF(T1 .GT. T2) TS = -TS
			CALL SUB_OUTPUT(T1,VEC,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'START')
			T0I = T1
			DO T=T1,T2,TS
				IF( DABS(T2-T) .LT. DABS(TS)) TS = T2-T
				TOUT = T0I+TS

!				CALL DLSODE(FUNC,NQ,VEC,T0I,TOUT,ITOL,REL_TOL,ABS_TOL,ITASK,ISTATE,IOPT,RWORK,LRW,IWORK,LIW,JAC,MF)

				CALL ODEINT(VEC,NQ,T,T+TS,REL_TOL,H1,HMIN,DTSAV,FUNC,RKQS,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,
     +						SIM_T0_DAY,SEG_T0_DAY)

				IF(ISTATE.LT.0) THEN
					WRITE(*,*) 'ERROR IN DLSODE: ISTATE=',ISTATE,'  STOP'
					STOP
				ENDIF
				IF(TOUT.EQ.T2) EXIT
			ENDDO
		ENDIF

	CASE DEFAULT

		PAUSE 'SELECT INTEGMETHOD: STOP'
		STOP

	ENDSELECT

	VEC2(1:NQ) = VEC(1:NQ)

      RETURN

      END
C------------------------------------------------------------------------------


C------------------------------------------------------------------------------
	SUBROUTINE DLSODE_JAC(NEQ,T,Y,ML,MU,PD,NROWPD)
C------------------------------------------------------------------------------
!	DUMMY ROUTINE FOR DLSODE/DVODE; NO JACOBIAN IS BEING COMPUTED FOR MF=10
	IMPLICIT NONE
	INTEGER NEQ,ML,MU,NROWPD
	DOUBLE PRECISION T,Y(*),PD(NROWPD,*)
	WRITE(*,*) 'THIS JAC ROUTINE IS EMPTY; MODIFY ACCORDINGLY'
	PAUSE
	END
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
      SUBROUTINE DVODE_JAC (NEQ, T, Y, ML, MU, PD, NROWPD, RPAR, IPAR)
!------------------------------------------------------------------------------
      DOUBLE PRECISION T, Y(NEQ), PD(NROWPD,NEQ), RPAR
	WRITE(*,*) 'THIS DVODE_JAC ROUTINE IS EMPTY; MODIFY ACCORDINGLY'
	PAUSE
	END
!------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C *** INTEGRATOR RUNGE-KUTTA-4TH ORDER
C------------------------------------------------------------------------------
      SUBROUTINE RK4(Y,N,DERIVS,T0,TF,RK4TS1,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER GRAPHSTATE,WRITESTATE,FILE
      PARAMETER (NEQMAX=110)
      DIMENSION Y(NEQMAX),DYDX(NEQMAX),YT(NEQMAX)
      DIMENSION DYT(NEQMAX),DYM(NEQMAX)
      INTEGER LASTP
	DOUBLE PRECISION SIM_T0_DAY,SEG_T0_DAY	!EPOCH TIMES SPECIFIC TO COPERNICUS

      LASTP = 0

      TS = DABS(RK4TS1)

C     CHOOSE THE CORRECT SIGN FOR THE TIME STEP TS
      IF(T0 .LT. TF) TS =  TS
      IF(T0 .GT. TF) TS = -TS

      H = TS

      CALL SUB_OUTPUT(T0,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'START')

      DO T=T0,TF,H

        IF( DABS(TF-T) .LT. DABS(H)) H = TF-T

        CALL DERIVS(N,T,Y,DYDX)

        HH=H*0.5D0
        H6=H/6.0D0
        TH=T+HH
        DO I=1,N
          YT(I) = Y(I) + HH*DYDX(I)
        ENDDO
        CALL DERIVS(N,TH,YT,DYT)
        DO I=1,N
          YT(I) = Y(I) + HH*DYT(I)
        ENDDO
        CALL DERIVS(N,TH,YT,DYM)
        DO I=1,N
          YT(I) = Y(I)   + H*DYM(I)
          DYM(I)= DYT(I) + DYM(I)
        ENDDO
        CALL DERIVS(N,T+H,YT,DYT)
        DO I=1,N
          Y(I)=Y(I)+H6*(DYDX(I)+DYT(I)+2.*DYM(I))
        ENDDO

        CALL SUB_OUTPUT(T+H,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'')

      ENDDO

      RETURN

      END
C------------------------------------------------------------------------------


C------------------------------------------------------------------------------
C ***             INTEGRATOR RUNGE-KUTTA-FEHLBERG 4(5)
C------------------------------------------------------------------------------
      SUBROUTINE RKF(Y,NQ,DERIVS,T1,T2,TOL,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      INTEGER NQ,GRAPHSTATE,WRITESTATE,IDIR,FILE
      DOUBLE PRECISION DABS
      PARAMETER(NEQMAX=110)
      DIMENSION A(6),B(6,5),CH(6),CT(6)
      DIMENSION Y(NQ),DY(NEQMAX),YTEMP(NEQMAX),TE(NEQMAX),F(6,NEQMAX)
      LOGICAL   DONE
	DOUBLE PRECISION SIM_T0_DAY,SEG_T0_DAY	!EPOCH TIMES SPECIFIC TO COPERNICUS

      A(2)    = 2.0D0/9.0D0
      A(3)    = 1.0D0/3.0D0
      A(4)    = 3.0D0/4.0D0
      A(5)    = 1.0D0
      A(6)    = 5.0D0/6.0D0

      B(2,1)  = 2.0D0/9.0D0
      B(3,1)  = 1.0D0/12.0D0
      B(3,2)  = 1.0D0/4.0D0
      B(4,1)  = 69.0D0/128.0D0
      B(4,2)  = -243.0D0/128.0D0
      B(4,3)  = 135.0D0/64.0D0

      B(5,1)  = -17.0D0/12.0D0
      B(5,2)  = 27.0D0/4.0D0
      B(5,3)  = -27.0D0/5.0D0
      B(5,4)  = 16.0D0/15.0D0
      B(6,1)  = 65.0D0/432.0D0
      B(6,2)  = -5.0D0/16.0D0
      B(6,3)  = 13.0D0/16.0D0
      B(6,4)  = 4.0D0/27.0D0
      B(6,5)  = 5.0D0/144.0D0

      CH(1)   = 47.0D0/450.0D0
      CH(2)   = 0.0D0
      CH(3)   = 12.0D0/25.0D0
      CH(4)   = 32.0D0/225.0D0
      CH(5)   = 1.0D0/30.0D0
      CH(6)   = 6.0D0/25.0D0

      CT(1)   = -1.0D0/150.0D0
      CT(2)   = 0.0D0
      CT(3)   = 3.0D0/100.0D0
      CT(4)   = -16.0D0/75.0D0
      CT(5)   = -1.0D0/20.0D0
      CT(6)   =  6.0D0/25.0D0

C     DETERMINE DIRECTION OF THE INTEGRATION
      IF(T2 .GE. T1) THEN
          H =  .1
          IDIR = 1
      ENDIF
      IF(T2 .LT. T1) THEN
          H = -.1
          IDIR = -1
      ENDIF
C     H IS ALSO THE FIRST INTEGRATION STEP SIZE

      TSF = .90D0
      T   = T1

      DONE = .FALSE.

C     PRINT THE FIRST STATE AT T=T0
      CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'START')

C     INTEGRATION BEGINS HERE

C     SAVE THE TIME FOR THE START OF THE STEP

  10  TTEMP = T

      CALL DERIVS(NQ,T,Y,DY)

      DO N = 1,NQ
        F(1,N) = DY(N)
        YTEMP(N) = Y(N)
      ENDDO

C     RETURN HERE FOR A SMALLER STEPSIZE IF NECESSARY

  30  CONTINUE

C     DETERMINE IF INTEGRATION IS DONE
      IF( (IDIR.EQ.1.AND.T.GT.T2).OR.(IDIR.EQ.-1.AND.T.LT.T2)) THEN
        H = T2 - TTEMP
        DONE = .TRUE.
      ENDIF

C     THIS DO LOOP EVALUATES F1 TO F6 FOR ALL NQ EQUATIONS
      DO K = 2,6
        T = TTEMP + A(K) * H
        DO  N = 1,NQ
          Y(N) = YTEMP(N)
          K1 = K - 1
          DO L = 1,K1
              Y(N) = Y(N) + H*B(K,L)*F(L,N)
          ENDDO
        ENDDO
        CALL DERIVS(NQ,T,Y,DY)
C       DUMMY = DERIVS(T,Y,DY)
        DO N = 1,NQ
          F(K,N) = DY(N)
        ENDDO
      ENDDO
C     FINISHED EVALUATING F1 TO F6

C     LOOP EVALUATES Y1 AND TE FOR ALL NQ EQUATIONS
      DO N = 1,NQ
        TE(N) = 0.0D0
        Y(N)  = YTEMP(N)
        DO K = 1,6
          Y(N)  = Y(N)  + H*CH(K)*F(K,N)
          TE(N) = TE(N) + H*CT(K)*F(K,N)
        ENDDO
        TE(N) = DABS(TE(N))
      ENDDO
C     FINISHED EVALUATING Y1 AND TE

C     DETERMINE WHICH TE(N) HAS THE LARGEST VALUE AND STORE IT IN TEMAX
      TEMAX = 0.0D0
      DO N = 1,NQ
        IF(TEMAX .GE. TE(N)) THEN
          TEMAX = TEMAX
        ELSE
          TEMAX = TE(N)
        ENDIF
      ENDDO

C     STORE THE CURRENT TIME STEP JUST USED
      HTEMP = H

C     CALCULATE NEW STEPSIZE
      H = TSF*H*(TOL/TEMAX)**(1.0D0/5.0D0)

C     IF TEMAX>TOL H WILL BE SMALLER AND STEP HAS TO BE REPEATED
      IF(TEMAX .GT. TOL) GO TO 30

C     A SUCCESSFUL STEP COMPLETED
      T = TTEMP + HTEMP

      IF(IDIR.EQ. 1.AND.T.LE.T2)
     &   CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'')
      IF(IDIR.EQ.-1.AND.T.GE.T2)
     &   CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'')

      IF(.NOT. DONE) GOTO 10

100   CONTINUE
      END
C     ***  END OF INTEGRATION ROUTINE FOR SIMULATION ***
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C ODEINT
C DRIVER TO CALL RKQS OR BSSTEP (SPECIFIED IN NRINTEG)
C------------------------------------------------------------------------------
      SUBROUTINE ODEINT(YSTART,NVAR,T1,T2,EPS,H1,HMIN,DTSAV,
     *                  DERIVS,NRINTEG,
     *                  GRAPHSTATE,WRITESTATE,SUB_OUTPUT,OUTFILE_UNIT,SIM_T0_DAY,SEG_T0_DAY)

      INTEGER NVAR,MAXSTP,NEQMAX,OUTFILE_UNIT
	LOGICAL GRAPHSTATE,WRITESTATE
      DOUBLE PRECISION EPS,H1,HMIN,T1,T2,YSTART(NVAR),TINY
      EXTERNAL DERIVS,NRINTEG
      PARAMETER (MAXSTP=1000000,NEQMAX=110,TINY=1.d-30)
      INTEGER I,NSTP
      DOUBLE PRECISION DTSAV,H,HDID,HNEXT,T,TSAV,DYDT(NEQMAX),Y(NEQMAX)
      DOUBLE PRECISION YSCAL(NEQMAX)
	DOUBLE PRECISION SIM_T0_DAY,SEG_T0_DAY	!EPOCH TIMES SPECIFIC TO COPERNICUS

      T=T1
      H=SIGN(H1,T2-T1)

      DO I=1,NVAR
        Y(I)=YSTART(I)
      ENDDO

C     PRINT THE FIRST STATE
      CALL DERIVS(NVAR,T,Y,DYDT)		!FIRST CALL TO DERIVS NEEDED BECAUSE SOME OUTPUT PARAMETERS NEEDEDIN STATEOUT ARE COMPUTED IN DERIVS ROUTINE
      CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,OUTFILE_UNIT,SIM_T0_DAY,SEG_T0_DAY,'START')

      TSAV=T

      DO NSTP=1,MAXSTP

        CALL DERIVS(NVAR,T,Y,DYDT)

        DO I=1,NVAR
          YSCAL(I)=ABS(Y(I))+ABS(H*DYDT(I))+TINY
        ENDDO

        IF(ABS(T-TSAV).GT.ABS(DTSAV)) THEN
          CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,OUTFILE_UNIT,SIM_T0_DAY,SEG_T0_DAY,'')
          TSAV=T
        ENDIF

C       TIME STEP FOR THE LAST STEP
        IF((T+H-T2)*(T+H-T1).GT.0.D0) H=T2-T

        CALL NRINTEG(Y,DYDT,NVAR,T,H,EPS,YSCAL,HDID,HNEXT,DERIVS)

C       DETERMINE IF INTEGRATION IS DONE AND PRINT THE LAST STEP
        IF((T-T2)*(T2-T1).GE.0.D0) THEN
          DO I=1,NVAR
            YSTART(I)=Y(I)
          ENDDO
          CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,OUTFILE_UNIT,SIM_T0_DAY,SEG_T0_DAY,'END')
          RETURN
        ENDIF

        IF(ABS(HNEXT).LT.HMIN) PAUSE
     *  'STEPSIZE SMALLER THAN MINIMUM IN ODEINT'
        H=HNEXT

      ENDDO
      PAUSE 'TOO MANY STEPS IN ODEINT'
      RETURN
      END
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C RKQS
C
C NUMERICAL RECIPES DOUBLE PRECISION FILE
C CALLLED BY ODEINT
C------------------------------------------------------------------------------
      SUBROUTINE rkqs(y,dydt,n,t,htry,eps,yscal,hdid,hnext,derivs)
      INTEGER n,NEQMAX
      DOUBLE PRECISION eps,hdid,hnext,htry,t,dydt(n),y(n),yscal(n)
      EXTERNAL derivs
      PARAMETER (NEQMAX=110)
CU    USES derivs,rkck
      INTEGER i
      DOUBLE PRECISION errmax,h,tnew,yerr(NEQMAX),
     * ytemp(NEQMAX),SAFETY,PGROW
     *,PSHRNK,
     *ERRCON
      PARAMETER (SAFETY=0.9d0,PGROW=-.2d0,PSHRNK=-.25d0,ERRCON=1.89d-4)
!     PARAMETER (SAFETY=0.9d0,PGROW=-.2d0,PSHRNK=-.25d0,ERRCON=1.89d-4)	!ORIGINAL LINE
      h=htry
1     call rkck(y,dydt,n,t,h,ytemp,yerr,derivs)
      errmax=0.d0
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.d0)then
        h=SAFETY*h*(errmax**PSHRNK)
        if(h.lt.0.1d0*h)then
          h=.1d0*h
        endif
        tnew=t+h
!       if(tnew.eq.t)write(*,*) 'stepsize underflow in rkqs'
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.d0*h
        endif
        hdid=h
        t=t+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C RKCK
C
C NUMERICAL RECIPES DOUBLE PRECISION FILE
C CALLED BY RKQS
C------------------------------------------------------------------------------
      SUBROUTINE rkck(y,dydt,n,t,h,yout,yerr,derivs)
      INTEGER n,NEQMAX
      DOUBLE PRECISION h,t,dydt(n),y(n),yerr(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NEQMAX=110)
CU    USES derivs
      INTEGER i
      DOUBLE PRECISION ak2(NEQMAX),ak3(NEQMAX),
     *                 ak4(NEQMAX),ak5(NEQMAX),ak6(NEQMAX),
     *ytemp(NEQMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER (A2=.2d0,A3=.3d0,A4=.6d0,A5=1.d0,A6=.875d0,B21=.2d0,B31
     *=3.d0/40.d0,
     *B32=9.d0/40.d0,B41=.3d0,B42=-.9d0,B43=1.2d0,B51=-11.d0/54.d0,B52
     *=2.5d0,
     *B53=-70.d0/27.d0,B54=35.d0/27.d0,B61=1631.d0/55296.d0,B62=175.d0
     */512.d0,
     *B63=575.d0/13824.d0,B64=44275.d0/110592.d0,B65=253.d0/4096.d0,C1
     *=37.d0/378.d0,
     *C3=250.d0/621.d0,C4=125.d0/594.d0,C6=512.d0/1771.d0,DC1=C1-2825.d0
     */27648.d0,
     *DC3=C3-18575.d0/48384.d0,DC4=C4-13525.d0/55296.d0,DC5=-277.d0
     */14336.d0,
     *DC6=C6-.25d0)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydt(i)
11    continue
      call DERIVS(N,t+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydt(i)+B32*ak2(i))
12    continue
      call DERIVS(N,t+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydt(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call DERIVS(N,t+A4*h,ytemp,ak4)
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydt(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call DERIVS(N,t+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydt(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call DERIVS(N,t+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydt(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydt(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0

C------------------------------------------------------------------------------
C RKF_CONDITION
C
C ***             INTEGRATOR RUNGE-KUTTA-FEHLBERG 4(5)            ***
C ***             SPECIAL VERSION TO STOP WHEN A CERTAIN          ***
C ***             CONDITION IS SATISFIED                          ***
C------------------------------------------------------------------------------
      SUBROUTINE RKF_CONDITION(Y1,Y2,FCONDITION,NQ,DERIVS,T1,TI,T2,TOL,
     &                         TARGTOL,GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      INTEGER NQ,GRAPHSTATE,WRITESTATE
      DOUBLE PRECISION DABS
      INTEGER          NQMAX
      PARAMETER(NQMAX=110)

      DOUBLE PRECISION  A(6),B(6,5),CH(6),CT(6)
      DOUBLE PRECISION  Y(NQMAX),DY(110),YTEMP(110),TE(110),F(6,110)
      DOUBLE PRECISION  Y1(NQ)
      DOUBLE PRECISION  Y2(NQ)
      INTEGER           DIRECTION

      DOUBLE PRECISION  TARGTOL

C     DECLARE THE FUNCTIONS CALLED IN THIS ROUTINE
      DOUBLE PRECISION  FCONDITION

      DOUBLE PRECISION  FVALUE,FVALUE_PREV
      INTEGER           SIGNOF

      INTEGER           CHECKH,LOOPCNT,CHECK_CONVERGENCE

	DOUBLE PRECISION  SIM_T0_DAY,SEG_T0_DAY	!EPOCH TIMES SPECIFIC TO COPERNICUS

      A(2)    = 2.0D0/9.0D0
      A(3)    = 1.0D0/3.0D0
      A(4)    = 3.0D0/4.0D0
      A(5)    = 1.0D0
      A(6)    = 5.0D0/6.0D0

      B(2,1)  = 2.0D0/9.0D0
      B(3,1)  = 1.0D0/12.0D0
      B(3,2)  = 1.0D0/4.0D0
      B(4,1)  = 69.0D0/128.0D0
      B(4,2)  = -243.0D0/128.0D0
      B(4,3)  = 135.0D0/64.0D0

      B(5,1)  = -17.0D0/12.0D0
      B(5,2)  = 27.0D0/4.0D0
      B(5,3)  = -27.0D0/5.0D0
      B(5,4)  = 16.0D0/15.0D0
      B(6,1)  = 65.0D0/432.0D0
      B(6,2)  = -5.0D0/16.0D0
      B(6,3)  = 13.0D0/16.0D0
      B(6,4)  = 4.0D0/27.0D0
      B(6,5)  = 5.0D0/144.0D0

      CH(1)   = 47.0D0/450.0D0
      CH(2)   = 0.0D0
      CH(3)   = 12.0D0/25.0D0
      CH(4)   = 32.0D0/225.0D0
      CH(5)   = 1.0D0/30.0D0
      CH(6)   = 6.0D0/25.0D0

      CT(1)   = -1.0D0/150.0D0
      CT(2)   = 0.0D0
      CT(3)   = 3.0D0/100.0D0
      CT(4)   = -16.0D0/75.0D0
      CT(5)   = -1.0D0/20.0D0
      CT(6)   =  6.0D0/25.0D0

      CHECKH   = 0
      LOOPCNT  = 0

C     DETERMINE INITIAL STEP SIZE AND DIRECTION OF THE INTEGRATION
      IF(T2 .GE. T1) THEN
        H =  1.D-9
        DIRECTION = 1
      ENDIF
      IF(T2 .LT. T1) THEN
        H = -1.D-9
        DIRECTION = -1
      ENDIF

      TSF = .90D0
      T   = T1

C     COPY Y1 INTO Y
      DO I=1,NQ
        Y(I)=Y1(I)
      ENDDO

C     PRINT/PLOT THE THE STATE AT T=T0
C     CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'START')

C     COMPUTE THE INITIAL VALUE FOR THE STOPPING CONDITION
      FVALUE = FCONDITION(Y,NQ)
      FVALUE_PREV = FVALUE

C     IF INITIAL STATE SATISFIES STOPPING CONDITION, DON'T CHECK FOR
C     CONVERGENCE YET
      IF(DABS(FVALUE).GT.TARGTOL) CHECK_CONVERGENCE=1
      IF(DABS(FVALUE).LE.TARGTOL) CHECK_CONVERGENCE=0

C     INTEGRATION BEGINS HERE

C     SAVE THE TIME FOR THE START OF THE STEP

  10  TTEMP = T

      CALL DERIVS(NQ,T,Y,DY)

      DO N = 1,NQ
        F(1,N) = DY(N)
        YTEMP(N) = Y(N)
      ENDDO

C     RETURN HERE FOR A SMALLER STEPSIZE IF NECESSARY

  30  CONTINUE

C     THIS DO LOOP EVALUATES F1 TO F6 FOR ALL NQ EQUATIONS
      DO K = 2,6
        T = TTEMP + A(K) * H
        DO  N = 1,NQ
          Y(N) = YTEMP(N)
          K1 = K - 1
          DO L = 1,K1
            Y(N) = Y(N) + H*B(K,L)*F(L,N)
          ENDDO
        ENDDO
        CALL DERIVS(NQ,T,Y,DY)
        DO N = 1,NQ
          F(K,N) = DY(N)
        ENDDO
      ENDDO
C     FINISHED EVALUATING F1 TO F6

C     LOOP EVALUATES Y1 AND TE FOR ALL NQ EQUATIONS
      DO N = 1,NQ
        TE(N) = 0.0D0
        Y(N)  = YTEMP(N)
        DO K = 1,6
          Y(N)  = Y(N)  + H*CH(K)*F(K,N)
          TE(N) = TE(N) + H*CT(K)*F(K,N)
        ENDDO
        TE(N) = DABS(TE(N))
      ENDDO
C     FINISHED EVALUATING Y1 AND TE

      HTEMP = H

      IF(CHECKH .NE. 1) THEN

C       DETERMINE WHICH TE(N) HAS LARGEST VALUE: STORE IN TEMAX
	TEMAX = 0.0D0
	DO N = 1,NQ
	  IF(TEMAX .GE. TE(N)) THEN
	    TEMAX = TEMAX
	  ELSE
	    TEMAX = TE(N)
	  ENDIF
        ENDDO

C       STORE THE CURRENT TIME STEP JUST USED
        HTEMP = H

C       CALCULATE NEW STEPSIZE
        H = TSF*H*(TOL/TEMAX)**(1.0D0/5.0D0)

C       IF TEMAX>TOL H WILL BE SMALLER AND STEP HAS TO BE REPEATED
        IF(TEMAX .GT. TOL) GO TO 30

      ENDIF
C     ENDIF FOR CHECKH .NE. 1

C     COMPUTE THE STOPPING CONDITION
      FVALUE = FCONDITION(Y,NQ)

C     CHECK FOR CONVERGENCE OF THE CONDITION: IF SO THEN TERMINATE
      IF(CHECK_CONVERGENCE.EQ.1) THEN
        IF(DABS(FVALUE).LE.TARGTOL) THEN
            T=TTEMP+HTEMP
            GOTO 100
        ENDIF
      ENDIF

C     IF CONDITION CANNOT BE MET TERMINATE INTEGRATION (ALLOW 50 LOOPS)
      IF(LOOPCNT.GT.50) THEN
        PRINT *,'RKF_CONTITION: TARGET CONDITION NOT POSSIBLE'
        PRINT *,'PROGRAM WILL TERMINATE'
        READ(*,*)
!        CALL CLOSE_WINDOWS()
        STOP
      ENDIF

C     CHECK TO SEE IF FVALUE HAS CHANGED SIGN, IF SO NAIL IT DOWN BY HALVING THE STEP SIZE
      IF(CHECK_CONVERGENCE.EQ.1) THEN
        IF(SIGNOF(FVALUE).EQ.-SIGNOF(FVALUE_PREV)) THEN   !FVALUE HAS CHANGED SIGNS
            CHECKH = 1
            H = H/2.0D0
C           COUNT HOW MANY TIMES IN THIS LOOP
            LOOPCNT = LOOPCNT+1
            GOTO 30
        ENDIF
      ENDIF

      FVALUE_PREV = FVALUE

      IF(DABS(FVALUE).GT.TARGTOL) CHECK_CONVERGENCE=1

C     A SUCCESSFUL STEP COMPLETED

      T = TTEMP + HTEMP

      IF(DABS(T).LE.DABS(T2)) THEN
        IF(CHECKH.EQ.0) CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'')
      ENDIF

C     DETERMINE IF THE LAST TIME STEP HAS BEEN TAKEN
      IF(T2 .GE. T1 .AND. H .LT. 0.0D0) THEN
        GOTO 100
      ENDIF
      IF(T2 .LT. T1 .AND. H .GT. 0.0D0) THEN
        GOTO 100
      ENDIF

C     DETERMINE IF CURRENT TIME IS LESS THAN FINAL TIME, IF NOT
C     THEN INTEGRATION IS NOT YET FINISHED

      IF(T2 .GE. T1 .AND. T .LT. T2) THEN
        GOTO 10
      ENDIF

      IF(T2 .LT. T1 .AND. T .GT. T2) THEN
        GOTO 10
      ENDIF

C     STEP SIZE FOR THE LAST INTEGRATION STEP
      H = T2 - T

      GO TO 10

100   CONTINUE

C     PRINT/PLOT THE THE STATE AT T=TF
      CALL SUB_OUTPUT(T,Y,GRAPHSTATE,WRITESTATE,FILE,SIM_T0_DAY,SEG_T0_DAY,'END')

      DO I=1,NQ
        Y2(I)=Y(I)
      ENDDO
      TI = T

      END
C     ***  END OF INTEGRATION ROUTINE FOR SIMULATION ***

C------------------------------------------------------------------------------
C SIGNOF
C
C ALGEBRAIC SIGN OF A FUNCTION
C------------------------------------------------------------------------------
C     FUNCTION TO COMPUTE THE SIGNUM FUNCTION OF X
      INTEGER FUNCTION SIGNOF(X)

      IMPLICIT NONE

      DOUBLE PRECISION X

      IF(X.GE.0.0D0) SIGNOF= 1
      IF(X.LT.0.0D0) SIGNOF=-1

      END
C     ***  END FUNCTION SIGNOF ***

C------------------------------------------------------------------------------
C *** ODEINT_CONDITION  (INTEGRATES UNTIL A CONDITION IS SATISFIED)
C------------------------------------------------------------------------------
      SUBROUTINE ODEINT_CONDITION(YSTART,FCONDITION,TARGTOL,
     &                            FCONDITION_2_CHECK,FCONDITION_2_MAX,
     &                            NVAR,T1,TI,T2,RKFTOL,
     &                            DERIVS,NRINTEG,
     &                            GRAPHSTATE,WRITESTATE,SUB_OUTPUT,FILE,SIM_T0_DAY,SEG_T0_DAY)

      INTEGER NVAR,MAXSTP,NEQMAX,GRAPHSTATE,WRITESTATE,FILE
      DOUBLE PRECISION EPS,H1,HMIN,T1,TI,T2,YSTART(NVAR),TINY
      EXTERNAL DERIVS,NRINTEG
      PARAMETER (MAXSTP=1000000,NEQMAX=110,TINY=1.d-30)
      INTEGER I,NSTP
      DOUBLE PRECISION H,HDID,HNEXT,T,TSAV,DYDT(NEQMAX),Y(NEQMAX)
      DOUBLE PRECISION YSCAL(NEQMAX),YSAV(NEQMAX)
      DOUBLE PRECISION YERR(NEQMAX),YTEMP(NEQMAX)
      DOUBLE PRECISION RKFTOL
      DOUBLE PRECISION FCONDITION,FVALUE,FVALUE_PREV,TARGTOL
      DOUBLE PRECISION FCONDITION_2_MAX
      LOGICAL          FCONDITION_2_CHECK
      INTEGER          SIGNOF,CHECK_CONVERGENCE
	DOUBLE PRECISION SIM_T0_DAY,SEG_T0_DAY				!EPOCH TIMES SPECIFIC TO COPERNICUS

      EPS  = RKFTOL
      H1   = .01D0
      HMIN = 0.D0
      DTSAV=.00D0

      WRITESTATE=WRITESTATE

      T=T1
      H=SIGN(H1,T2-T1)

      DO I=1,NVAR
        Y(I)=YSTART(I)
      ENDDO

C     PRINT THE FIRST STATE
      CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'START')

      TSAV=T

C     COMPUTE THE INITIAL VALUE FOR THE STOPPING CONDITION
      FVALUE_PREV = FCONDITION(Y,NVAR)

C     IF INITIAL STATE SATISFIES STOPPING CONDITION, DON'T CHECK FOR
C     CONVERGENCE YET
      IF(DABS(FVALUE_PREV).GE.TARGTOL) CHECK_CONVERGENCE=1
      IF(DABS(FVALUE_PREV).LT.TARGTOL) CHECK_CONVERGENCE=0

      DO NSTP=1,MAXSTP

        CALL DERIVS(NVAR,T,Y,DYDT)
        DO I=1,NVAR
          YSCAL(I)=ABS(Y(I))+ABS(H*DYDT(I))+TINY
        ENDDO

        CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'')

C       TIME STEP FOR THE LAST STEP
        IF((T+H-T2)*(T+H-T1).GT.0.D0) H=T2-T

        DO I =1,NVAR
          YSAV(I) = Y(I)
        ENDDO
        TSAV = T

        CALL NRINTEG(Y,DYDT,NVAR,T,H,EPS,YSCAL,HDID,HNEXT,DERIVS)

        FVALUE = FCONDITION(Y,NVAR)

        IF( DABS(FVALUE_PREV).LT.TARGTOL
     &     .AND.DABS(FVALUE).GE.TARGTOL) FVALUE_PREV=FVALUE

        IF(DABS(FVALUE).GE.TARGTOL) CHECK_CONVERGENCE=1

        IF(CHECK_CONVERGENCE.EQ.1) THEN
          IF(SIGNOF(FVALUE).EQ.-SIGNOF(FVALUE_PREV)) THEN
 5          CONTINUE
            DO I=1,NVAR
              Y(I) = YSAV(I)
            ENDDO
            T = TSAV
            H = H/2.0D0
 10         CONTINUE
            DO I=1,NVAR
              YSAV(I) = Y(I)
            ENDDO
            TSAV = T

C           ADVANCE THE STATE ONE STEP (RUNGE-KUTTA-CASH-KARP)
            CALL DERIVS(NVAR,T,Y,DYDT)
            CALL RKCK(Y,DYDT,NVAR,T,H,YTEMP,YERR,DERIVS)
            T=T+H
            DO I=1,NVAR
              Y(I)=YTEMP(I)
            ENDDO

            FVALUE = FCONDITION(Y,NVAR)
            IF(DABS(FVALUE).LT.TARGTOL) THEN
              DO I=1,NVAR
                YSTART(I)=Y(I)
              ENDDO
              TI = T
              CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'')
              RETURN
            ENDIF

            IF(SIGNOF(FVALUE).EQ.-SIGNOF(FVALUE_PREV)) GOTO 5
            GOTO 10
          ENDIF
        ENDIF

C       DETERMINE IF INTEGRATION IS DONE AND THEN PRINT LAST STEP
        IF((T-T2)*(T2-T1).GE.0.D0)THEN
          DO I=1,NVAR
            YSTART(I)=Y(I)
          ENDDO
          CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'END')
          RETURN
        ENDIF

C       CHECK IF CONDITION 2 IS SATISFIED
        IF(FCONDITION_2_CHECK(Y,NVAR,FCONDITION_2_MAX)) THEN
          DO I=1,NVAR
            YSTART(I)=Y(I)
          ENDDO
          TI = T
          CALL SUB_OUTPUT(T,Y,GRAPHSTATE,0,FILE,SIM_T0_DAY,SEG_T0_DAY,'END')
          RETURN
        ENDIF

        IF(ABS(HNEXT).LT.HMIN) PAUSE
     *    'STEPSIZE SMALLER THAN MINIMUM IN ODEINT'

        H=HNEXT

      ENDDO

      PAUSE 'TOO MANY STEPS IN ODEINT_CONDITION'
      RETURN
      END

C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C BSSTEP
C
C BULIRSCH-STOER EXTRAPOLATION INTEGERATION METHOD
C CALLED BY ODEINT OR ODEINT_CONDITION
C------------------------------------------------------------------------------
      SUBROUTINE bsstep(y,dydx,nv,x,htry,eps,yscal,hdid,hnext,derivs)
      INTEGER nv,NMAX,KMAXX,IMAX
      DOUBLE PRECISION eps,hdid,hnext,htry,x,dydx(nv),y(nv),yscal(nv)
     *,SAFE1,SAFE2,
     *REDMAX,REDMIN,TINY,SCALMX
      PARAMETER (NMAX=110,KMAXX=8,IMAX=KMAXX+1,SAFE1=.25d0,SAFE2=.7d0,
     *REDMAX=1.d-5,REDMIN=.7d0,TINY=1.d-30,SCALMX=.1d0)
CU    USES derivs,mmid,pzextr
      INTEGER i,iq,k,kk,km,kmax,kopt,nseq(IMAX)
      DOUBLE PRECISION eps1,epsold,errmax,fact,h,red,scale,work,wrkmin
     *,xest,xnew,
     *a(IMAX),alf(KMAXX,KMAXX),err(KMAXX),yerr(NMAX),ysav(NMAX),
     *yseq(NMAX)
      LOGICAL first,reduct
      SAVE a,alf,epsold,first,kmax,kopt,nseq,xnew
      EXTERNAL derivs
      DATA first/.true./,epsold/-1.d0/
      DATA nseq /2,4,6,8,10,12,14,16,18/
      if(eps.ne.epsold)then
        hnext=-1.d29
        xnew=-1.d29
        eps1=SAFE1*eps
        a(1)=nseq(1)+1
        do 11 k=1,KMAXX
          a(k+1)=a(k)+nseq(k+1)
11      continue
        do 13 iq=2,KMAXX
          do 12 k=1,iq-1
            alf(k,iq)=eps1**((a(k+1)-a(iq+1))/((a(iq+1)-a(1)+1.d0)*(2*k+
     *
     *1)))
12        continue
13      continue
        epsold=eps
        do 14 kopt=2,KMAXX-1
          if(a(kopt+1).gt.a(kopt)*alf(kopt-1,kopt))goto 1
14      continue
1       kmax=kopt
      endif
      h=htry
      do 15 i=1,nv
        ysav(i)=y(i)
15    continue
      if(h.ne.hnext.or.x.ne.xnew)then
        first=.true.
        kopt=kmax
      endif
      reduct=.false.
2     do 17 k=1,kmax
        xnew=x+h
        if(xnew.eq.x)pause 'step size underflow in bsstep'
        call mmid(ysav,dydx,nv,x,h,nseq(k),yseq,derivs)
        xest=(h/nseq(k))**2
        call pzextr(k,xest,yseq,y,yerr,nv)
        if(k.ne.1)then
          errmax=TINY
          do 16 i=1,nv
            errmax=max(errmax,abs(yerr(i)/yscal(i)))
16        continue
          errmax=errmax/eps
          km=k-1
          err(km)=(errmax/SAFE1)**(1.d0/(2*km+1))
        endif
        if(k.ne.1.and.(k.ge.kopt-1.or.first))then
          if(errmax.lt.1.d0)goto 4
          if(k.eq.kmax.or.k.eq.kopt+1)then
            red=SAFE2/err(km)
            goto 3
          else if(k.eq.kopt)then
            if(alf(kopt-1,kopt).lt.err(km))then
              red=1.d0/err(km)
              goto 3
            endif
          else if(kopt.eq.kmax)then
            if(alf(km,kmax-1).lt.err(km))then
              red=alf(km,kmax-1)*SAFE2/err(km)
              goto 3
            endif
          else if(alf(km,kopt).lt.err(km))then
            red=alf(km,kopt-1)/err(km)
            goto 3
          endif
        endif
17    continue
3     red=min(red,REDMIN)
      red=max(red,REDMAX)
      h=h*red
      reduct=.true.
      goto 2
4     x=xnew
      hdid=h
      first=.false.
      wrkmin=1.d35
      do 18 kk=1,km
        fact=max(err(kk),SCALMX)
        work=fact*a(kk+1)
        if(work.lt.wrkmin)then
          scale=fact
          wrkmin=work
          kopt=kk+1
        endif
18    continue
      hnext=h/scale
      if(kopt.ge.k.and.kopt.ne.kmax.and..not.reduct)then
        fact=max(scale/alf(kopt-1,kopt),SCALMX)
        if(a(kopt+1)*fact.le.wrkmin)then
          hnext=h/fact
          kopt=kopt+1
        endif
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C MMID
C
C CALLED BY BSSTEP
C------------------------------------------------------------------------------
      SUBROUTINE mmid(y,dydx,nvar,xs,htot,nstep,yout,derivs)
      INTEGER nstep,nvar,NMAX
      DOUBLE PRECISION htot,xs,dydx(nvar),y(nvar),yout(nvar)
      EXTERNAL derivs
      PARAMETER (NMAX=110)
      INTEGER i,n
      DOUBLE PRECISION h,h2,swap,x,ym(NMAX),yn(NMAX)
      h=htot/nstep
      do 11 i=1,nvar
        ym(i)=y(i)
        yn(i)=y(i)+h*dydx(i)
11    continue
      x=xs+h
      call DERIVS(NVAR,x,yn,yout)
      h2=2.d0*h
      do 13 n=2,nstep
        do 12 i=1,nvar
          swap=ym(i)+h2*yout(i)
          ym(i)=yn(i)
          yn(i)=swap
12      continue
        x=x+h
        call DERIVS(NVAR,x,yn,yout)
13    continue
      do 14 i=1,nvar
        yout(i)=0.5d0*(ym(i)+yn(i)+h*yout(i))
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C PZEXTR
C
C CALLED BY BSSTEP
C------------------------------------------------------------------------------
      SUBROUTINE pzextr(iest,xest,yest,yz,dy,nv)
      INTEGER iest,nv,IMAX,NMAX
      DOUBLE PRECISION xest,dy(nv),yest(nv),yz(nv)
      PARAMETER (IMAX=13,NMAX=110)
      INTEGER j,k1
      DOUBLE PRECISION delta,f1,f2,q,d(NMAX),qcol(NMAX,IMAX),x(IMAX)
      SAVE qcol,x
      x(iest)=xest
      do 11 j=1,nv
        dy(j)=yest(j)
        yz(j)=yest(j)
11    continue
      if(iest.eq.1) then
        do 12 j=1,nv
          qcol(j,1)=yest(j)
12      continue
      else
        do 13 j=1,nv
          d(j)=yest(j)
13      continue
        do 15 k1=1,iest-1
          delta=1.d0/(x(iest-k1)-xest)
          f1=xest*delta
          f2=x(iest-k1)*delta
          do 14 j=1,nv
            q=qcol(j,k1)
            qcol(j,k1)=dy(j)
            delta=d(j)-q
            dy(j)=f1*delta
            d(j)=f2*delta
            yz(j)=yz(j)+dy(j)
14        continue
15      continue
        do 16 j=1,nv
          qcol(j,iest)=dy(j)
16      continue
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Bmi1#!-01.d0
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
!	SUBROUTINE KEPLER_INTEG(RV1,RV2,DT)
C------------------------------------------------------------------------------
!	DRIVER FOR KEPLER_PROPAGATION
!	USE GLOBAL_DATA
!	IMPLICIT NONE
!	INTEGER			 CB
!	DOUBLE PRECISION RV1(7),RV2(7),R(3),V(3),DT
!	DOUBLE PRECISION VEC(3)
!	DOUBLE PRECISION COE(6)

!	CB = PROP_CENTRAL_BODY_I

!	!CONVERT VEC(1:6) TO ORBITAL ELEMENTS
!	R(1:3) = RV1(1:3);V(1:3)=RV1(4:6)
!	CALL RV_TO_COE_MA(R,V,GM(CB),COE)
!	!PROPAGATE WITH KEPLER ALGORITHM
!	CALL KEPLER_PROPAGATION(GM(CB),COE,DT,R,V)
!	RV2(1:3)=R(1:3);RV2(4:6)=V(1:3)
!!	RV(7) (MASS) DOES NOT CHANGE	

!	END
C------------------------------------------------------------------------------
