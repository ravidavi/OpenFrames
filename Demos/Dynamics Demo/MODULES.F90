!---------------------------------------------------------------------------------------------------------------------------------------
	MODULE DYNAMIC_VARIABLES
!---------------------------------------------------------------------------------------------------------------------------------------
	IMPLICIT NONE

	DOUBLE PRECISION::		TIME_0 = 0.0D0, TIME_F = 186400.0D0
	DOUBLE PRECISION,PARAMETER::	TIME_DELTA = 0.1D0
	DOUBLE PRECISION		TIME_CURR

	DOUBLE PRECISION		R_KM(3),V_KMPS(3),X_DIM(6)
	DOUBLE PRECISION		R_GU(3)

	DOUBLE PRECISION,PARAMETER::	MOD2GRAF = 10000.0D0

	INTEGER,PARAMETER::		ID_SPACECRAFT = 10000

	DOUBLE PRECISION::		GM = 3.986D+05		    !GM OF CENTRAL BODY
	DOUBLE PRECISION::		CBODY_RADIUS_KM = 6378.137  !EARTH EQUATORIAL RADIUS
	DOUBLE PRECISION::		SC_BASE_LEN_KM	= 500.00    !BASE DIMENSION FOR SPACECRAFT


	END MODULE DYNAMIC_VARIABLES
!---------------------------------------------------------------------------------------------------------------------------------------
