      DOUBLE PRECISION FUNCTION SPROD(N,V1,V2)
		use kinds, only: DP
		implicit none
		integer  :: N
      REAL(DP) :: V1(N),V2(N)
		integer  :: I
      real(DP), PARAMETER :: ZERO=0.0D0
      SPROD = ZERO
      DO 10 I=1,N
      SPROD = SPROD + V1(I)*V2(I)
 10   CONTINUE
      RETURN
      END
