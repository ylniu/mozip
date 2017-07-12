      SUBROUTINE VSCAL(N,SKAL,V)
		use kinds, only: DP
		implicit none
		integer :: N
		integer :: I
		real(DP) :: V(N), SKAL
      DO 10 I=1,N
      V(I) = V(I)*SKAL
 10   CONTINUE
      RETURN
      END
