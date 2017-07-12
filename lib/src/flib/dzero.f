      SUBROUTINE DZERO(N,A)
		use kinds, only: DP
		implicit none
      !IMPLICIT REAL*8(A-H,O-Z)
		integer :: n, i
      REAL(DP) :: A(N)
      real(DP), PARAMETER :: ZERO=0.0D0
      DO 10 I=1,N
      A(I) = ZERO
 10   CONTINUE
      RETURN
      END
