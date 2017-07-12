      SUBROUTINE PRNTMAT(N,NROW,NCOL,A)
		use kinds, only: DP
		implicit none
		integer  :: N, NROW, NCOL
		integer  :: NP, NT, IMIN, IMAX, I, J, K, NS, NLEFT
      REAL(DP) :: A(NROW,NCOL)
      integer, PARAMETER :: MAXCOL=6
      NP = N
      IF(NP.GT.NCOL) NP = NCOL     ! CAN'T PRINT MORE THAN NCOL
      NT = NP/MAXCOL
      IF(NT.EQ.0) GO TO 30
      DO 20 I=1,NT
      IMIN = (I-1)*MAXCOL + 1
      IMAX = I*MAXCOL
      WRITE(6,1000)
      DO 10 J=1,NROW
      WRITE(6,1100) (A(J,K),K=IMIN,IMAX)
 10   CONTINUE
 20   CONTINUE
 30   CONTINUE
      NS = NT*MAXCOL
      NLEFT = NP - NS
      IF(NLEFT.EQ.0) RETURN
      WRITE(6,1000)
      DO 40 J=1,NROW
      WRITE(6,1100) (A(J,K),K=NS+1,NP)
 40   CONTINUE
      RETURN
 1000 FORMAT(/)
 1100 FORMAT(1X,6F12.6)
      END
