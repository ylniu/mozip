SUBROUTINE NEXPER(N, A, MTC, EVEN)
implicit none
INTEGER N, I, J, M
INTEGER, DIMENSION(N) :: A
INTEGER S, D, NM3, IA, I1, L
LOGICAL MTC, EVEN
    IF (MTC) GOTO 10
    NM3 = N-3
    DO 1 I=1,N
1   A(I)=I
    MTC=.TRUE.
    EVEN=.TRUE.
    IF(N .EQ. 1) GOTO 8
6   IF(A(N) .NE. 1 .OR. A(1) .NE. 2+MOD(N,2)) RETURN
    IF(N .LE. 3) GOTO 8
    DO 7 I=1,NM3
    IF(A(I+1) .NE. A(I)+1) RETURN
7   CONTINUE
8   MTC=.FALSE.
    RETURN
10  IF(N .EQ. 1) GOTO 27
    IF(.NOT. EVEN) GOTO 20
    IA=A(1)
    A(1)=A(2)
    A(2)=IA
    EVEN=.FALSE.
    GOTO 6
20  S=0
    DO 26 I1=2,N
    IA=A(I1)
    I=I1-1
    D=0
    DO 30 J=1,I
30  IF(A(J) .GT. IA) D=D+1
    S=D+S
    IF(D .NE. I*MOD(S,2)) GOTO 35
26  CONTINUE
27  A(1)=0
    GOTO 8
35  M=MOD(S+1,2)*(N+1)
    DO 40 J=1,I
    IF(ISIGN(1,A(J)-IA) .EQ. ISIGN(1,A(J)-M)) GOTO 40
    M=A(J)
    L=J
40  CONTINUE
    A(L)=IA
    A(I1)=M
    EVEN=.TRUE.
    RETURN
END
