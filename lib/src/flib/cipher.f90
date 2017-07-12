MODULE cipher
  !IMPLICIT NONE
  INTEGER,PARAMETER    :: keystep=Z'4348454e'
  INTEGER,PARAMETER    :: keysum=Z'6908a9c0'
  integer,parameter    :: key(4)=(/1840, 1901, 1949, 1978/)
  CONTAINS
   
  INTEGER FUNCTION mx(v,total,k1,k2)
    !((v<<4)+k1) xor (v+total) xor ((v>>5)+k2)
    INTEGER                       :: v,total,k1,k2,temp1,temp2,temp3

    temp1=ISHFT(v,4)+k1
    temp2=v+total
    temp3=ISHFT(v,-5)+k2
    mx=xor(xor(temp1,temp2),temp3)
  END FUNCTION mx

  SUBROUTINE encrypt(v,k)
    INTEGER,INTENT(INOUT)         :: v(2)
    INTEGER,INTENT(IN)            :: k(4)
    INTEGER                       :: total,i
    INTEGER                       :: delta=keystep

    total=0
    do i=1, 32
      total=total+delta
      v(1)=v(1)+mx(v(2),total,k(1),k(2))
      v(2)=v(2)+mx(v(1),total,k(3),k(4))
    end do
    !v(1)=v1; v(2)=v2
  END SUBROUTINE encrypt

  SUBROUTINE decrypt(v,k)
    INTEGER,INTENT(INOUT)         :: v(2)
    INTEGER,INTENT(IN)            :: k(4)
    INTEGER                       :: total,i
    INTEGER                       :: delta=keystep

    total=keysum
    do i=1, 32
      v(2)=v(2)-mx(v(1),total,k(3),k(4))
      v(1)=v(1)-mx(v(2),total,k(1),k(2))
      total=total-delta
    end do
  END SUBROUTINE decrypt

  SUBROUTINE encrypt_information(v,k,n)
    INTEGER,INTENT(IN)            :: n
    INTEGER,INTENT(INOUT)         :: v(n)
    INTEGER,INTENT(IN)            :: k(4)
    INTEGER                       :: i,start_idx,end_idx
    DO i=1,n/2
      start_idx=2*i-1
      end_idx=2*i
      !write(6,*) 'before cycle (v):',i, v(1:4)
      !write(6,*) 'before cycle (k):',i, k(1:4)
      CALL encrypt(v(start_idx:end_idx),k)
      !write(6,*) 'after  cycle (v):',i, v(1:4)
      !write(6,*) 'after  cycle (k):',i, k(1:4)
    END DO
  END SUBROUTINE

  SUBROUTINE decrypt_information(v,k,n)
    INTEGER,INTENT(IN)            :: n
    INTEGER,INTENT(INOUT)         :: v(n)
    INTEGER,INTENT(IN)            :: k(4)
    INTEGER                       :: i,start_idx,end_idx

    DO i=1,n/2
      start_idx=2*i-1
      end_idx=2*i
      !write(6,*) 'decrypt:', v(start_idx:end_idx)
      CALL decrypt(v(start_idx:end_idx),k)
    END DO
  END SUBROUTINE


END MODULE cipher
