	subroutine diagonalize(n,tmp,EigenValue)
	use kinds, only: DP
	implicit none
	!Implicit real*8(a-h,o-z)
c	parameter(n=18)
c	Complex*16 H(n,n)
	Real*8 EigenValue(n)
c	Complex*16 EigenVector(n,n)
c	real*8     EigenVector(n*n)
	integer :: n, i
	real*8 ar(n,n),ai(n,n),zr(n,n),zi(n,n)
	real*8 w(n),fv1(n),fv2(n)
	real*8 fm1(2,n)
	real*8 norm,tmp(N*N)
	Integer	matz,ierr 
	Integer j,k
	matz = 1	! want both Eigenvalue and EigenVector

       do i=1,n
        do j=1,n
          ar(i,j)= tmp(j+(i-1)*n)   
          ai(i,j)=0.0
        enddo
       enddo
	call ch(n,n,ar,ai,w,matz,zr,zi,fv1,fv2,fm1,ierr)
	if(ierr.ne.0)	stop 'Diag Hn Error Stop !!!'

	do j=1,n
		EigenValue(j)=w(j)
	end do

	do j=1,n
		do k=1,n
          tmp(k+(j-1)*n)=zr(k,j)
		end do
	end do
cccccccccccccccccc
       do i=1,n
          norm = 0.
         do j=1,n
         norm = norm+ tmp(j+(i-1)*n)* tmp(j+(i-1)*n)
         enddo
         norm = sqrt(norm)
         do j=1,n     
           tmp(j+(i-1)*n) =tmp(j+(i-1)*n)/norm
         enddo    
       enddo   
ccccccccccccccccccc
	do i = 1, n
	write(6,1) EigenValue(i),(tmp(k+(i-1)*n),k=1,n)
	enddo
1	format(1x,6(2x,f15.8))

	return
	end
