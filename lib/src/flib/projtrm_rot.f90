	subroutine projtrm_rot(n, x, mass, hess, info)
	use kinds
	implicit none
	!--------------------------------------------------------------------
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: mass(n)
	real(DP), intent(in   ) :: x(3,n)
	real(DP), intent(inout) :: hess(n*3,n*3)
	integer , intent(  out) :: info
	!--------------------------------------------------------------------
	real(DP), parameter     :: ONE=1.0_DP
	integer                 :: i, j, k, n3
	integer                 :: ier
	real(DP)                :: xc(3,n), P(3*n,3*n)
	real(DP)                :: TRVEC(3*n,3*n)
	!--------------------------------------------------------------------
	!
	info = 0
	xc=x
	call com(n, mass, xc, ier)
	!--------------------------------------------------------------------
	n3 = 3*n
	call FormTRM1(n,mass,xc,TRVEC)
	!--------------------------------------------------------------------
	P = 0.0_DP
	do k=1,6
		do j=1,n3
			do i=1,n3
				P(i,j) = P(i,j) - TRVEC(i,k)*TRVEC(j,k)
			end do
		end do
	end do
	!
	do i=1,n3
		P(i,i) = ONE + P(i,i)
	end do
	!--------------------------------------------------------------------
! 	call set_sym_matrix(n3, hess)
	hess=matmul(P,matmul(hess,P))
! 	call set_sym_matrix(n3, hess)
	! call check_sym_matrix(n3, hess)
	!--------------------------------------------------------------------
	return
	end subroutine
	!
	!====================================================================
	!
	subroutine FormTRM1(n,mass,xc,V)
	use kinds
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: mass(n)
	real(DP), intent( in) :: xc(3,n)
	real(DP), intent(out) :: v(3,n,6)
	!--------------------------------------------------------------------
	real(DP), parameter   :: ONE=1.0d0
	real(DP), parameter   :: TollZERO=1.0d-8
	integer               :: i, n3
	real(DP)              :: ami
	real(DP)              ::  x,  y,  z
	real(DP)              :: cx, cy, cz
	real(DP)              :: qx, qy, qz
	real(DP)              :: skal
	real(DP), external    :: SProd
	!--------------------------------------------------------------------
	n3 = 3*n
	!--------------------------------------------------------------------
	V = 0.0_DP
	do i=1,n
		x = xc(1,i)
		y = xc(2,i)
		z = xc(3,i)
		!
		ami = DSQRT(mass(i))
		!
		qx = ami*x
		qy = ami*y
		qz = ami*z
		!
		cx = ami*x
		cy = ami*y
		cz = ami*z
		!
		V(1,i,1) = ami
		V(2,i,2) = ami
		V(3,i,3) = ami
		!
		V(2,i,4) = -cz
		V(3,i,4) =  cy
		!
		V(1,i,5) =  cz
		V(3,i,5) = -cx
		!
		V(1,i,6) = -cy
		V(2,i,6) =  cx
	end do
	!--------------------------------------------------------------------
	do i=1,6
		skal = SProd(n3,V(1,1,i),V(1,1,i))
		if (skal > TollZERO) then
			skal = ONE/SQRT(skal)
			call VScal(n3,skal,V(1,1,i))
		end if
	end do
	!--------------------------------------------------------------------
	return
	end subroutine