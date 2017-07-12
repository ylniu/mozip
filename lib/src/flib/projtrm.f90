	subroutine projtrm(n, x0, mass, hess, info)
	use kinds
	implicit none
	!--------------------------------------------------------------------
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: x0(3,n)
	real(DP), intent(in   ) :: mass(n)
	real(DP), intent(inout) :: hess(n*3,n*3)
	real(DP), intent(  out) :: info
	!--------------------------------------------------------------------
	real(DP), parameter     :: ONE=1.0_DP
	integer                 :: i, j, k, n3, ier
	real(DP)                :: xc(3,n)
	real(DP)                :: x, y, z
	real(DP)                :: P(3*n,3*n)
	real(DP)                :: TRVEC(3*n,3*n)
	real(DP)                :: moment(3,3), eval(3), vector(3,3)
	!--------------------------------------------------------------------
	info = 0
	!--------------------------------------------------------------------
	n3 = 3*n
	!--------------------------------------------------------------------
	!call inertm_fc(n, mass, xc, hess, vector)
	xc=x0
	call com(n, mass, xc, ier)
	moment = 0.D0
	do i=1,n
		x = xc(1,i)
		y = xc(2,i)
		z = xc(3,i)
		moment(1,1) = moment(1,1) + mass(i) * (y**2 + z**2)
		moment(2,2) = moment(2,2) + mass(i) * (z**2 + x**2)
		moment(3,3) = moment(3,3) + mass(i) * (x**2 + y**2)
		moment(2,1) = moment(2,1) - mass(i) * x * y
		moment(3,2) = moment(3,2) - mass(i) * y * z
		moment(1,3) = moment(1,3) - mass(i) * z * x
	end do
	moment(1,2) = moment(2,1)
	moment(3,1) = moment(1,3)
	moment(2,3) = moment(3,2)
	!--------------------------------------------------------------------
	call diag_symm (3,moment,eval,vector,ier)
	call swapn  (1, eval  (  1), eval  (  3))
	call swapn  (3, vector(1,1), vector(1,3))
	!--------------------------------------------------------------------
	! To ensure right-hand system of vector
	!
	vector(1,3)=vector(2,1)*vector(3,2)-vector(3,1)*vector(2,2)
	vector(2,3)=vector(3,1)*vector(1,2)-vector(1,1)*vector(3,2)
	vector(3,3)=vector(1,1)*vector(2,2)-vector(2,1)*vector(1,2)
	vector = transpose(vector)
	! vector      = 0.D0
	! vector(1,1) = 1.D0
	! vector(2,2) = 1.D0
	! vector(3,3) = 1.D0
	!--------------------------------------------------------------------
	call FormTRM(n,mass,xc,vector,TRVEC)
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
	! call MATAB(n3,n3,n3,hess,P,TRVEC,0)
	! call MATAB(n3,n3,n3,P,TRVEC,hess,0)
	!--------------------------------------------------------------------
! 	call set_sym_matrix(n3, hess)
	hess=matmul(P,matmul(hess,P))
! 	call set_sym_matrix(n3, hess)
	!call check_sym_matrix(n3, hess)
	!--------------------------------------------------------------------
	return
	end subroutine
	!
	!====================================================================
	!
	subroutine FormTRM(n,mass,xc,vec,V)
	use kinds
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: mass(n)
	real(DP), intent( in) :: xc(3,n)
	real(DP), intent( in) :: vec(3,3)
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
		cx = (qx*vec(1,1) + qy*vec(2,1) + qz*vec(3,1))
		cy = (qx*vec(1,2) + qy*vec(2,2) + qz*vec(3,2))
		cz = (qx*vec(1,3) + qy*vec(2,3) + qz*vec(3,3))
		!
		V(1,i,1) = ami
		V(2,i,2) = ami
		V(3,i,3) = ami
		!
		V(1,i,4) = -cz*vec(1,2) + cy*vec(1,3)
		V(2,i,4) = -cz*vec(2,2) + cy*vec(2,3)
		V(3,i,4) = -cz*vec(3,2) + cy*vec(3,3)
		!
		V(1,i,5) =  cz*vec(1,1) - cx*vec(1,3)
		V(2,i,5) =  cz*vec(2,1) - cx*vec(2,3)
		V(3,i,5) =  cz*vec(3,1) - cx*vec(3,3)
		!
		V(1,i,6) = -cy*vec(1,1) + cx*vec(1,2)
		V(2,i,6) = -cy*vec(2,1) + cx*vec(2,2)
		V(3,i,6) = -cy*vec(3,1) + cx*vec(3,2)
	end do
	!--------------------------------------------------------------------
	do i=1,6
		skal = SProd(n3,V(1,1,i),V(1,1,i))
		if(skal > TollZERO) then
			skal = ONE/SQRT(skal)
			call VScal(n3,skal,V(1,1,i))
		end if
	end do
	!--------------------------------------------------------------------
	return
	end subroutine
