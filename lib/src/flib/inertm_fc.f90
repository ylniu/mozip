subroutine inertm_fc(n, mass, x, fc, vector)
	use kinds
	implicit none
	!---------------------------------------------------------------------------
	! Input Variables
	!
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: mass(n)
	!---------------------------------------------------------------------------
	! Output Variables
	!
	real(DP), intent(inout) :: x(3,n)
	real(DP), intent(inout) :: fc(n*3,n*3)
	real(DP), intent(  out) :: vector(3,3)
	!---------------------------------------------------------------------------
	! Local Variables
	!
	integer                 :: ix , ier, iat
	integer                 :: n3
	real(DP)                :: mass_tot
	real(DP)                :: mcenter(3)
	real(DP)                :: eval(3)
	!real(DP)                :: wk(3)
	real(DP)                :: moment(3,3)
	real(DP), allocatable   :: fc1(:,:)
	real(DP), external      :: max_abs
	!---------------------------------------------------------------------------
	n3 = n*3
	allocate(fc1(n3,n3))
	!---------------------------------------------------------------------------
	! Find center of mass
	!
	mass_tot = 0.0_DP
	mcenter  = 0.0_DP
	vector   = 0.0_DP
	do iat=1, n
		do ix=1, 3
			mcenter(ix) = mcenter(ix) + mass(iat) * x(ix, iat)
		end do
		mass_tot = mass_tot + mass(iat)
	end do
	mcenter = mcenter / mass_tot
	do iat=1, n
		x(:,iat) = x(:,iat) - mcenter
	end do
	!----------------------------------------------------------------------------
	! Find moment of inertia
	!
	moment = 0.D0
	do iat=1, n
		moment(1,1)=moment(1,1) + mass(iat)*(x(2,iat)**2 + x(3,iat)**2)
		moment(2,2)=moment(2,2) + mass(iat)*(x(3,iat)**2 + x(1,iat)**2)
		moment(3,3)=moment(3,3) + mass(iat)*(x(1,iat)**2 + x(2,iat)**2)
		moment(1,2)=moment(1,2) - mass(iat)* x(1,iat)    * x(2,iat)
		moment(2,3)=moment(2,3) - mass(iat)* x(2,iat)    * x(3,iat)
		moment(3,1)=moment(3,1) - mass(iat)* x(3,iat)    * x(1,iat)
		moment(2,1)=moment(1,2)
		moment(3,2)=moment(2,3)
		moment(1,3)=moment(3,1)
	end do
	!----------------------------------------------------------------------------
	! Diagonalize inertia matrix
	!
	!call tred2e (3,3,moment,eval,wk,vector)
	!call tql2e  (3,3,       eval,wk,vector,ier)
	call diag_symm(3, moment, eval, vector, ier)
	do ix=1, 3
		if (max_abs(3,vector(1,ix))<0.D0) vector(:,ix) = -vector(:,ix)
	end do
	! call swapn  (1, eval  (  1), eval  (  3))
	! call swapn  (3, vector(1,1), vector(1,3))
	!----------------------------------------------------------------------------
	!
	! To ensure right-hand system of vector
	!
	vector(1,3)  = vector(2,1)*vector(3,2)-vector(3,1)*vector(2,2)
	vector(2,3)  = vector(3,1)*vector(1,2)-vector(1,1)*vector(3,2)
	vector(3,3)  = vector(1,1)*vector(2,2)-vector(2,1)*vector(1,2)
	!----------------------------------------------------------------------------
	vector = transpose(vector)
	!----------------------------------------------------------------------------
	! Rotate coordinates x
	!
	call rotn  (n, vector, x )
	call rot_fc(n, vector, fc)
	!----------------------------------------------------------------------------
	return
end subroutine
