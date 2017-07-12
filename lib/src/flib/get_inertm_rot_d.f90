subroutine get_inertm_rot_d(n, mass, x, rot, d)
	use kinds
	implicit none
	!---------------------------------------------------------------------------
	! Input Variables
	!
	integer , intent( in) :: n
	real(DP), intent( in) :: mass(n)
	real(DP), intent( in) :: x(3,n)
	!---------------------------------------------------------------------------
	! Output Variables
	!
	real(DP), intent(out) :: rot(3,3), d(3)
	!---------------------------------------------------------------------------
	! Local Variables
	!
	integer  :: iat, ix, ier
	real(DP) :: mass_tot
	real(DP) :: x1(3,n)
	real(DP) :: mcenter(3)
	real(DP) :: eval(3)
	real(DP) :: wk(3)
	real(DP) :: moment(3,3)
	real(DP) :: vector(3,3)
	real(DP), external :: max_abs
	!---------------------------------------------------------------------------
	! Find center of mass
	!
	x1       = x
	mass_tot = 0.0_DP
	mcenter  = 0.0_DP
	vector   = 0.0_DP
	do iat=1, 3
		do ix=1, 3
			mcenter(ix) = mcenter(ix) + mass(iat) * x1(ix, iat)
		end do
		mass_tot = mass_tot + mass(iat)
	end do
	mcenter = mcenter / mass_tot
	d       = mcenter
	do iat=1, n
		x1(:,iat) = x1(:,iat) - mcenter
	end do
	!----------------------------------------------------------------------------
	! Find moment of inertia
	!
	moment = 0.D0
	do iat=1, n
		moment(1,1)=moment(1,1) + mass(iat)*(x1(2,iat)**2 + x1(3,iat)**2)
		moment(2,2)=moment(2,2) + mass(iat)*(x1(3,iat)**2 + x1(1,iat)**2)
		moment(3,3)=moment(3,3) + mass(iat)*(x1(1,iat)**2 + x1(2,iat)**2)
		moment(1,2)=moment(1,2) - mass(iat)* x1(1,iat)    * x1(2,iat)
		moment(2,3)=moment(2,3) - mass(iat)* x1(2,iat)    * x1(3,iat)
		moment(3,1)=moment(3,1) - mass(iat)* x1(3,iat)    * x1(1,iat)
		moment(2,1)=moment(1,2)
		moment(3,2)=moment(2,3)
		moment(1,3)=moment(3,1)
	end do
	!----------------------------------------------------------------------------
	! Diagonalize inertia matrix
	!
	call tred2e (3,3,moment,eval,wk,vector)
	call tql2e  (3,3,       eval,wk,vector,ier)
	do ix=1, 3
		if (max_abs(3,vector(1,ix))<0.D0) vector(:,ix) = -vector(:,ix)
	end do
	!call swapn  (1, eval  (  1), eval  (  3))
	!call swapn  (3, vector(1,1), vector(1,3))
	!----------------------------------------------------------------------------
	!
	! To ensure right-hand system of vector
	!
	vector(1,3)=vector(2,1)*vector(3,2)-vector(3,1)*vector(2,2)
	vector(2,3)=vector(3,1)*vector(1,2)-vector(1,1)*vector(3,2)
	vector(3,3)=vector(1,1)*vector(2,2)-vector(2,1)*vector(1,2)
	!---------------------------------------------------------------------------
	vector = transpose(vector)
	!---------------------------------------------------------------------------
	! Rotate coordinates x1
	!
	call rotn(n, vector, x1)
	rot=vector
	!---------------------------------------------------------------------------
	return
end subroutine
