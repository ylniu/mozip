subroutine inertmv_itype(n, mass, x, rot, itype)
	use kinds, only: DP
	implicit none
	!---------------------------------------------------------------------------
	! Input Variables
	!
	integer , intent(in   ) :: n, itype
	real(DP), intent(in   ) :: mass(n)
	!---------------------------------------------------------------------------
	! Output Variables
	!
	real(DP), intent(inout) :: rot(3,3)
	real(DP), intent(inout) :: x(3,n)
	!---------------------------------------------------------------------------
	! Local Variables
	!
	integer               :: iat, ix
	integer               :: info
	real(DP)              :: eval(3)
	real(DP)              :: moment(3,3)
	real(DP)              :: vecrots(3,3,4)
	real(DP)              :: vecrot(3,3)
	real(DP)              :: vector(3,3)
	real(DP), external    :: max_abs
	!----------------------------------------------------------------------------
	! center of mass
	!
	call com(n, mass, x, info)
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
	call diag_symm(3,moment,eval,vector,info)
	do ix=1, 3
		if (max_abs(3,vector(1,ix))<0.D0) vector(:,ix) = -vector(:,ix)
	end do
	!----------------------------------------------------------------------------
	! To ensure right-hand system of vector
	!
	vector(1,3)=vector(2,1)*vector(3,2)-vector(3,1)*vector(2,2)
	vector(2,3)=vector(3,1)*vector(1,2)-vector(1,1)*vector(3,2)
	vector(3,3)=vector(1,1)*vector(2,2)-vector(2,1)*vector(1,2)
	!----------------------------------------------------------------------------
	vector = transpose(vector)
	!----------------------------------------------------------------------------
	vecrots = 0.D0
	!
	vecrots(1,1,1) =  1.D0
	vecrots(2,2,1) =  1.D0
	vecrots(3,3,1) =  1.D0
	!
	vecrots(1,1,2) =  1.D0
	vecrots(2,2,2) = -1.D0
	vecrots(3,3,2) = -1.D0
	!
	vecrots(1,1,3) = -1.D0
	vecrots(2,2,3) =  1.D0
	vecrots(3,3,3) = -1.D0
	!
	vecrots(1,1,4) = -1.D0
	vecrots(2,2,4) = -1.D0
	vecrots(3,3,4) =  1.D0
	!---------------------------------------------------------------------------
	vecrot = vecrots(:,:,itype)
	rot    = matmul(vecrot, vector )
	call rotn(n, rot, x)
	!---------------------------------------------------------------------------
	return
end subroutine
