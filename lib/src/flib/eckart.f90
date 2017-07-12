subroutine eckart(n,x1,x2,mass,ifix,rot,ier)
	use kinds
	!----------------------------------------------------------------------------
	!
	! Reference 1:
	!   The Franck-Condon principle for polyatomic molecules
	!   N. J. D. Lucas
	!   J. Phys. B: Atom. Moles. Phys., Vol. 6, 155-163 January 1973
	!
	! Reference 2:
	!   Computation of the pseudorotation matrix to satisfy the Eckart axis conditions
	!   Anatoly Y. Dymarsky
	!   Konstantin N. Kudin
	!   J. Chem. Phys. Vol. 122, 124103, 2005
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	!
	! Input variables
	!
	integer,  intent(in   ) :: n
	integer,  intent(in   ) :: ifix
	real(DP), intent(in   ) :: mass(n)
	!----------------------------------------------------------------------------
	!
	! Output variables
	!
	integer,  intent(  out) :: ier
	real(DP), intent(  out) :: rot(3,3)
	real(DP), intent(inout) :: x1 (3,n)
	real(DP), intent(inout) :: x2 (3,n)
	!----------------------------------------------------------------------------
	!
	! Local variables
	!
	integer                 :: i,j,k,iat
	integer                 :: imin
	real(DP)                :: c1, xmin
	real(DP)                :: xr (3,n)
	real(DP)                :: xr1(3,n)
	real(DP)                :: xf (3,n)
	real(DP)                :: dx (3,n)
	real(DP)                :: A(3,3),A1(3,3),A2(3,3),it,P(3,3,3),TT(3,3)
	real(DP)                :: T(3,3,4)
	!----------------------------------------------------------------------------
	!
	! LAPACK
	!
	integer :: info
	!
	real(8) :: d1(3),e1(3),z1(3,3),d2(3),e2(3),z2(3,3)
	!----------------------------------------------------------------------------
	!
	! Set Default value
	!
	ier=0
	!----------------------------------------------------------------------------
	!
	! Default: ifix = 1
	!
	select case (ifix)
		case(1)
			xr=x1
			xf=x2
		case(2)
			xr=x2
			xf=x1
		case default
			ier=1
			return
	end select
	!----------------------------------------------------------------------------
	!
	! A = x1^T * mass * x2 = x2^T * mass * x1
	!
	A =0.0_DP
	do i=1, 3
		do j=1, 3
			do k=1, n
				A(i,j)=A(i,j)+mass(k)*x1(i,k)*x2(j,k)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	!
	! A1 = A   * A^T
	! A2 = A^T * A
	!
	A1=0.0_DP
	A2=0.0_DP
	do i=1,3
		do j=1,3
			do k=1,3
				A1(i,j) = A1(i,j)+A(i,k)*A(j,k)
				A2(i,j) = A2(i,j)+A(k,i)*A(k,j)
			end do
		end do
	end do
	!-----------------------------------------------------------------------------
	do i=1,3
		d1(i) = A1(i,i)
		d2(i) = A2(i,i)
	end do
	!-----------------------------------------------------------------------------
	do i=1,2
		e1(i) = A1(i,i+1)
		e2(i) = A2(i,i+1)
	end do
	!-----------------------------------------------------------------------------
	!
	! z1 and z2 are vectors of A1 and A2
	!
	call tred2e(3,3,A1,d1,e1,z1)
	call tql2e(3,3,d1,e1,z1,info)
	!
	call tred2e(3,3,A2,d2,e2,z2)
	call tql2e(3,3,d2,e2,z2,info)
	!-----------------------------------------------------------------------------
	!
	! To ensure right-hand system of z1
	!
	z1(1,3)=z1(2,1)*z1(3,2)-z1(3,1)*z1(2,2)
	z1(2,3)=z1(3,1)*z1(1,2)-z1(1,1)*z1(3,2)
	z1(3,3)=z1(1,1)*z1(2,2)-z1(2,1)*z1(1,2)
	!-----------------------------------------------------------------------------
	do k=1,2
		!--------------------------------------------------------------------------
		!
		! To ensure the angles of (z1(:,1), z2(:,1)) and (z1(:,2), z2(:,2)) are
		! less then 90 degree (acute angles)
		!
		it=0.0_DP
		do i=1,3
			it = it + z1(i,k) * z2(i,k)
		end do
		!
		if ( it < 0.0_DP ) then
			z2(:,k) = - z2(:,k)
		end if
	end do
	!-----------------------------------------------------------------------------
	!
	! To ensure right-hand system of z2
	!
	z2(1,3)=z2(2,1)*z2(3,2)-z2(3,1)*z2(2,2)
	z2(2,3)=z2(3,1)*z2(1,2)-z2(1,1)*z2(3,2)
	z2(3,3)=z2(1,1)*z2(2,2)-z2(2,1)*z2(1,2)
	!-----------------------------------------------------------------------------
	!
	! P = z2^T * z1
	!
	do k=1,3
		do i=1,3
			do j=1,3
				P(i,j,k)=z2(i,k)*z1(j,k)
			end do
		end do
	end do
	!-----------------------------------------------------------------------------
	!
	! Construct four kinds of rotations
	!
	T(:,:,1) =   P(:,:,1) + P(:,:,2) + P(:,:,3)
	T(:,:,2) = - P(:,:,1) - P(:,:,2) + P(:,:,3)
	T(:,:,3) =   P(:,:,1) - P(:,:,2) - P(:,:,3)
	T(:,:,4) = - P(:,:,1) + P(:,:,2) - P(:,:,3)
	!-----------------------------------------------------------------------------
	!
	! Find the right rotation
	!
	xmin=1.E99_DP
	xr1=xr
	do i=1, 4
		xr=xr1
		call rotn(n, T(1,1,i), xr)
		dx=xf-xr
		c1=0.0_DP
		do iat=1, n
			do j=1, 3
				c1 = c1 + mass(iat) * dx(j,iat)**2
			end do
		end do
		if (xmin>c1) then
			xmin=c1
			imin=i
		end if
	end do
	rot=T(:,:,imin)
	!-----------------------------------------------------------------------------
	!
	! Test orthogonality
	!
	TT=matmul(transpose(rot), rot)
	c1=0.0_DP
	do i=1, 3
		do j=1, 3
			if (i==j) then
				c1 = c1 + abs(abs(TT(i,j)) - 1.0_DP)
			else
				c1 = c1 + abs(abs(TT(i,j)) - 0.0_DP)
			end if
		end do
	end do
	!
	if (c1>1.E-3_DP) then
		ier=2
		return
	end if
	!-----------------------------------------------------------------------------
	!
	! Rotate the molecule
	!
	xr=xr1
	call rotn(n, rot, xr)
	select case (ifix)
		case(1)
			x1=xr
		case(2)
			x2=xr
	end select
	!-----------------------------------------------------------------------------
	return
end subroutine
