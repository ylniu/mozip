subroutine zmat_to_xyz(natom, zrel, zmat, x)
	use kinds
	use math, only: v3_cross
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: natom
	integer , intent( in) :: zrel(3,natom)
	real(DP), intent( in) :: zmat(3,natom)
	real(DP), intent(out) :: x   (3,natom)
	!----------------------------------------------------------------------------
	integer               :: i, j
	real(DP)              :: pi
	real(DP)              :: rot (3,3)
	real(DP)              :: rota(3,3)
	real(DP)              :: rotb(3,3)
	real(DP)              :: rotc(3,3)
	real(DP)              :: bond     (natom)
	real(DP)              :: angle    (natom)
	real(DP)              :: dihedral (natom)
	real(DP)              :: x0(3), x1(3), x2(3), x3(3)
	real(DP)              :: e1(3), e2(3), e3(3), a, b
	real(DP)              :: eps
	!----------------------------------------------------------------------------
	! zmat(1,n): bond
	! zmat(2,n): angle
	! zmat(3,n): dihedral
	!----------------------------------------------------------------------------
	! Atom 1: ( 0,  0,  0)
	! Atom 2: ( 0,  0, z2)
	! Atom 3: (x3,  0, z3)
	! Atom 4: (x4, y4, z4)
	! ......
	!----------------------------------------------------------------------------
	eps      = 1.D-20
	pi       = acos(-1.0_DP)
	bond     = 0.0_DP
	angle    = 0.0_DP
	dihedral = 0.0_DP
	!----------------------------------------------------------------------------
	do i=1, natom
		bond(i)     = zmat(1,i)
	end do
	!----------------------------------------------------------------------------
	do i=2, natom
		angle(i)    = zmat(2,i) / 180.0_DP * pi
	end do
	!----------------------------------------------------------------------------
	do i=3, natom
		dihedral(i) = zmat(3,i) / 180.0_DP * pi
	end do
	!----------------------------------------------------------------------------
	x  = 0.D0
	x0 = 0.D0
	call set_matrix_unit(3,rot)
	!----------------------------------------------------------------------------
	! For Atom 2
	!
	x(3,2)   = bond(2)
	if(natom==2) return
	!----------------------------------------------------------------------------
	! For Atom 3
	!
	x0(3)    = bond(3)
	a        = angle(3)
	call set_matrix_unit(3,rot)
	rot(1,1) = cos(a)
	rot(1,3) = sin(a)
	rot(3,1) =-sin(a)
	rot(3,3) = cos(a)
	call rotn(1, rot, x0)
	x(:,3)   = x0
	if(natom==3) return
	!----------------------------------------------------------------------------
	do j=4, natom
		!-------------------------------------------------------------------------
		! the coordinates for the jth atom
		!
		x1 = x(:,zrel(1,j))
		x2 = x(:,zrel(2,j))
		x3 = x(:,zrel(3,j))
		!
		e3 = x2 - x1
		call normalize_vectors(3, 1, e3)
		!-------------------------------------------------------------------------
		if ( abs(angle(j)) < eps ) then
			x0 =  e3 * bond(j)
		else if ( abs(angle(j)-180.D0) < eps ) then
			x0 = -e3 * bond(j)
		else
			!----------------------------------------------------------------------
			! Plane 2-3-4
			!
			!----------------------------------------------------------------------
			e1 = x3 - x2
			e2 = v3_cross(e3, e1)
			e1 = v3_cross(e2, e3)
			call normalize_vectors(3, 1, e1)
			call normalize_vectors(3, 1, e2)
			!----------------------------------------------------------------------
			x0    = 0.0_DP
			x0(3) = bond(j)
			a     = angle(j)
			b     = dihedral(j)
			!----------------------------------------------------------------------
			! rotate angle around y
			!
			call set_matrix_unit(3,rota)
			rota(1,1) = cos(a)
			rota(1,3) = sin(a)
			rota(3,1) =-sin(a)
			rota(3,3) = cos(a)
			!----------------------------------------------------------------------
			! rotate -dihedral around z
			!
			call set_matrix_unit(3,rotb)
			rotb(1,1) = cos(b)
			rotb(1,2) = sin(b)
			rotb(2,1) =-sin(b)
			rotb(2,2) = cos(b)
			!----------------------------------------------------------------------
			rotc(:,1) = e1
			rotc(:,2) = e2
			rotc(:,3) = e3
			!----------------------------------------------------------------------
			call rotn(1, rota, x0)
			call rotn(1, rotb, x0)
			call rotn(1, rotc, x0)
		end if
		x0 = x0 + x1
		x(:,j)= x0
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
