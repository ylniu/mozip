subroutine celldm_to_a(ibrav, celldm, aa)
	use kinds, only: DP
	use param, only: au2a
	use math , only: v3_cross, v3_normlize, rotvec
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: ibrav
	real(DP), intent( in) :: celldm(6)
	real(DP), intent(out) :: aa(3,3)
	real(DP)              :: aa1(3,3)
	real(DP)              :: a, b, c, alpha, beta, gamma
	real(DP)              :: ab(3), bc(3), na(3), cosa, a1(3)
	real(DP)              :: PI
	!----------------------------------------------------------------------------
	PI    = acos(-1.D0)
	a     = celldm(1)
	alpha = acos( 0.D0) / PI * 180.D0
	beta  = acos( 0.D0) / PI * 180.D0
	gamma = acos( 0.D0) / PI * 180.D0
	!----------------------------------------------------------------------------
	if (ibrav==4) then
		b     = a
		c     = celldm(3) * a
		alpha =  90.D0
		beta  =  90.D0
		gamma = 120.D0
		call lattice_constants_to_a(aa1, a, b, c, alpha, beta, gamma)
	else if (ibrav==5) then
		b     = a
		c     = a
		alpha = acos(celldm(4)) / PI * 180
		beta  = alpha
		gamma = alpha
		call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
		a1    = aa(:,1)
		a1    = v3_normlize(a1)
		ab    = aa(:,2) - aa(:,1)
		bc    = aa(:,3) - aa(:,2)
		na    = v3_normlize(v3_cross(ab, bc))
		cosa  = dot_product(na, a1)
		!
		aa1(1,2) = 0.D0
		aa1(3,2) = a * cosa
		aa1(2,2) = sqrt(a**2 - aa1(3,2)**2)
		!
		aa1(:,1) = aa1(:,2)
		aa1(:,3) = aa1(:,2)
		call rotvec(aa1(1,3), 120.D0, 3)
		call rotvec(aa1(1,1), 240.D0, 3)
	else if (ibrav==8) then
		b     = celldm(2) * a
		c     = celldm(3) * a
		call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
	else
		write(*,*) "lib/celldm_to_a, not implemented!"
		write(*,*) "stop"
		stop
	end if
	!----------------------------------------------------------------------------
	aa = aa1 * au2a
	!----------------------------------------------------------------------------
end subroutine
