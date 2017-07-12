subroutine celldm_to_lattice_constants(ibrav, celldm, a, b, c, alpha, beta, gamma)
	use kinds, only: DP
	use param, only: au2a
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: ibrav
	real(DP), intent( in) :: celldm(6)
	real(DP), intent(out) :: a, b, c, alpha, beta, gamma
	real(DP)              :: PI
	!----------------------------------------------------------------------------
	PI    = acos(-1.D0)
	a     = celldm(1) * au2a
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
	else if (ibrav==5) then
		b     = a
		c     = a
		alpha = acos(celldm(4)) / PI * 180
		beta  = alpha
		gamma = alpha
	else if (ibrav==8) then
		b     = celldm(2) * a
		c     = celldm(3) * a
	else
		write(*,*) "lib/celldm_to_a, not implemented!"
		write(*,*) "stop"
		stop
	end if
	!----------------------------------------------------------------------------
end subroutine
