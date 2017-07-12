subroutine get_coul(natom, x, charge, coul)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: natom
	real(DP), intent( in) :: x     (3,natom)
	real(DP), intent( in) :: charge(  natom)
	real(DP), intent(out) :: coul
	!----------------------------------------------------------------------------
	integer               :: i, j
	real(DP)              :: dx(3), dr
	!----------------------------------------------------------------------------
	coul = 0.D0
	!----------------------------------------------------------------------------
	do i=1, natom - 1
		do j=i+1, natom
			dx = x(:,j) - x(:,i)
			dr = sqrt(dot_product(dx,dx))
			coul = coul + charge(i) * charge(j) / dr
		end do
	end do
	!----------------------------------------------------------------------------
	coul = coul * 2.0_DP
	!----------------------------------------------------------------------------
	return
end subroutine
