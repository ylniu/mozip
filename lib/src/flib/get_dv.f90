subroutine get_dv(n, v1, v2, d, info)
	use kinds, only: DP
	implicit none
	integer , intent( in) :: n
	real(DP), intent( in) :: v1(n)
	real(DP), intent( in) :: v2(n)
	real(DP), intent(out) :: d
	integer , intent(out) :: info
	integer               :: i
	!----------------------------------------------------------------------------
	info=0
	if (n<=0) then
		info=-1
		return
	end if
	d=0.0_DP
	do i=1, n
		d = d + (v1(i) - v2(i))**2
	end do
	d = sqrt(d/n)
	!----------------------------------------------------------------------------
	return
end subroutine get_dv
