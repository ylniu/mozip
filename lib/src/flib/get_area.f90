subroutine get_area(n, x, y, area)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent( in) :: n
	real(DP), intent( in) :: x(n)
	real(DP), intent( in) :: y(n)
	!----------------------------------------------------------------------------
	! Output variable
	!
	real(DP), intent(out) :: area
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i
	!----------------------------------------------------------------------------
	area=0.0_DP
	do i=1, n-1
		area = area + ( y(i+1) + y(i) ) * ( x(i+1) - x(i) ) / 2.0_DP
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
