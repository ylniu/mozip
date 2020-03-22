subroutine normalize_ref(n, x,  y, xr, yr)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: xr
	real(DP), intent(in   ) :: yr
	real(DP), intent(in   ) :: x(n)
	!--------------------------------------------------------------------
	real(DP), intent(inout) :: y(n)
	!--------------------------------------------------------------------
	! Local  variables
	!
	real(DP)              :: y1
	real(DP)              :: yn
	real(DP)              :: yr1
	real(DP)              :: s
	real(DP), allocatable :: y2(:)
	!--------------------------------------------------------------------
	! Get y2a, second derivative
	!
	allocate(y2(n))
	!--------------------------------------------------------------------
	y1 = (y(2)-y(  1)) / (x(2) - x(  1))
	yn = (y(n)-y(n-1)) / (x(n) - x(n-1))
	!--------------------------------------------------------------------
	call spline(x,y,n,y1,yn,y2)
	!--------------------------------------------------------------------
	! fit y
	!
	call splint(x,y,y2,n,xr,yr1)
	s = yr / yr1
	y = y * s
	!--------------------------------------------------------------------
	deallocate(y2)
	!--------------------------------------------------------------------
end subroutine