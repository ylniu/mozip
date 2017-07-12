subroutine splinf(xa,ya,n,x,y,m)
	use kinds, only: DP
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer , intent( in) :: n
	integer , intent( in) :: m
	real(DP), intent( in) :: xa(n)
	real(DP), intent( in) :: ya(n)
	real(DP), intent( in) :: x (m)
	!--------------------------------------------------------------------
	! Output variables
	!
	real(DP), intent(out) :: y  (m)
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i
	real(DP)              :: yp1
	real(DP)              :: ypn
	real(DP), allocatable :: y2a(:)
	!--------------------------------------------------------------------
	! Get y2a, second derivative
	!
	allocate(y2a(n))
	!--------------------------------------------------------------------
	yp1 = (ya(2)-ya(  1)) / (xa(2) - xa(  1))
	ypn = (ya(n)-ya(n-1)) / (xa(n) - xa(n-1))
	!--------------------------------------------------------------------
	call spline(xa,ya,n,yp1,ypn,y2a)
	!--------------------------------------------------------------------
	! fit y
	!
	do i=1, m
		call splint(xa,ya,y2a,n,x(i),y(i))
	end do
	!--------------------------------------------------------------------
	deallocate(y2a)
	!--------------------------------------------------------------------
	return
end subroutine
