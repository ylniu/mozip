subroutine spline(x,y,n,yp1,ypn,y2)
	use kinds, only: DP
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer,  intent( in) :: n
	real(DP), intent( in) :: yp1
	real(DP), intent( in) :: ypn
	real(DP), intent( in) :: x(n)
	real(DP), intent( in) :: y(n)
	!--------------------------------------------------------------------
	! Output variables
	!
	real(DP), intent(out) :: y2(n)
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i, k
	real(DP)              :: p, qn
	real(DP)              :: sig, un
	real(DP)              :: eps
	real(DP), allocatable :: u(:)
	!--------------------------------------------------------------------
	eps = 0.99E30_DP
	!--------------------------------------------------------------------
	allocate(u(n))
	!--------------------------------------------------------------------
	if (yp1 > eps) then
		y2(1) = 0.0_DP
		u (1) = 0.0_DP
	else
		y2(1) = -0.5_DP
		u (1) = (3.0_DP/(x(2)-x(1))) * ((y(2)-y(1))/(x(2)-x(1))-yp1)
	end if
	!--------------------------------------------------------------------
	do i=2, n-1
		sig   = (x(i)-x(i-1)) / (x(i+1)-x(i-1))
		p     = sig * y2(i-1) + 2.0_DP
		y2(i) = (sig-1.0_DP) / p
		u(i)  = ( 6.0_DP * ((y(i+1)-y(i))/(x(i+1)-x(i)) &
					- (y(i)-y(i-1))/(x(i)-x(i-1))) &
					/ (x(i+1)-x(i-1))-sig*u(i-1) ) / p
	end do
	!--------------------------------------------------------------------
	if (ypn > eps) then
		qn = 0.0_DP
		un = 0.0_DP
	else
		qn = 0.5_DP
		un = (3.0_DP/(x(n)-x(n-1))) * (ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
	endif
	!--------------------------------------------------------------------
	y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
	!--------------------------------------------------------------------
	do k=n-1, 1, -1
      y2(k) = y2(k) * y2(k+1) + u(k)
	end do
	!--------------------------------------------------------------------
	deallocate(u)
	!--------------------------------------------------------------------
	return
end