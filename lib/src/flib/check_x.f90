subroutine check_x(n, x, info)
	use kinds, only: DP
	implicit none
	!------------------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: x(3,n)
	integer , intent(out) :: info
	!------------------------------------------------------------------------------
	integer               :: i, j
	real(DP)              :: dx(3)
	real(DP)              :: bond
	real(DP)              :: eps
	!------------------------------------------------------------------------------
	info = 0
	!------------------------------------------------------------------------------
	eps= 0.1D0
	do i=1, n-1
		do j=i+1, n
			dx   = x(:,i) - x(:,j)
			bond = sqrt(dot_product(dx,dx))
			if (bond<eps) then
				info=-1
				return
			end if
		end do
	end do
	!------------------------------------------------------------------------------
end subroutine
