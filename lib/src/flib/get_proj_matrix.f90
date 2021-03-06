subroutine get_proj_matrix(n, a, m)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: a(n)
	real(DP), intent(out) :: m(n,n)
	!----------------------------------------------------------------------------
	integer               :: i, j
	!----------------------------------------------------------------------------
	m = 0.D0
	do i=1, n
		do j=1, n
			m(i,j) = a(i) * a(j)
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine