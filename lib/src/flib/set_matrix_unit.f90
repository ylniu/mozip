subroutine set_matrix_unit(n,matrix)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent(out) :: matrix(n,n)
	!----------------------------------------------------------------------------
	integer               :: i
	!----------------------------------------------------------------------------
	matrix=0.0_DP
	do i=1, n
		matrix(i,i) = 1.0_DP
	end do
	!----------------------------------------------------------------------------
	return
end subroutine

