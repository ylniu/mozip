subroutine set_sym_matrix(n, a)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent(in   ) :: n
	real(DP), intent(inout) :: a(n,n)
	!--------------------------------------------------------------------
	integer                 :: i, j
	!--------------------------------------------------------------------
	do i=2, n
		do j=1, i-1
			a(i,j) = (a(i,j) + a(j,i)) / 2.0_DP
			a(j,i) = a(i,j)
		end do
	end do
	!--------------------------------------------------------------------
	return
end subroutine
