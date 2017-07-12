subroutine matab(nr, nc, ni, a, b, c, iacc)
	!-------------------------------------------------------------------------
	! c = a * b
	!-------------------------------------------------------------------------
	use kinds, only: DP
	implicit none
	!-------------------------------------------------------------------------
	integer , intent(in ) :: nr, nc, ni, iacc
	real(DP), intent(in ) :: a(nr, ni)
	real(DP), intent(in ) :: b(ni, nc)
	real(DP), intent(out) :: c(nr, nc)
	integer               :: i, j, k
	!-------------------------------------------------------------------------
	if(iacc == 0) call dzero(nr*nc,c)
	c=0.0_DP
	do i=1, nr
		do j=1, nc
			do k=1, ni
				c(i,j) = c(i,j) + a(i,k) * b(k,j)
			end do
		end do
	end do
	!-------------------------------------------------------------------------
	return
end subroutine
