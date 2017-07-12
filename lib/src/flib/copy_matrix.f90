subroutine matrix_copy(nrow1,ncol1,m1,nrow2,ncol2,m2,info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: nrow1, ncol1
	integer , intent( in) :: nrow2, ncol2
	real(DP), intent( in) :: m1(nrow1, ncol1)
	real(DP), intent(out) :: m2(nrow2, ncol2)
	integer , intent(out) :: info
	!----------------------------------------------------------------------------
	integer               :: nr, nc, ir, ic
	!----------------------------------------------------------------------------
	info = 0
	nr   = min(nrow1, nrow2)
	nc   = min(ncol1, ncol2)
	!----------------------------------------------------------------------------
	m2=0.D0
	!----------------------------------------------------------------------------
	do ir=1, nr
		do ic=1, nc
			m2(ir,ic) = m1(ir,ic)
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
