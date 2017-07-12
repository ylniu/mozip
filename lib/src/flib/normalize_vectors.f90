subroutine normalize_vectors(n, m, vectors)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer , intent(in   ) :: n, m
	real(DP), intent(inout) :: vectors(n,m)
	!----------------------------------------------------------------------------
	integer                 :: i, j
	real(DP)                :: r
	!----------------------------------------------------------------------------
	do i=1, m
		r=0.0_DP
		do j=1, n
			r = r + vectors(j,i)**2
		end do
		r = sqrt(r)
		vectors(:,i) = vectors(:,i) / r
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
