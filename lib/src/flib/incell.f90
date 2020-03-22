subroutine incell(natom, x)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer  :: natom
	real(DP) :: x(3, natom)
	!----------------------------------------------------------------------------
	integer  :: i, j
	!----------------------------------------------------------------------------
	do i=1, natom
		do j=1, 3
			do while (x(j,i)<0.D0 .or. x(j,i) >= 1.D0)
				if (x(j,i)< 0.D0) x(j,i) = x(j,i) + 1.D0
				if (x(j,i)>=1.D0) x(j,i) = x(j,i) - 1.D0
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine