function determinant3(a)
	use kinds
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	real(DP), intent(in) :: a(3,3)
	real(DP)             :: determinant3
	!----------------------------------------------------------------------------
	determinant3 = 0.D0
	determinant3 = a(1,1) * a(2,2) * a(3,3) &
					 + a(1,2) * a(2,3) * a(3,1) &
					 + a(1,3) * a(2,1) * a(3,2) &
					 - a(1,3) * a(2,2) * a(3,1) &
					 - a(1,1) * a(2,3) * a(3,2) &
					 - a(1,2) * a(2,1) * a(3,3)
	!----------------------------------------------------------------------------
	return
end function
