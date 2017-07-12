subroutine a2b(a,b)
	use kinds, only: DP
	implicit none
	!------------------------------------------------------------------------------
	real(DP), intent( in) :: a(3,3)
	real(DP), intent(out) :: b(3,3)
	!------------------------------------------------------------------------------
	real(DP)              :: v, c
	!------------------------------------------------------------------------------
	v =   a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2) &
		 + a(1,2)*a(2,3)*a(3,1) - a(1,2)*a(2,1)*a(3,3) &
		 + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1)
	v = abs(v)
	c = 1.0_DP / v
	!------------------------------------------------------------------------------
	b(1,1) = c * ( a(2,2) * a(3,3) - a(3,2) * a(2,3) )
	b(2,1) = c * ( a(3,2) * a(1,3) - a(1,2) * a(3,3) )
	b(3,1) = c * ( a(1,2) * a(2,3) - a(2,2) * a(1,3) )
	!------------------------------------------------------------------------------
	b(1,2) = c * ( a(2,3) * a(3,1) - a(3,3) * a(2,1) )
	b(2,2) = c * ( a(3,3) * a(1,1) - a(1,3) * a(3,1) )
	b(3,2) = c * ( a(1,3) * a(2,1) - a(2,3) * a(1,1) )
	!------------------------------------------------------------------------------
	b(1,3) = c * ( a(2,1) * a(3,2) - a(3,1) * a(2,2) )
	b(2,3) = c * ( a(3,1) * a(1,2) - a(1,1) * a(3,2) )
	b(3,3) = c * ( a(1,1) * a(2,2) - a(2,1) * a(1,2) )
	!------------------------------------------------------------------------------
	return
end subroutine
