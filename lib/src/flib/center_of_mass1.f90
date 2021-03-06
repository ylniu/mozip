subroutine com1(n, mass, xc, x, y, z)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	real(DP), parameter     :: ZERO=0.0_DP
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: mass(n)
	real(DP), intent(inout) :: xc(3, n)
	!----------------------------------------------------------------------------
	integer                 :: i
	real(DP)                :: mt
	real(DP)                :: x, y, z
	!----------------------------------------------------------------------------
	mt = ZERO
	x  = ZERO
	y  = ZERO
	z  = ZERO
	!----------------------------------------------------------------------------
	do i=1, n
		mt = mt + mass(i)
		x  = x + mass(i) * xc(1, i)
		y  = y + mass(i) * xc(2, i)
		z  = z + mass(i) * xc(3, i)
	end do
	!----------------------------------------------------------------------------
	x = x/mt
	y = y/mt
	z = z/mt
	!----------------------------------------------------------------------------
	do i=1, n
		xc(1,i) = xc(1,i) - x
		xc(2,i) = xc(2,i) - y
		xc(3,i) = xc(3,i) - z
	end do
	!----------------------------------------------------------------------------
	return
end
