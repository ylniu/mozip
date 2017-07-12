subroutine coord_cart_to_crys(natom, a, x)
	use kinds, only: DP
	use math , only: inverse3
	!----------------------------------------------------------------------------
	integer                 :: natom
	real(DP), intent(in   ) :: a(3,3)
	real(DP), intent(inout) :: x(3, natom)
	real(DP)                :: b(3,3)
	!----------------------------------------------------------------------------
	b=inverse3(a)
	call rotn(natom, b, x)
	!----------------------------------------------------------------------------
	return
end subroutine
