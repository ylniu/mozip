subroutine coord_crys_to_cart(natom, a, x)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer                 :: natom
	real(DP), intent(in   ) :: a(3,3)
	real(DP), intent(inout) :: x(3, natom)
	!----------------------------------------------------------------------------
	call rotn(natom, a, x)
	!----------------------------------------------------------------------------
	return
end subroutine
