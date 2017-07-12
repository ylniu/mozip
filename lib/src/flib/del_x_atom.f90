subroutine del_x_atom(natom, natom1, x, x1, symbol, symbol1)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent( in) :: natom
	real(DP)    , intent( in) :: x(3, natom)
	character(*), intent( in) :: symbol(natom)
	integer     , intent(out) :: natom1
	real(DP)    , intent(out) :: x1(3, natom)
	character(*), intent(out) :: symbol1(natom)
	!----------------------------------------------------------------------------
	integer                   :: i
	!----------------------------------------------------------------------------
	natom1=0
	do i=1, natom
		if ( trim(symbol(i)) /= "x" .and. trim(symbol(i)) /= "X" ) then
			natom1=natom1+1
			x1     (:,natom1) = x     (:,i)
			symbol1(  natom1) = symbol(  i)
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
