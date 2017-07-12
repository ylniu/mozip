subroutine integration(n, x, y, s, itype)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	integer , intent( in) :: itype
	real(DP), intent( in) :: x(n)
	real(DP), intent( in) :: y(n)
	!--------------------------------------------------------------------
	real(DP), intent(out) :: s
	!--------------------------------------------------------------------
	integer               :: i
	!--------------------------------------------------------------------
	if (itype==1) then
		s=0.0_DP
		do i=1, n-1
			s = s + 0.5_DP * ( y(i) + y(i+1) ) / ( x(i+1) - x(i) )
		end do
	end if
	!--------------------------------------------------------------------
end subroutine