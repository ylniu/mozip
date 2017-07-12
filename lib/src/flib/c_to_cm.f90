subroutine c_to_cm(n3, sqtmass, c, cm, direction, info)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n3
	integer , intent( in) :: direction
	real(DP), intent( in) :: sqtmass(n3)
	real(DP)              :: c (n3, n3)
	real(DP)              :: cm(n3, n3)
	integer , intent(out) :: info
	!--------------------------------------------------------------------
	integer               :: i, j
	!--------------------------------------------------------------------
	info = 0
	!--------------------------------------------------------------------
	if (n3 <=0) then
		info = -1
		return
	end if
	!--------------------------------------------------------------------
	if (direction==1) then
		do i=1, n3
			do j=1, n3
				cm(j,i) = c (j,i) / sqtmass(j)
			end do
		end do
	else if (direction==-1) then
		do i=1, n3
			do j=1, n3
				c (j,i) = cm(j,i) * sqtmass(j)
			end do
		end do
	else
		info=-2
	end if
	!--------------------------------------------------------------------
	return
end subroutine c_to_cm
