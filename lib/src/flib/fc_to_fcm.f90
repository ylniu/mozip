subroutine fc_to_fcm(n3, sqtmass, fc, fcm, direction, info)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n3
	integer , intent( in) :: direction
	real(DP), intent( in) :: sqtmass(n3)
	real(DP)              :: fc (n3, n3)
	real(DP)              :: fcm(n3, n3)
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
			do j=1, i
				fcm(j,i) = fc (j,i) / sqtmass(j) / sqtmass(i)
				if (i/=j) fcm(i,j) = fcm(j,i)
			end do
		end do
	else if (direction==-1) then
		do i=1, n3
			do j=1, i
				fc (j,i) = fcm(j,i) * sqtmass(j) * sqtmass(i)
				if (i/=j) fc(i,j) = fc(j,i)
			end do
		end do
	else
		info=-2
	end if
	!--------------------------------------------------------------------
	return
end subroutine fc_to_fcm