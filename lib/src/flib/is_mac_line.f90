logical function is_mac_line(line)
	implicit none
	!--------------------------------------------------------------------
	character(*)          :: line
	!--------------------------------------------------------------------
	integer               :: nl
	integer               :: i, n
	character(19)         :: s
	logical    , external :: is_mac_string
	!--------------------------------------------------------------------
	is_mac_line = .false.
	nl          = len_trim(line) + 1
	n           = nl - 18
	do i=1, n
		s=line(i:i+18)
		if(is_mac_string(s)) then
			is_mac_line = .true.
			return
		end if
	end do
	return
end function
