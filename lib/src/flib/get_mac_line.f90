subroutine get_mac_line(line, mac)
	implicit none
	!--------------------------------------------------------------------
	character(*)          :: line
	character(*)          :: mac
	!--------------------------------------------------------------------
	integer               :: nl, n, i
	character(19)         :: s
	logical    , external :: is_mac_string
	!--------------------------------------------------------------------
	nl          = len_trim(line) + 1
	n           = nl - 18
	mac         = ""
	do i=1, n
		s=line(i:i+18)
		if(is_mac_string(s)) then
			mac(1:17)=s(2:18)
			return
		end if
	end do
	!--------------------------------------------------------------------
	return
	!--------------------------------------------------------------------
end subroutine
