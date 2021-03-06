logical function is_mac_string(s)
	implicit none
	!--------------------------------------------------------------------
	character(*)          :: s
	!--------------------------------------------------------------------
	integer               :: nl
	integer               :: i
	character(12)         :: s1
	character( 5)         :: s2
	character( 1)         :: s3
	character( 1)         :: s4
	logical    , external :: is_number_letter
	!--------------------------------------------------------------------
	is_mac_string = .false.
	nl            = len(s)
	!--------------------------------------------------------------------
	if (nl/=19) then
		write(*,'(2x, "Warning: string length is not 19 !")')
		return
	end if
	!--------------------------------------------------------------------
	s1( 1: 2) = s( 2: 3)
	s1( 3: 4) = s( 5: 6)
	s1( 5: 6) = s( 8: 9)
	s1( 7: 8) = s(11:12)
	s1( 9:10) = s(14:15)
	s1(11:12) = s(17:18)
	!--------------------------------------------------------------------
	s2( 1: 1) = s( 4: 4)
	s2( 2: 2) = s( 7: 7)
	s2( 3: 3) = s(10:10)
	s2( 4: 4) = s(13:13)
	s2( 5: 5) = s(16:16)
	!--------------------------------------------------------------------
	s3        = s( 1: 1)
	s4        = s(19:19)
	!--------------------------------------------------------------------
	if (.not. is_number_letter(s1)) return
	!--------------------------------------------------------------------
	do i=1, 5
		if(s2(i:i)/=":" .and. s2(i:i)/="-") return
	end do
	!--------------------------------------------------------------------
	if (iachar(s3)/=32) return
	!--------------------------------------------------------------------
	if (iachar(s4)/=32 .and. iachar(s4)/=10 .and. iachar(s4)/=13) return
	!--------------------------------------------------------------------
	is_mac_string = .true.
	!--------------------------------------------------------------------
	return
end function
