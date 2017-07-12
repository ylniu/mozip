function search_word_first_letter(fid, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word_first_letter
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	character(  1)            :: c
	character(200)            :: tmp
	!----------------------------------------------------------------------------
	logical, external         :: is_letter
	!----------------------------------------------------------------------------
	search_word_first_letter = .false.
	read(fid,'(a)',iostat=iostat) line
	!
	if (len_trim(line)/=0) then
		read(line,*) tmp
		c=tmp(1:1)
		if( is_letter(c) ) then
			search_word_first_letter = .true.
			return
		end if
	end if
	!
	do while(iostat==0)
		read(fid,'(a)',iostat=iostat) line
		!
		if (len_trim(line)/=0) then
			read(line,*) tmp
			c=tmp(1:1)
			if( is_letter(c) ) then
				search_word_first_letter = .true.
				return
			end if
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
