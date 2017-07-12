function search_word_first_number(fid, line)
	!----------------------------------------------------------------------------
	IMPLICIT NONE
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word_first_number
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	character(200)            :: tmp
	!----------------------------------------------------------------------------
	logical, external         :: is_number
	!----------------------------------------------------------------------------
	search_word_first_number = .false.
	read(fid,'(a)',iostat=iostat) line
	!
	if (len_trim(line)/=0) then
		read(line,*) tmp
		if( is_number(tmp) ) then
			search_word_first_number = .true.
			return
		end if
	end if
	!
	do while(iostat==0)
		read(fid,'(a)',iostat=iostat) line
		!
		if (len_trim(line)/=0) then
			read(line,*) tmp
			if( is_number(tmp) ) then
				search_word_first_number = .true.
				return
			end if
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
