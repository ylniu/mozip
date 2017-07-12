function search_word_back(fid, i, j, word, line)
	!----------------------------------------------------------------------------
	IMPLICIT NONE
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in)           :: fid, i, j
	character(*), intent( in)           :: word
	!----------------------------------------------------------------------------
	! output variables
	!
	integer                   :: iostat
	logical                   :: search_word_back
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	search_word_back = .false.
	read(fid,'(a)',iostat=iostat) line
	!
	if (line(i:j) == word .and. iostat==0) then
		search_word_back = .true.
		return
	end if
	!
	do while(line(i:j) /= word .and. iostat==0)
		backspace(fid)
		backspace(fid)
		read(fid,'(a)',iostat=iostat) line
		!
		if (line(i:j) == word .and. iostat==0) then
			search_word_back = .true.
			return
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
