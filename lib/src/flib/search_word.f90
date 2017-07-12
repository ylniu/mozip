function search_word(fid, i, j, word, line)
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
	logical                   :: search_word
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	search_word = .false.
	read(fid,'(a)',iostat=iostat) line
	!
	if (line(i:j) == word .and. iostat==0) then
		search_word = .true.
		return
	end if
	!
	do while(line(i:j) /= word .and. iostat==0)
		read(fid,'(a)',iostat=iostat) line
		!
		if (line(i:j) == word .and. iostat==0) then
			search_word = .true.
			return
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
