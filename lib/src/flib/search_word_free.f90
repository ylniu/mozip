function search_word_free(fid, word, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	character(*), intent( in) :: word
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word_free
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	integer                   :: iostat
	!----------------------------------------------------------------------------
	search_word_free = .false.
	read(fid,'(a)',iostat=iostat) line
	!
	if (index(line,word)>0) then
		search_word_free = .true.
		return
	end if
	!
	do while( index(line,word) == 0 .and. iostat==0)
		read(fid,'(a)',iostat=iostat) line
		!
		if (index(line,word)/=0) then
			search_word_free = .true.
			return
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
