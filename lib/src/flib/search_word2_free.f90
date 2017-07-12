function search_word2_free(fid, word1, word2, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	character(*), intent( in) :: word1, word2
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word2_free
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	integer                   :: iostat
	!----------------------------------------------------------------------------
	search_word2_free=.false.
	read(fid,'(a)',iostat=iostat) line
	!
	if ( index(line,trim(word1))>0 .and. index(line,trim(word2))>0 ) then
		search_word2_free = .true.
		return
	end if
	!
	do while( ( index(line,trim(word1)) == 0 .or. index(line,trim(word2)) == 0 ) .and. iostat==0)
		read(fid,'(a)',iostat=iostat) line
		!
		if ( index(line,trim(word1))>0 .and. index(line,trim(word2))>0 .and. iostat==0) then
			search_word2_free = .true.
			return
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
