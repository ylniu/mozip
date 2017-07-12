function search_word_first_word(fid, word, line)
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
	logical                   :: search_word_first_word
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat, n
	!----------------------------------------------------------------------------
	search_word_first_word = .false.
	n=len(word)
	!
	iostat = 0
	do while(iostat==0)
		read(fid,'(a)',iostat=iostat) line
		if (iostat /= 0) exit
		!
		if (len_trim(line)/=0) then
			line=trim(adjustl(line))
			if( line(1:n) == word ) then
				search_word_first_word = .true.
				return
			end if
		end if
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
