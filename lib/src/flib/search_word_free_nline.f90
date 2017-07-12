function search_word_free_nline(fid, word, nline, line)
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
	logical                   :: search_word_free_nline
	integer     , intent(out) :: nline
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	!----------------------------------------------------------------------------
	rewind(fid)
	search_word_free_nline = .false.
	!
	read(fid,'(a)',iostat=iostat) line
	!
	nline=0
	do while(iostat==0)
		if (index(line,word)>0) then
			nline = nline + 1
			search_word_free_nline = .true.
		end if
		read(fid, '(a)', iostat=iostat) line
	end do
	!
	return
	!----------------------------------------------------------------------------
end function