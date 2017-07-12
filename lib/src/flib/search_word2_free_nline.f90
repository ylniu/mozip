function search_word2_free_nline(fid, word1, word2, nline, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	character(*), intent( in) :: word1
	character(*), intent( in) :: word2
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word2_free_nline
	integer     , intent(out) :: nline
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	!----------------------------------------------------------------------------
	rewind(fid)
	search_word2_free_nline = .false.
	!
	read(fid,'(a)',iostat=iostat) line
	!
	nline=0
	do while(iostat==0)
		if (index(line,word1)>0 .and. index(line,word2)>0) then
			nline = nline + 1
			search_word2_free_nline = .true.
		end if
		read(fid, '(a)', iostat=iostat) line
	end do
	!
	return
	!----------------------------------------------------------------------------
end function