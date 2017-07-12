function search_word_free_last(fid, word, line)
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
	logical                   :: search_word_free_last
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	integer                   :: n, m
	!----------------------------------------------------------------------------
	rewind(fid)
	search_word_free_last = .false.
	!
	read(fid,'(a)',iostat=iostat) line
	!
	n=0
	do while(iostat==0)
		if (index(line,word)>0) n = n + 1
		read(fid, '(a)', iostat=iostat) line
	end do
	!
	if (n==0) return
	!
	rewind(fid)
	!
	m=0
	read(fid, '(a)', iostat=iostat) line
	do while(iostat==0)
		if (index(line,word)>0) then
			m = m + 1
			if (m==n) then
				search_word_free_last = .true.
				return
			end if
		end if
		!
		read(fid, '(a)', iostat=iostat) line
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
