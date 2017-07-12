function search_word_last(fid, i, j, word, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in)           :: fid, i, j
	character(*), intent( in)           :: word
	!----------------------------------------------------------------------------
	! output variables
	!
	integer                   :: iostat
	logical                   :: search_word_last
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: n, m
	!----------------------------------------------------------------------------
	rewind(fid)
	search_word_last = .false.
	!
	n=0
	read(fid, '(a)', iostat=iostat) line
	do while(iostat==0)
		if (line(i:j) == word) n = n + 1
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
		if (line(i:j) == word) then
			m = m + 1
			if (m==n) then
				search_word_last = .true.
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
