function search_word_free_n(fid, ncyc, word, line)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	!
	integer     , intent( in) :: fid
	integer     , intent( in) :: ncyc
	character(*), intent( in) :: word
	!----------------------------------------------------------------------------
	! output variables
	!
	logical                   :: search_word_free_n
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: iostat
	integer                   :: n, m
	!----------------------------------------------------------------------------
	rewind(fid)
	search_word_free_n = .false.
	!
	read(fid,'(a)',iostat=iostat) line
	!
	n=0
	do while(iostat==0)
		if (index(line,word)>0) n = n + 1
		read(fid, '(a)', iostat=iostat) line
	end do
	!
	if (n==0 .or. ncyc>n ) return
	!
	rewind(fid)
	!
	m=0
	read(fid, '(a)', iostat=iostat) line
	do while(iostat==0)
		if (index(line,word)>0) then
			m = m + 1
			if (m==ncyc) then
				search_word_free_n = .true.
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
