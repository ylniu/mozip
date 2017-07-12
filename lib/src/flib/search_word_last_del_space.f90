function search_word_last_del_space(fid, word, line)
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
	integer                   :: iostat
	logical                   :: search_word_last_del_space
	character(*), intent(out) :: line
	!----------------------------------------------------------------------------
	! local variables
	!
	integer                   :: n, m
	character(len=len(word))  :: word1
	character(len=len(line))  :: line1
	!----------------------------------------------------------------------------
	search_word_last_del_space = .false.
	rewind(fid)
	!
	!----------------------------------------------------------------------------
	! Delete space from word1
	!
	word1=word
	call del_space(word1)
	!----------------------------------------------------------------------------
	! Delete space from line1
	!
	read(fid, '(a)', iostat=iostat) line1
	call del_space(line1)
	!----------------------------------------------------------------------------
	n=0
	!----------------------------------------------------------------------------
	do while(iostat==0)
		if (index(line1,trim(word1))>0) n=n+1
		read(fid, '(a)', iostat=iostat) line1
		call del_space(line1)
	end do
	!
	line=line1
	rewind(fid)
	!
	if (n==0) return
	!
	m=0
	read(fid, '(a)', iostat=iostat) line
	line1=line
	call del_space(line1)
	do while(iostat==0)
		if (index(line1,trim(word1))>0) then
			m=m+1
			if (m==n) then
				search_word_last_del_space = .true.
				return
			end if
		end if
		!
		read(fid, '(a)', iostat=iostat) line
		line1=line
		call del_space(line1)
	end do
	!
	return
	!----------------------------------------------------------------------------
end function
