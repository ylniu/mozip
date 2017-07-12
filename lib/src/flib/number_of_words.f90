function number_of_words(words)
	!----------------------------------------------------------------------------
	! if words="a1 a2 a3"
	! then number_of_words(words) = 3
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	character(*) :: words
	integer      :: number_of_words
	!----------------------------------------------------------------------------
	integer      :: ln, i, j, n
	character(1) :: c
	logical      :: find_cha, find_space
	!----------------------------------------------------------------------------
	find_cha   = .true.
	find_space = .false.
	!----------------------------------------------------------------------------
	n  = 0
	ln = len(trim(words))
	number_of_words = 0
	if (ln==0) return
	do i=1, ln
		c=words(i:i)
		j=iachar(c)
		!-------------------------------------------------------------------------
		if (find_cha .and. j/=32) then
			n=n+1
			find_cha   = .false.
			find_space = .true.
		end if
		!-------------------------------------------------------------------------
		if (find_space .and. j==32) then
			find_cha   = .true.
			find_space = .false.
		end if
		!-------------------------------------------------------------------------
	end do
	number_of_words = n
	!----------------------------------------------------------------------------
	return
end function
