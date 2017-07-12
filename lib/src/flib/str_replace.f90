subroutine str_replace(original_str, replaced_str, original_word, replaced_word, info)
	!----------------------------------------------------------------------------
	! Usage:
	! call str_replace("-", ";", "a-b-c", replaced_word, info)
	! replaced_word="a;b;c"
	!
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: original_str
	character(*), intent( in) :: replaced_str
	character(*), intent( in) :: original_word
	character(*), intent(out) :: replaced_word
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	integer                   :: la, lb, lc, ld
	integer                   :: idx
	!----------------------------------------------------------------------------
	la = len(original_str)
	lb = len(replaced_str)
	lc = len(trim(original_word))
	ld = lc
	info = 0
	if (la==0 .or. lb==0 .or. lc==0) then
		info = -1
		return
	end if
	!----------------------------------------------------------------------------
	replaced_word=original_word
	do while (.true.)
		idx=index(replaced_word, original_str)
		info=idx
		if (idx>0) then
			if (idx+la<=ld) then
				replaced_word=replaced_word(1:idx-1)//replaced_str//replaced_word(idx+la:ld)
			else
				replaced_word=replaced_word(1:idx-1)//replaced_str
				exit
			end if
			ld=len(trim(replaced_word))
		else
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
