subroutine str_replace_same(original_str, replaced_str, replaced_word, info)
	!----------------------------------------------------------------------------
	! Usage:
	! replaced_word="a-b-c"
	! call str_replace("-", ";", replaced_word, info)
	! replaced_word="a;b;c"
	!
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	character(*), intent( in) :: original_str
	character(*), intent( in) :: replaced_str
	!----------------------------------------------------------------------------
	! Output variables
	!
	character(*), intent(out) :: replaced_word
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: la, lb, lc, ld
	integer                   :: idx
	!----------------------------------------------------------------------------
	la = len(original_str)
	lb = len(replaced_str)
	lc = len(trim(replaced_word))
	ld = lc
	info = 0
	if (la==0 .or. lb==0 .or. lc==0) then
		info = -1
		return
	end if
	!----------------------------------------------------------------------------
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
