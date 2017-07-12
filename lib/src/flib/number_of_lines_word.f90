function number_of_lines_word(fid,word)
	implicit none
	integer           :: fid
	integer           :: number_of_lines_word
	character(*)      :: word
	integer           :: istat
	logical, external :: search_word_free
	character(500)    :: line
	!---------------------------------------------------------------------------
	rewind(fid)
	istat=0
	number_of_lines_word = 0
	do while (istat==0)
		if(search_word_free(fid,word,line)) then
			number_of_lines_word = number_of_lines_word + 1
		else
			exit
		end if
	end do
	!---------------------------------------------------------------------------
	return
end function