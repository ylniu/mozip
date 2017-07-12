function char_to_int(string)
	implicit none
	!----------------------------------------------------------------------------
	! input and output  variable
	character(*),   intent(in) :: string
	!----------------------------------------------------------------------------
	! local  variable
	integer                    :: char_to_int
	!----------------------------------------------------------------------------
	read(string,*) char_to_int
	return
end function