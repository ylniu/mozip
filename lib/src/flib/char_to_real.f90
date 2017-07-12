function char_to_real(string)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! input and output  variable
	character(*),   intent(in) :: string
	!----------------------------------------------------------------------------
	! local  variable
	real(DP)                   :: char_to_real
	!----------------------------------------------------------------------------
	read(string,*) char_to_real
	return
end function