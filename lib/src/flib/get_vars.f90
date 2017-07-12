subroutine get_vars(line, var_name, var_value, var_unit)
	implicit none
	!----------------------------------------------------------------------------
	! input and output variable
	character(*) ,   intent(inout) :: line
	!----------------------------------------------------------------------------
	! output variable
	character(*) ,   intent(  out) :: var_name, var_value, var_unit
	!----------------------------------------------------------------------------
	! local  variable
	integer                      :: i, j, m
	!----------------------------------------------------------------------------
	m = len(line)
	do i=1,m
		if(line(i:i) == "=" ) then
			j=i
			exit
		end if
	end do
	var_name = trim( adjustl( line(   1 : j-1 ) ) )
	line     = trim( adjustl( line( j+1 : m   ) ) )
	!----------------------------------------------------------------------------
	do i=1,len(line)
		if(line(i:i) == " " ) then
			j=i
			exit
		end if
	end do
	var_value = trim( adjustl( line( 1   : j-1 ) ) )
	var_unit  = trim( adjustl( line( j+1 : m   ) ) )
	return
end subroutine
