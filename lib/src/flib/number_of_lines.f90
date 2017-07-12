function number_of_lines(fid)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	integer, intent( in) :: fid
	!----------------------------------------------------------------------------
	! output variable
	integer              :: number_of_lines
	!----------------------------------------------------------------------------
	! local  variable
	integer              :: ier
	!----------------------------------------------------------------------------
	ier             = 0
	number_of_lines = 0
	do while ( ier == 0 )
		read(fid,'(a)',iostat=ier)
		if ( ier == 0 ) number_of_lines = number_of_lines + 1
	end do
	return
end function
