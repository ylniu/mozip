subroutine search_char(origstring, searchchar, pos)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	character(*) ,   intent( in) :: origstring
	character(*) ,   intent( in) :: searchchar
	!----------------------------------------------------------------------------
	! output variable
	integer,         intent(out) :: pos
	!----------------------------------------------------------------------------
	! local  variable
	integer                      :: i, m, n
	!----------------------------------------------------------------------------
	pos=0
	n = len(origstring)
	m = len(searchchar)
	if( m > n ) pos = -1
	do i=1,n - m + 1
		if( searchchar == origstring(i:i+m-1) ) then
			pos = i
			exit
		end if
	end do
	return
end subroutine
