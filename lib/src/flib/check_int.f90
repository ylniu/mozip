subroutine check_int(name,value,a,b)
	implicit none
	character(*) :: name
	integer      :: value, a, b
	!----------------------------------------------------------------------------
	if (value<a .or. value > b) then
		write(*,*) "The number "//trim(name)//" should be within [",a,",",b,"]"
		write(*,*) "Stop"
		stop
	end if
	!----------------------------------------------------------------------------
	return
end subroutine check_int
