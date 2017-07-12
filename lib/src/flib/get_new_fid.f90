function get_new_fid(fid)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	integer,   intent(in) :: fid
	!----------------------------------------------------------------------------
	! output variable
	integer               :: get_new_fid
	!----------------------------------------------------------------------------
	! local  variable
	logical               :: opened
	!----------------------------------------------------------------------------
	opened      = .true.
	get_new_fid = fid
	do while (opened)
		inquire(unit=get_new_fid,opened=opened)
		if (.not. opened) then
			exit
		else
			get_new_fid = get_new_fid + 1
		end if
	end do
	return
end function
