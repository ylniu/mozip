function find_int_crd(rel_crd_name, crd_name, ncrd)
	implicit none
	integer      :: ncrd
	character(*) :: rel_crd_name, crd_name(ncrd)
	integer      :: find_int_crd
	integer      :: i
	!----------------------------------------------------------------------------
	find_int_crd = 0
	do i=1, ncrd
		if (trim(rel_crd_name) == trim(crd_name(i))) then
			find_int_crd = i
			return
		end if
	end do
	!----------------------------------------------------------------------------
	return
end function