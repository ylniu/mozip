subroutine get_first_mac_file(fmac, mac)
	implicit none
	!--------------------------------------------------------------------
	character(*)                :: fmac
	character(*)                :: mac
	!--------------------------------------------------------------------
	integer                     :: nmac_loc
	character( 17), allocatable :: mac_loc(:)
	!--------------------------------------------------------------------
	call get_mac_num(fmac, nmac_loc)
	allocate(mac_loc(nmac_loc))
	!--------------------------------------------------------------------
	call get_mac_array(fmac, nmac_loc, mac_loc)
	mac=mac_loc(1)
	!--------------------------------------------------------------------
	deallocate(mac_loc)
	!--------------------------------------------------------------------
end subroutine
