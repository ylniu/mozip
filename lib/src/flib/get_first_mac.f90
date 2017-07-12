subroutine get_first_mac(mac)
	implicit none
	!--------------------------------------------------------------------
	character(*)                :: mac
	!--------------------------------------------------------------------
	integer                     :: nmac_loc, fid
	character( 17), allocatable :: mac_loc(:)
	character(200)              :: cmd
	character(200)              :: fmac
	!--------------------------------------------------------------------
	fmac=".loc.tmp.mac.dat"
#ifdef WIN 
	cmd="C:\Windows\System32\ipconfig /all > "//trim(fmac)
#else
	cmd="/sbin/ifconfig -a 2> /dev/null > "//trim(fmac)
#endif
	call execute_command_line(cmd)
	!--------------------------------------------------------------------
	call get_mac_num(fmac, nmac_loc)
	allocate(mac_loc(nmac_loc))
	!--------------------------------------------------------------------
	call get_mac_array(fmac, nmac_loc, mac_loc)
	mac=mac_loc(1)
	!--------------------------------------------------------------------
	open(newunit=fid, file=fmac)
	close(fid, status="delete")
	!--------------------------------------------------------------------
	deallocate(mac_loc)
	!--------------------------------------------------------------------
end subroutine
