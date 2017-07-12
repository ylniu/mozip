subroutine get_mac_array(fname, nmac, mac)
	implicit none
	!--------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent( in) :: nmac
	character(*), intent(out) :: mac(nmac)
	!--------------------------------------------------------------------
	integer                   :: fid, n
	integer                   :: stat
	character(500)            :: line
	logical       , external  :: is_mac_line
	!--------------------------------------------------------------------
	n=0
	open(newunit=fid, file=fname, iostat=stat, status="old")
		read(fid, '(a)', iostat=stat) line
		do while (stat==0)
			if (is_mac_line(line)) then
				n=n+1
				call get_mac_line(line, mac(n))
			end if
			read(fid, '(a)', iostat=stat) line
		end do
	close(fid)
	!--------------------------------------------------------------------
end subroutine
