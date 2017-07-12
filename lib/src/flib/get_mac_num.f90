subroutine get_mac_num(fname, n)
	implicit none
	!--------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent(out) :: n
	!--------------------------------------------------------------------
	integer                   :: fid
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
			end if
			read(fid, '(a)', iostat=stat) line
		end do
	close(fid)
	!--------------------------------------------------------------------
end subroutine
