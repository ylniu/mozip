subroutine file_num_lines(fname,nline)
	!----------------------------------------------------------------------------
	! This subroutine returns the number of lines of the file
	!----------------------------------------------------------------------------
	implicit none
	character(*), intent( in) :: fname
	integer     , intent(out) :: nline
	!
	integer                   :: fid, iostat
	!----------------------------------------------------------------------------
	fid=1
	open(fid,file=fname,status="old")
	nline  = 0
	iostat = 0
	do while ( iostat == 0)
		read(fid,*, iostat=iostat)
		if (iostat/=0) exit
		nline = nline + 1
	end do
	close(fid)
	!----------------------------------------------------------------------------
return
end subroutine
