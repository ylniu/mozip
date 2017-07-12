subroutine file_copy(fname1, fname2)
	implicit none
	!--------------------------------------------------------------------
	character(*), intent(in) :: fname1
	character(*), intent(in) :: fname2
	!--------------------------------------------------------------------
	integer                  :: fid1
	integer                  :: fid2
	integer                  :: ierr, irec
	character(1)             :: a
	!--------------------------------------------------------------------
	call get_free_fid(fid1)
	open(fid1, file=fname1, access="direct", status="old"    , action="read" , iostat=ierr, recl=1)
	!--------------------------------------------------------------------
	call get_free_fid(fid2)
	open(fid2, file=fname2, access="direct", status="replace", action="write", iostat=ierr, recl=1)
	!--------------------------------------------------------------------
	! Read first character
	!
	irec=1
	read (fid1, rec=irec, iostat=ierr) a
	do while(ierr==0)
		!-----------------------------------------------------------------
		! Write the character to file
		!
		write(fid2, rec=irec, iostat=ierr) a
		!-----------------------------------------------------------------
		! Read next character
		!
		irec = irec + 1
		read (fid1, rec=irec, iostat=ierr) a
	end do
	!--------------------------------------------------------------------
	close(fid1)
	close(fid2)
	!--------------------------------------------------------------------
	return
end subroutine
