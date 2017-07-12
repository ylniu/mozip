subroutine qm_cube_natom_nxyz(fname, natom, ngrid, info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	integer     , intent(out) :: natom
	integer     , intent(out) :: ngrid(3)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, istat
	character(200)            :: line
	!----------------------------------------------------------------------------
	natom = 0
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
		if (istat/=0) call error(2, fname, line)
		read(fid,'(a)') line
		read(fid,'(a)') line
		read(fid,*) natom, line
		read(fid,*) ngrid(1)
		read(fid,*) ngrid(2)
		read(fid,*) ngrid(3)
	close(fid)
	if (istat==0) info=0
	!----------------------------------------------------------------------------
	return
end subroutine
