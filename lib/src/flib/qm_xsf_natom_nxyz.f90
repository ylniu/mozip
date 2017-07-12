subroutine qm_xsf_natom_nxyz(fname, natom, ngrid, info)
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
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	natom = 0
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
		scanok=search_word_free(fid, "PRIMCOORD", line)
		read(fid,*) natom
		rewind(fid)
		scanok=search_word_free(fid, "BEGIN_BLOCK_DATAGRID3D", line)
		if (scanok) then
			if (search_word_free(fid, "DATAGRID_3D", line)) then
				read(fid,*) ngrid
			end if
		else
			rewind(fid)
			scanok=search_word_free(fid, "BEGIN_BLOCK_DATAGRID_3D", line)
			if (scanok) then
				if (search_word_free(fid, "DATAGRID_3D", line)) then
					read(fid,*) ngrid
				end if
			end if
		end if
	close(fid)
	if (istat==0) info=0
	!----------------------------------------------------------------------------
	return
end subroutine
