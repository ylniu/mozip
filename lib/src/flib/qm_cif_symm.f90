subroutine qm_cif_symm(fname, symm, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	character(*), intent(out) :: symm
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, istat
	character(200)            :: line, tmp
	!----------------------------------------------------------------------------
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
		scanok=search_word_free(fid, "_symmetry_space_group", line)
		read(line,*) tmp, symm
	close(fid)
	if (istat==0) info=0
	!----------------------------------------------------------------------------
	return
end subroutine
