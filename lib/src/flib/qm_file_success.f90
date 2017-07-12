subroutine qm_file_success(fname,info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, istat
	character(200)            :: line, file_type, version
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	if (info/=0) return
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
	if (istat/=0) return
	select case(trim(file_type))
		!-------------------------------------------------------------------------
		case ("CHEMSHELL")
			!
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word_free(fid,"Normal termination",line)
			if (scanok) info=0
			!----------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word_free(fid,"GA Statistics for process",line)
			if (scanok) info=0
			!----------------------------------------------------------------------
		case ("VASP")
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word_free(fid,"General timing and accounting",line)
			if (scanok) info=0
			!----------------------------------------------------------------------
		case default
			!----------------------------------------------------------------------
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
