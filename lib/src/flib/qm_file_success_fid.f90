subroutine qm_file_success_fid(fid,fname,info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	integer     , intent( in) :: fid
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: istat
	character(200)            :: line, file_type, version
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call qm_file_type_fid(fid, fname, file_type, version, info)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
	if (istat/=0) then
		write(*,*) "Checking file ", trim(fname), " failed!"
		write(*,*) "iostat = ", istat
		return
	end if
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
			if (scanok) then
				info=0
			else
				write(*,*) "Checking file ", trim(fname), " failed"!
			end if
			!----------------------------------------------------------------------
		case ("VASP")
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word_free(fid,"General timing and accounting",line)
			if (scanok) then
				info=0
			else
				write(*,*) "Checking file ", trim(fname), " failed!"
				write(*,*) "info = ", info
			end if
			!----------------------------------------------------------------------
		case default
			!----------------------------------------------------------------------
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
