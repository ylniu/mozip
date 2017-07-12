subroutine qm_file_force(fname,natom,force,info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: force(3,natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, istat
	character(200)            :: line, file_type, version, tmp
	logical                   :: scanok
	logical     , external    :: search_word_last_del_space
	logical     , external    :: search_word_free_last
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	force=0.D0
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
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
			scanok=search_word_last_del_space(fid,"Center Atomic Forces",line)
			if (scanok) then
				info=0
			else
				return
			end if
			scanok=search_word_first_number(fid,line)
			backspace(fid)
			do i=1, natom
				read(fid,*) tmp, tmp, force(:,i)
			end do
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
			scanok=search_word_last_del_space(fid,"total DFT gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			do i=1, natom
				read(fid,*) force(:,i)
			end do
			!----------------------------------------------------------------------
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word_free_last(fid,"POSITION",line)
			if (scanok) then
				info=0
			else
				return
			end if
			read(fid,*)
			do i=1, natom
				read(fid,*) tmp, tmp, tmp, force(:,i)
			end do
		case default
			!----------------------------------------------------------------------
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
