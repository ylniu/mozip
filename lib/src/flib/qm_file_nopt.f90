!-------------------------------------------------------------------------------
! Subroutine : qm_file_coord
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_file_nopt(fname, nopt, info)
	use kinds
	use Param, only: au2a
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	integer     , intent(out) :: nopt
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid
	character(200)            :: line, file_type, version
	logical     , external    :: search_word_free_nline
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	nopt =  0
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		case ("CHEMSHELL")
			!----------------------------------------------------------------------
			! CHEMSHELL
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			if (search_word_free_nline(fid, "Input orientation", nopt, line)) then
				info=0
			else
				info=-1
				close(fid)
				return
			end if
		!-------------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!----------------------------------------------------------------------
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
		case default
			write(*,*) "qm_file_nopt not implimented for this file!"
			call exit(1)
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
