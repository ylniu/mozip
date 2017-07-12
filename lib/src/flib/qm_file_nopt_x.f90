!-------------------------------------------------------------------------------
! Subroutine : qm_file_coord
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_file_nopt_x(fname, natom, nopt, x, info)
	use kinds
	use Param, only: au2a
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	integer     , intent( in) :: nopt
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: x(3,natom, nopt)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j, k
	character(200)            :: tmp, line, file_type, version
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
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
			do i=1, nopt
				if (search_word_free(fid, "Input orientation", line)) then
					read(fid, '(///)')
					do j=1, natom
						read(fid, *) (tmp, k=1, 3), x(:,j,i)
					end do
					info=0
				else
					info=-1
					close(fid)
					return
				end if
			end do
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
