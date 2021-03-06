!-------------------------------------------------------------------------------
! Subroutine : qm_file_coord
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_file_coord(fname, natom, x, info)
	use kinds
	use Param, only: au2a
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
	real(DP)    , intent(out) :: x(3,natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j
	character(200)            :: line, tmp, file_type, version
	logical                   :: scanok
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	logical     , external    :: search_word
	logical     , external    :: search_word_last_del_space
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	if (natom<=0) then
		write(*,'(2x,"Error!")')
		write(*,'(2x,"natom =", i20)') natom
		write(*,'(2x,"Stop!")')
		stop
	end if
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
			if(search_word(fid,32,48,'Input Coordinates',line)) then
				read (fid,*)
				read (fid,*)
				do i=1, natom
					read(fid,*) (tmp,j=1,3), x(:,i)
				end do
				info=0
			else
				write(*,'("Error!")')
				write(*,'("Can not find coordinate in ChemShell output file, stop!")')
				stop
			end if
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			if (search_word_last_del_space(fid, "Center Atomic Atomic Coordinates",line)) then
				if (search_word_first_number(fid, line)) then
					backspace(fid)
					do i=1, natom
						read(fid,*) (tmp, j=1, 3), x(:,i)
					end do
					x = x / au2a
					info=0
				end if
			else
				!write(*,'("Error!")')
				!write(*,'("Can not find coordinate in Gaussian log file, stop!")')
				!stop
				info=-1
				close(fid)
				return
			end if
		!-------------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			if (search_word(fid,2,40,"Dump information in style XYZ to output",line)) then
				do i=1,3
					read(fid,'(a)') line
				end do
				do i=1, natom
					read(fid,*) tmp, x(:,i)
				end do
				info=0
			else
				write(*,'("Error!")')
				write(*,'("Can not find coordinate in Molpro output file, stop!")')
				stop
			end if
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			if (search_word_free(fid,"atomic coordinates", line)) then
				do i=1, natom
					read(fid,*) x(:,i)
				end do
				info=0
			else
				write(*,'("Error!")')
				write(*,'("Can not find coordinate in TURBOMOLE output file, stop!")')
				stop
			end if
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			scanok=search_word_free_last(fid,"Output coordinates in angstroms",line)
			scanok=search_word_free(fid,"No.",line)
			read(fid,'(a)') line
			do i=1, natom
				read(fid,*) tmp, tmp, tmp, x(:,i)
			end do
			x = x / au2a
			info=0
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			if (search_word_free(fid,"Coordinate", line)) then
				read(fid,*)
				do i=1, natom
					read(fid,*) tmp, tmp, tmp, x(:,i)
				end do
				x = x / au2a
				info=0
			end if
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!----------------------------------------------------------------------
			if (search_word_free_last(fid,"POSITION", line)) then
				read(fid,*)
				do i=1, natom
					read(fid,*) x(:,i)
				end do
				x = x / au2a
				info=0
			end if
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
