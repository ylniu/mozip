subroutine qm_file_crystal(fname, natom, aa, symbol, x, info)
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
	real(DP)    , intent(out) :: aa(3,3)
	real(DP)    , intent(out) :: x(3,natom)
	character(2), intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i
	real(DP)                  :: a, b, c, alpha, beta, gamma
	character(200)            :: line, tmp, file_type, version
	logical     , external    :: search_word_free
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
		case ("CIF")
			!----------------------------------------------------------------------
			! CHEMSHELL
			!----------------------------------------------------------------------
			if(search_word_free(fid,'_cell_length_a',line)) then
				backspace(fid)
				read (fid,*) tmp, a
				read (fid,*) tmp, b
				read (fid,*) tmp, c
				read (fid,*) tmp, alpha
				read (fid,*) tmp, beta
				read (fid,*) tmp, gamma
				call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
				if (search_word_free(fid,'_atom_site_occupancy',line)) then
					do i=1, natom
						read(fid,*) tmp, symbol(i), x(:,i)
					end do
				end if
				info=0
			else
				write(*,'("Error!")')
				write(*,'("Can not find coordinate in ChemShell output file, stop!")')
				stop
			end if
			!----------------------------------------------------------------------
		case default
			stop "Not implement!"
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
