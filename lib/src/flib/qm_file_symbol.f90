subroutine qm_file_symbol(fname, natom, symbol, info)
	use kinds
	use Param, only: au2ev
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
	character(*), intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j
	integer     , allocatable :: nat(:)
	character(200)            :: line, tmp, file_type, version
	logical                   :: scanok
	logical     , external    :: search_word_free
	logical     , external    :: search_word
	logical     , external    :: search_word_del_space
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	if (natom<=0) then
		write(*,'(2x,"Error!")')
		write(*,'(2x,"natom =", i20)') natom
		write(*,'(2x,"Stop!")')
		stop
	end if
	!----------------------------------------------------------------------------
	allocate(nat(natom))
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
			if (search_word_free(fid,"Input Coordinates",line)) then
				if (search_word_first_number(fid,line)) then
					backspace(fid)
					do i=1, natom
						read(fid,*) tmp, symbol(i)
					end do
				end if
			end if
			call symbol_to_nat(natom, symbol, nat)
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			scanok=search_word_del_space(fid,"Center Atomic Atomic Coordinates",line)
			scanok=search_word_first_number(fid,line)
			backspace(fid)
			do i=1, natom
				read(fid,*) tmp, nat(i)
			end do
		!-------------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			scanok=search_word(fid,2,19,"ATOMIC COORDINATES",line)
			read(fid, *)
			read(fid, *)
			read(fid, *)
			do i=1, natom
				read(fid,*) tmp, symbol(i)
			end do
			call symbol_to_nat(natom, symbol, nat)
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			scanok=search_word(fid,17,64,"Atomic coordinate, charge and isotop information",line)
			read(fid,'(a)') line
			call search_word_first_letter(fid, line)
			do i=1, natom
				read(fid,*) (tmp,j=1,3), symbol(i)
			end do
			call symbol_to_nat(natom, symbol, nat)
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"Output coordinates in angstroms",line)
			scanok=search_word_free(fid,"No.",line)
			read(fid,'(a)') line
			do i=1, natom
				read(fid,*) tmp, symbol(i)
			end do
			call symbol_to_nat(natom, symbol, nat)
			info=0
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			scanok=search_word(fid,2,11,"Coordinate",line)
			read(fid,'(a)') line
			do i=1, natom
				read(fid,*) tmp, nat(i)
				write(*,*) i, nat(i)
			end do
			!----------------------------------------------------------------------
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!----------------------------------------------------------------------
			nat = 0
			!----------------------------------------------------------------------
		case default
			
	end select
	close(fid)
	call nat_to_symbol(natom, nat, symbol)
	!----------------------------------------------------------------------------
	deallocate(nat)
	!----------------------------------------------------------------------------
	return
end subroutine
