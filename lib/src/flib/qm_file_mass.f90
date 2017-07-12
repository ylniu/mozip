subroutine qm_file_mass(fname, natom, mass, info)
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
	real(DP)    , intent(out) :: mass(natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j, ib, ie
	character(200)            :: line, tmp, file_type, version
	logical                   :: scanok
	integer     , allocatable :: nat(:)
	real(DP)    , allocatable :: charge(:)
	real(DP)    , allocatable :: tranele(:,:)
	character(3), allocatable :: symbol(:)
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
	allocate(nat    (  natom))
	allocate(symbol (  natom))
	allocate(charge (  natom))
	allocate(tranele(3,natom))
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
			if( search_word(fid,11,24,'atomic weights',line) ) then
				if (search_word_first_number(fid,line)) then
					backspace(fid)
					do i=1, natom
						read(fid,*) tmp, tmp, mass(i)
					end do
				end if
			else
				rewind(fid)
				scanok=search_word(fid,17,64,"Atomic coordinate, charge and isotop information",line)
				read(fid,'(a)') line
				call search_word_first_letter(fid, line)
				do i=1, natom
					read(fid,*) (tmp,j=1,3), symbol(i)
				end do
				call symbol_to_nat(natom, symbol, nat)
				call nat_to_mass(natom, nat, mass)
			end if
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			if( search_word_free(fid,"AtmWgt",line) ) then
				backspace(fid)
				ib=1
				ie=ib+9
				ie=min(ie,natom)
				do while(ib<=natom)
					scanok=search_word_free(fid,"AtmWgt",line)
					read(line,*) tmp, mass(ib:ie)
					ib=ib+10
					ie=ib+9
					ie=min(ie,natom)
				end do
			else
				rewind(fid)
				if( search_word_free(fid,"Thermochemistry",line) ) then
					if( search_word_free(fid,"Atom",line) ) then
						backspace(fid)
						do i=1, natom
							read(fid,*) (tmp,j=1,8), mass(i)
						end do
					end if
				else
					rewind(fid)
					scanok=search_word_del_space(fid,"Center Atomic Atomic Coordinates",line)
					scanok=search_word_first_number(fid,line)
					backspace(fid)
					do i=1, natom
						read(fid,*) tmp, nat(i)
					end do
					call nat_to_mass(natom, nat, mass)
				end if
			end if
		!-------------------------------------------------------------------------
		case ("GAUSSIAN FCHK")
			!----------------------------------------------------------------------
			! GAUSSIAN FCHK 
			!----------------------------------------------------------------------
			if ( search_word_free(fid,'Real atomic weights',line) ) then
				read(fid, *) mass
			end if
		!-------------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			if ( search_word(fid,2,14,'Atomic Masses',line) ) then
				read(fid,'(a)') line
				read(line,*) tmp,tmp,mass
			else
				rewind(fid)
				scanok=search_word(fid,2,19,"ATOMIC COORDINATES",line)
				read(fid, *)
				read(fid, *)
				read(fid, *)
				do i=1, natom
					read(fid,*) tmp, symbol(i)
				end do
				call symbol_to_nat(natom, symbol, nat)
				call nat_to_mass(natom, nat, mass)
			end if
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			if( search_word_free(fid,'ATOMIC WEIGHTS',line) ) then
				if (search_word_first_number(fid,line)) then
					backspace(fid)
					do i=1, natom
						read(fid,*) tmp, tmp, mass(i)
					end do
				end if
			else
				rewind(fid)
				scanok=search_word(fid,17,64,"Atomic coordinate, charge and isotop information",line)
				read(fid,'(a)') line
				call search_word_first_letter(fid, line)
				do i=1, natom
					read(fid,*) (tmp,j=1,3), symbol(i)
				end do
				call symbol_to_nat(natom, symbol, nat)
				call nat_to_mass(natom, nat, mass)
			end if
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
			call nat_to_mass(natom, nat, mass)
			info=0
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			if ( search_word_free(fid,'Coordinate',line) ) then
				read(fid,*)
				do i=1, natom
					read(fid,*) tmp, tmp, mass(i)
				end do
			end if
			!----------------------------------------------------------------------
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(nat    )
	deallocate(symbol )
	deallocate(charge )
	deallocate(tranele)
	!----------------------------------------------------------------------------
	return
end subroutine
