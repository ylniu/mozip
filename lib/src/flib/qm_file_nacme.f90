subroutine qm_file_nacme(fname, natom, nacme, tranele, E_vert, info)
	!----------------------------------------------------------------------------
	! Transition electric field
	! tranele(x,i) = \int dr \rho_{fi}(r) e/r^2
	!
	! Nonadiabatic coupling matrix element
	! nacme(x,i)   = charge(i) * tranele(x, i) / E_vert
	! 
	!----------------------------------------------------------------------------
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
	real(DP)    , intent(out) :: nacme  (3,natom)
	real(DP)    , intent(out) :: tranele(3,natom)
	real(DP)    , intent(out) :: E_vert
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j, ib, ie, n
	character(200)            :: line, tmp, tmp1, file_type, version, fmt1, fmt2
	logical                   :: scanok
	real(DP)                  :: fversion
	integer     , allocatable :: nat(:)
	real(DP)    , allocatable :: charge(:)
	character(2), allocatable :: symbol(:)
	logical     , external    :: search_word_free
	logical     , external    :: search_word
	logical     , external    :: search_word_del_space
	logical     , external    :: search_word_first_number
	logical     , external    :: search_word_first_letter
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
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			! Read symbol
			scanok=search_word(fid,2,9,"Charge =",line)
			do i=1, natom
				read(fid,*) symbol(i)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_del_space(fid,"Center Atomic Atomic Coordinates",line)
			scanok=search_word_first_number(fid,line)
			backspace(fid)
			do i=1, natom
				read(fid,*) tmp, nat(i)
				charge(i) = real(nat(i), DP)
			end do
			scanok=search_word(fid,2,18,"Excited State   1",line)
			read(line,*) (tmp,i=1,4), E_vert
			E_vert = E_vert / au2ev
			scanok=search_word_del_space(fid,"Electrostatic Properties (Atomic Units)",line)
			scanok=search_word_first_number(fid,line)
			backspace(fid)
			do i=1, natom
				read(fid,*) (tmp,j=1,3), tranele(:,i)
				do j=1, 3
					nacme(j, i) = charge(i) * tranele(j, i) / E_vert
				end do
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
				read(fid,*) tmp, symbol(i), charge(i)
			end do
			!----------------------------------------------------------------------
			rewind(fid)
			scanok=search_word(fid,2,38,"Energy Difference at present geometry",line)
			read(line,*) (tmp, i=1, 5), E_vert
			scanok=search_word(fid,2,35,"Derivative Coupling before scaling",line)
			do i=1, natom
				read(fid,*) nacme(:,i)
				tranele(:,i) = nacme(:,i) / charge(i) * E_vert
			end do
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			scanok=search_word_free(fid,"Atomic coordinate, charge and isotop information",line)
			if (.not. scanok) then
				write(*,'("Can not find")')
				write(*,'("Atomic coordinate, charge and isotop information")')
				write(*,*) "in "// trim(fname)
				write(*,'("Stop!")')
				stop
			end if
			scanok=search_word_first_letter(fid, line)
			read(line,*) (tmp1, i=1,4)
			do i=1, natom
				if ( trim(tmp1) == "charge" ) then
					read(fid,*) (tmp,j=1,3), symbol(i), charge(i)
				else
					read(fid,*) (tmp,j=1,3), symbol(i), tmp, charge(i)
				end if
			end do
			!------------------------------------------------------------------------
			scanok=search_word(fid,38,47,"excitation"        ,line)
			scanok=search_word(fid, 2,19,"Excitation energy:",line)
			read(line,*) tmp, tmp, E_vert
			!------------------------------------------------------------------------
			scanok=search_word(fid,12,58,"cartesian nonadiabatic coupling matrix elements",line)
			read(fid,*)
			!
			ib = 1
			ie = min(ib+4,natom)
			n  = ie - ib + 1
			!
			! gfortran format
			write(fmt1,'("(5x,",I0,"(x,E14.7))")') n
			write(fmt2,'("(7x,",I0,"(  E14.7))")') n
			read(version,*) fversion
			do while ( ie <= natom)
				read(fid,*)
				read(fid,*)
				do j=1, 3
					!----------------------------------------------------------------
					! Actually, the version check is from 6.5
					!
					if (fversion>6.49) then
						read(fid,fmt1) (nacme(j, i), i=ib, ie)
					else
						read(fid,fmt2) (nacme(j, i), i=ib, ie)
					end if
				end do
				!
				if (ie==natom) exit
				!
				ib = ie + 1
				ie = min(ib+4,natom)
			end do
			do i=1, natom
				do j=1, 3
					tranele(j, i) = nacme(j, i) * E_vert / charge(i)
				end do
			end do
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			! Read symbol
			scanok=search_word_free(fid,"Output coordinates in angstroms",line)
			scanok=search_word_free(fid,"No.",line)
			read(fid,'(a)') line
			do i=1, natom
				read(fid,*) tmp, symbol(i)
			end do
			!----------------------------------------------------------------------
			call symbol_to_nat(natom, symbol, nat)
			do i=1, natom
				charge(i) = real(nat(i), DP)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"Root   1",line)
			line = line(25:55)
			read(line,*) E_vert
			!----------------------------------------------------------------------
			scanok=search_word_del_space(fid,"Electrostatic Properties (Atomic Units)",line)
			scanok=search_word_first_number(fid,line)
			backspace(fid)
			do i=1, natom
				read(fid,*) (tmp,j=1,2), tranele(:,i)
				do j=1, 3
					nacme(j, i) = charge(i) * tranele(j, i) / E_vert
				end do
			end do
			!----------------------------------------------------------------------
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(nat    )
	deallocate(symbol )
	deallocate(charge )
	!----------------------------------------------------------------------------
	return
end subroutine
