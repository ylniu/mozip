subroutine qm_file_natom(fname, natom, info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	integer     , intent(out) :: natom
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, ios, i
	character(200)            :: line, tmp, file_type, version
	logical                   :: scanok, is_number
	logical     , external    :: search_word
	logical     , external    :: search_word_free
	logical     , external    :: search_word_del_space
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	natom = 0
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		!-------------------------------------------------------------------------
		case ("CHEMSHELL")
			!----------------------------------------------------------------------
			! CHEMSHELL
			!----------------------------------------------------------------------
			if (search_word_free(fid,"Input Coordinates",line)) then
				if (search_word_first_number(fid,line)) then
					backspace(fid)
					natom=0
					read(fid,*) tmp
					do while( is_number(tmp) )
						natom=natom+1
						read(fid,*) tmp
					end do
					info=0
				end if
			end if
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
			if(search_word(fid,2,8,"NAtoms=",line)) then
				read(line,*) tmp, natom
				close(fid)
				info=0
				return
			end if
			!----------------------------------------------------------------------
		case ("GAUSSIAN FCHK")
			!----------------------------------------------------------------------
			! GAUSSIAN FCHK
			!----------------------------------------------------------------------
			if(search_word(fid,1,15,"Number of atoms",line)) then
				read(line,*) (tmp,i=1,4), natom
				close(fid)
				info=0
				return
			end if
			!----------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"ATOMIC COORDINATES",line)
			scanok=search_word_first_number(fid,line)
			i=len_trim(line)
			natom = 0
			do while( i>0 )
				natom = natom + 1
				read(fid ,'(a)', iostat=ios) line
				i=len_trim(line)
			end do
			info=0
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			!scanok=search_word_del_space(fid,"type atoms prim cont basis",line)
			scanok=search_word_del_space(fid,"total:",line)
			read(line,*) tmp, natom
			info=0
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			scanok=search_word_del_space(fid,"NAtoms",line)
			read(line,*) tmp, natom
			info=0
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"Geometry",line)
			scanok=search_word_free(fid,"No.",line)
			read(fid,'(a)') line
			read(fid,'(a)') line
			natom=0
			do while(len(trim(adjustl(line)))>0)
				natom = natom + 1
				read(fid,'(a)') line
			end do
			info=0
			!----------------------------------------------------------------------
		case ("CIF")
			!----------------------------------------------------------------------
			! CIF
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"_atom_site_occupancy",line)
			read(fid ,'(a)', iostat=ios) line
			natom=0
			do while (ios==0)
				natom=natom+1
				read(fid ,'(a)', iostat=ios) line
				if (trim(line)=="loop_") exit
			end do
			info=0
		case ("PWOUT")
			!----------------------------------------------------------------------
			! PWOUT
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"number of atoms",line)
			line=line(34:)
			read(line,*) natom
			info=0
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"number of ions",line)
			line=line(67:)
			read(line,*) natom
			info=0
		case default
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
