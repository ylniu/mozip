subroutine qm_file_type(fname, file_type, version, info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	character(*), intent(out) :: file_type
	character(*), intent(out) :: version
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer           :: fid, i, j, istat
	character(200)    :: line, tmp
	logical           :: find
	logical, external :: search_word_free
	logical, external :: search_word
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	open(fid, file=fname, status="old", iostat=istat)
	!----------------------------------------------------------------------------
	if (istat/=0) then
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if Chemshell turbomole freq file
	!----------------------------------------------------------------------------
	rewind(fid)
	if (search_word_free(fid,"ChemShell",line)) then
		file_type="CHEMSHELL"
		read(line,*) tmp, tmp, version
		info = 4
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if Gaussian freq log file
	!----------------------------------------------------------------------------
	rewind(fid)
	read(fid,'(a)') line
	backspace(fid)
	if ( search_word_free(fid,"Entering Gaussian System",line) ) then
		file_type="GAUSSIAN LOG"
		if ( search_word_free(fid,"*****************",line) ) then
			read(fid,'(a)') line
			read(line,*) tmp, tmp, tmp
			i=index(tmp,"-")
			version=tmp(i+1:)
			info = 2
			close(fid)
			return
		end if
	end if
	!----------------------------------------------------------------------------
	! Check if Gaussian freq log file
	!----------------------------------------------------------------------------
	rewind(fid)
	read(fid,'(a)') line
	backspace(fid)
	if ( search_word(fid,1,15,"Number of atoms",line) ) then
		file_type="GAUSSIAN FCHK"
		version="UNKNOWN"
	end if
	!----------------------------------------------------------------------------
	! Check if Molpro freq log file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word(fid,62,67,"MOLPRO",line)
	if (find) then
		file_type="MOLPRO"
		info = 3
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	rewind(fid)
	if ( search_word_free(fid,"TURBOMOLE V",line) ) then
		file_type="TURBOMOLE"
		i=index(line,"TURBOMOLE")
		line=line(i+9:)
		read(line,*) tmp
		version=tmp(2:)
		do i=1, len(version)
			j=iachar(version(i:i))
			if ( j < 46 .or. j>46 .and. j< 48 .or. j>57) version(i:i) = ''
		end do
		version=trim(version)
		info = 1
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if numforce file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word(fid,2,20,"NUMERICAL FREQUENCY",line)
	if (find) then
		file_type="NUMFORCE"
		find=search_word(fid,2,8,"VERSION",line)
		read(line,*) tmp, version
		info = 3
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if Quantum Espresso file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word_free(fid,"PWSCF",line)
	if (find) then
		file_type="PWOUT"
		read(line,*) tmp, tmp, version
		version=trim(adjustl(version))
		version=version(3:)
		info = 3
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if Vasp file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word_free(fid,"vasp",line)
	if (find) then
		file_type="VASP"
		read(line,*) version
		version=trim(adjustl(version))
		version=version(6:)
		info = 3
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if NWChem file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word_free(fid,"NWChem",line)
	if (find) then
		file_type="NWCHEM"
		read(line,*) (tmp,i=1,5), version
		version=trim(adjustl(version))
		info = 0
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if cif file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word_free(fid,"_cell_length_a",line)
	if (find) then
		file_type="CIF"
		version="0"
		info = 0
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if WIEN2K_output1 file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word(fid,11,30,"POTENTIAL PARAMETERS",line)
	if (find) then
		file_type="WIEN2K_1"
		version="0"
		info = 0
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	! Check if WIEN2K_spaghetti_ene file
	!----------------------------------------------------------------------------
	rewind(fid)
	find=search_word(fid,3,12,"bandindex:",line)
	if (find) then
		file_type="WIEN2K_spe"
		version="0"
		info = 0
		close(fid)
		return
	end if
	!----------------------------------------------------------------------------
	close(fid)
	return
end subroutine
