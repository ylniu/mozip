subroutine qm_file_energy_fid(fid,fname,energy,info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	integer     , intent( in) :: fid
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: energy
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: i, istat
	character(200)            :: line, file_type, version, tmp
	logical                   :: scanok
	logical     , external    :: search_word_free_last
	!----------------------------------------------------------------------------
	energy=0.D0
	!----------------------------------------------------------------------------
	call qm_file_type_fid(fid, fname, file_type, version, info)
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
			scanok=search_word_free_last(fid,"SCF Done",line)
			read(line,*) (tmp, i=1, 4), energy
			if (scanok) info=0
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
			scanok=search_word_free_last(fid,"Total DFT energy",line)
			if (scanok) then
				info=0
				read(line,*) (tmp, i=1, 4), energy
			else
				info=-1
			end if
			!----------------------------------------------------------------------
		case ("VASP")
			!----------------------------------------------------------------------
			! VASP
			!
			rewind(fid)
			scanok=search_word_free_last(fid,"free  energy   TOTEN",line)
			read(line,*) (tmp, i=1, 4), energy
			if (scanok) info=0
			!----------------------------------------------------------------------
		case default
			!----------------------------------------------------------------------
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
