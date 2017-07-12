subroutine qm_cif_natom(fname, natom, info)
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
	integer                   :: fid, istat
	character(200)            :: line, tmp
	!----------------------------------------------------------------------------
	logical                   :: scanok, end_atom
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
		scanok=search_word_free(fid, "_atom_site_occupancy", line)
		end_atom=.false.
		natom=0
		do while ( .not. end_atom )
			read(fid, '(a)', iostat=istat) line
			if (istat==0) then
				read(line, *) tmp
				if (trim(tmp)/="loop_") then
					natom=natom + 1
				else
					exit
				end if
			else
				exit
			end if
		end do
	close(fid)
	if (istat==0) info=0
	!----------------------------------------------------------------------------
	return
end subroutine
