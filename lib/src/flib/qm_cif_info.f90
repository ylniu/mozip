subroutine qm_cif_info(fname, natom, latt, symbol, x, info)
	use kinds, only: DP
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
	real(DP)    , intent(out) :: latt(6)
	real(DP)    , intent(out) :: x(3,natom)
	character(*), intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, istat, i
	character(200)            :: line, tmp
	!----------------------------------------------------------------------------
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
		scanok=search_word_free(fid, "_cell_length_a", line)
		read(line,*) tmp, latt(1)
		scanok=search_word_free(fid, "_cell_length_b", line)
		read(line,*) tmp, latt(2)
		scanok=search_word_free(fid, "_cell_length_c", line)
		read(line,*) tmp, latt(3)
		scanok=search_word_free(fid, "_cell_angle_alpha", line)
		read(line,*) tmp, latt(4)
		scanok=search_word_free(fid, "_cell_angle_beta", line)
		read(line,*) tmp, latt(5)
		scanok=search_word_free(fid, "_cell_angle_gamma", line)
		read(line,*) tmp, latt(6)
		!-------------------------------------------------------------------------
		scanok=search_word_free(fid, "_atom_site_occupancy", line)
		do i=1, natom
			read(fid,*) tmp, symbol(i), x(:,i)
		end do
	close(fid)
	if (istat==0) info=0
	!----------------------------------------------------------------------------
	return
end subroutine
