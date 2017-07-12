subroutine qm_vasp_poscar_info(fname, title, alat, a, ntype, ntypes, &
	symbols, opt, coordtype, info)
	use kinds, only: DP
	implicit none
	integer     , intent( in) :: ntype
	character(*), intent( in) :: fname
	integer     , intent(out) :: ntypes(ntype)
	character(*), intent(out) :: title, opt, coordtype, symbols(ntype)
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: alat, a(3,3)
	integer                   :: fid, ios
	character(200)            :: line, tmp
	!----------------------------------------------------------------------------
	logical     , external    :: is_number
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	info=0
	symbols=""
	open(fid, file=fname, status="old", iostat=ios)
		read(fid, '(a)') title
		read(fid, *) alat
		read(fid, *) a(:,1)
		read(fid, *) a(:,2)
		read(fid, *) a(:,3)
		read(fid, '(a)') line
		read(line,*) tmp
		if (.not.is_number(trim(tmp))) then
			read(line,*) symbols
			read(fid, '(a)') line
		end if
		read(line, *) ntypes
		read(fid, '(a)') opt
		read(fid, '(a)') coordtype
	close(fid)
	if (ios/=0) info=ios
	return
end subroutine
