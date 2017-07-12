subroutine qm_vasp_poscar_natom(fname, natom, ntype, info)
	use kinds, only: DP
	implicit none
	character(*), intent( in) :: fname
	integer     , intent(out) :: natom
	integer     , intent(out) :: ntype
	integer     , intent(out) :: info
	integer                   :: fid, i, ii(1000)
	integer     , external    :: number_of_words
	character(200)            :: line, tmp
	logical     , external    :: is_number
	call get_free_fid(fid)
	info = 0
	ii=0
	open(fid, file=fname, status="old")
		do i=1, 6
			read(fid, '(a)') line
		end do
		ntype=number_of_words(line)
		read(line,*) tmp
		if (.not.is_number(trim(tmp))) then
			read(fid, '(a)') line
		end if
		read(line,*) ii(1:ntype)
	close(fid)
	natom=0
	do i=1, 1000
		if (ii(i)==0) exit
		natom = natom + ii(i)
	end do
	return
end subroutine
