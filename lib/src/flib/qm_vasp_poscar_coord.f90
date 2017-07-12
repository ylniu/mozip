subroutine qm_vasp_poscar_coord(fname, natom, x, fix, info)
	use kinds, only: DP
	implicit none
	character(*)      :: fname
	integer           :: natom
	integer           :: info, i
	real(DP)          :: x(3,natom)
	character(1)      :: fix(3,natom)
	integer           :: fid
	integer, external :: number_of_words
	character(200)    :: line
	logical           :: search_word_free
	call get_free_fid(fid)
	x=-1.D-99
	fix="F"
	info=-1
	open(fid, file=fname, status="old")
		if (search_word_free(fid,"direct",line)) then
			do i=1, natom
				read(fid,*) x(:,i), fix(:,i)
			end do
			info=0
		end if
	close(fid)
	return
end subroutine
