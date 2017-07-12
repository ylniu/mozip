subroutine qm_wien2k_struct_info(fstruct, natom, cell, symbol, x, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer           :: natom, info, i
	real(DP)          :: cell(6)
	real(DP)          :: x(3,natom)
	character(*)      :: symbol(natom)
	character(*)      :: fstruct
	!----------------------------------------------------------------------------
	integer           :: fid
	character(200)    :: line
	!----------------------------------------------------------------------------
	logical           :: scanok
	logical, external :: search_word_free
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info=-1
	open(fid, file=fstruct, status="old")
		scanok=search_word_free(fid, "MODE OF CALC", line)
		read(fid,*) cell
		do i=1, natom
			scanok=search_word_free(fid, "ATOM", line)
			read(line,'(9x,3(3x,f10.8))') x(:,i)
			read(fid,*)
			read(fid,'(a2)') symbol(i)
		end do
	close(fid)
	info=1
	!----------------------------------------------------------------------------
	return
end subroutine
