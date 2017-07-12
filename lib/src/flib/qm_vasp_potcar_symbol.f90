subroutine qm_vasp_potcar_symbol(fname, ntype, nsymbol, info)
	use kinds, only: DP
	implicit none
	character(*)      :: fname
	integer           :: ntype
	integer           :: info, i
	character(*)      :: nsymbol(ntype)
	integer           :: fid
	character(200)    :: line, tmp
	logical           :: search_word_free
	call get_free_fid(fid)
	info=-1
	open(fid, file=fname, status="old")
		do i=1, ntype
			if (search_word_free(fid,"TITEL",line)) then
				read(line,*) tmp, tmp, tmp, nsymbol(i)
			end if
		end do
	close(fid)
	info=0
	return
end subroutine
