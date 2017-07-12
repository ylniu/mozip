subroutine qm_vasp_potcar_zval(fname, ntype, zvals, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent( in) :: ntype
	real(DP)    , intent(out) :: zvals(ntype)
	integer     , intent(out) :: info
	integer                   :: i, j
	integer                   :: fid
	character(200)            :: line, tmp
	logical                   :: search_word_free
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	info=-1
	open(fid, file=fname, status="old")
		do i=1, ntype
			if (search_word_free(fid,"ZVAL",line)) then
				read(line,*) (tmp, j=1, 5), zvals(i)
			end if
		end do
	close(fid)
	info=0
	return
end subroutine
