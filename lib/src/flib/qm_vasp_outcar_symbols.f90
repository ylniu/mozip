subroutine qm_vasp_outcar_symbols(fout, ntype, symbols, info)
	implicit none
	!------------------------------------------------------------------------------
	character(*), intent( in) :: fout
	integer     , intent( in) :: ntype
	character(*), intent(out) :: symbols(ntype)
	integer     , intent(out) :: info
	!------------------------------------------------------------------------------
	integer                   :: fid
	!------------------------------------------------------------------------------
	integer                   :: i, j, itype
	character(200)            :: line
	logical                   :: scanok
	logical     , external    :: search_word_free
	integer     , external    :: number_of_words
	!------------------------------------------------------------------------------
	info = 0
	!------------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fout,  status="old", iostat=info)
		do itype=1, ntype
			scanok=search_word_free(fid, "VRHFIN", line)
			i=index(line,"=")
			j=index(line,":")
			line=line(i+1:j-1)
			read(line,*) symbols(itype)
		end do
	close(fid)
	!------------------------------------------------------------------------------
	return
end subroutine
