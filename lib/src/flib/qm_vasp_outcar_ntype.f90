subroutine qm_vasp_outcar_ntype(fout, ntype, info)
	implicit none
	!------------------------------------------------------------------------------
	character(*), intent( in) :: fout
	integer     , intent(out) :: ntype, info
	!------------------------------------------------------------------------------
	integer                   :: fid
	!------------------------------------------------------------------------------
	integer                   :: i
	character(200)            :: line
	logical                   :: scanok
	logical     , external    :: search_word_free
	integer     , external    :: number_of_words
	!------------------------------------------------------------------------------
	info = 0
	!------------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fout,  status="old", iostat=info)
		scanok=search_word_free(fid, "ions per type", line)
		i=index(line,"=")
		line=line(i+1:)
		ntype=number_of_words(line)
	close(fid)
	!------------------------------------------------------------------------------
	return
end subroutine
