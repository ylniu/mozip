subroutine qm_vasp_outcar_natoms(fout, ntype, natoms, info)
	implicit none
	!------------------------------------------------------------------------------
	character(*), intent( in) :: fout
	integer     , intent( in) :: ntype
	integer     , intent(out) :: natoms(ntype), info
	!------------------------------------------------------------------------------
	integer                   :: fid
	!------------------------------------------------------------------------------
	integer                   :: i
	character(200)            :: line
	logical                   :: scanok
	logical     , external    :: search_word_free
	!------------------------------------------------------------------------------
	info = 0
	!------------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fout,  status="old", iostat=info)
		scanok=search_word_free(fid, "ions per type", line)
		i=index(line,"=")
		line=line(i+1:)
		read(line,*) natoms
	close(fid)
	!------------------------------------------------------------------------------
	return
end subroutine
