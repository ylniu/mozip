subroutine get_gjf_coord(fname, natom, x, symbol)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	character(*)   , intent( in) :: fname
	integer        , intent( in) :: natom
	real(DP)       , intent(out) :: x(3,natom)
	character(*)   , intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	integer                      :: fid, i, j, atom_begin
	integer                      :: nlines
	character(200) , allocatable :: lines(:)
	integer        , external    :: number_of_lines
	logical        , external    :: is_number
	!----------------------------------------------------------------------------
	call get_gjf_atom_linenumber(fname,atom_begin)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fname, status="old")
	!----------------------------------------------------------------------------
		nlines=number_of_lines(fid)
		allocate(lines(nlines))
		rewind(fid)
		do i=1, nlines
			read(fid,'(a)') lines(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	! find atom_begin
	!
	j=0
	do i=atom_begin, atom_begin+natom-1
		j=j+1
		read(lines(i),*) symbol(j), x(:,j)
	end do
	!----------------------------------------------------------------------------
	deallocate(lines)
	!----------------------------------------------------------------------------
	return
end subroutine
