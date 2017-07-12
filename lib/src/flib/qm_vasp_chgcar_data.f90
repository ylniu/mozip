subroutine qm_vasp_chgcar_data(fchg, natom, n, ntype, symbols, natoms, x, &
	title, alat, a, coord_type, grid)
	use kinds, only: DP
	use array, only: array_sum
	implicit none
	!----------------------------------------------------------------------------
	character(*)  , intent( in) :: fchg
	integer       , intent( in) :: natom
	integer       , intent( in) :: n(3)
	integer       , intent( in) :: ntype
	character(  2), intent(out) :: symbols(ntype)
	integer       , intent(out) :: natoms (ntype)
	real(DP)      , intent(out) :: x(3,natom)
	real(DP)      , intent(out) :: grid(n(1), n(2), n(3))
	!----------------------------------------------------------------------------
	character(200), intent(out) :: title
	character(200), intent(out) :: coord_type
	real(DP)      , intent(out) :: alat
	real(DP)      , intent(out) :: a(3,3)
	!----------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: i
	character(200)              :: line, tmp
	logical                     :: scanok
	integer       , external    :: number_of_words
	logical       , external    :: is_number
	logical       , external    :: search_word_free_last
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fchg, status="old")
		read(fid,*) title
		read(fid,*) alat
		do i=1, 3
			read(fid,*) a(:,i)
		end do
		read(fid,'(a)') line
		!-------------------------------------------------------------------------
		read(line,*) tmp
		if (is_number(tmp)) then
			symbols=""
		else
			read(line,*) symbols
			read(fid,'(a)') line
		end if
		read(line,*) natoms
		!-------------------------------------------------------------------------
		read(fid,*) coord_type
		do i=1, natom
			read(fid,*) x(:,i)
		end do
		read(fid,*)
		read(fid,'(a)') line
		tmp=trim(line)
		rewind(fid)
		scanok=search_word_free_last(fid, trim(tmp), line)
		read(fid,'(a)') line
		i=index(line,"E")
		backspace(fid)
		if (i>0) then
			read(fid,'(5e18.11)') grid
		else
			read(fid,*) grid
		end if
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine