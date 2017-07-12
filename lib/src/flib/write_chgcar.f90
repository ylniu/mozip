subroutine write_chgcar(fchg, natom, n, ntype, symbols, natoms, x, &
	title, alat, a, coord_type, grid)
	use kinds, only: DP
	use array, only: array_sum
	implicit none
	!----------------------------------------------------------------------------
	character(*)  , intent(in) :: fchg
	integer       , intent(in) :: natom
	integer       , intent(in) :: n(3)
	integer       , intent(in) :: ntype
	character(  2), intent(in) :: symbols(ntype)
	integer       , intent(in) :: natoms (ntype)
	real(DP)      , intent(in) :: x(3,natom)
	real(DP)      , intent(in) :: grid(n(1), n(2), n(3))
	!----------------------------------------------------------------------------
	character(200), intent(in) :: title
	character(200), intent(in) :: coord_type
	real(DP)      , intent(in) :: alat
	real(DP)      , intent(in) :: a(3,3)
	!----------------------------------------------------------------------------
	integer                    :: fid
	integer                    :: i
	character(200)             :: fmt
	integer       , external   :: number_of_words
	logical       , external   :: is_number
	logical       , external   :: search_word_free_last
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	write(fmt,*) ntype
	open(fid, file=fchg)
		write(fid,'(a)') trim(title)
		write(fid,'(x,f12.6)') alat
		do i=1, 3
			write(fid,'(x,3f12.6)') a(:,i)
		end do
		if (trim(symbols(1))/="") then
			write(fid,'('//trim(fmt)//'a6)') (symbols(i), i=1, ntype)
		end if
		!-------------------------------------------------------------------------
		write(fid,'('//trim(fmt)//'i6)') (natoms(i), i=1, ntype)
		!-------------------------------------------------------------------------
		write(fid,'(a)') trim(coord_type)
		do i=1, natom
			write(fid,'(3f10.6)') x(:,i)
		end do
		write(fid,*)
		write(fid,'(3i5)') n
		write(fid,'(5(es18.11,x))') grid
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
