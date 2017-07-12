subroutine write_poscar(fpos, title, a, natom, symbol, x, fix)
	use kinds, only: DP
	use array, only: array_group_number, array_group
	implicit none
	!----------------------------------------------------------------------------
	character(*)              , intent(in)  :: fpos, title
	!----------------------------------------------------------------------------
	integer                   , intent(in)  :: natom
	real(DP)                  , intent(in)  :: a(3,3)
	real(DP)                  , intent(in)  :: x(3,natom)
	character(*)              , intent(in)  :: fix(3,natom)
	character(*)              , intent(in)  :: symbol(natom)
	!----------------------------------------------------------------------------
	integer                                 :: fid, i, j
	integer                                 :: ng
	integer                   , allocatable :: natoms (:)
	integer                   , allocatable :: idx    (:)
	character(len(symbol(1))) , allocatable :: symbols(:)
	character(200)                          :: fmt
	!----------------------------------------------------------------------------
	ng = array_group_number(natom, symbol)
	!----------------------------------------------------------------------------
	allocate(idx    (natom))
	allocate(natoms (ng   ))
	allocate(symbols(ng   ))
	write(fmt,*) ng
	!----------------------------------------------------------------------------
	call array_group(natom, symbol, ng, natoms, symbols)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	call sort_array_string(natom, symbol, idx, 1)
	!----------------------------------------------------------------------------
	open(fid, file=fpos)
		write(fid,'(a)') trim(title)
		write(fid,'(f17.10)') 1.D0
		do i=1, 3
			write(fid,'(3f17.10)') (a(j,i), j=1, 3)
		end do
		write(fid,'('//trim(fmt)//'a6)') (trim(symbols(i)), i=1, ng)
		write(fid,'('//trim(fmt)//'i6)') (natoms (i), i=1, ng)
		write(fid,'("Selective dynamics")')
		write(fid,'("direct")')
		do i=1, natom
			j=idx(i)
			write(fid,'(3f12.7, 3a4)') x(:,j), fix(:,j)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(idx    )
	deallocate(natoms )
	deallocate(symbols)
	!----------------------------------------------------------------------------
	return
end subroutine