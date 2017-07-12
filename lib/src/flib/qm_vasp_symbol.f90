subroutine qm_vasp_symbol(fname, natom, symbol, info)
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	!----------------------------------------------------------------------------
	character(*), intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	integer                   :: info
	integer                   :: i, j, k
	integer                   :: ntype
	integer     , allocatable :: ntypes(:)
	character(2), allocatable :: symbols(:)
	!----------------------------------------------------------------------------
	call qm_vasp_outcar_ntype  (fname, ntype,          info)
	!----------------------------------------------------------------------------
	allocate(ntypes (ntype))
	allocate(symbols(ntype))
	!----------------------------------------------------------------------------
	call qm_vasp_outcar_natoms (fname, ntype, ntypes , info)
	call qm_vasp_outcar_symbols(fname, ntype, symbols, info)
	!----------------------------------------------------------------------------
	k=0
	do i=1, ntype
		do j=1, ntypes(i)
			k=k+1
			symbol(k) = symbols(i)
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(ntypes )
	deallocate(symbols)
	!----------------------------------------------------------------------------
	return
end subroutine