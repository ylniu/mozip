program wienpsi2xsf
	use kinds, only: DP
	use param, only: au2a
	use file,  only: name_main
	!----------------------------------------------------------------------------
	integer                     :: ngrid(3), info
	real(DP)                    :: cell1(6), cell2(6)
	real(DP)                    :: a1(3,3), a2(3,3)
	real(DP)                    :: a, b, c
	real(DP)                    :: alpha, beta, gamma
	real(DP)      , allocatable :: x(:,:), grid(:,:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: fpsink, fstruct, fxsf
	!----------------------------------------------------------------------------
	call getarg(1, fstruct)
	call getarg(2, fpsink)
	fxsf=trim(name_main(fpsink))//".xsf"
	!----------------------------------------------------------------------------
	call qm_wien2k_struct_natom(fstruct, natom, info)
	call qm_wien2k_psink_ngrid (fpsink , ngrid, info)
	!----------------------------------------------------------------------------
	allocate(x      (3,natom))
	allocate(symbol (  natom))
	allocate(grid   (ngrid(1), ngrid(2), ngrid(3)))
	!----------------------------------------------------------------------------
	call qm_wien2k_struct_info(fstruct, natom, cell1, symbol, x, info)
	call qm_wien2k_psink_info (fpsink , ngrid, cell2, grid, info)
	!----------------------------------------------------------------------------
	a     = cell1(1)
	b     = cell1(2)
	c     = cell1(3)
	alpha = cell1(4)
	beta  = cell1(5)
	gamma = cell1(6)
	call lattice_constants_to_a(a1, a, b, c, alpha, beta, gamma)
	!----------------------------------------------------------------------------
	a     = cell2(1)
	b     = cell2(2)
	c     = cell2(3)
	alpha = cell2(4)
	beta  = cell2(5)
	gamma = cell2(6)
	call lattice_constants_to_a(a2, a, b, c, alpha, beta, gamma)
	!----------------------------------------------------------------------------
	a1    = a1 * au2a
	a2    = a2 * au2a
	call coord_crys_to_cart(natom, a1, x)
	!----------------------------------------------------------------------------
	call write_xsf_3dgrid(fxsf, a1, natom, symbol, x, ngrid(1), ngrid(2), ngrid(3), grid)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	deallocate(grid  )
	!----------------------------------------------------------------------------
	stop
end