program gout2xyz
	use kinds, only: DP
	use file , only: name_main
	implicit none
	!--------------------------------------------------------------------
	integer                     :: fid
	integer                     :: natom, nopt, info
	real(DP)      , allocatable :: x(:,:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: fout, fxyz
	!--------------------------------------------------------------------
	fid = 1
	call getarg(1, fout)
	fxyz=trim(name_main(fout))//".xyz"
	!--------------------------------------------------------------------
	call qm_file_natom (fout, natom, info)
	call qm_file_nopt  (fout, nopt , info)
	!--------------------------------------------------------------------
	allocate(symbol(   natom      ))
	allocate(x     (3, natom, nopt))
	!--------------------------------------------------------------------
	call qm_file_symbol(fout, natom, symbol,    info)
	call qm_file_nopt_x(fout, natom, nopt  , x, info)
	call write_xyz_n   (fxyz, nopt , natom , symbol, x)
	!--------------------------------------------------------------------
	deallocate(symbol)
	!--------------------------------------------------------------------
	call exit(0)
end