program nwout2gjf
	use kinds, only: DP
	use param, only: au2a
	use file , only: name_main
	!----------------------------------------------------------------------------
	implicit none
	integer                     :: info, natom, fid
	character(200)              :: fxyz, fgjf
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	call getarg(1, fxyz)
	fgjf=trim(name_main(fxyz))//".gjf"
	!----------------------------------------------------------------------------
	call qm_file_natom(fxyz, natom, info)
	!-------------------------------------------------------------------------
	allocate(x     (3,natom))
	allocate(symbol(  natom))
	!-------------------------------------------------------------------------
	call qm_file_symbol(fxyz, natom, symbol, info)
	call qm_file_coord (fxyz, natom,      x, info)
	x = x*au2a
	!----------------------------------------------------------------------------
	call write_gjf(fgjf, natom, symbol, x)
	!----------------------------------------------------------------------------
	deallocate(symbol   )
	deallocate(x        )
	!----------------------------------------------------------------------------
	stop
end
