program gjf2xyz_charge
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, natom, info
	real(DP)      , allocatable :: x(:,:)
	integer       , allocatable :: nat(:)
	real(DP)      , allocatable :: charge(:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: fgjf, fxyz
	!----------------------------------------------------------------------------
	fid = 1
	call getarg(1,fgjf)
	fxyz=trim(name_main(fgjf))//".xyz"
	!----------------------------------------------------------------------------
	call qm_file_natom(fgjf, natom, info)
	!----------------------------------------------------------------------------
	allocate(x     (3,natom))
	allocate(nat   (  natom))
	allocate(charge(  natom))
	allocate(symbol(  natom))
	call qm_file_coord   (fgjf, natom,    x  , info)
	call qm_file_nat     (fgjf, natom,    nat, info)
	call nat_to_symbol   (natom,  nat,       symbol)
	call qm_file_charge  (fgjf, natom, charge, info)
	call write_xyz_charge(fxyz,natom,symbol,x,charge)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(nat   )
	deallocate(charge)
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
