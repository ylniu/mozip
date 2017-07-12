program gjf2xyz
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, natom
	real(DP)      , allocatable :: x(:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: fgjf, fxyz
	!----------------------------------------------------------------------------
	fid    = 1
	call getarg(1,fgjf)
	fxyz=trim(name_main(fgjf))//".xyz"
	!----------------------------------------------------------------------------
	call get_gjf_natom(fgjf, natom)
	!----------------------------------------------------------------------------
	allocate(x     (3,natom))
	allocate(symbol(  natom))
	call get_gjf_coord(fgjf, natom, x, symbol)
	call write_xyz(fxyz,natom,symbol,x)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
