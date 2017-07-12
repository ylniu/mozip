program cif2gjf
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	integer                     :: info, natom
	real(DP)                    :: a(3,3)
	character(200)              :: fcif, fgjf, file_type, version
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	!----------------------------------------------------------------------------
	call getarg(1, fcif)
	call qm_file_type(fcif, file_type, version, info)
	if (info<0 .or. trim(file_type) /= "CIF") then
		write(*,*) trim(fcif)//" is not cif file, stop!"
		stop
	end if
	!----------------------------------------------------------------------------
	call qm_file_natom(fcif, natom, info)
	!----------------------------------------------------------------------------
	allocate(symbol(natom))
	allocate(x   (3,natom))
	!----------------------------------------------------------------------------
	call qm_file_crystal(fcif, natom, a, symbol, x, info)
	call rotn(natom, a, x)
	fgjf=trim(name_main(fcif))//".gjf"
	!----------------------------------------------------------------------------
	call write_gjf(fgjf, natom, symbol, x)
	!----------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	!----------------------------------------------------------------------------
	stop
end