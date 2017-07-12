program cube2xsf
	use kinds, only: DP
	use file,  only: name_main
	use param, only: au2a
	use math,  only: v2_cos
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, i, ix, iy, iz, info
	integer                     :: natom, ngrid(3), nx, ny, nz
	integer       , allocatable :: nat(:)
	real(DP)                    :: center(3), a(3,3), step(3,3)
	real(DP)      , allocatable :: x(:,:), nata(:), grid(:,:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: title, fcube, fxsf
	character(200)              :: ctype
	!----------------------------------------------------------------------------
	! In cube file, the unit of length is Angstrom
	!
	fid=1
	call getarg(1, fcube)
	fxsf=trim(name_main(fcube))//".xsf"
	!----------------------------------------------------------------------------
	call qm_cube_natom_nxyz(fcube, natom, ngrid, info)
	!----------------------------------------------------------------------------
	nx=ngrid(1)
	ny=ngrid(2)
	nz=ngrid(3)
	!
	allocate(x      (3,   natom))
	allocate(nat    (     natom))
	allocate(nata   (     natom))
	allocate(symbol (     natom))
	allocate(grid   (nx, ny, nz))
	!----------------------------------------------------------------------------
	call qm_cube_info(fcube, natom, ngrid, title, ctype, center, step, nat, nata, x, grid, info)
	!----------------------------------------------------------------------------
	call nat_to_symbol(natom,nat,symbol)
	!----------------------------------------------------------------------------
	do i=1, 3
		a(:,i) = step(:,i) * ngrid(i)
	end do
	do i=1, natom
		x(:,i) = x(:,i) - center
	end do
	!----------------------------------------------------------------------------
	a = a * au2a
	x = x * au2a
	!----------------------------------------------------------------------------
	! In xsf file, the unit of length should be transformed from Angstrom to Bohr.
	!
	! Angstrom -> Bohr
	!
	!----------------------------------------------------------------------------
	call write_xsf_3dgrid(fxsf, a, natom, symbol, x, nx, ny, nz, grid)
	!----------------------------------------------------------------------------
	write(*,'(" Input  file: ", a)') trim(fcube)
	write(*,'(" Output file: ", a)') trim(fxsf)
	write(*,'(" Normal terminated!")')
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(nat   )
	deallocate(nata  )
	deallocate(symbol)
	deallocate(grid   )
	!----------------------------------------------------------------------------
	stop
end