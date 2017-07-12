program xsf2cube
	use kinds, only: DP
	use file,  only: name_main
	use param, only: au2a
	!----------------------------------------------------------------------------
	implicit none
	integer               :: natom, ngrid(3), info, i, j, k, ipara
	real(DP)              :: center(3), a(3,3), step(3,3), disp(3)
	integer , allocatable :: nat(:)
	real(DP), allocatable :: x(:,:), grid(:,:,:)
	real(DP)              :: scale, xmin, xmax
	character(200)        :: title, caltype
	character(200)        :: fxsf, fcube, tmp, ctype
	!----------------------------------------------------------------------------
	ipara=iargc()
	scale=1.D0
	call getarg(1, fxsf)
	if (ipara==2) then
		call getarg(2, tmp)
		read(tmp,*) scale
	else if (ipara==3) then
		call getarg(2, tmp)
		read(tmp,*) scale
		call getarg(3, tmp)
		read(tmp,*) ctype
	end if
	!----------------------------------------------------------------------------
	fcube=trim(name_main(fxsf))//".cube"
	!----------------------------------------------------------------------------
	call qm_xsf_natom_nxyz(fxsf, natom, ngrid, info)
	!----------------------------------------------------------------------------
	allocate(nat(  natom))
	allocate(x  (3,natom))
	allocate(grid(ngrid(1), ngrid(2), ngrid(3)))
	!----------------------------------------------------------------------------
	call qm_xsf_info(fxsf, natom, ngrid, center, a, nat, x, grid, info)
	!----------------------------------------------------------------------------
	disp=0.5D0
	call rotn(1,a,disp)
	!----------------------------------------------------------------------------
	center=center-disp
	do i=1, natom
		x(:,i) = x(:,i) - disp
	end do
	!----------------------------------------------------------------------------
	title="Cubfile created from xsf file"
	caltype="Total SCF Density"
	center = center / au2a
	a      = a      / au2a
	x      = x      / au2a
	grid   = grid   / au2a**3
	do i=1, 3
		step(:,i) = a(:,i) / ngrid(i)
	end do
	grid = grid * scale
	!----------------------------------------------------------------------------
	xmin =  1.D99
	xmax = -1.D99
	do i=1, ngrid(1)
		do j=1, ngrid(2)
			do k=1, ngrid(3)
				if ( xmin > grid(i,j,k) ) xmin = grid(i,j,k)
				if ( xmax < grid(i,j,k) ) xmax = grid(i,j,k)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	write(*,'("Range from",2x, es18.7, 2x, "to", 2x, es18.7)') xmin, xmax
	!----------------------------------------------------------------------------
	if (trim(ctype)=="den") then
		title="Title Card Required fdensity=scf"
		caltype="Electron density from Total SCF Density"
	else if (trim(ctype)=="esp") then
		title="Title Card Required potential=scf"
		caltype="Electrostatic potential from Total SCF Density"
	end if
	!----------------------------------------------------------------------------
	call write_cube(fcube, title, caltype, natom, center, ngrid, step, nat, x, grid)
	!----------------------------------------------------------------------------
	deallocate(nat )
	deallocate(x   )
	deallocate(grid)
	!----------------------------------------------------------------------------
	stop
end
