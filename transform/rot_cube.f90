program rot_cube
	use kinds, only: DP
	use param, only: au2a
	use math,  only: v3_norm, v3_normlize, v2_cos, v3_cross, inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, info, natom, ngrids(3), ngrid(3)
	integer                     :: atom_origin, atom_end_x, atom_end_xy
	integer                     :: nx, ny, nz, i, j
	integer       , allocatable :: nat(:)
	real(DP)                    :: margin, steps(3), center(3), stepa(3,3), a(3,3), b(3,3), d(3), aa(3,3)
	real(DP)                    :: orientation(3)
	real(DP)                    :: r, inva(3,3), xmin(3), xmax(3), da(3), h(3), d0(3), d1(3)
	real(DP)      , allocatable :: nata(:), x(:,:), x1(:,:), grid(:,:,:)
	!----------------------------------------------------------------------------
	character(200)              :: finp, fcube, priority, title, ctype, priority_rot
	character(  2), allocatable :: symbol(:)
	!----------------------------------------------------------------------------
	namelist /control/ atom_origin, atom_end_x, atom_end_xy, fcube, margin, &
		& orientation, ngrids, steps, priority, priority_rot
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	!----------------------------------------------------------------------------
	fid=1
	open(fid, file=finp, status="old")
		read(fid, control)
	close(fid)
	!----------------------------------------------------------------------------
	call qm_cube_natom_nxyz(fcube, natom, ngrid, info)
	!----------------------------------------------------------------------------
	nx=ngrid(1)
	ny=ngrid(2)
	nz=ngrid(3)
	!
	allocate(x      (3,   natom))
	allocate(x1     (3,   natom))
	allocate(nat    (     natom))
	allocate(nata   (     natom))
	allocate(symbol (     natom))
	allocate(grid   (nx, ny, nz))
	!----------------------------------------------------------------------------
	call qm_cube_info(fcube, natom, ngrid, title, ctype, center, stepa, nat, nata, x, grid, info)
	!----------------------------------------------------------------------------
	call nat_to_symbol(natom,nat,symbol)
	!----------------------------------------------------------------------------
	a=0.D0
	if (trim(priority_rot)=="orientation") then
		if (orientation(1)==0.D0 .and. orientation(2)==0.D0 .and. orientation(3)==0.D0) then
			do i=1, 3
				a(i,i) = 1.D0
			end do
		else
			orientation = v3_normlize(orientation)
			write(*,*) orientation
			if (orientation(1)==0.D0) then
				a(1,1) = 1.D00
			end if
			a(:,2) = orientation
			a(:,3) = v3_cross(a(1,1), a(1,2))
		end if
		write(*,'(3f15.7)') a(:,1)
		write(*,'(3f15.7)') a(:,2)
		write(*,'(3f15.7)') a(:,3)
	else if (trim(priority_rot)=="atom") then
		d      = x(:,atom_end_x) - x(:,atom_origin)
		a(:,1) = d / v3_norm(d)
		!-------------------------------------------------------------------------
		d      = x(:,atom_end_xy) - x(:,atom_origin)
		a(:,2) = d / v3_norm(d)
		!-------------------------------------------------------------------------
		r      = v2_cos(a(1,1), a(1,2))
		a(:,2) = a(:,2) - a(:,1) * r
		!-------------------------------------------------------------------------
		a(:,3) = v3_cross(a(1,1), a(1,2))
	else
		write(*,*) "priority_rot is not implemented, please set priority='orientation' or priority='atom'"
		stop
	end if
	!----------------------------------------------------------------------------
	x1 = x
	!----------------------------------------------------------------------------
	inva = inverse3(a)
	!----------------------------------------------------------------------------
	call rotn (natom,inva,x1)
	!----------------------------------------------------------------------------
	xmin=x1(:,1)
	xmax=x1(:,1)
	do i=2, natom
		do j=1, 3
			if ( xmin(j) > x1(j,i) ) xmin(j) = x1(j,i)
			if ( xmax(j) < x1(j,i) ) xmax(j) = x1(j,i)
		end do
	end do
	xmin   = xmin-margin
	xmax   = xmax+margin
	center = xmin
	da     = xmax - xmin
	!----------------------------------------------------------------------------
	aa = 1.D0
	do i=1, 3
		aa(i,i) = da(i)
	end do
	!----------------------------------------------------------------------------
	if (trim(priority)=="steps") then
		h = steps
		do j=1, 3
			ngrids(j)= int(da(j)/h(j))
		end do
	else if (trim(priority)=="ngrids") then
		do j=1, 3
			h(j) = da(j) / ngrids(j)
		end do
	else
		write(*,*) "priority is not implemented, please set priority='step' or priority='ngrids'"
		stop
	end if
	!----------------------------------------------------------------------------
	b=0.D0
	do j=1, 3
		b(j,j) = h(j)
	end do
	!----------------------------------------------------------------------------
	call rotn(1, a, center)
	call rotn(3, a, b     )
	!----------------------------------------------------------------------------
! 	do j=1, 3
! 		aa(:,j) = b(:,j) * ngrids(j)
! 	end do
! 	do i=1, natom
! 		x(:,i)=x(:,i)-center
! 	end do
! 	call write_xsf("test.xsf", aa, natom, symbol, x)
! 	do i=1, natom
! 		x(:,i)=x(:,i)+center
! 	end do
	!----------------------------------------------------------------------------
	write(*,'("-----------------------------------------------------------")')
	write(*,'("  Original points in cube:", i10)') nx*ny*nz
	write(*,'("  New      points        :", i10)') ngrids(1) * ngrids(2) * ngrids(3)
	write(*,'("-----------------------------------------------------------")')
	write(*,'("  Unit: Angstrom, used as input to generate cube file")')
	write(*,'("-----------------------------------------------------------")')
	write(*,'("!",i10, 3f15.7)') -10, center * au2a
	do j=1, 3
		write(*,'("!",i10, 3f15.7)') ngrids(j), b(:,j) * au2a
	end do
	write(*,'("-----------------------------------------------------------")')
	write(*,'("  Unit: Bohr")')
	write(*,'("-----------------------------------------------------------")')
	write(*,'(i10, 3f15.7)') -10, center
	do j=1, 3
		write(*,'(i10, 3f15.7)') ngrids(j), b(:,j)
	end do
	write(*,'("-----------------------------------------------------------")')
	!----------------------------------------------------------------------------
	stop
end