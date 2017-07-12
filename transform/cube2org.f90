program cube2org
	use kinds, only: DP
	use file,  only: name_main
	use param, only: au2a
	use math,  only: v3_norm, volume_v3
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: i, j, k, ix, iy, iz, kx, ky, kz, direction, ntype
	integer                     :: natom, nx, ny, nz, nmax, dims, near_point(3)
	integer                     :: irecl, ngrid(3), info
	integer                     :: fid, fid1, fid2
	integer       , allocatable :: nat(:)
	real(DP)                    :: center(3), step(3,3), a(3,3), da(3,3), dr(3), pointx(3), pointy(3)
	real(DP)                    :: r(3), dx, dy, dz, xx, yy, zz, dv, total_value
	real(DP)                    :: a1, a2, a3
	real(DP)      , allocatable :: x(:,:), nata(:), ggrid3(:,:,:), dgrid3(:,:,:,:)
	real(DP)      , allocatable :: ggrid1(:,:), dgrid1(:,:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: title, fcube, fg2, fd2, fdg1,fg1, tmp, fmt, fg3
	character(200)              :: ctype, prefix, prefix1
	!----------------------------------------------------------------------------
	! In cube file, the unit of length is Bohr
	!
	fid   = 1
	fid1  = 2
	fid2  = 3
	call getarg(1, fcube)
	prefix1=trim(name_main(fcube))
	fg3=trim(prefix1)//"_xyz.dat"
	call getarg(2, tmp)
	read(tmp, * ) dims
	if (dims==1 .or. dims==2) then
		call getarg(3, tmp)
		read(tmp, *) direction
		call getarg(4, tmp)
		read(tmp, *) pointx(1)
		call getarg(5, tmp)
		read(tmp, *) pointx(2)
		call getarg(6, tmp)
		read(tmp, *) pointx(3)
		call getarg(7, tmp)
		read(tmp, *) ntype
	else if (dims>=4) then
		write(*,*) "Not implemented"
		stop
	end if
	!----------------------------------------------------------------------------
	call qm_cube_natom_nxyz(fcube, natom, ngrid, info)
	!----------------------------------------------------------------------------
	nx=ngrid(1)
	ny=ngrid(2)
	nz=ngrid(3)
	nmax=max(max(nx, ny), nz)
	!----------------------------------------------------------------------------
	allocate(x       (3,      natom))
	allocate(nat     (        natom))
	allocate(nata    (        natom))
	allocate(symbol  (        natom))
	allocate(ggrid3  (nx, ny, nz   ))
	allocate(dgrid3  (nx, ny, nz, 3))
	allocate(ggrid1  (nmax,       3))
	allocate(dgrid1  (nmax,    3, 3))
	!----------------------------------------------------------------------------
	call qm_cube_info(fcube, natom, ngrid, title, ctype, center, &
		& step, nat, nata, x, ggrid3, info)
	!----------------------------------------------------------------------------
	call nat_to_symbol(natom,nat,symbol)
	!----------------------------------------------------------------------------
	dx = v3_norm(step(1,1))
	dy = v3_norm(step(1,2))
	dz = v3_norm(step(1,3))
	!----------------------------------------------------------------------------
	a(:,1) = step(:,1) * nx
	a(:,2) = step(:,2) * ny
	a(:,3) = step(:,3) * nz
	!----------------------------------------------------------------------------
	if (dims==3) then
		open(fid, file=fg3)
			do i=1, ngrid(1)
				da(:,1) = (i-1) * step(:,1)
				do j=1, ngrid(2)
					da(:,2) = (j-1) * step(:,2)
					do k=1, ngrid(3)
						da(:,3) = (k-1) * step(:,3)
						dr = da(:,1) + da(:,2) + da(:,3)
						write(fid,'(3f20.10, es20.10)') dr, ggrid3(i,j,k)
					end do
				end do
			end do
		close(fid)
		write(*,'(" Input  file: ", a)') trim(fcube)
		write(*,'(" Output file: ", a)') trim(fg3)
		write(*,'(" Normal terminated!")')
		stop
	end if
	!----------------------------------------------------------------------------
	do i=1, natom
		x(:,i) = x(:,i) - center
	end do
	!----------------------------------------------------------------------------
	total_value=0.d0
	do ix=1, nx
		do iy=1, ny
			do iz=1, nz
				total_value=total_value+ggrid3(ix,iy,iz)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	dv = volume_v3(step)
	total_value = total_value * dv
	write(*,'("Step")')
	do i=1, 3
		write(*,'(3f13.7)') (step(i,j), j=1, 3)
	end do
	if (total_value>1.D5) then
		write(*,'("Volume   : ",  f17.7)') dv
		write(*,'("Integral : ", es17.7)') total_value
	else
		write(*,'("Volume   : ",  f17.7)') dv
		write(*,'("Integral : ",  f17.7)') total_value
	end if
	!----------------------------------------------------------------------------
	call search_nearest_point_3D(center, step, ngrid, pointx, pointy, near_point)
	!----------------------------------------------------------------------------
	kx   = near_point(1)
	ky   = near_point(2)
	kz   = near_point(3)
	!----------------------------------------------------------------------------
	call get_dgrid3(ngrid, step, ggrid3, dgrid3)
	!----------------------------------------------------------------------------
	call get_grid_int1(ngrid, nmax, step, ggrid3, ggrid1, near_point, ntype)
	do i=1, 3
		call get_grid_int1(ngrid, nmax, step, dgrid3(1,1,1,i), dgrid1(1,1,i), near_point, ntype)
	end do
	!----------------------------------------------------------------------------
	call number_filename3(fg2,trim(prefix1),pointy,7,3)
	prefix=trim(fg2)
	!----------------------------------------------------------------------------
	if (dims==1) then
		if      (direction==1) then
			!----------------------------------------------------------------------
			! Along x
			!
			fg1 = trim(prefix1)//"_Lx.dat"
			open(fid, file=fg1, status="unknown")
				write(fid, '(a15, 4a20)') "x(Angstrom)", "g(au)", &
					& "dg/dx(au)", "dg/dy(au)", "dg/dz(au)"
				do ix=1, nx
					xx=center(1) + (ix-1) * dx
					write(fid, '(f15.8, 4es20.10)') xx * au2a, &
						& ggrid1(ix,1), (dgrid1(ix,i,1), i=1, 3)
				end do
			close(fid)
			!----------------------------------------------------------------------
		else if (direction==2) then
			!----------------------------------------------------------------------
			! Along y
			!
			fg1 = trim(prefix1)//"_Ly.dat"
			open(fid, file=fg1, status="unknown")
				write(fid, '(a15, 4a20)') "y(Angstrom)", "g(au)", &
					& "dg/dx(au)", "dg/dy(au)", "dg/dz(au)"
				do iy=1, ny
					yy=center(2) + (iy-1) * dy
					write(fid, '(f15.8, 4es20.10)') yy * au2a, &
						& ggrid1(iy,2), (dgrid1(iy,i,2), i=1, 3)
				end do
			close(fid)
			!----------------------------------------------------------------------
		else if (direction==3) then
			!----------------------------------------------------------------------
			! Along y
			!
			fg1 = trim(prefix1)//"_Lz.dat"
			open(fid, file=fg1, status="unknown")
				write(fid, '(a15, 4a20)') "z(Angstrom)", "g(au)", &
					& "dg/dx(au)", "dg/dy(au)", "dg/dz(au)"
				do iz=1, nz
					zz=center(3) + (iz-1) * dz
					write(fid, '(f15.8, 4es20.10)') zz * au2a, &
						& ggrid1(iz,3), (dgrid1(iz,i,3), i=1, 3)
				end do
			close(fid)
		end if
	else if(dims==2) then
		if (direction==1) then
			!----------------------------------------------------------------------
			fg2 = trim(prefix)//"_Syz.dat"
			fd2 = trim(prefix)//"_Syz_dx.dat"
			irecl=20 * ny+10
			write(fmt,*) ny
			!
			open(fid1, file=fg2, recl=irecl)
			open(fid2, file=fd2, recl=irecl)
				do iz=1, nz
					write(fid1,'('//trim(fmt)//'es20.10)') (ggrid3(kx, iy, iz  ), iy=1, ny)
					write(fid2,'('//trim(fmt)//'es20.10)') (dgrid3(kx, iy, iz,1), iy=1, ny)
				end do
			close(fid1)
			close(fid2)
			
		else if (direction==2) then
			!----------------------------------------------------------------------
			fg2 = trim(prefix)//"_Szx.dat"
			fd2 = trim(prefix)//"_Szx_dy.dat"
			irecl=20 * nz+10
			write(fmt,*) nz
			open(fid1, file=fg2, recl=irecl)
			open(fid2, file=fd2, recl=irecl)
				do ix=1, nx
					write(fid1,'('//trim(fmt)//'es20.10)') (ggrid3(ix, ky, iz  ), iz=1, nz)
					write(fid2,'('//trim(fmt)//'es20.10)') (dgrid3(ix, ky, iz,2), iz=1, nz)
				end do
			close(fid1)
			close(fid2)
			!----------------------------------------------------------------------
		else if (direction==3) then
			fg2 = trim(prefix)//"_Sxy.dat"
			fd2 = trim(prefix)//"_Sxy_dz.dat"
			irecl=20 * nx+10
			write(fmt,*) nx
			open(fid1, file=fg2, recl=irecl)
			open(fid2, file=fd2, recl=irecl)
				do iy=1, ny
					write(fid1,'('//trim(fmt)//'es20.10)') (ggrid3(ix, iy, kz  ), ix=1, nx)
					write(fid2,'('//trim(fmt)//'es20.10)') (dgrid3(ix, iy, kz,3), ix=1, nx)
				end do
			close(fid1)
			close(fid2)
		end if
	end if
	!----------------------------------------------------------------------------
	write(*,'(" Input  file: ", a)') trim(fcube)
	if (dims==1) then
		write(*,'(" Output file: ", a)') trim(fg1)
	else if (dims==2) then
		write(*,'(" Output file: ", a)') trim(fg2)
		write(*,'(" Output file: ", a)') trim(fd2)
	end if
	write(*,'(" Normal terminated!")')
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(nat   )
	deallocate(nata  )
	deallocate(symbol)
	deallocate(ggrid3)
	deallocate(dgrid3)
	deallocate(ggrid1)
	deallocate(dgrid1)
	!----------------------------------------------------------------------------
	stop
end
