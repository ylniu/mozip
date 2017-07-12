subroutine get_grid_int1(ngrid, nmax, step,  grid,  grid1, near_point, ntype)
	!----------------------------------------------------------------------------
	! ntype = 1, integration
	!
	! ntype = 2, average
	!
	use kinds, only: DP
	use math,  only: v3_norm
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: ngrid(3), nmax, near_point(3), ntype
	real(DP), intent( in) :: step(3,3)
	real(DP), intent( in) :: grid(ngrid(1), ngrid(2), ngrid(3))
	!----------------------------------------------------------------------------
	real(DP), intent(out) :: grid1(nmax, 3)
	!----------------------------------------------------------------------------
	integer               :: kx, ky, kz
	!----------------------------------------------------------------------------
	integer               :: ix, iy, iz, nx, ny, nz, nxy, nyz, nzx
	real(DP)              :: dx, dy, dz, dSxy, dSyz, dSzx
	!----------------------------------------------------------------------------
	nx = ngrid(1)
	ny = ngrid(2)
	nz = ngrid(3)
	!----------------------------------------------------------------------------
	if (ntype==0) then
		kx   = near_point(1)
		ky   = near_point(2)
		kz   = near_point(3)
		!-------------------------------------------------------------------------
		do ix=1, nx
			grid1(ix,1) = grid(ix,ky,kz)
		end do
		!-------------------------------------------------------------------------
		do iy=1, ny
			grid1(iy,2) = grid(kx,iy,kz)
		end do
		!-------------------------------------------------------------------------
		do iz=1, nz
			grid1(iz,3) = grid(kx,ky,iz)
		end do
		!-------------------------------------------------------------------------
	else
		nxy  = nx * ny
		nyz  = ny * nz
		nzx  = nz * nx
		!-------------------------------------------------------------------------
		dx   = v3_norm(step(1,1))
		dy   = v3_norm(step(1,2))
		dz   = v3_norm(step(1,3))
		!
		dSxy = dx * dy
		dSyz = dy * dz
		dSzx = dz * dx
		!
		!-------------------------------------------------------------------------
		! summation respected to the plane
		!
		grid1=0.D0
		do ix=1, nx
			do iy=1, ny
				do iz=1, nz
					grid1(ix,1) = grid1(ix,1) + grid(ix, iy, iz)
					grid1(iy,2) = grid1(iy,2) + grid(ix, iy, iz)
					grid1(iz,3) = grid1(iz,3) + grid(ix, iy, iz)
				end do
			end do
		end do
		!-------------------------------------------------------------------------
		if (ntype==1) then
			!----------------------------------------------------------------------
			! For integral value
			!
			grid1(:,1) = grid1(:,1) * dSyz
			grid1(:,2) = grid1(:,2) * dSzx
			grid1(:,3) = grid1(:,3) * dSxy
		else if (ntype==2) then
			!----------------------------------------------------------------------
			! For average value, divided by the area.
			!
			grid1(:,1) = grid1(:,1) / nyz
			grid1(:,2) = grid1(:,2) / nzx
			grid1(:,3) = grid1(:,3) / nxy
		end if
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
