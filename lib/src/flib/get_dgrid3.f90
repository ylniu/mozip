subroutine get_dgrid3(ngrid, step, grid, dgrid)
	use kinds, only: DP
	use math,  only: v3_norm
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: ngrid(3)
	real(DP), intent( in) :: step(3,3)
	real(DP), intent( in) :: grid(ngrid(1), ngrid(2), ngrid(3))
	!----------------------------------------------------------------------------
	real(DP), intent(out) :: dgrid(ngrid(1), ngrid(2), ngrid(3),3)
	!----------------------------------------------------------------------------
	integer               :: ix, iy, iz, nx, ny, nz
	real(DP)              :: dx, dy, dz
	!----------------------------------------------------------------------------
	dx = v3_norm(step(1,1))
	dy = v3_norm(step(1,2))
	dz = v3_norm(step(1,3))
	!
	nx = ngrid(1)
	ny = ngrid(2)
	nz = ngrid(3)
	!----------------------------------------------------------------------------
	! dgrid / dx
	!
	do iy=1, ny
		do iz=1, nz
			do ix=2, nx-1
				dgrid(ix,iy,iz,1)=grid(ix+1,iy,iz)-grid(ix-1,iy,iz)
			end do
			dgrid( 1,iy,iz,1)=(grid( 2,iy,iz)-grid(   1,iy,iz)) * 2.D0
			dgrid(nx,iy,iz,1)=(grid(nx,iy,iz)-grid(nx-1,iy,iz)) * 2.D0
		end do
	end do
	dgrid(:,:,:,1) = dgrid(:,:,:,1) / (2.D0 * dx)
	!----------------------------------------------------------------------------
	! dgrid / dy
	!
	do iz=1, nz
		do ix=1, nx
			do iy=2, ny-1
				dgrid(ix,iy,iz,2)=grid(ix,iy+1,iz)-grid(ix,iy-1,iz)
			end do
			dgrid(ix, 1,iz,2)=(grid(ix, 2,iz)-grid(ix,   1,iz)) * 2.D0
			dgrid(ix,ny,iz,2)=(grid(ix,ny,iz)-grid(ix,ny-1,iz)) * 2.D0
		end do
	end do
	dgrid(:,:,:,2) = dgrid(:,:,:,2) / (2.D0 * dy)
	!----------------------------------------------------------------------------
	! dgrid / dz
	!
	do ix=1, nx
		do iy=1, ny
			do iz=2, nz-1
				dgrid(ix,iy,iz,3)=grid(ix,iy,iz+1)-grid(ix,iy,iz-1)
			end do
			dgrid(ix,iy, 1,3)=(grid(ix,iy, 2)-grid(ix,iy,   1)) * 2.D0
			dgrid(ix,iy,nz,3)=(grid(ix,iy,nz)-grid(ix,iy,nz-1)) * 2.D0
		end do
	end do
	dgrid(:,:,:,3) = dgrid(:,:,:,3) / (2.D0 * dz)
	!----------------------------------------------------------------------------
	return
end subroutine
