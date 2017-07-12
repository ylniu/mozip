subroutine qm_wien2k_psink_info(fpsink , ngrid, cell, grid, info)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer      :: ngrid(3), info
	real(DP)     :: cell(6)
	real(DP)     :: grid(ngrid(1), ngrid(2), ngrid(3))
	character(*) :: fpsink
	!----------------------------------------------------------------------------
	integer      :: fid, nx, ny, nz, ix, iy, iz
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	nx   = ngrid(1)
	ny   = ngrid(2)
	nz   = ngrid(3)
	!----------------------------------------------------------------------------
	open(fid, file=fpsink, status="old")
		read(fid,*)
		read(fid,*) cell(1)
		read(fid,*) cell(2), cell(4)
		read(fid,*) cell(3), cell(5), cell(6)
		read(fid,*)
		read(fid,*)
		read(fid,*) (((grid(ix,iy,iz),iz=1,nz),iy=1,ny),ix=1,nx)
	close(fid)
	info=1
	!----------------------------------------------------------------------------
	return
end subroutine
