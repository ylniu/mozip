subroutine qm_cube_info(fcube, natom, ngrid, title, ctype, center, step, nat, nata, x, grid, info)
	use kinds, only: DP
	implicit none
	integer     , intent( in) :: natom, ngrid(3)
	character(*), intent( in) :: fcube
	!----------------------------------------------------------------------------
	integer     , intent(out) :: nat(natom), info
	character(*), intent(out) :: title, ctype
	real(DP)    , intent(out) :: center(3), step(3,3)
	real(DP)    , intent(out) :: nata(natom), x(3,natom)
	real(DP)    , intent(out) :: grid(ngrid(1), ngrid(2), ngrid(3))
	!----------------------------------------------------------------------------
	integer                   :: fid
	integer                   :: natom1, nx1, ny1, nz1, nx, ny, nz, ix, iy, iz, i
	!----------------------------------------------------------------------------
	info=-1
	nx=ngrid(1)
	ny=ngrid(2)
	nz=ngrid(3)
	call get_free_fid(fid)
	open(fid, file=fcube, status="old")
		read(fid,'(a)') title
		read(fid,'(a)') ctype
		read(fid,*) natom1, center
		read(fid,*) nx1, step(:,1)
		read(fid,*) ny1, step(:,2)
		read(fid,*) nz1, step(:,3)
		!-------------------------------------------------------------------------
		if (natom1 /= natom .or. nx1 /= nx .or. ny1 /= ny .or. nz1 /= nz) then
			close(fid)
			return
		end if
		!-------------------------------------------------------------------------
		do i=1, natom
			read(fid,*) nat(i), nata(i), x(:,i)
		end do
		do ix=1, nx
			do iy=1, ny
				read(fid,*) (grid(ix, iy, iz), iz=1, nz)
			end do
		end do
	close(fid)
	info=0
	!----------------------------------------------------------------------------
	return
end subroutine
