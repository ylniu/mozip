subroutine write_xsf_3dgrid(fname, a, natom, symbol, x, nx, ny, nz, grid)
	use kinds, only: DP
	use math,  only: v2_cos
	implicit none
	!----------------------------------------------------------------------------
	integer      :: natom, nx, ny, nz
	real(DP)     :: a(3,3), x(3,natom), grid(nx, ny, nz)
	character(*) :: symbol(natom)
	character(*) :: fname
	!----------------------------------------------------------------------------
	integer      :: fid, i, ix, iy, iz
	!----------------------------------------------------------------------------
	! In xsf file, the unit of length is Bohr.
	!
	call get_free_fid(fid)
	open(fid, file=fname)
		write(fid,*) "CRYSTAL"
		write(fid,*) "PRIMVEC"
		write(fid,'(3(f15.10,x))') a(:,1)
		write(fid,'(3(f15.10,x))') a(:,2)
		write(fid,'(3(f15.10,x))') a(:,3)
		write(fid,*) "PRIMCOORD"
		write(fid,*) natom,1
		do i=1, natom
			write(fid,'(a3,6f12.6)') symbol(i), x(:,i)
		end do
		write(fid,'("BEGIN_BLOCK_DATAGRID_3D")')
		write(fid,'("3D_datagrid")')
		write(fid,'("BEGIN_DATAGRID_3D_this_is_3Dgrid#1")')
		write(fid,'(3i6)') nx, ny, nz
		write(fid,'(3f15.7)') 0.0, 0.0, 0.0
		write(fid,'(3f15.7)') a(:,1)
		write(fid,'(3f15.7)') a(:,2)
		write(fid,'(3f15.7)') a(:,3)
		write(fid, '(5e19.11)') (((grid(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
		write(fid,'("END_DATAGRID_3D")')
		write(fid,'("END_BLOCK_DATAGRID_3D")')
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
