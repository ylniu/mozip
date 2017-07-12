subroutine write_cube(fout, title, caltype, natom, center, ngrid, a, nat, x, grid)
	use kinds, only: DP
	implicit none
	character(*) :: title
	character(*) :: caltype
	character(*) :: fout
	integer      :: ngrid(3)
	integer      :: natom, nat(natom)
	real(DP)     :: center(3), a(3,3), nata(natom), x(3,natom)
	real(DP)     :: grid(ngrid(1), ngrid(2), ngrid(3))
	integer      :: fid, i, ix, iy, iz, nx, ny, nz
	!----------------------------------------------------------------------------
	nx=ngrid(1)
	ny=ngrid(2)
	nz=ngrid(3)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!
	do i=1, natom
		nata(i)=real(nat(i))
	end do
	open(fid, file=fout)
		write(fid,*) trim(title)
		write(fid,*) trim(caltype)
		write(fid,'(i5,3f12.6,i5)') natom, center, 1
		write(fid,'(i5,3f12.6   )') nx, a(:,1)
		write(fid,'(i5,3f12.6   )') ny, a(:,2)
		write(fid,'(i5,3f12.6   )') nz, a(:,3)
		do i=1, natom
			write(fid,'(i5,4f12.6)') nat(i), nata(i), x(:,i)
		end do
		do ix=1, nx
			do iy=1, ny
				write(fid,'(6es13.5)') (grid(ix,iy,iz), iz=1, nz)
			end do
		end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
