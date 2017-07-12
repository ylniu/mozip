subroutine write_xsf_vib(fname, nvib, natom, a, symbol, x, v)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer      :: natom, nvib
	real(DP)     :: a(3,3), x(3,natom), v(3,natom,nvib)
	character(*) :: symbol(natom)
	character(*) :: fname
	!----------------------------------------------------------------------------
	integer      :: fid, i, j
	!----------------------------------------------------------------------------
	! In xsf file, the unit of length is Bohr.
	!
	call get_free_fid(fid)
	open(fid, file=fname)
		write(fid,'("ANIMSTEPS",i4)') nvib
		write(fid,'("CRYSTAL")')
		write(fid,'("PRIMVEC")')
		write(fid,'(3(f18.10,x))') a(:,1)
		write(fid,'(3(f18.10,x))') a(:,2)
		write(fid,'(3(f18.10,x))') a(:,3)
		do j=1, nvib
			write(fid,'("PRIMCOORD",i4)') j
			write(fid,'(i4,x,i0)') natom,1
			do i=1, natom
				write(fid,'(a3,6f12.6)') symbol(i), x(:,i), v(:,i,j)
			end do
		end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
