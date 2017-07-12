subroutine write_axsf(fname, a, natom, symbol, ncycle, x)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer      :: natom, ncycle
	real(DP)     :: a(3,3), x(3,natom,ncycle)
	character(*) :: symbol(natom)
	character(*) :: fname
	!----------------------------------------------------------------------------
	integer      :: fid, i, ic
	!----------------------------------------------------------------------------
	! In xsf file, the unit of length is Bohr.
	!
	call get_free_fid(fid)
	open(fid, file=fname)
		write(fid,'("ANIMSTEPS",I8)') ncycle
		write(fid,'("CRYSTAL")')
		write(fid,'("PRIMVEC")')
		write(fid,'(3(f18.10,x))') a(:,1)
		write(fid,'(3(f18.10,x))') a(:,2)
		write(fid,'(3(f18.10,x))') a(:,3)
		do ic=1, ncycle
			write(fid,'("PRIMCOORD",i4)') ic
			write(fid,'(i4,x,"1")') natom
			do i=1, natom
				write(fid,'(a3,6f12.6)') symbol(i), x(:,i,ic)
			end do
		end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
