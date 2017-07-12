subroutine write_xsf(fname, a, natom, symbol, x)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer      :: natom
	real(DP)     :: a(3,3), x(3,natom)
	character(*) :: symbol(natom)
	character(*) :: fname
	!----------------------------------------------------------------------------
	integer      :: fid, i
	!----------------------------------------------------------------------------
	! In xsf file, the unit of length is Bohr.
	!
	call get_free_fid(fid)
	open(fid, file=fname)
		write(fid,*) "CRYSTAL"
		write(fid,*) "PRIMVEC"
		write(fid,'(3(f18.10,x))') a(:,1)
		write(fid,'(3(f18.10,x))') a(:,2)
		write(fid,'(3(f18.10,x))') a(:,3)
		write(fid,*) "PRIMCOORD"
		write(fid,*) natom,1
		do i=1, natom
			write(fid,'(a3,6f12.6)') symbol(i), x(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine