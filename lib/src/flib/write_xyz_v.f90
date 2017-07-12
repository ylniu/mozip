subroutine write_xyz_v(fxyz,n,symbol,x,v)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: n
	real(DP)     , intent(in) :: x(3,n)
	real(DP)     , intent(in) :: v(3,n)
	character(*) , intent(in) :: symbol(n)
	character(*) , intent(in) :: fxyz
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer                   :: fid, i
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!
	open(fid, file=trim(fxyz), status="unknown")
		write(fid,'(i4)') n
		write(fid,'(" title")')
		do i=1,n
			write(fid,'(a4,6f16.10)') symbol(i), x(:,i), v(:,i)
		end do
	close(fid)
end subroutine
