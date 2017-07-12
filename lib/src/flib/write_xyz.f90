subroutine write_xyz(fxyz,n,symbol,x)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: n
	real(DP)     , intent(in) :: x(3,n)
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
			write(fid,'(a4,3f16.10)') symbol(i), x(:,i)
		end do
	close(fid)
end subroutine
