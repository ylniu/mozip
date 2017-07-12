subroutine write_xyz_charge(fxyz,n,symbol,x,charge)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: n
	real(DP)     , intent(in) :: x(3,n)
	real(DP)     , intent(in) :: charge(n)
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
			write(fid,'(a4,3f16.10, 2x, f16.10)') symbol(i), x(:,i), charge(i)
		end do
	close(fid)
end subroutine
