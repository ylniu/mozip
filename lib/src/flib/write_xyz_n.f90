subroutine write_xyz_n(fxyz,nmol,natom,symbol,x)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: natom, nmol
	real(DP)     , intent(in) :: x(3,natom,nmol)
	character(*) , intent(in) :: symbol(natom)
	character(*) , intent(in) :: fxyz
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer                   :: fid, i, j
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!
	open(fid, file=trim(fxyz), status="unknown")
		do j=1, nmol
			write(fid,'(i4)') natom
			write(fid,'(" Step", i4)') j
			do i=1,natom
				write(fid,'(a4,3f16.10)') symbol(i), x(:,i,j)
			end do
		end do
	close(fid)
end subroutine
