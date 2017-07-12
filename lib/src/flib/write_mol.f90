subroutine write_mol(fmol,n,symbol,x)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: n
	real(DP)     , intent(in) :: x(3,n)
	character(*) , intent(in) :: symbol(n)
	character(*) , intent(in) :: fmol
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer                   :: fid, i
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!
	open(fid, file=trim(fmol), status="unknown")
		write(fid,'("Molecule")')
		write(fid,'("Created by MOMAP")')
		write(fid,*)
		write(fid,'(9i3, 2x, a4, x, a5)') n,0,0,0,0,0,0,0,0,"0999","V2000"
		do i=1,n
			write(fid,'(3f10.4, x, a2, 10i3)') x(:,i), symbol(i), &
				0,0,0,0,0,0,0,0,0,0
		end do
		write(fid,'("M  END")')
	close(fid)
end subroutine
