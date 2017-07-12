subroutine write_xyz_vib(fxyz,n,symbol,x,v,freq)
	use kinds, only: DP
	use param, only: au2cm, au2a
	implicit none
	!--------------------------------------------------------------------
	! Input variables
	!
	integer      , intent(in) :: n
	real(DP)     , intent(in) :: x     (3,n    )
	real(DP)     , intent(in) :: v     (3,n,n*3)
	real(DP)     , intent(in) :: freq  (    n*3)
	character(*) , intent(in) :: symbol(  n    )
	character(*) , intent(in) :: fxyz
	!--------------------------------------------------------------------
	! Local variables
	!
	integer                   :: fid, i, j, k
	real(DP)                  :: vmax, s
	!--------------------------------------------------------------------
	s   = 2.0_DP
	vmax= 0.0_DP
	do j=1, n*3
		do i=1, n
			do k=1, 3
				if (vmax<abs(v(k,i,j))) vmax = abs(v(k,i,j))
			end do
		end do
	end do
	call get_free_fid(fid)
	!
	open(fid, file=trim(fxyz), status="unknown")
		do j=1, n*3
			write(fid,'(i4)') n
			write(fid,'(" freq = ", f11.2, " cm^-1")') freq(j) * au2cm
			do i=1,n
				write(fid,'(a4,6f16.10)') symbol(i), x(:,i) * au2a, v(:,i,j) / vmax * s
			end do
		end do
	close(fid)
end subroutine
