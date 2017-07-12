subroutine rot_fc(n, vector, fc)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: vector(3,3)
	!--------------------------------------------------------------------
	! Output Variables
	!
	real(DP), intent(inout) :: fc(n*3,n*3)
	!--------------------------------------------------------------------
	! Local Variables
	!
	integer                 :: iat, jat
	integer                 :: ix , jx , ii , jj
	integer                 :: ix1, jx1, ii1, jj1
	integer                 :: n3
	real(DP)                :: tmpr
	real(DP), allocatable   :: fc1(:,:)
	!--------------------------------------------------------------------
	n3 = n*3
	allocate(fc1(n3,n3))
	!--------------------------------------------------------------------
	fc1=0.D0
	do iat=1, n
	do jat=1, n
		do ix=1, 3
		do jx=1, 3
			ii=(iat-1) * 3 + ix
			jj=(jat-1) * 3 + jx
			do ix1=1, 3
			do jx1=1, 3
				ii1=(iat-1) * 3 + ix1
				jj1=(jat-1) * 3 + jx1
				tmpr=fc(ii1, jj1) * vector(ix,ix1) * vector(jx,jx1)
				!tmpr=fc(ix1, iat, jx1, jat) * vector(ix,ix1) * vector(jx,jx1)
				!===========================================================
				! Here set tmpr = 0.D0 if tmpr is very small
				! If it is not set to 0.D0, it will be different
				! for different optimization level O0 or O2
				! You can compile the fortran test code: test.f90
				! --------------------------------------------------
				! program test
				! write(*,*) 9.99999994742478692800D-01 * 1.94548229362907651700D-319
				! end
				! --------------------------------------------------
				! ifort -O0 test.f90
				! ./a.out
				! 1.945482293629077E-319
				!
				! ifort -O1 test.f90
				! 0.000000000000000E+000
				!===========================================================
				!
				if ( abs(tmpr) < 1.D-300 ) tmpr = 0.0_DP
				fc1(ii,jj) = fc1(ii,jj) + tmpr
				! fc1(ix, iat, jx, jat) = fc1(ix, iat, jx, jat) + tmpr
			end do
			end do
		end do
		end do
	end do
	end do
	fc=fc1
	!--------------------------------------------------------------------
	deallocate(fc1)
	!--------------------------------------------------------------------
	call set_sym_matrix(n3,fc)
	!--------------------------------------------------------------------
	return
	!--------------------------------------------------------------------
end subroutine rot_fc