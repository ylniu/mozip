subroutine rotn3 (n,vector,fc)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	!
	! Input  variables
	!
	integer , intent(in ) :: n
	real(DP), intent(in ) :: vector(3,3)

	!----------------------------------------------------------------------------
	!
	! Output variables
	!
	real(DP), intent(out) :: fc(n*3,n*3)
	!----------------------------------------------------------------------------
	!
	! Local  variables
	!
	integer  :: i, j, k
	integer  :: iat, jat, ix, jx, ii, jj, ix1, jx1, ii1, jj1
	real(DP) :: fc1(n*3,n*3), tmpr
	!----------------------------------------------------------------------------
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
				!===================================================================
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
				!===================================================================
				!
				if ( abs(tmpr) < 1.D-300 ) tmpr = 0.0_DP
				fc1(ii,jj) = fc1(ii,jj) + tmpr
			end do
			end do
		end do
		end do
	end do
	end do
	fc=fc1
	!----------------------------------------------------------------------------
	return
end
