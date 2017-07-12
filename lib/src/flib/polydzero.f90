subroutine polydzero(n, x, y, m, c, x0)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer    , intent( in) :: n
	integer    , intent( in) :: m
	real   (DP), intent( in) :: x(n)
	real   (DP), intent( in) :: y(n)
	real   (DP), intent( in) :: c(0:m-1)
	!--------------------------------------------------------------------
	! Output variables
	!
	real   (DP), intent(out) :: x0
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer                  :: i, its
	integer                  :: imin(1)
	real   (DP)              :: xmin
	real   (DP)              :: ZERO
	complex(DP)              :: zx
	complex(DP), allocatable :: zc(:)
	!--------------------------------------------------------------------
	ZERO    = 0.0_DP
	!--------------------------------------------------------------------
	allocate(zc(1:m-1))
	!--------------------------------------------------------------------
	imin    = minloc(y)
	xmin    = x(imin(1))
	zx      = cmplx(xmin, ZERO, DP)
	!--------------------------------------------------------------------
	do i=1, m-1
		zc(i) = cmplx(i*c(i), ZERO, DP)
	end do
	!--------------------------------------------------------------------
	call laguer(zc,m-2,zx,its)
	x0 = real(zx, DP)
	!--------------------------------------------------------------------
	deallocate(zc)
	!--------------------------------------------------------------------
	return
end subroutine
