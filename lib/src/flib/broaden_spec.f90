subroutine broaden_spec(n_in, x_in, y_in, n_out, x_out, y_out, x_out_min, dx, FWHM)
	use kinds
	use Param
	!----------------------------------------------------------------------------
	integer , intent( in) :: n_in
	integer , intent( in) :: n_out
	real(DP), intent( in) :: x_in(n_in)
	real(DP), intent( in) :: y_in(n_in)
	real(DP), intent( in) :: x_out_min
	real(DP), intent( in) :: dx
	real(DP), intent( in) :: FWHM
	!----------------------------------------------------------------------------
	real(DP), intent(out) :: x_out(n_out)
	real(DP), intent(out) :: y_out(n_out)
   !---------------------------------------------------------------------------
	! Local variables
	integer               :: i, j, dn, nbroad
	real(DP)              :: xx, A, sigma, test_broad
	real(DP)              :: broad_range, gauss1
	real(DP), allocatable :: gauss(:)
	!---------------------------------------------------------------------------
	! For Gaussian
	!
	sigma        = FWHM / 2.0_DP    / sqrt(log(2.0_DP))
	A            = 1.0_DP /sqrt(PI) / sigma
	broad_range  = FWHM * 20 
	!---------------------------------------------------------------------------
	! Find nbroad
	!
	nbroad = nint(broad_range / dx)
	do i=0, nbroad
		xx = dx * i
		gauss1 = exp(-xx**2/sigma**2)
		if (gauss1<1.D-200) exit
	end do
	nbroad = i - 1
	!---------------------------------------------------------------------------
	! Calculate Gaussian broadening function gauss
	
	allocate(gauss(0:nbroad))
	do i=0, nbroad
		xx = dx * i
		gauss(i) = A * exp(-xx**2/sigma**2)
	end do
	!---------------------------------------------------------------------------
	! Test if the area of Gaussian function equal to 1
	!
	test_broad = 0
	do i=1, nbroad
		test_broad = test_broad + (gauss(i) + gauss(i-1))
	end do
	test_broad = test_broad * dx
	!
	if (abs( test_broad - 1.D0 ) > 1.D-5 ) then
		write(*,*) "Error! Gaussian function is wrong for broadening! "
		stop
	end if
	!---------------------------------------------------------------------------
	!
	y_out = 0.0_DP
	do i=1, n_out
		x_out(i) = x_out_min + (i-1) * dx
		!------------------------------------------------------------------------
		do j=1, n_in
			dn = nint(abs(x_out(i)-x_in(j)) / dx)
			if ( dn <= nbroad ) then
				y_out(i) = y_out(i) + y_in(j) * gauss(dn)
			end if
		end do
		!------------------------------------------------------------------------
	end do
	!---------------------------------------------------------------------------
	y_out = y_out * dx
	!----------------------------------------------------------------------------
	deallocate(gauss)
	!----------------------------------------------------------------------------
	return
end subroutine
