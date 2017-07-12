subroutine broaden(n_in, x_in, y_in, n_out, x_out, y_out, extend, FWHM_n)
	!---------------------------------------------------------------------------
	!
	! This subroutine broaden an input array y_in(x_in) with Gaussian function.
	! x_out(1    ) = min_x - ( max_in - min_x ) * extend
	! x_out(n_out) = max_x + ( max_in - min_x ) * extend
	! FWHM (Full Width at Half Maximum) is optional. If FWHM is not set,
	! its default value is:
	!
	! For example:
	!
	! n_in  = 4
	! n_out = 8
	! x_in(1), x_in(2), x_in(3), x_in(4)
	! y_in(1), y_in(2), y_in(3), y_in(4)
	!
	! dx = ( max_x - min_x ) / ( n_out - 1 )
	!
	!---------------------------------------------------------------------------
	use kinds
	!---------------------------------------------------------------------------
	implicit none
	!---------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent(in ) :: n_in
	integer , intent(in ) :: n_out
	integer , intent(in ) :: FWHM_n
	real(DP), intent(in ) :: extend
	real(DP), intent(in ) :: x_in(n_in)
	real(DP), intent(in ) :: y_in(n_in)
	!---------------------------------------------------------------------------
	! Output variables
	!
	real(DP), intent(out) :: x_out(n_out)
	real(DP), intent(out) :: y_out(n_out)
	!---------------------------------------------------------------------------
	! Local  variables
	!
	integer               :: nbroad, i, j, dn
	real(DP)              :: min_x, max_x
	real(DP)              :: sigma, dx, dx_in, dx_g, gauss1, FWHM
	real(DP)              :: broad_range, PI, A, xx, test_broad
	real(DP), allocatable :: gauss(:)
	!---------------------------------------------------------------------------
	! Initialization
	!
	PI   = acos(-1.0_DP)
	min_x=1.D99
	max_x=0.D0
	do i=1, n_in
		if (min_x>x_in(i)) min_x=x_in(i)
		if (max_x<x_in(i)) max_x=x_in(i)
	end do
	dx_in        = ( max_x - min_x ) / (n_in - 1)
	x_out(1    ) = min_x - ( max_x - min_x ) * extend
	x_out(n_out) = max_x + ( max_x - min_x ) * extend
	dx           = (x_out(n_out) - x_out(1)) / (n_out-1)
	!---------------------------------------------------------------------------
	! For Gaussian
	!
	dx_g         = dx / 50
	FWHM         = FWHM_n * dx
	!
	sigma        = FWHM / 2.0_DP    / sqrt(log(2.0_DP))
	A            = 1.0_DP /sqrt(PI) / sigma
	broad_range  = FWHM * 20
	!---------------------------------------------------------------------------
	! Find nbroad
	!
	nbroad = nint(broad_range / dx_g)
	do i=0, nbroad
		xx = dx_g * i
		gauss1 = exp(-xx**2/sigma**2)
		if (gauss1<1.D-200) exit
	end do
	nbroad = i - 1
	!---------------------------------------------------------------------------
	! Calculate Gaussian broadening function gauss
	!
	allocate(gauss(0:nbroad))
	do i=0, nbroad
		xx = dx_g * i
		gauss(i) = A * exp(-xx**2/sigma**2)
	end do
	!---------------------------------------------------------------------------
	! Test if the area of Gaussian function equal to 1
	!
	test_broad = 0
	do i=1, nbroad
		test_broad = test_broad + (gauss(i) + gauss(i-1))
	end do
	test_broad = test_broad * dx_g
	!
	if (abs( test_broad - 1.D0 ) > 1.D-5 ) then
		write(*,*) "Error! Gaussian function is wrong for broadening! "
		stop
	end if
	!---------------------------------------------------------------------------
	!
	y_out = 0.0_DP
	do i=1, n_out
		x_out(i) = x_out(1) + (i-1) * dx
		!------------------------------------------------------------------------
		do j=1, n_in
			dn = nint(abs(x_out(i)-x_in(j)) / dx_g)
			if ( dn <= nbroad ) then
				y_out(i) = y_out(i) + y_in(j) * gauss(dn)
			end if
		end do
		!------------------------------------------------------------------------
	end do
	!---------------------------------------------------------------------------
	y_out = y_out * dx_in
	!---------------------------------------------------------------------------
	deallocate( gauss )
	!---------------------------------------------------------------------------
	return
end subroutine
