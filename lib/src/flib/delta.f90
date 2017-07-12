function delta(x, FWHM, itype)
	use kinds
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent( in) :: itype
	real(DP), intent( in) :: FWHM
	real(DP), intent( in) :: x
	!----------------------------------------------------------------------------
	! Output variable
	!
	real(DP)              :: delta
	!----------------------------------------------------------------------------
	! Local  variables
	!
	real(DP)              :: A, sigma
	real(DP)              :: PI
	!----------------------------------------------------------------------------
	PI=acos(-1.D0)
	if (itype==1) then
		sigma = FWHM   / 2.0_DP  / sqrt(log(2.0_DP))
		A     = 1.0_DP /sqrt(PI) / sigma
	end if
	if ( x <= FWHM * 20.D0 ) then
		delta = A * exp(-x**2/sigma**2)
	else
		delta = 0.D0
	end if
	!----------------------------------------------------------------------------
	return
end function
