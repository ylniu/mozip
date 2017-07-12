subroutine move_fire(n, m, f, x, v, dt, acoef)
	use kinds     , only: DP
	use math      , only: dot_product2
	use module_afm, only: fire_falpha
	use module_afm, only: fire_dtmax
	use module_afm, only: fire_damping
	use module_afm, only: fire_finc
	use module_afm, only: fire_fdec
	implicit none
	!--------------------------------------------------------------------
	! "Fast Inertial Realxation Engine" according to
	! Erik Bitzek, Pekka Koskinen, Franz Gaehler, Michael Moseler, and Peter Gumbsch,
	! Structural relaxation made simple.
	! Phys. Rev. Lett. 97, 170201 (2006).
	!
	! Eidel, B., Stukowski, A. & Schreodinger, J. 
	! Energy-Minimization in Atomic-to-Continuum Scale-Bridging Methods.
	! Pamm 11, 509-510 (2011).
	! http://users.jyu.fi/~pekkosk/resources/pdf/FIRE.pdf
	!
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer     , intent( in) :: n
	real(DP)    , intent( in) :: m(3,n)
	real(DP)    , intent( in) :: f(3,n)
	!--------------------------------------------------------------------
	! Output variables
	!
	real(DP)    , intent(out) :: x(3,n)
	real(DP)    , intent(out) :: v(3,n)
	real(DP)    , intent(out) :: dt
	real(DP)    , intent(out) :: acoef
	!--------------------------------------------------------------------
	! Local  variables
	!
	real(DP)                  :: ff
	real(DP)                  :: vv
	real(DP)                  :: vf
	real(DP)                  :: cf
	real(DP)                  :: cv
	!--------------------------------------------------------------------
	ff = dot_product2(f, f)
	vv = dot_product2(v, v)
	vf = dot_product2(f, v)
	!--------------------------------------------------------------------
	if (vf < 0) then
		!-----------------------------------------------------------------
		! if velocity against direction of force
		!
		v     = 0.0_DP
		dt    = dt * fire_fdec
		acoef = fire_damping
	else
		!-----------------------------------------------------------------
		! if velocity along direction of force
		!
		cv    = 1.0_DP - acoef 
		cf    = acoef * sqrt(vv/ff)
		v     = v * cv + f * cf
		!-----------------------------------------------------------------
		dt    = min( dt * fire_finc, fire_dtmax )
		acoef = acoef * fire_falpha;
	end if
	!--------------------------------------------------------------------
	! normal leap-frog times step
	!
	v = v + (f/m) * dt
	x = x + v  * dt
	!--------------------------------------------------------------------
	return
end subroutine
