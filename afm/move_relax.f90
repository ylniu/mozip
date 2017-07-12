subroutine move_relax(n, f, r, v)
	use kinds     , only: DP
	!use module_afm, only: maxIters
	implicit none
	!--------------------------------------------------------------------
	! relaxation step for simple damped-leap-frog molecular dynamics
	! ( just for testing, less efficinet than FIRE )
	!
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: f(3,n)
	real(DP), intent(out) :: r(3,n)
	real(DP), intent(out) :: v(3,n)
	!--------------------------------------------------------------------
	!real(DP)  :: convF2   = 1.D-8
	real(DP)  :: dt       = 0.5D0
	real(DP)  :: damping  = 0.1D0
	!--------------------------------------------------------------------
	v = v * (1.0 - damping)
	v = v + f * dt
	r = v * dt
	!--------------------------------------------------------------------
end subroutine
