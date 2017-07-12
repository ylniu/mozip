subroutine getLenardJonesFF(ix, iy, iz)
	use kinds     , only: DP
	use module_afm, only: FF, tip, bas
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer, intent(in) :: ix, iy, iz
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer  :: itip, ibas
	real(DP) :: x_tip(3), x_bas(3), v(3), r, f(3)
	real(DP) :: Rij, Eij, Rij2, Rij6, Rij12, C6, C12, r2, r6, r12
	!--------------------------------------------------------------------
	x_tip = FF%x(:,ix,iy,iz)
	do itip=1, tip%n
		f  = 0.0_DP
		do ibas=1, bas%n
			x_bas = bas%x(:,ibas)
			!--------------------------------------------------------------
			v = x_tip - x_bas
			r = sqrt(dot_product(v,v))
			v = v / r
			!--------------------------------------------------------------
			Rij   = tip%r0(itip) + bas%r0(ibas)
			Eij   = sqrt(tip%ep(itip) * bas%ep(ibas))
			!
			Rij2  = Rij  * Rij
			Rij6  = Rij2 * Rij2  * Rij2
			Rij12 = Rij6 * Rij6
			!
			C6    = Eij  * Rij6  * 2.0_DP
			C12   = Eij  * Rij12
			!
			r2    = r    * r
			r6    = r2   * r2    * r2
			r12   = r6   * r6
			!--------------------------------------------------------------
			f     = f + (12.0_DP * c12/r12 - 6.0_DP * c6/r6) / r * v
		end do
		FF%L(:, ix, iy, iz, itip) = f
	end do
	!--------------------------------------------------------------------
	return
end subroutine
