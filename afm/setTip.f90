subroutine setTip()
	use module_afm, only: eVA_Nm, DP, r0Probe, stiffness, tip !, makeConsistent
	implicit none
	!----------------------------------------------------------------------------
	real(DP) :: lRadial
	real(DP) :: kRadial
	real(DP) :: rPP0(3)
	real(DP) :: kSpring(3)
	!----------------------------------------------------------------------------
	lRadial    = r0Probe(3)
	kRadial    = -stiffness(3) / eVA_Nm
	rPP0(1)    = r0Probe(1)
	rPP0(2)    = r0Probe(2)
	rPP0(3)    = 0.D0
	kSpring(1) = -stiffness(1) / eVA_Nm 
	kSpring(2) = -stiffness(2) / eVA_Nm 
	kSpring(3) = 0.D0
	!----------------------------------------------------------------------------
	TIP%lRadial = lRadial
	TIP%kRadial = kRadial
	TIP%rPP0    = rPP0
	TIP%kSpring = kSpring
	! rPP0 to be consistent with lRadial
	!call makeConsistent(TIP)
	!----------------------------------------------------------------------------
	return
end subroutine
