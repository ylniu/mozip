subroutine init()
	use module_afm
	implicit none
	!----------------------------------------------------------------------------
	fid           = 1
	iout          = 11
	fout          = "afm.log"
	fxyz          = "input.xyz"
	ftip          = "tip.xyz"
	fcolor        = "color.ini"
	debug_xy      = .false.
	debug_imin    = -1
	debug_imax    = -1
	fire_dtmax    = 5.00_DP
	fire_finc     = 1.10_DP
	fire_fdec     = 0.50_DP
	fire_falpha   = 0.99_DP
	fire_damping  = 0.10_DP
	!----------------------------------------------------------------------------
	if_shiftXY    = .true.
	if_fitCell    = .true.
	border        = 3.D0
	moleculeShift = (/  0.0000D0,    0.0000D0,    0.0000D0 /)
	!----------------------------------------------------------------------------
	open(iout, file=fout)
	!----------------------------------------------------------------------------
	Para                 =       .True.
	PBC                  =      .False.                               ! Periodic boundary conditions ? [ True/False ]
	showpic              =      .False.
	ForceCurve           =            2                               ! Calculate Force Curve
	ForceAtom            =            1                               ! Calculate Force Curve on this atom
	ForceXYZ0            = (/  0.1000D0,    0.1000D0,    0.0000D0 /)  ! Calculate Force Curve on this coordinate. If ForceAtom > 0, this parameter is disabled.
	ForceXYZ1            = (/  0.0000D0,   -2.0000D0,    0.0000D0 /)  ! Calculate Force Curve on this coordinate. If ForceAtom > 0, this parameter is disabled.
	ForceXYZ2            = (/  0.0000D0,    2.0000D0,    0.0000D0 /)  ! Calculate Force Curve on this coordinate. If ForceAtom > 0, this parameter is disabled.
	colorscale           =       "gray"
	imageInterpolation   =    "nearest"
	probeStart           =            0
	probeType            =            8                               ! atom type of ProbeParticle (to choose L-J potential ),e.g. 8 for CO, 24 for Xe
	AtomRadius           =    10.0000D0
	charge               =     0.0500D0                               ! effective charge of probe particle [e]
	dstep                =     0.1000D0
	ColorAlpha           =     0.8000D0
	Amplitude            =     1.0000D0                               ! [] oscilation amplitude for conversion Fz->df
	border               =     3.0000D0
	gridN                = (/       100,         100,         100 /)
	stiffness            = (/  0.5000D0,    0.5000D0,   20.0000D0 /)  ! [N/m] harmonic spring potential (x,y,R) components, x,y is bending stiffnes, R particle-tip bond-length stiffnes,
	r0Probe              = (/  0.0000D0,    0.0000D0,    4.0000D0 /)  ! [] equilibirum position of probe particle (x,y,R) components, R is bond length, x,y introduce tip asymmetry
	gridA                = (/ 20.0000D0,    0.0000D0,    0.0000D0 /)  ! a-vector of unit cell; recomanded format (x,y,0)
	gridB                = (/  0.0000D0,   20.0000D0,    0.0000D0 /)  ! b-vector of unit cell; recomanded format (x,y,0)
	gridC                = (/  0.0000D0,    0.0000D0,   10.0000D0 /)  ! c-vector of unit cell; recomanded format (0,0,z)
	scanMin              = (/  0.0000D0,    0.0000D0,    2.0000D0 /)  ! start of scanning (x,y,z)
	scanMax              = (/ 20.0000D0,   20.0000D0,   12.0000D0 /)  ! end of scanning (x,y,z)
	figsize              = (/ 20.0000D0,   20.0000D0,    0.0000D0 /)
	scanStep             = (/  0.1000D0,    0.1000D0,    0.1000D0 /)
	!----------------------------------------------------------------------------
	return
end subroutine
