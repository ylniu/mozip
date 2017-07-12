subroutine readin()
	use string    , only:	StrLowCase
	use param     , only:	au2a
	use module_afm, only:	iout,						&
									maxIters,				&
									f_eps,					&
									fpara, 					&
									Para,						&
									PBC,						&
									showpic,					&
									scanDim,					&
									ForceCurve,				&
									ForceAtom,				&
									ForceXYZ0,				&
									ForceXYZ1,				&
									ForceXYZ2,				&
									colorscale,				&
									imageInterpolation,	&
									probeStart,				&
									probeType,				&
									AtomRadius,				&
									charge,					&
									dstep,					&
									ColorAlpha,				&
									Amplitude,				&
									border,					&
									gridN,					&
									stiffness,				&
									r0Probe,					&
									gridA,					&
									gridB,					&
									gridC,					&
									scanMin,					&
									scanMax,					&
									figsize,					&
									scanStep,				&
									fire_dtmax,				&
									fire_finc,				&
									fire_fdec,				&
									fire_falpha,			&
									fire_damping,			&
									Tip_type
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer        :: fid
	integer        :: ios
	character(200) :: line, key, tmp
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fpara, status="old")
	read(fid, '(a)', iostat=ios) line
	write(iout,*)
	write(iout,'(2x, "Reading parameter ......")')
	write(iout,*)
	do while (ios==0)
		read(line,*) key
		key=StrLowCase(key)
		select case(key)
			case ("fire_dtmax")
				read(line,*) tmp, fire_dtmax
				write(iout,'(2x,"fire_dtmax          ",4x,"=",  F15.7)') fire_dtmax
			case ("fire_inc")
				read(line,*) tmp, fire_finc
				write(iout,'(2x,"fire_finc           ",4x,"=",  F15.7)') fire_finc
			case ("fire_fdec")
				read(line,*) tmp, fire_fdec
				write(iout,'(2x,"fire_fdec           ",4x,"=",  F15.7)') fire_fdec
			case ("fire_falpha")
				read(line,*) tmp, fire_falpha
				write(iout,'(2x,"fire_falpha         ",4x,"=",  F15.7)') fire_falpha
			case ("fire_damping")
				read(line,*) tmp, fire_damping
				write(iout,'(2x,"fire_damping        ",4x,"=",  F15.7)') fire_damping
			case ("f_eps")
				read(line,*) tmp, f_eps
				write(iout,'(2x,"f_eps               ",4x,"=",  L15  )') f_eps
			case ("maxiters")
				read(line,*) tmp, maxIters
				write(iout,'(2x,"maxIters            ",4x,"=",  L15  )') maxIters
			case ("para")
				read(line,*) tmp, para
				write(iout,'(2x,"Para                ",4x,"=",  L15  )') para
			case ("Tip_type")
				read(line,*) tmp, Tip_type
				write(iout,'(2x,"Tip_type            ",4x,"=",  L15  )') Tip_type
			case ("pbc")
				read(line,*) tmp, pbc
				write(iout,'(2x,"PBC                 ",4x,"=",  L15  )') pbc
			case ("scandim")
				read(line,*) tmp, scanDim
				write(iout,'(2x,"scanDim             ",4x,"=",  L15  )') scanDim
			case ("showpic")
				read(line,*) tmp, showpic
				write(iout,'(2x,"showpic             ",4x,"=",  L15  )') showpic
			case ("forcecurve")
				read(line,*) tmp, forcecurve
				write(iout,'(2x,"ForceCurve          ",4x,"=",  i15  )') forcecurve
			case ("forceatom")
				read(line,*) tmp, forceatom
				write(iout,'(2x,"ForceAtom           ",4x,"=",  i15  )') forceatom
			case ("forcexyz0")
				read(line,*) tmp, forcexyz0
				write(iout,'(2x,"ForceXYZ0           ",4x,"=", 3f15.6)') forcexyz0
			case ("forcexyz1")
				read(line,*) tmp, forcexyz1
				write(iout,'(2x,"ForceXYZ1           ",4x,"=", 3f15.6)') forcexyz1
			case ("forcexyz2")
				read(line,*) tmp, forcexyz2
				write(iout,'(2x,"ForceXYZ2           ",4x,"=", 3f15.6)') forcexyz2
			case ("colorscale")
				read(line,*) tmp, colorscale
				write(iout,'(2x,"colorscale          ",4x,"=",  a15  )') trim(colorscale)
			case ("imageinterpolation")
				read(line,*) tmp, imageinterpolation
				write(iout,'(2x,"imageInterpolation  ",4x,"=",  a15  )') trim(imageinterpolation)
			case ("probestart")
				read(line,*) tmp, probestart
				write(iout,'(2x,"probeStart          ",4x,"=",  i15  )') probestart
			case ("probetype")
				read(line,*) tmp, probetype
				write(iout,'(2x,"probeType           ",4x,"=",  i15  )') probetype
			case ("atomradius")
				read(line,*) tmp, atomradius
				write(iout,'(2x,"AtomRadius          ",4x,"=",  f15.6)') atomradius
				atomradius = atomradius / au2a
			case ("charge")
				read(line,*) tmp, charge
				write(iout,'(2x,"charge              ",4x,"=",  f15.6)') charge
			case ("dstep")
				read(line,*) tmp, dstep
				write(iout,'(2x,"dstep               ",4x,"=",  f15.6)') dstep
				dstep = dstep / au2a
			case ("coloralpha")
				read(line,*) tmp, coloralpha
				write(iout,'(2x,"ColorAlpha          ",4x,"=",  f15.6)') coloralpha
			case ("amplitude")
				read(line,*) tmp, amplitude
				write(iout,'(2x,"Amplitude           ",4x,"=",  f15.6)') amplitude
				amplitude = amplitude / au2a
			case ("border")
				read(line,*) tmp, border
				write(iout,'(2x,"border              ",4x,"=",  f15.6)') border
				border = border / au2a
			case ("gridn")
				read(line,*) tmp, gridn
				write(iout,'(2x,"gridN               ",4x,"=", 3i15  )') gridn
			case ("stiffness")
				read(line,*) tmp, stiffness
				write(iout,'(2x,"stiffness           ",4x,"=", 3f15.6)') stiffness
			case ("r0probe")
				read(line,*) tmp, r0probe
				write(iout,'(2x,"r0Probe             ",4x,"=", 3f15.6)') r0probe
			case ("grida")
				read(line,*) tmp, grida
				write(iout,'(2x,"gridA               ",4x,"=", 3f15.6)') grida
				grida = grida / au2a
			case ("gridb")
				read(line,*) tmp, gridb
				write(iout,'(2x,"gridB               ",4x,"=", 3f15.6)') gridb
				gridb = gridb / au2a
			case ("gridc")
				read(line,*) tmp, gridc
				write(iout,'(2x,"gridC               ",4x,"=", 3f15.6)') gridc
				gridc = gridc / au2a
			case ("scanmin")
				read(line,*) tmp, scanmin
				write(iout,'(2x,"scanMin             ",4x,"=", 3f15.6)') scanmin
				scanmin = scanmin / au2a
			case ("scanmax")
				read(line,*) tmp, scanmax
				write(iout,'(2x,"scanMax             ",4x,"=", 3f15.6)') scanmax
			case ("figsize")
				read(line,*) tmp, figsize
				write(iout,'(2x,"figsize             ",4x,"=", 3f15.6)') figsize
				scanmax = scanmax / au2a
			case ("scanstep")
				read(line,*) tmp, scanstep
				write(iout,'(2x,"scanStep            ",4x,"=", 3f15.6)') scanstep
				scanstep = scanstep / au2a
		end select
		read(fid, '(a)', iostat=ios) line
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
