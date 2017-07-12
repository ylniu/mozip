program afmf
	use kinds     , only: DP
	use math      , only: v3_disp3
	use file      , only: get_exe_dir
	use module_obj, only: allocate_obj, deallocate_obj
	use module_scn, only: allocate_scn, deallocate_scn
	use module_afm, only: FF, scn, gridA, gridB, gridC, gridN
	use module_afm, only: scanmin, scanmax, withElectrostatics
	use module_afm, only: scanstep, zTips, showpic
	use module_afm, only: ScanXYZ, ScanXYZ1, ScanXYZ2
	use module_afm, only: scanDim, rTips, ntips, moleculeshift
	use module_afm, only: iout, if_shiftXY, if_fitCell
	use module_afm, only: ForceXYZ0, ForceXYZ1, ForceXYZ2
	use module_afm, only: fpara, ForceCurve, ForceAtom, charge
	use module_afm, only: rP, rT, fs, rs
	use module_afm, only: tip, bas, maxIters
	use module_afm, only: debug_imin, debug_imax, debug_xy
	use param     , only: au2a
	implicit none
	!--------------------------------------------------------------------
	integer        :: i, j, k, fid
	real(DP)       :: dz
	real(DP)       :: v1(3), v2(3), v3(3), v(3)
	character(200) :: tmp
	!--------------------------------------------------------------------
	call init()
	!--------------------------------------------------------------------
	!call get_exe_dir(path_exe)
	!
	i=1
	call getarg(1, fpara)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imin(1)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imin(2)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imin(3)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imax(1)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imax(2)
	!--------------------------------------------------------------------
	i=i+1
	call getarg(i, tmp)
	read(tmp, *) debug_imax(3)
	!--------------------------------------------------------------------
	if ( all(debug_imin >=0 ) .and. all(debug_imax >=0 )  ) then
		debug_xy = .true.
	end if
	!--------------------------------------------------------------------
	call readin()
	call get_LJ_param()
	call readbas()
	call readtip()
	call getAtomsLJ()
	call read_force()
	call loadAtomscolor()
	!--------------------------------------------------------------------
	withElectrostatics = abs( charge )>0.001
	!--------------------------------------------------------------------
	if (ForceCurve==1) then
		if_shiftXY = .False.
		if_fitCell = .False.
	else
		if_shiftXY = .True.
		if_fitCell = .True.
	end if
	!--------------------------------------------------------------------
	if (scanDim==3) then
		call autoGeom()
	end if
	!--------------------------------------------------------------------
	! 	call get_FF_grid()
	!--------------------------------------------------------------------
	! shift molecule so that we sample reasonable part of potential
	!
	call v3_disp3(bas%n, bas%x, moleculeshift)
	bas%xout = bas%x
	!--------------------------------------------------------------------
	FF%n = gridN
	!--------------------------------------------------------------------
	! v1, v2, v3 is the step along a, b, c
	!
	v1 = gridA / gridN(1)
	v2 = gridB / gridN(2)
	v3 = gridC / gridN(3)
	FF%dCell(:,1) = v1
	FF%dCell(:,2) = v2
	FF%dCell(:,3) = v3
	!--------------------------------------------------------------------
	call allocate_obj(FF, tip%n)
	!--------------------------------------------------------------------
	do i=0, FF%n(1)
		do j=0, FF%n(2)
			do k=0, FF%n(3)
				v = v1 * i + v2 * j + v3 * k
				FF%x(:,i,j,k) = v
			end do
		end do
	end do
	!-------------------------------------------------------------------
	if (ForceCurve==1) then
		ScanXYZ = ForceXYZ0
		showpic = .False.
		if (ForceAtom > 0 .and. ForceAtom < bas%n) then
			ScanXYZ = bas%xout(:,ForceAtom)
		end if
	else if (ForceCurve==2) then
		ScanXYZ1 = ForceXYZ1
		ScanXYZ2 = ForceXYZ2
		showpic = .False.
	end if
	!--------------------------------------------------------------------
	call plotModel()
	!--------------------------------------------------------------------
	dz       = scanstep(3)
	ntips    = nint((scanmax(3) - scanmin(3))/dz) + 1
	!--------------------------------------------------------------------
	scn%dx   = scanstep(1)
	scn%dy   = scanstep(2)
	scn%dz   = scanstep(3)
	scn%n(1) = nint((scanmax(1) - scanmin(1)) / scn%dx) + 1
	scn%n(2) = nint((scanmax(2) - scanmin(2)) / scn%dy) + 1
	scn%n(3) = nint((scanmax(3) - scanmin(3)) / scn%dz) + 1
	!--------------------------------------------------------------------
	call allocate_scn(scn, tip%n)
	!--------------------------------------------------------------------
	do i=0, scn%n(1)
		do j=0, scn%n(2)
			do k=0, scn%n(3)
				scn%x(1,i,j,k) = scanmin(1) + i * scn%dx
				scn%x(2,i,j,k) = scanmin(2) + j * scn%dy
				scn%x(3,i,j,k) = scanmin(3) + k * scn%dz
			end do
		end do
	end do
	!--------------------------------------------------------------------
	if (.not. allocated(zTips)) allocate(zTips(  ntips))
	if (.not. allocated(rTips)) allocate(rTips(3,ntips))
	if (.not. allocated(rs   )) allocate(rs   (  ntips))
	if (.not. allocated(fs   )) allocate(fs   (  ntips))
	if (.not. allocated(rT   )) allocate(rT   (  ntips))
	if (.not. allocated(rP   )) allocate(rP   (  ntips))
	!--------------------------------------------------------------------
	rTips=0.D0
	do i=1, ntips
		zTips(  i) = (i-1) * dz + scanmin(3)
		rTips(3,i) = zTips(i)
	end do
	write(iout,'(2x, "zTips : ")')
	write(iout,'(10f15.7)') zTips
	!--------------------------------------------------------------------
	write(iout,*)
	write(iout, '(2x, "gradA  ", 2x, 3f15.7)') gridA
	write(iout, '(2x, "gradB  ", 2x, 3f15.7)') gridB
	write(iout, '(2x, "gradC  ", 2x, 3f15.7)') gridC
	write(iout, '(2x, "v1     ", 2x, 3f15.7)') v1 * au2a
	write(iout, '(2x, "v2     ", 2x, 3f15.7)') v2 * au2a
	write(iout, '(2x, "v3     ", 2x, 3f15.7)') v3 * au2a
	write(iout, '(2x, "dCell1 ", 2x, 3f15.7)') FF%dCell(:,1)
	write(iout, '(2x, "dCell2 ", 2x, 3f15.7)') FF%dCell(:,2)
	write(iout, '(2x, "dCell3 ", 2x, 3f15.7)') FF%dCell(:,3)
	write(iout, '(2x, "gridN  ", 2x, 3i15  )') gridN
	write(iout, '(2x, "scanMin", 2x, 3f15.7)') scanmin * au2a
	write(iout, '(2x, "scanMax", 2x, 3f15.7)') scanmax * au2a
	write(iout, '(2x, "scandx ", 2x, 3f15.7)') scn%dx * au2a, scn%dy * au2a, scn%dz * au2a
	write(iout, '(2x, "scann  ", 2x, 3i15  )') scn%n
	write(iout, '(2x, "tip")')
	do i=1, tip%n
		write(iout, '(2x, a4, 3f15.7)') tip%symbol(i), tip%x(:,i)
	end do
	write(iout,*)
	flush(iout)
	!--------------------------------------------------------------------
	call get_force_field
	call relaxTipStroke
	!--------------------------------------------------------------------
	do i=0, scn%n(1)
		do j=0, scn%n(2)
			do k=0, scn%n(3)
				if (.not. scn%conv(i,j,k)) then
					write(iout,'(2x, "Not converged, cycles = ", &
						& i10, " / ", i10, 3i6, f15.7)') &
						& scn%cy(i,j,k), maxIters, i, j, k, scn%Fr(i,j,k)
				end if
			end do
		end do
	end do
	!--------------------------------------------------------------------
	write(*, '(2x, "Writing data to scan.bin")')
	open(newunit=fid, file='scan.bin', form='unformatted', access='stream')
		write(fid) bas%n
		write(fid) tip%n
		write(fid) scn%n
		!-----------------------------------------------------------------
		! xmin, ymin, zmin, xmax, ymax, zmax
		!
		write(fid) scn%x(:,       0 ,       0 ,       0 )
		write(fid) scn%x(:, scn%n(1), scn%n(2), scn%n(3))
		!-----------------------------------------------------------------
		write(fid) bas%nat
		write(fid) bas%x
		write(fid) tip%nat
		write(fid) tip%x
		write(fid) scn%Fz
		write(fid) scn%xp
		!write(fid) scn%fb
		!write(fid) scn%ft
	close(fid)
	!--------------------------------------------------------------------
	! call setTip()
	!
	call deallocate_obj(FF )
	call deallocate_scn(scn)
	!--------------------------------------------------------------------
	close(iout)
	call terminated()
	!--------------------------------------------------------------------
	stop
end
