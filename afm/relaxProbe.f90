subroutine relaxProbe(ix, iy)
	use kinds     , only: DP
	use module_afm, only: scn
	use module_afm, only: tip
	use module_afm, only: probeStart
	use module_afm, only: debug_imin
	use module_afm, only: debug_imax
	use module_afm, only: debug_xy
	use module_afm, only: f_eps
	use module_afm, only: fire_dtmax
	use module_afm, only: fire_damping
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer , intent( in) :: ix, iy
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer               :: cy
	integer               :: iz, nz
	integer               :: it, nt
	integer               :: izmin, izmax
	logical               :: conv
	real(DP)              :: fmol(3)
	real(DP)              :: fmola(3)
	real(DP)              :: dt
	real(DP)              :: dz
	real(DP)              :: acoef
	real(DP), allocatable :: fb(:,:)
	real(DP), allocatable :: ft(:,:)
	real(DP), allocatable :: x0(:,:)
	real(DP), allocatable :: x1(:,:)
	real(DP), allocatable :: xp(:,:)
	real(DP), allocatable :: v (:,:)
	!--------------------------------------------------------------------
	izmin       = 0
	izmax       = scn%n(3)
	!--------------------------------------------------------------------
	if(debug_xy) then
		izmin = debug_imin(3)
		izmax = debug_imax(3)
	end if
	!--------------------------------------------------------------------
	nt = tip%n
	nz = scn%n(3)
	!--------------------------------------------------------------------
	allocate(x0(3, nt))
	allocate(x1(3, nt))
	allocate(xp(3, nt))
	allocate(fb(3, nt))
	allocate(ft(3, nt))
	allocate(v (3, nt))
	!--------------------------------------------------------------------
	do it=1, nt
		x1(:,it) = tip%x(:,it) + scn%x(:,ix,iy,izmax)
	end do
	!--------------------------------------------------------------------
	dt    = fire_dtmax
	acoef = fire_damping
	!--------------------------------------------------------------------
	if (izmax>=1) then
		dz=abs(scn%x(3,0,0,1) - scn%x(3,0,0,0))
	end if
	!--------------------------------------------------------------------
	do iz=izmax, izmin, -1
		!-----------------------------------------------------------------
		! Set x0
		!
		do it=1, nt
			x0(:,it) = tip%x(:,it) + scn%x(:,ix,iy,iz)
		end do
		!-----------------------------------------------------------------
		! Set xp
		!
		if      (probeStart==0) then
			xp = x0
		else if (probeStart==1) then
			xp = x1
		end if
		!-----------------------------------------------------------------
		v     = 0.00_DP
		!-----------------------------------------------------------------
		call relaxTip(nt, xp, fb, ft, cy, conv, fmol, fmola, f_eps, v, dt, acoef)
		x1 = xp
		if (iz<izmax .and. izmax/=izmin) then
			do it=1, nt
				x1(3, it) = xp(3, it) - dz
			end do
		end if
		!-----------------------------------------------------------------
		scn%conv(    ix,iy,iz) = conv
		scn%cy  (    ix,iy,iz) = cy
		scn%xp  (:,:,ix,iy,iz) = xp
		scn%fb  (:,:,ix,iy,iz) = fb
		scn%ft  (:,:,ix,iy,iz) = ft
		!-----------------------------------------------------------------
		scn%F (:,ix,iy,iz) = fmol
		scn%Fz(  ix,iy,iz) = fmol(3)
		scn%Fa(  ix,iy,iz) = fmola(3)
		scn%Fr(  ix,iy,iz) = sqrt(dot_product(fmol,fmol))
		!-----------------------------------------------------------------
	end do
	!--------------------------------------------------------------------
	deallocate(x0)
	deallocate(xp)
	deallocate(fb)
	deallocate(ft)
	deallocate(v )
	!--------------------------------------------------------------------
	return
end subroutine
