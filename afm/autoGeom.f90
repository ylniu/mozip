subroutine autoGeom()
	use module_afm, only: DP, iout, border, if_shiftXY, if_fitCell, &
		grida, gridb, scanmin, scanmax, bas
	use math      , only: v3_max, v3_min, v3_disp1
	implicit none
	!----------------------------------------------------------------------------
	! set Force-Filed and Scanning supercell to fit optimally given geometry
	! then shifts the geometry in the center of the supercell
	!----------------------------------------------------------------------------
	integer               :: i
	integer               :: natom
	real(DP)              :: xmin, xmax, ymin, ymax, zmax
	real(DP)              :: dx, dy
	real(DP), allocatable :: x(:,:)
	!----------------------------------------------------------------------------
	natom = bas%n
	!----------------------------------------------------------------------------
	allocate(x(3,natom))
	!----------------------------------------------------------------------------
	x    = bas%x
	zmax = v3_max(natom, x, 3)
	!----------------------------------------------------------------------------
	do i=1, natom
		x(3,i) = x(3,i) - zmax
	end do
	!----------------------------------------------------------------------------
	write(iout, '(2x,"autoGeom substracted zmax ", f18.7)') zmax
	!----------------------------------------------------------------------------
	xmin = v3_min(natom, x, 1)
	xmax = v3_max(natom, x, 1)
	ymin = v3_min(natom, x, 2)
	ymax = v3_max(natom, x, 2)
	!----------------------------------------------------------------------------
	if (if_fitCell) then
		grida  (1) = (xmax-xmin) + 2.D0 * border
		grida  (2) = 0.D0
		gridb  (1) = 0.D0
		gridb  (2) = (ymax-ymin) + 2.D0 * border
		scanmin(1) = 0.D0
		scanmax(1) = grida(1)
		scanmin(2) = 0.D0
		scanmax(2) = gridb(2)
		write(iout,'(2x,"autoGeom changed cell to", 2x, 3f18.7)') scanmax
	end if
	!----------------------------------------------------------------------------
	write(iout,'(2x, "scanMin", x, 3f18.7)') scanmin
	write(iout,'(2x, "scanMax", x, 3f18.7)') scanmax
	!----------------------------------------------------------------------------
	if (if_shiftXY) then
		dx = -0.5D0*(xmin+xmax) + 0.5D0*( grida(1) + gridb(1) )
		call v3_disp1(natom, x, 1, dx)
		dy = -0.5D0*(ymin+ymax) + 0.5D0*( grida(2) + gridb(2) )
		call v3_disp1(natom, x, 2, dy)
		write(iout,'(2x,"autoGeom moved geometry by", 2f18.7)') dx, dy
	end if
	!----------------------------------------------------------------------------
	bas%x=x
	deallocate(x)
	!----------------------------------------------------------------------------
	return
end subroutine
