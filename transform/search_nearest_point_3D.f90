subroutine search_nearest_point_3D(center, step, n, pointx, pointy, near_point)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: n(3)
	real(DP), intent( in) :: center(3)
	real(DP), intent( in) :: step(3,3)
	real(DP), intent( in) :: pointx(3)
	real(DP), intent(out) :: pointy(3)
	integer , intent(out) :: near_point(3)
	!----------------------------------------------------------------------------
	integer               :: ix, iy, iz, ii(3)
	real(DP)              :: x(3), dx(3), r, rmin
	!----------------------------------------------------------------------------
	rmin       = 1.D99
	near_point = 0
	do ix=1, n(1)
		ii(1) = ix
		x(1)=center(1) + (ix-1) * step(1,1)
		do iy=1, n(2)
			ii(2) = iy
			x(2)=center(2) + (iy-1) * step(2,2)
			do iz=1, n(3)
				ii(3) = iz
				x(3)=center(3) + (iz-1) * step(3,3)
				dx=x-pointx
				r=sqrt(dot_product(dx, dx))
				if (r < rmin) then
					rmin       = r
					pointy     = x
					near_point = ii
				end if
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
