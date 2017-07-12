subroutine average_amp(nv, nvz, scn_n, scn_fv, scn_fz, vxmin, vxmax)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: nv
	integer , intent( in) :: nvz  (3)
	integer , intent( in) :: scn_n(3)
	real(DP), intent( in) :: scn_fz(0:scn_n(1), 0:scn_n(2), 0:scn_n(3))
	real(DP), intent(out) :: scn_fv(0:nvz  (1), 0:nvz  (2), 0:nvz  (3))
	real(DP), intent(out) :: vxmin(3)
	real(DP), intent(out) :: vxmax(3)
	!--------------------------------------------------------------------
	integer               :: ix, iy, iz, i, jz
	real(DP)              :: dx
	!--------------------------------------------------------------------
	scn_fv = 0.0_DP
	do ix=0, nvz(1)
		do iy=0, nvz(2)
			do iz=0, nvz(3)
				do i=0, nv-1
					jz=iz+i
					scn_fv(ix,iy,iz) = scn_fv(ix,iy,iz) + scn_fz(ix, iy, jz)
				end do
			end do
		end do
	end do
	scn_fv = scn_fv / nv
	!--------------------------------------------------------------------
	dx       = ( vxmax(3) - vxmin(3) ) / scn_n(3)
	vxmin(3) = vxmin(3) + (nv - 1) * dx / 2.0_DP
	vxmax(3) = vxmax(3) - (nv - 1) * dx / 2.0_DP
	!--------------------------------------------------------------------
	return
end subroutine
