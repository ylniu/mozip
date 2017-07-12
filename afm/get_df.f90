subroutine get_df(dnsz, scn_n, scn_dfz, scn_fz, dsxmin, dsxmax)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: dnsz (3)
	integer , intent( in) :: scn_n(3)
	real(DP), intent( in) :: scn_fz (0:scn_n(1), 0:scn_n(2), 0:scn_n(3))
	real(DP), intent(out) :: scn_dfz(0:dnsz (1), 0:dnsz (2), 0:dnsz (3))
	real(DP), intent(out) :: dsxmin(3)
	real(DP), intent(out) :: dsxmax(3)
	!--------------------------------------------------------------------
	integer               :: ix, iy, iz, i, jz
	real(DP)              :: dx
	!--------------------------------------------------------------------
	scn_dfz = 0.0_DP
	do ix=0, dnsz(1)
		do iy=0, dnsz(2)
			do iz=0, dnsz(3)
				do i=0, 2
					jz=iz+i
					scn_dfz(ix,iy,iz) = scn_dfz(ix,iy,iz) + scn_fz(ix, iy, jz)
				end do
			end do
		end do
	end do
	scn_dfz = scn_dfz / 2
	!--------------------------------------------------------------------
	dx        = ( dsxmax(3) - dsxmin(3) ) / scn_n(3)
	dsxmin(3) = dsxmin(3) + dx 
	dsxmax(3) = dsxmax(3) - dx
	!--------------------------------------------------------------------
	return
end subroutine
