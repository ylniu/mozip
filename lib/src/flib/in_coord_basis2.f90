subroutine in_coord_basis2(n, n_symm2, jreln, idx_symm2, iat, jat, kat, lat, ip)
	use symmetry1, only: op
	use kinds, only: DP
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	integer , intent( in) :: n_symm2
	integer , intent( in) :: jreln(n,8)
	integer , intent( in) :: idx_symm2(2,n*n)
	integer , intent( in) :: iat
	integer , intent( in) :: jat
	integer , intent(out) :: kat
	integer , intent(out) :: lat
	integer , intent(out) :: ip
	!--------------------------------------------------------------------
	integer               :: i, j1, j2, k1, k2
	!--------------------------------------------------------------------
	kat = 0
	lat = 0
	if (n_symm2<=0) return
	!--------------------------------------------------------------------
	do i=1, n_symm2
		j1=idx_symm2(1,i)
		j2=idx_symm2(2,i)
		!-----------------------------------------------------------------
		do ip=1, 8
			if (op(ip)) then
				!-----------------------------------------------------------
				k1=jreln(j1,ip)
				k2=jreln(j2,ip)
				if ( k1==iat .and. k2==jat ) then
					kat = j1
					lat = j2
					return
				end if
				!-----------------------------------------------------------
			end if
		end do
		!-----------------------------------------------------------------
	end do
	!--------------------------------------------------------------------
	return
end subroutine