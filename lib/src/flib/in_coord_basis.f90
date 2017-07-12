subroutine in_coord_basis(n, n_symm, jreln, idx_symm, iat, jat, ip)
	use symmetry1, only: op
	use kinds, only: DP
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	integer , intent( in) :: n_symm
	integer , intent( in) :: jreln(n,8)
	integer , intent( in) :: idx_symm(n)
	integer , intent( in) :: iat
	integer , intent(out) :: jat
	integer , intent(out) :: ip
	!--------------------------------------------------------------------
	integer               :: i, j, k
	!--------------------------------------------------------------------
	jat = 0
	if (n_symm<=0) return
	!--------------------------------------------------------------------
	do i=1, n_symm
		j=idx_symm(i)
		do ip=1, 8
			if (op(ip)) then
				k=jreln(j,ip)
				if ( k==iat ) then
					jat = j
					return
				end if
			end if
		end do
	end do
	!--------------------------------------------------------------------
	return
end subroutine