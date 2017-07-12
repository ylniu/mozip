function in_basis3(n, n_symm, jreln, idx_symm, iat, ip)
	use symmetry1, only: op
	use kinds, only: DP
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	integer , intent( in) :: n_symm
	integer , intent( in) :: jreln(n*3,8)
	integer , intent( in) :: idx_symm(n)
	integer , intent( in) :: iat
	integer , intent(out) :: ip
	integer               :: in_basis3
	!--------------------------------------------------------------------
	integer               :: i, j, k
	!--------------------------------------------------------------------
	in_basis3 = 0
	!--------------------------------------------------------------------
	do i=1, n_symm
		j=idx_symm(i)
		do ip=1, 8
			if (op(ip)) then
				k=jreln(j,ip)
				if ( k==iat ) then
					in_basis3 = j
					return
				end if
			end if
		end do
	end do
	!--------------------------------------------------------------------
	return
end function
