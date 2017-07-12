subroutine search_symm_xyz (disp_sign, n, x, symm_op, jrels, op, tol, n6, &
	n_calc, f_calc, n_calc_n6, m_calc, m_symm)
	use kinds
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer      , intent(in ) :: disp_sign, n, n6
	real(DP)     , intent(in ) :: symm_op(3,3,8)
	integer      , intent(in ) :: jrels(n,8)
	real(DP)     , intent(in ) :: tol
	real(DP)     , intent(in ) :: x(3,n)
	logical      , intent(in ) :: op(8)
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer      , intent(out) :: n_calc
	integer      , intent(out) :: n_calc_n6(n6)
	integer      , intent(out) :: m_symm(n6)
	integer      , intent(out) :: m_calc(n6)
	logical      , intent(out) :: f_calc(n6)
	!----------------------------------------------------------------------------
	! Local variables
	!
	real(DP)              :: x1(3, n)
	!----------------------------------------------------------------------------
	integer               :: i6, iat, istep, ixyz, ip
	integer               :: j6, jat, jstep, jxyz
	real(DP)              :: vi(3), v1(3), vj(3), dv(3), dvr
	!----------------------------------------------------------------------------
	x1    = x
	!----------------------------------------------------------------------------
	! 1+x  .t.,  1+x  , E   ,  1   0   0   0   1   0   0   0   1
	! 1+y  .t.,  1+y  , E   ,  1   0   0   0   1   0   0   0   1
	! 1+z  .t.,  1+z  , E   ,  1   0   0   0   1   0   0   0   1
	! 1-x  .f.,  1+y  , Cz  ,  a11 a12 0   a21 a22 0   0   0   1
	! 1-y  .f.,  1+x  , Cz  ,  a11 a12 0   a21 a22 0   0   0   1
	! 1-z  .f.,  1+z  , Cz  ,  a11 a12 0   a21 a22 0   0   0   1
	! 2+x
	! 2+y
	! 2+z
	!
	f_calc   = .true.
	n_calc   = 0
	m_calc   = 0
	n_calc_n6= 0
	m_symm   = 1
	!----------------------------------------------------------------------------
	do i6=1, n6
		m_calc(i6) = i6
	end do
	!----------------------------------------------------------------------------
	do iat   = 1, n
	do ixyz  = 1, 3
	do istep = 1, 2
		!-------------------------------------------------------------------------
		i6         = (iat-1) * 6 + (ixyz-1) * 2 + istep
		if ( .not. f_calc(i6) ) cycle
		n_calc              = n_calc + 1
		n_calc_n6( n_calc ) = i6
		!-------------------------------------------------------------------------
		! istep * 2 - 3 = -1 or 1
		!
		vi       = 0
		vi(ixyz) = (istep * 2 - 3) * disp_sign
		!-------------------------------------------------------------------------
		do ip=2, 8
			if ( op(ip) ) then
				j6=0
				do jat   = iat, n
				if(jrels(jat,ip)==iat) then
				do jxyz  = 1, 3
				do jstep = 1, 2
					!----------------------------------------------------------------
					j6 = (jat-1) * 6 + (jxyz-1) * 2 + jstep
					if ( .not. f_calc(j6) ) cycle
					if (i6 /= j6) then
						!-------------------------------------------------------------
						! jstep * 2 - 3 = -1 or 1
						!
						vj       = 0
						vj(jxyz) = (jstep * 2 - 3) * disp_sign
						!-------------------------------------------------------------
						v1       = vi
						call rotn(1, symm_op(1,1,ip), v1)
						!-------------------------------------------------------------
						dv  = v1 - vj
						dvr = sqrt(dot_product(dv,dv))
						!-------------------------------------------------------------
						if ( dvr <= tol ) then
							f_calc(j6) = .false.
							m_calc(j6) = i6
							m_symm(j6) = ip
							exit
						end if
					end if
				end do
				end do
				end if
				end do
			end if
		end do
	end do
	end do
	end do
	!----------------------------------------------------------------------------
	return
end
