!-----------------------------------------------------------------------
! Trilinear interpolation
! https://en.wikipedia.org/wiki/Trilinear_interpolation
!
subroutine get_force_bas(n, x0, f)
	use kinds     , only: DP
	use module_afm, only: FF
	use math      , only: v3_cross, v3_normlize
	use module_afm, only: tip
	implicit none
	!--------------------------------------------------------------------
	integer     , intent( in) :: n
	real(DP)    , intent( in) :: x0 (3,n)
	real(DP)    , intent(out) :: f  (3,n)
	!--------------------------------------------------------------------
	integer                   :: i, j
	integer                   :: n1 (3  )
	integer                   :: n2 (3  )
	real(DP)                  :: vt
	real(DP)                  :: dx (3  )
	real(DP)                  :: vg (  8)
	real(DP)                  :: fg (3,8)
	real(DP)                  :: xg (3,8)
	real(DP)                  :: dxg(3,8)
	!--------------------------------------------------------------------
	f=0.0_DP
	dx(1) = FF%dCell(1,1)
	dx(2) = FF%dCell(2,2)
	dx(3) = FF%dCell(3,3)
	!--------------------------------------------------------------------
	do i=1, n
		if (tip%opt_sum(i)=="T") then
		do j=1, 3
			n1(j) = int(x0(j,i)/dx(j))
		end do
		n2 = n1 + 1
		if (any(n1<0) .or. any(n2>FF%n)) then
	!--------------------------------------------------------------------
			write(*, '("Error for n1, n2")')
			write(*, '("FF%n     = ", 3i15  )')  FF%n
			write(*, '("atom i   = ",  i15  )')  i
			write(*, '("x        = ", 3f15.7)')  x0(:,i)
			write(*, '("dx       = ", 3f15.7)')  dx
			write(*, '("n1       = ", 3i15  )')  n1
			write(*, '("n2       = ", 3i15  )')  n2
			write(*, '("Stop!")')
			call exit(1)
			!return
		end if
		!--------------------------------------------------------------
		xg(:,1) = FF%x(:,n1(1), n1(2), n1(3)   )
		xg(:,2) = FF%x(:,n2(1), n1(2), n1(3)   )
		xg(:,3) = FF%x(:,n2(1), n2(2), n1(3)   )
		xg(:,4) = FF%x(:,n1(1), n2(2), n1(3)   )
		xg(:,5) = FF%x(:,n1(1), n1(2), n2(3)   )
		xg(:,6) = FF%x(:,n2(1), n1(2), n2(3)   )
		xg(:,7) = FF%x(:,n2(1), n2(2), n2(3)   )
		xg(:,8) = FF%x(:,n1(1), n2(2), n2(3)   )
		!--------------------------------------------------------------
		fg(:,1) = FF%T(:,n1(1), n1(2), n1(3), i)
		fg(:,2) = FF%T(:,n2(1), n1(2), n1(3), i)
		fg(:,3) = FF%T(:,n2(1), n2(2), n1(3), i)
		fg(:,4) = FF%T(:,n1(1), n2(2), n1(3), i)
		fg(:,5) = FF%T(:,n1(1), n1(2), n2(3), i)
		fg(:,6) = FF%T(:,n2(1), n1(2), n2(3), i)
		fg(:,7) = FF%T(:,n2(1), n2(2), n2(3), i)
		fg(:,8) = FF%T(:,n1(1), n2(2), n2(3), i)
		!--------------------------------------------------------------
		vt = 0.0_DP
		do j=1, 8
			dxg(:,j) = abs(xg(:,j) - x0(:,i))
			vg (  j) = abs(dxg(1,j) * dxg(2,j) * dxg(3,j))
			vt       = vt + vg(j)
		end do
		vg = vg / vt
		!--------------------------------------------------------------
		f(:,i) =   fg(:,1) * vg(7) &
					+ fg(:,2) * vg(8) &
					+ fg(:,3) * vg(5) &
					+ fg(:,4) * vg(6) &
					+ fg(:,5) * vg(3) &
					+ fg(:,6) * vg(4) &
					+ fg(:,7) * vg(1) &
					+ fg(:,8) * vg(2)
		!--------------------------------------------------------------
		end if
	end do
	!--------------------------------------------------------------------
	return
end subroutine
