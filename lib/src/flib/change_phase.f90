subroutine change_phase(n, dr, dm)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer     :: n
	real(DP)    :: dr(3,n), dm(3,n)
	integer     :: i, j, di, dj
	real(DP)    :: dd, dmax
	complex(DP) :: zmax, rmax
	complex(DP), allocatable :: z(:,:)
	!----------------------------------------------------------------------------
	di=-1
	!----------------------------------------------------------------------------
	allocate(z(3,n))
	!----------------------------------------------------------------------------
	do i=1, n
		do j=1, 3
			z(j,i) = cmplx(dr(j,i),dm(j,i),DP)
		end do
	end do
	!----------------------------------------------------------------------------
	do i=1, n
		do j=1, 3
			dd = abs(z(j,i))
			if (dmax<dd) then
				dmax = dd
				zmax = z(j,i)
				rmax = cmplx(dd,0.0_DP,DP)
				di   = i
				dj   = j
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	if (dmax < 1.D-20) then
		write(*,*) "dmax is too small in  flib/change_phase.f90, stop"
		stop
	end if
	!----------------------------------------------------------------------------
	do i=1, n
		do j=1, 3
			z (j,i) = z(j,i) * rmax / zmax
			dr(j,i) = real (z(j,i))
			dm(j,i) = aimag(z(j,i))
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(z)
	!----------------------------------------------------------------------------
	return
end subroutine
