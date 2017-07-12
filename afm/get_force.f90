subroutine get_force(n, x, fb, ft, f2)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer     , intent( in) :: n
	real(DP)    , intent( in) :: x(3,n)
	!--------------------------------------------------------------------
	! Output variables
	!
	real(DP)    , intent(out) :: fb(3,n)
	real(DP)    , intent(out) :: ft(3,n)
	real(DP)    , intent(out) :: f2(3,n)
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: i, j
	!--------------------------------------------------------------------
	! Debug  variables
	!
	real(DP)                  :: y(6)
	real(DP)    , allocatable :: xt(:,:)
	logical                   :: debug
	!--------------------------------------------------------------------
	! debug
	!
	debug = .false.
	if (debug) then
		allocate(xt(3,n))
		xt=x
		!-----------------------------------------------------------------
		y(1) = xt(3,1)
		y(2) = xt(3,2)
		y(3) = xt(3,3)
		y(4) = xt(3,4)
		y(5) = xt(3,5)
		y(6) = xt(3,6)
		!-----------------------------------------------------------------
		write(1,'(a10, 3a15)'), "n", "x6", "f1", "f2"
		do i=1, 2000
			do j=5, 6
				xt(3,j) = y(j) - y(6) + i * 0.01
			end do
			call get_force_tip(n, xt, ft, f2)
			call get_force_bas(n, xt, fb)
			!--------------------------------------------------------------
			write(1,'(i10, 3f15.7)') i, xt(3,6), &
				ft(3,5) + ft(3,6), fb(3,5) + fb(3,6)
		end do
		deallocate(xt)
		stop
	end if
	!--------------------------------------------------------------------
	call get_force_tip(n, x, ft, f2)
	call get_force_bas(n, x, fb)
	!--------------------------------------------------------------------
	return
end subroutine
