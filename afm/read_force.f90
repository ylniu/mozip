subroutine read_force
	use module_afm, only: k_E, k_F, xrange
	use param     , only: au2a
	use kinds     , only: DP
	implicit none
	!--------------------------------------------------------------------
	real(DP)       :: PI, a
	!real(DP)       :: xx, yy, y
	integer        :: fid, i, j
	character(200) :: fforce, tmp
	!--------------------------------------------------------------------
	PI = acos(-1.0_DP)
	a  = 180.0_DP / PI
	call get_free_fid(fid)
	!--------------------------------------------------------------------
	fforce="Force-constants.txt"
	!--------------------------------------------------------------------
	open(fid, file=fforce, status="old")
		do i=1, 4
			read(fid, *)
			read(fid, *) xrange(:,i)
			do j=0, 9
				read(fid, *) tmp, k_E(j,i)
			end do
		end do
	close(fid)
	!--------------------------------------------------------------------
	! To atomic unit
	!
	! E = \sum_i ( C_i  x^i )
	! E = \sum_i ( ( C_i * au2a^i)  (x / au2a)^i )
	!
	do i=1, 2
		xrange(:,i) = xrange(:,i) / au2a
		do j=0, 9
			!write(*,'(2i10, f20.10)') i, j, k_E(j,i)
			k_E(j,i) = k_E(j,i) * au2a**j
			!write(*,'(2i10, f20.10)') i, j, k_E(j,i)
		end do
	end do
! 	do i=1, 1000
! 		xx=xrange(1,1) + (i-1) * 0.01D0
! 		if(xx>xrange(2,1)) exit
! 		yy=0.D0
! 		do j=1, 9
! 			yy = yy - j * k_E(j,1) * xx**(j-1)
! 		end do
! 		write(51, '(f15.7, es20.10)') xx*au2a, yy
! 	end do
! 	stop
! 	do i=280, 900
! 		dx = (i-1) * 0.01
! 		b = 0.0_DP
! 		do j=0, 9
! 			b = b + k_E(j,1) * dx**j
! 		end do
! 		write(8, '(f15.7, e20.10)') dx, b
! 	end do
	!--------------------------------------------------------------------
	! To atomic unit
	!
	! E = \sum_i ( C_i  x^i )
	! E = \sum_i ( ( C_i * (180/PI)^i)  (x / (180/PI) )^i )
	!
	do i=3, 4
		xrange(:,i) = xrange(:,i) / a
		do j=0, 9
			k_E(j,i) = k_E(j,i) * a**j
		end do
	end do
	!--------------------------------------------------------------------
	!--------------------------------------------------------------------
	! E =   \sum_{j=0}^{9} k_E(j) * x^j
	! F = - dE/dx
	!   = - \sum_{j=1}^{9} k_E(j) * j * x^(j-1)
	!
	 do i=1, 4
		  do j=1, 9
			 k_F(j,i) = - k_E(j,i) * j
		  end do
	 end do
	!--------------------------------------------------------------------
! 	do i=280, 900
! 		dx = (i-1) * 0.01
! 		b = 0.0_DP
! 		do j=1, 9
! 			b = b - j * k_E(j,1) * dx**(j-1)
! 		end do
! 		write(3, '(f15.7, e20.10)') dx, b
! 	end do
	!--------------------------------------------------------------------
	return
end subroutine
