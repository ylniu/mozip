subroutine get_trans_rot_vec(n, mass, x, v, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: mass(n)
	real(DP), intent( in) :: x(3,n)
	integer , intent(out) :: info
	real(DP), intent(out) :: v(n*3,6)
	!----------------------------------------------------------------------------
	integer               :: i, j, ix, iy, iz
	real(DP)              :: qx, qy, qz
	real(DP), allocatable :: sqm(:)
	real(DP), allocatable :: q(:,:)
	!----------------------------------------------------------------------------
	info=0
	!----------------------------------------------------------------------------
	if (n<0) then
		info=-1
		return
	end if
	!----------------------------------------------------------------------------
	allocate(q  (3,n))
	allocate(sqm(  n))
	!----------------------------------------------------------------------------
	do i=1, n
		sqm(i) = sqrt(mass(i))
	end do
	!----------------------------------------------------------------------------
	do i=1, n
		do j=1, 3
			q(j,i) = sqm(i) * x(j,i)
		end do
	end do
	!----------------------------------------------------------------------------
	do i=1, n
		ix      =  (i-1) * 3 + 1
		iy      =  (i-1) * 3 + 2
		iz      =  (i-1) * 3 + 3
		!
		qx      =  q(1,i)
		qy      =  q(2,i)
		qz      =  q(3,i)
		!
		v(ix,1) =  sqm(i)
		v(iy,1) =  0.0_DP
		v(iz,1) =  0.0_DP
		!
		v(ix,2) =  0.0_DP
		v(iy,2) =  sqm(i)
		v(iz,2) =  0.0_DP
		!
		v(ix,3) =  0.0_DP
		v(iy,3) =  0.0_DP
		v(iz,3) =  sqm(i)
		!
		v(ix,4) =  0.0_DP
		v(iy,4) = -qz
		v(iz,4) =  qy
		!
		v(ix,5) =  qz
		v(iy,5) =  0.0_DP
		v(iz,5) = -qx
		
		v(ix,6) = -qy
		v(iy,6) =  qx
		v(iz,6) =  0.0_DP
	end do
	!----------------------------------------------------------------------------
	deallocate(q  )
	deallocate(sqm)
	!----------------------------------------------------------------------------
	return
end subroutine get_trans_rot_vec
