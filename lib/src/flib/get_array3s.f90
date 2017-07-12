subroutine get_array3s(na, id3, v0, tol, info)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: na
	integer , intent( in) :: id3(3, 3, na)
	real(DP), intent( in) :: tol
	real(DP), intent(out) :: v0(3,na)
	integer               :: info
	!--------------------------------------------------------------------
	!
	integer               :: n3, sg
	integer               :: ia, ix
	integer               :: ja, jx
	integer               :: isym, sv
	logical               :: lsame          ! irreps of x, v are same
	logical               :: ldiff          ! irreps of x, v are different
	integer               :: rs
	real(DP)              :: dx
	real(DP), allocatable :: vs(:,:)
	!--------------------------------------------------------------------
	allocate(vs(3,na))
	!--------------------------------------------------------------------
	info  = 0
	n3    = na * 3
	dx    = 0.D0
	rs    = 0
	lsame = .true.
	ldiff = .true.
	do ia=1, na
		do ix=1, 3
			ja = id3(1,ix,ia)
			jx = id3(2,ix,ia)
			sg = id3(3,ix,ia)
			if (ia/=ja ) then
				if (abs( v0(ix,ia) - v0(jx,ja) * sg ) > tol) lsame=.false.
				if (abs( v0(ix,ia) + v0(jx,ja) * sg ) > tol) ldiff=.false.
			end if
		end do
	end do
	!--------------------------------------------------------------------
	if (lsame) then
		isym =  1
	else if (ldiff) then
		isym = -1
	else
		isym =  0
		info = -1
	end if
	!--------------------------------------------------------------------
	if (isym/=0) then
		do ia=1, na
			do ix=1, 3
				ja = id3(1,ix,ia)
				jx = id3(2,ix,ia)
				sg = id3(3,ix,ia)
				sv = sg * isym
				!-----------------------------------------------------------
				if (ia/=ja ) sg = sg * isym
				!-----------------------------------------------------------
				vs(ix,ia) = v0(jx,ja) * sg
				dx = dx + (v0(ix,ia)-vs(ix,ia))**2
				!-----------------------------------------------------------
			end do
		end do
	else
		vs = v0
	end if
	!--------------------------------------------------------------------
	dx = dx / n3
	dx = sqrt(dx)
	!--------------------------------------------------------------------
	if (dx > tol) then
		write(*,'(2x,"tol = ", f15.7)') tol
		write(*,'(2x,"dx  = ", f15.7)') dx
		write(*,'(2x,"dx > tol, fs is not the same as f0, stop!")')
		stop
	end if
	!--------------------------------------------------------------------
	v0 = vs
	!--------------------------------------------------------------------
	deallocate(vs)
	!--------------------------------------------------------------------
	return
end subroutine