subroutine get_array3(na, id3, v0, tol, info)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: na
	integer , intent( in) :: id3(3, 3, na)
	real(DP), intent( in) :: tol
	real(DP), intent(out) :: v0(3,na)
	
	integer , intent(out) :: info
	!--------------------------------------------------------------------
	integer               :: n3, sg
	integer               :: ia, ix
	integer               :: ja, jx
	real(DP), allocatable :: vs(:,:)
	real(DP)              :: s, dx, tol1
	!--------------------------------------------------------------------
	allocate(vs(3,na))
	!--------------------------------------------------------------------
	tol1 = tol * 1.0D0
	info = 0
	n3   = na * 3
	dx   = 0.D0
	do ia=1, na
		do ix=1, 3
			ja = id3(1,ix,ia)
			jx = id3(2,ix,ia)
			sg = id3(3,ix,ia)
			vs(ix,ia) = v0(jx,ja) * sg
			s  = abs(v0(ix,ia)-vs(ix,ia))
			if (s>tol1) then
				write(*,'("get_array3: ", 4i4, 4f15.7)') ia,ix,ja,jx,v0(ix,ia), v0(jx,ja), vs(ix,ia), s
			end if
			dx = dx + s**2
		end do
	end do
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
