subroutine get_array9(na, id9, f0, tol, info)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: na
	integer , intent( in) :: id9(5, 3, na, 3, na)
	real(DP), intent( in) :: tol
	real(DP), intent(out) :: f0(3,na,3,na)
	integer , intent(out) :: info
	!--------------------------------------------------------------------
	integer               :: n3, sg
	integer               :: ia, ix
	integer               :: ja, jx
	integer               :: ka, kx
	integer               :: la, lx
	integer               :: ii, jj, kk, ll
	real(DP)              :: s, dx
	real(DP), allocatable :: fs(:,:,:,:)
	!--------------------------------------------------------------------
	allocate(fs(3,na,3,na))
	!--------------------------------------------------------------------
	info = 0
	n3   = na * 3
	!--------------------------------------------------------------------
	dx   = 0.D0
	do ia=1, na
		do ix=1, 3
			ii = (ia-1) * 3 + ix
			do ja=1, na
				do jx=1, 3
					jj=(ja-1) * 3 + jx
					ka = id9(1,jx,ja,ix,ia)
					kx = id9(2,jx,ja,ix,ia)
					la = id9(3,jx,ja,ix,ia)
					lx = id9(4,jx,ja,ix,ia)
					sg = id9(5,jx,ja,ix,ia)
					kk = (ka-1) * 3 + kx
					ll = (la-1) * 3 + lx
					fs(jx,ja,ix,ia) = f0(lx,la,kx,ka) * sg
					s  = (fs(jx,ja,ix,ia) - f0(jx,ja,ix,ia))**2
					dx = dx + s
					if (sqrt(s)>tol) then
						write(*,'("get_array9:", 4(i4,2x), 4x 4(2i4,2x), 4x, i4, 4f15.7)') &
							ii,jj, kk, ll, &
							jx,ja, &
							ix,ia, &
							lx,la, &
							kx,ka, &
							sg, s, fs(jx,ja,ix,ia), f0(jx,ja,ix,ia), f0(lx,la,kx,ka)
					end if
				end do
			end do
		end do
	end do
	!--------------------------------------------------------------------
	dx = dx / n3 / n3
	dx = sqrt(dx)
	!--------------------------------------------------------------------
	if (dx > tol) then
		write(*,'(2x,"tol = ", f15.7)') tol
		write(*,'(2x,"dx  = ", f15.7)') dx
		write(*,'(2x,"dx > tol, fs is not the same as f0, stop!")')
		stop
	end if
	!--------------------------------------------------------------------
	f0 = fs
	call set_sym_matrix(n3,f0)
	!--------------------------------------------------------------------
	deallocate(fs)
	!--------------------------------------------------------------------
	return
end subroutine