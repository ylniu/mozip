subroutine get_Wilson_G_matrix(n, m, mass, B, G)
	use kinds, only: DP
	!--------------------------------------------------------------------
	integer , intent( in) :: n, m
	real(DP), intent( in) :: mass(   n  )
	real(DP), intent( in) :: B   (m, n*3)
	real(DP), intent(out) :: G   (m, m  )
	!--------------------------------------------------------------------
	integer               :: n3
	integer               :: i, j, k
	real(DP), allocatable :: mass_inv(:)
	!--------------------------------------------------------------------
	n3 = n * 3
	!--------------------------------------------------------------------
	allocate(mass_inv(n3))
	!--------------------------------------------------------------------
	do i=1, n
		do j=1, 3
			k=(i-1) * 3 + j
			mass_inv(k) = 1.0_DP / mass(i)
		end do
	end do
	!--------------------------------------------------------------------
	G = 0.0_DP
	do i=1, m
		do j=1, i
			do k=1, n3
				G(i,j) = G(i,j) + B(i,k) * B(j,k) * mass_inv(k)
			end do
			if (i/=j) G(j,i) = G(i,j)
		end do
	end do
	!--------------------------------------------------------------------
	deallocate(mass_inv)
	!--------------------------------------------------------------------
	return
end subroutine
