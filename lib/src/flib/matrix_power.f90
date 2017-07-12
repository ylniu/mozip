subroutine matrix_power (n, power, a, e, v, ap)
	use kinds, only: DP
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: n
	real(DP), intent( in) :: power
	real(DP), intent( in) :: a (n,n)
	real(DP), intent(out) :: ap(n,n)
	real(DP), intent(out) :: e (  n)
	real(DP), intent(out) :: v (n,n)
	!--------------------------------------------------------------------
	integer  :: i, j, k
	integer  :: ier
	!--------------------------------------------------------------------
	!  raises a symmetric matrix to a power
	!  input
	!    n    = dimension
	!    a    = matrix
	!    power= power
	!  output
	!    ap  = a**power
	!    v   = eigenvectors of a and ap
	!    e   = eigenvalues of ap
	!--------------------------------------------------------------------
	!
	call diag_symm(n, a, e, v, ier)
	!--------------------------------------------------------------------
	call sort_vec_large(n,n,v)
	!--------------------------------------------------------------------
	do i= 1, n
		if (power == -1.0_DP) then
			e(i) = 1.0_DP / e(i)
		else if (power < 0.0_DP .and. e(i)>-1.E-6_DP .and. e(i)< 0.0_DP) then
			e(i) = 0.0
		else
			e(i) = e(i)**power
		end if
	end do
	!--------------------------------------------------------------------
	! Back transformation of eigenvalues
	!
	ap=0.0_DP
	do i=1, n
		do j=1, i
			do k=1, n
				ap(i,j) = ap(i,j) + v(i,k) * e(k) * v(j,k)
			end do
			if (i/=j) ap(j,i) = ap(i,j)
		end do
	end do
	!--------------------------------------------------------------------
	return
end subroutine
