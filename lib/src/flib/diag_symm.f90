subroutine diag_symm (n,matrix,eval,vector,info)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer,  intent( in) :: n
	integer,  intent( in) :: info
	real(DP), intent( in) :: matrix(n,n)
	!----------------------------------------------------------------------------
	! Output variables
	!
	real(DP), intent(out) :: eval(n)
	real(DP), intent(out) :: vector(n,n)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i
	integer               :: lda
	integer               :: ldz
	integer               :: lwork
	character             :: compz
	integer               :: diag
	real(DP)              :: const
	real(DP), allocatable :: wk  (:  )
	real(DP), allocatable :: d   (:  )
	real(DP), allocatable :: e   (:  )
	real(DP), allocatable :: tau (:  )
	real(DP), allocatable :: work(:  )
	real(DP), allocatable :: a   (:,:)
	real(DP), allocatable :: z   (:,:)
	character(1)          :: uplo
	logical               :: order
	!----------------------------------------------------------------------------
	allocate(d(n  ))
	allocate(a(n,n))
	allocate(z(n,n))
	!----------------------------------------------------------------------------
	!const = 1000.0_DP
	const = 0.0_DP
	diag  = 1
	order = .true.
	!----------------------------------------------------------------------------
	a     = matrix
	!----------------------------------------------------------------------------
	do i=1, n
		a(i,i) = a(i,i) + const
	end do
	!----------------------------------------------------------------------------
	if (diag==1) then
		!-------------------------------------------------------------------------
		allocate(wk (n))
		!-------------------------------------------------------------------------
		call tred2e (n,n,a,d,wk,z)
		call tql2e  (n,n,  d,wk,z,info)
		!-------------------------------------------------------------------------
		deallocate(wk  )
		!-------------------------------------------------------------------------
	else if (diag==2) then
		!-------------------------------------------------------------------------
		uplo  = "U"
		compz = "I"
		lda   = n
		ldz   = n
		lwork = n * 64
		!-------------------------------------------------------------------------
		allocate(e   (n    ))
		allocate(tau (n    ))
		allocate(work(lwork))
		!-------------------------------------------------------------------------
		call dsytrd ( uplo,n,a,lda,d,e,tau,work,lwork,info )
		!-------------------------------------------------------------------------
		deallocate(work)
		allocate(work(max(1, 2*n-2)))
		!-------------------------------------------------------------------------
		call dsteqr ( compz, n, d, e, z, ldz, work, info )
		!-------------------------------------------------------------------------
		deallocate(e   )
		deallocate(tau )
		deallocate(work)
		!-------------------------------------------------------------------------
	end if
	!----------------------------------------------------------------------------
	if (order) call sort_vec_large(n,n,z)
	eval   = d - const
	vector = z
	!----------------------------------------------------------------------------
	deallocate(a)
	deallocate(d)
	deallocate(z)
	!----------------------------------------------------------------------------
	return
end
