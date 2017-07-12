subroutine project_mhessian_rot(n, x, mass, fcm, info)
	use kinds
	implicit none
	integer , intent(in   ) :: n
	real(DP), intent(in   ) :: mass(n)
	real(DP), intent(in   ) :: x(3,n)
	integer , intent(  out) :: info
	real(DP), intent(inout) :: fcm(n*3, n*3)
	real(DP), allocatable   :: pj(:,:,:)
	real(DP), allocatable   :: Im(:,:)
	real(DP), allocatable   :: pm(:,:)
	real(DP)                :: hessp(n*3, n*3)
	real(DP)                :: w1(n*3, n*3)
	real(DP)                :: w2(n*3, n*3)
	real(DP)                :: cm(n*3,6), xc(3, n)
	real(DP)                :: rnorm, dotval
	integer                 :: n3
	integer                 :: i, j, k, itemp, itype
	integer                 :: luout, ier
	! integer                 :: ix, iy, iz, l, mu, nu
	! real(DP)                :: test(6,6), test_norm, x1(3,n)
	!----------------------------------------------------------------------------
	real(DP), external      :: ddot
	!----------------------------------------------------------------------------
	!
	info = 0
	xc=x
	CALL com(n, mass, xc, ier)
	!----------------------------------------------------------------------------
	n3    = n*3
	luout = 6
	itype = 2
	!----------------------------------------------------------------------------
	allocate(pj(n3,n3,6))
	allocate(pm(n3,n3  ))
	allocate(Im(n3,n3  ))
	!----------------------------------------------------------------------------
	call set_matrix_unit(n3, Im)
	!----------------------------------------------------------------------------
	! i = 1
	! cm(1,1) =   sqrt(m(1))     ; cm(2,1) =   0.D0           ; cm(3,1) =   0.D0           ;
	! cm(1,2) =   0.D0           ; cm(2,2) =   sqrt(m(1))     ; cm(3,2) =   0.D0           ;
	! cm(1,3) =   0.D0           ; cm(2,3) =   0.D0           ; cm(3,3) =   sqrt(m(1))     ;
	! cm(1,4) =   0.D0           ; cm(2,4) =   sqrt(m(1)) * z ; cm(3,4) = - sqrt(m(1)) * y ;
	! cm(1,5) = - sqrt(m(1)) * z ; cm(2,5) =   0.D0           ; cm(3,5) =   sqrt(m(1)) * x ;
	! cm(1,6) =   sqrt(m(1)) * y ; cm(2,6) = - sqrt(m(1)) * x ; cm(3,6) =   0.D0           ;
	!
	! i = 2
	! cm(4,1) =   sqrt(m(2))     ; cm(5,1) =   0.D0           ; cm(6,1) =   0.D0           ;
	! cm(4,2) =   0.D0           ; cm(5,2) =   sqrt(m(2))     ; cm(6,2) =   0.D0           ;
	! cm(4,3) =   0.D0           ; cm(5,3) =   0.D0           ; cm(6,3) =   sqrt(m(2))     ;
	! cm(4,4) =   0.D0           ; cm(5,4) =   sqrt(m(2)) * z ; cm(6,4) = - sqrt(m(2)) * y ;
	! cm(4,5) = - sqrt(m(2)) * z ; cm(5,5) =   0.D0           ; cm(6,5) =   sqrt(m(2)) * x ;
	! cm(4,6) =   sqrt(m(2)) * y ; cm(5,6) = - sqrt(m(2)) * x ; cm(6,6) =   0.D0           ;
	!
	! i = 3
	! cm(7,1) =   sqrt(m(3))     ; cm(8,1) =   0.D0           ; cm(9,1) =   0.D0           ;
	! cm(7,2) =   0.D0           ; cm(8,2) =   sqrt(m(3))     ; cm(9,2) =   0.D0           ;
	! cm(7,3) =   0.D0           ; cm(8,3) =   0.D0           ; cm(9,3) =   sqrt(m(3))     ;
	! cm(7,4) =   0.D0           ; cm(8,4) =   sqrt(m(3)) * z ; cm(9,4) = - sqrt(m(3)) * y ;
	! cm(7,5) = - sqrt(m(3)) * z ; cm(8,5) =   0.D0           ; cm(9,5) =   sqrt(m(3)) * x ;
	! cm(7,6) =   sqrt(m(3)) * y ; cm(8,6) = - sqrt(m(3)) * x ; cm(9,6) =   0.D0           ;
	! ......
	!----------------------------------------------------------------------------
	call get_trans_rot_vec(n, mass, xc, cm, ier)
	!----------------------------------------------------------------------------
	! Normalization
	!
	do i=1, 6
		cm(:,i) = cm(:,i) / sqrt( dot_product( cm(:,i), cm(:,i) ) )
	end do
	!----------------------------------------------------------------------------
	!   Schmidt orthogonalize the constraint vectors
	!
	do i=2, 6     ! orthogonalize vector I to each of vectors J
		do j=1, i-1
			dotval=ddot(n3,cm(1,j),1,cm(1,i),1)
			do k=1, n3
				cm(k,i) = cm(k,i)-dotval*cm(k,j)
			end do
		end do
		rnorm = ddot(n3,cm(1,i),1,cm(1,i),1)
		if ( rnorm > 1.D-8) then
			rnorm = 1.D0 / sqrt(rnorm)
			call dscal(n3,rnorm,cm(1,i),1)
		else ! dependent constraint vector (linear molecule; set to zero)
			do k=1, n3
				cm(k,i) = 0.D0
			end do
			itemp = i-3
			!write(luout,*) 'Dependent rotation vector no.',ITEMP
			!write(luout,*) 'found in ECKART; assuming linear geometry'
		end if
	end do
	!----------------------------------------------------------------------------
	pm = 0.D0
	do i=1, 6
		call get_proj_matrix(n3, cm(1,i), pj(1,1,i))
		pm = pm + pj(:,:,i)
	end do
	pm = Im - pm
	if (itype==1) then
		hessp = matmul(pm, fcm)
		hessp = matmul(hessp, pm)
	else
		!-------------------------------------------------------------------------
		call dcopy(n3*n3,fcm,1,hessp,1)
		call dgemm('n','t',n3,n3, 6, 1d0,  cm,n3,  cm,n3,0d0,   w1,n3)
		call dgemm('n','n',n3,n3,n3, 1d0,  w1,n3, fcm,n3,0d0,   w2,n3)
		call dgemm('n','n',n3,n3,n3, 1d0,  w2,n3,  w1,n3,1d0,hessp,n3)
		call dgemm('n','n',n3,n3,n3,-1d0,  w1,n3, fcm,n3,1d0,hessp,n3)
		call dgemm('n','n',n3,n3,n3,-1d0, fcm,n3,  w1,n3,1d0,hessp,n3)
		!-------------------------------------------------------------------------
	end if
	!----------------------------------------------------------------------------
	fcm = hessp
	!----------------------------------------------------------------------------
	! construct Hessian in trans-rot subspace (should be zeroes)
	!
! 	do mu=1, 6
! 		do nu=1, 6
! 			test(mu, nu) = 0.D0
! 			do k=1, n3
! 				do l=1, n3
! 					test(mu,nu) = test(mu,nu) + cm(k,mu) * fcm(k,l) * cm(l,nu)
! 				end do
! 			end do
! 		end do
! 	end do
! 	test_norm = ddot((6*6),test,1,test,1)
! 	write(luout,'(a,1pd11.4)') ' Projected Nuclear Hessian trans-rot subspace norm:', test_norm
! 	write(luout,*) '                        (should be close to zero!) '
! 	write(luout,*) 'Hessian projected into trans-rot subspace (should be zeros):'
! 	write(luout,'(6es15.6)') test
	!----------------------------------------------------------------------------
	deallocate(pj)
	deallocate(pm)
	deallocate(Im)
	!----------------------------------------------------------------------------
	!----------------------------------------------------------------------------
	return
end subroutine
