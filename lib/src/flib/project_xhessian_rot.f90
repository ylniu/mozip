subroutine project_xhessian_rot(n, x, mass, hess, info)
	use kinds
	use math, only: v3_cross
	implicit none
	integer , intent(in   ) :: n
	integer , intent(  out) :: info
	real(DP), intent(in   ) :: x(3,n)
	real(DP), intent(inout) :: hess (n*3, n*3)
	real(DP)                :: hessp(n*3, n*3), mass(n), xc(3,n)
	real(DP)                :: hessd(n*3, n*3)
	real(DP)                :: w1(n*3, n*3)
	real(DP)                :: w2(n*3, n*3)
	real(DP)                :: vc(n*3,6), univec(3), test(6,6)
	real(DP)                :: vnorm, rnorm, dotval, test_norm
	integer                 :: n3
	integer                 :: i, j, k, l, iaxis, iatom, itemp
	integer                 :: luout, mu, nu, ier
	!----------------------------------------------------------------------------
	real(DP), external      :: ddot
	!----------------------------------------------------------------------------
	! construct translation unit vectors;  these are stored in the
	! first three columns of array vc, the rotation vectors will
	! be stored in the other 3 columns
	!
	info = 0
	xc = x
	call com(n, mass, xc, ier)
	!----------------------------------------------------------------------------
	n3 = n*3
	luout=6
	vc=0.D0
	vnorm = 1.D0 / sqrt( real(n) )
	do i=1, 3
		do iatom=1, n
			j=(iatom-1)*3+i
			vc(j,i) = vnorm
		end do
	end do
	!----------------------------------------------------------------------------
	! construct rotation unit vectors; store in last 3 columns of vc
	!
	do iaxis=1, 3
		do j=1, 3
			if (j==iaxis) then
				univec(j) = 1.D0
			else
				univec(j) = 0.D0
			end if
		end do
		do iatom=1, n
			j=(iatom-1)*3+1
			k=iaxis+3
			vc(j:j+2,k)=v3_cross(x(1,iatom), univec)
		end do
	end do
	!----------------------------------------------------------------------------
	!   Schmidt orthogonalize the constraint vectors
	!
	do i=2, 6     ! orthogonalize vector I to each of vectors J
		do j=1, i-1
			dotval=ddot(n3,vc(1,j),1,vc(1,i),1)
			do k=1, n3
				vc(k,i) = vc(k,i)-dotval*vc(k,j)
			end do
		end do
		rnorm = ddot(n3,vc(1,i),1,vc(1,i),1)
		if ( rnorm > 1.D-8) then
			rnorm = 1.D0 / sqrt(rnorm)
			call	dscal(n3,rnorm,vc(1,i),1)
		else ! dependent constraint vector (linear molecule; set to zero)
			do k=1, n3
				vc(k,i) = 0.D0
			end do
			itemp = i-3
			write(luout,*) 'Dependent rotation vector no.',ITEMP
			write(luout,*) 'found in ECKART; assuming linear geometry'
		end if
	end do
	!----------------------------------------------------------------------------
	call dcopy(n3*n3,hess,1,hessp,1)
	call dgemm('n','t',n3,n3, 6, 1d0,  vc,n3,  vc,n3,0d0,   w1,n3)
	call dgemm('n','n',n3,n3,n3, 1d0,  w1,n3,hess,n3,0d0,   w2,n3)
	call dgemm('n','n',n3,n3,n3, 1d0,  w2,n3,  w1,n3,1d0,hessp,n3)
	call dgemm('n','n',n3,n3,n3,-1d0,  w1,n3,hess,n3,1d0,hessp,n3)
	call dgemm('n','n',n3,n3,n3,-1d0,hess,n3,  w1,n3,1d0,hessp,n3)
	!----------------------------------------------------------------------------
	! place the projected Hessian in array HESS, and the difference
	! between projected and unprojected in HESSP
	!
	hessd = hessp - hess
	do i=1, n3
		do j=1, n3
			if (dabs(hessd(i,j)) < 1.D-9) hessd(i,j) = 0.D0
		end do
	end do
	!----------------------------------------------------------------------------
	hess = hessp
	!----------------------------------------------------------------------------
	! output projected Hessian
	!
	! write(luout,*) 'Hessian after projecting out external modes:'
	! write(luout,*) hess
	! write(luout,*) 'difference of Hessian with projected hessian:'
	! write(luout,*) hessp
	!----------------------------------------------------------------------------
	! construct Hessian in trans-rot subspace (should be zeroes)
	!
	do mu=1, 6
	do nu=1, 6
		test(mu, nu) = 0.D0
		do k=1, n3
		do l=1, n3
			test(mu,nu) = test(mu,nu) + vc(k,mu) * hess(k,l) * vc(l,nu)
		end do
		end do
	end do
	end do
	test_norm = ddot((6*6),test,1,test,1)
	! write(luout,'(a,1pd11.4)') ' Projected Nuclear Hessian trans-rot subspace norm:', test_norm
	! write(luout,*) '                        (should be close to zero!) '
	! write(luout,*) 'Hessian projected into trans-rot subspace (should be zeros):'
	! write(luout,'(6es15.6)') test
	!----------------------------------------------------------------------------
	return
end subroutine
