subroutine polyfit(nx, vx, vy, nc, coeff, ds)
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer , intent( in) :: nx, nc
	real(DP), intent( in) :: vx(nx)
	real(DP), intent( in) :: vy(nx)
	real(DP), intent(out) :: coeff(nc)
	real(DP), intent(out) :: ds
	!--------------------------------------------------------------------
	integer               :: im
	real(DP)              :: d(3), dm
	real(DP), allocatable :: Y  (  :)
	real(DP), allocatable :: Y0 (  :)
	real(DP), allocatable :: X  (:,:)
	real(DP), allocatable :: XT (:,:)
	real(DP), allocatable :: XTX(:,:)
	real(DP), allocatable :: XI (:,:)
	real(DP), allocatable :: E  (  :)
	real(DP), allocatable :: EI (:,:)
	real(DP), allocatable :: V  (:,:)
	real(DP), allocatable :: VT (:,:)
	real(DP), allocatable :: C1 (  :)
	real(DP), allocatable :: C2 (  :)
	real(DP), allocatable :: C3 (  :)
	real(DP), allocatable :: XR1(:,:)
	real(DP), allocatable :: XR2(:,:)
	real(DP), allocatable :: XR3(:,:)
	real(DP), allocatable :: Y1 (  :)
	real(DP), allocatable :: Y2 (  :)
	real(DP), allocatable :: Y3 (  :)
	real(DP), allocatable :: vy1(  :)
	real(DP), allocatable :: vy2(  :)
	real(DP), allocatable :: vy3(  :)
	real(DP), allocatable :: G  (:,:)
	!--------------------------------------------------------------------
	integer               :: i, j
	integer               :: info
	integer , allocatable :: ipiv(:)
	real(DP), allocatable :: work(:)
	!--------------------------------------------------------------------
	allocate(X   (nx, nc))
	allocate(XT  (nc, nx))
	allocate(vy1 (    nx))
	allocate(vy2 (    nx))
	allocate(vy3 (    nx))
	!--------------------------------------------------------------------
	allocate(ipiv(nc    ))
	allocate(work(nc    ))
	allocate(Y   (nc    ))
	allocate(Y0  (nc    ))
	allocate(E   (nc    ))
	allocate(V   (nc, nc))
	allocate(EI  (nc, nc))
	allocate(VT  (nc, nc))
	allocate(XTX (nc, nc))
	allocate(XI  (nc, nc))
	allocate(G   (nc, nc))
	!--------------------------------------------------------------------
	allocate(XR1 (nc, nc))
	allocate(XR2 (nc, nc))
	allocate(XR3 (nc, nc))
	allocate(Y1  (    nc))
	allocate(Y2  (    nc))
	allocate(Y3  (    nc))
	allocate(C1  (    nc))
	allocate(C2  (    nc))
	allocate(C3  (    nc))
	!--------------------------------------------------------------------
	! prepare the matrix
	!
	do i = 1, nx
		do j = 1, nc
			X(i,j) = vx(i)**(j-1)
		end do
	end do
	!--------------------------------------------------------------------
	XT  = transpose(X)
	XTX = matmul(XT, X)
	Y   = matmul(XT,vy)
	!--------------------------------------------------------------------
	! 1. Gauss-Jordan Elimination with FULL (simultaneous row & column) pivoting
	XR1 = XTX
	Y1  = Y
	call gaussj(XR1,nc,nc,Y1,1,1)
	C1  = Y1
	!--------------------------------------------------------------------
	! 2. Singular value decomposition
	!
	XR2 = XTX
	Y2  = Y
	call diag_symm (nc, XTX, E, V, info)
	!--------------------------------------------------------------------
	if ( info /= 0 ) then
		write(*,*)  "problem in diag_symm"
		return
	end if
	!--------------------------------------------------------------------
	VT  = transpose(V)
	EI  = 0.0_DP
	do i=1, nc
		EI(i,i) = 1.0_DP / E(i)
	end do
	XR2 = matmul(V, matmul(EI,VT))
	C2  = matmul(XR2, Y2)
	!--------------------------------------------------------------------
	! 3. Inverse of matrix
	!
	XR3 = XTX
	Y3  = Y
	call DGETRF(nc, nc, XR3, nc, ipiv, info)
	!--------------------------------------------------------------------
	if ( info /= 0 ) then
		write(*,*)  "problem in DGETRF"
		return
	end if
	!--------------------------------------------------------------------
	call DGETRI(nc, XR3, nc, ipiv, work, nc, info)
	!--------------------------------------------------------------------
	if ( info /= 0 ) then
	   write(*,*) "problem in DGETRI"
	   return
	end if
	!--------------------------------------------------------------------
	C3 = matmul( XR3, Y3)
	!--------------------------------------------------------------------
	vy1 = 0.0_DP
	vy2 = 0.0_DP
	vy3 = 0.0_DP
	!--------------------------------------------------------------------
	d = 0.0_DP
	do i=1, nx
		do j=1, nc
			vy1(i) = vy1(i) + C1(j) * vx(i)**(j-1)
			vy2(i) = vy2(i) + C2(j) * vx(i)**(j-1)
			vy3(i) = vy3(i) + C3(j) * vx(i)**(j-1)
		end do
		d(1) = d(1) + (vy(i) - vy1(i))**2
		d(2) = d(2) + (vy(i) - vy2(i))**2
		d(3) = d(3) + (vy(i) - vy3(i))**2
	end do
	!--------------------------------------------------------------------
	d = sqrt(d) / nx
	!--------------------------------------------------------------------
	dm = 1.D200
	do i=1, 3
		if (dm > d(i)) then
			dm = d(i)
			im = i
		end if
	end do
	!--------------------------------------------------------------------
	select case(im)
		case(1)
			coeff = C1
			ds    = d(1)
		case(2)
			coeff = C2
			ds    = d(2)
		case(3)
			coeff = C3
			ds    = d(3)
	end select
	!--------------------------------------------------------------------
	open(55, file="fitting.3")
	write(55,'("method", 3i15)') 1,2,3
	write(55,'(a6, 3f15.7)') "d", d
	do i=1, nc
		write(55,'(i6, 3f15.7)') i-1, C1(i), C2(i), C3(i)
	end do
	close(55)
	!--------------------------------------------------------------------
	deallocate(vy1 )
	deallocate(vy2 )
	deallocate(vy3 )
	!--------------------------------------------------------------------
	deallocate(ipiv)
	deallocate(work)
	deallocate(X   )
	deallocate(Y   )
	deallocate(E   )
	deallocate(V   )
	deallocate(VT  )
	deallocate(EI  )
	deallocate(XT  )
	deallocate(XI  )
	deallocate(XTX )
	deallocate(G   )
	!--------------------------------------------------------------------
	deallocate(XR1 )
	deallocate(XR2 )
	deallocate(XR3 )
	deallocate(Y1  )
	deallocate(Y2  )
	deallocate(Y3  )
	deallocate(C1  )
	deallocate(C2  )
	deallocate(C3  )
	!--------------------------------------------------------------------
end subroutine
