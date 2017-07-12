module math
	use kinds, only: DP
	implicit none
	interface sort_array
		module procedure sort_array_a
		module procedure sort_array_a3
		module procedure sort_array_i
		module procedure sort_array_d
		module procedure sort_array_d3
	end interface
	!--------------------------------------------------------------------
	public dot_product2
	!--------------------------------------------------------------------
   contains
	!
	!--------------------------------------------------------------------
	!
	function dot_product2(a,b)
		implicit none
		real(DP), intent( in) :: a(:,:)
		real(DP), intent( in) :: b(:,:)
		real(DP)              :: dot_product2
		!-----------------------------------------------------------------
		integer               :: n1, n2, m1, m2
		integer               :: i, j
		!-----------------------------------------------------------------
		n1=size(a,dim=1)
		n2=size(a,dim=2)
		m1=size(b,dim=1)
		m2=size(b,dim=2)
		!-----------------------------------------------------------------
		if (n1/=m1 .or. n2/=m2) then
			write(*,'("Error! Dimensions are not the same for dot_product2!")')
			write(*,'("Stop")')
			call exit(1)
		end if
		!-----------------------------------------------------------------
		dot_product2 = 0.0_DP
		!-----------------------------------------------------------------
		do i=1, n1
			do j=1, n2
				dot_product2 = dot_product2 + a(i,j) * b(i,j)
			end do
		end do
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	subroutine v3_disp1(n,v,id,a)
		implicit none
		!-----------------------------------------------------------------
		integer , intent( in) :: n, id
		real(DP), intent(out) :: v(3,n)
		real(DP), intent( in) :: a
		integer               :: i
		!-----------------------------------------------------------------
		do i=1, n
			v(id,i) = v(id,i) + a
		end do
		!-----------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine v3_disp3(n,v,a)
		implicit none
		!-----------------------------------------------------------------
		integer , intent( in) :: n
		real(DP), intent(out) :: v(3,n)
		real(DP), intent( in) :: a(3)
		integer               :: i
		!-----------------------------------------------------------------
		do i=1, n
			v(:,i) = v(:,i) + a
		end do
		!-----------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	function v3_min(n,v,id)
		implicit none
		!-----------------------------------------------------------------
		integer , intent(in)  :: n, id
		real(DP), intent(in)  :: v(3,n)
		real(DP)              :: v3_min
		integer               :: i
		!-----------------------------------------------------------------
		v3_min = 1.D99
		do i=1, n
			if (v3_min > v(id,i)) v3_min = v(id,i)
		end do
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3_max(n,v,id)
		implicit none
		!-----------------------------------------------------------------
		integer , intent(in)  :: n, id
		real(DP), intent(in)  :: v(3,n)
		real(DP)              :: v3_max
		integer               :: i
		!-----------------------------------------------------------------
		v3_max = -1.D99
		do i=1, n
			if (v3_max < v(id,i)) v3_max = v(id,i)
		end do
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function same_direction(n,a,b)
		implicit none
		!-----------------------------------------------------------------
		integer , intent(in)  :: n
		real(DP), intent(in)  :: a(n)
		real(DP), intent(in)  :: b(n)
		logical               :: same_direction
		real(DP), allocatable :: a1(:)
		real(DP), allocatable :: b1(:)
		real(DP), allocatable :: c1(:)
		real(DP), allocatable :: c2(:)
		real(DP)              :: r1, r2
		real(DP)              :: eps
		!-----------------------------------------------------------------
		allocate(a1(n))
		allocate(b1(n))
		allocate(c1(n))
		allocate(c2(n))
		!-----------------------------------------------------------------
		eps=1.D-6
		!-----------------------------------------------------------------
		a1=vn_normalize(n,a)
		b1=vn_normalize(n,b)
		!-----------------------------------------------------------------
		c1=a1-b1
		c2=a1+b1
		!-----------------------------------------------------------------
		r1=vn_norm(n,c1)
		r2=vn_norm(n,c2)
		!-----------------------------------------------------------------
		if (abs(r1)<eps .or. abs(r2) < eps) then
			same_direction=.true.
		else
			same_direction=.false.
		end if
		!-----------------------------------------------------------------
		deallocate(a1)
		deallocate(b1)
		deallocate(c1)
		deallocate(c2)
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function det3(a)
		implicit none
		!-----------------------------------------------------------------
		real(DP), intent(in) :: a(3,3)
		real(DP)             :: det3
		!-----------------------------------------------------------------
		det3 = 0.D0
		det3 =  a(1,1) * a(2,2) * a(3,3) &
				+ a(1,2) * a(2,3) * a(3,1) &
				+ a(1,3) * a(2,1) * a(3,2) &
				- a(1,3) * a(2,2) * a(3,1) &
				- a(1,1) * a(2,3) * a(3,2) &
				- a(1,2) * a(2,1) * a(3,3)
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function factorial(n)
		implicit none
		!----------------------------------------------------------------------
		integer   , intent( in) :: n
		integer(8)              :: factorial
		!----------------------------------------------------------------------
		integer                 :: i
		!----------------------------------------------------------------------
		if (n<0 .or. n>10) then
			factorial = -1
			return
		else if (n==0) then
			factorial = 1
			return
		else
			factorial = 1
			do i=1, n
				factorial = factorial * i
			end do
		end if
		!----------------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_array_a(n,a,indx)
		!----------------------------------------------------------------------
		implicit none
		!----------------------------------------------------------------------
		integer     , intent(in   ) :: n
		integer     , intent(in   ) :: indx(n)
		character(*), intent(inout) :: a(n)
		integer                     :: i
		character(len=len(a(1))), allocatable :: b(:)
		!----------------------------------------------------------------------
		allocate(b(n))
		do i=1, n
			b(i) = a(indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_array_a3(n,a,indx)
		!----------------------------------------------------------------------
		implicit none
		!----------------------------------------------------------------------
		integer                   , intent(in   ) :: n
		integer                   , intent(in   ) :: indx(n)
		character(*)              , intent(inout) :: a(3,n)
		integer                                   :: i
		character(len=len(a(1,1))), allocatable   :: b(:,:)
		!----------------------------------------------------------------------
		allocate(b(3,n))
		do i=1, n
			b(:,i) = a(:,indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_array_i(n,a,indx)
		!----------------------------------------------------------------------
		implicit none
		!----------------------------------------------------------------------
		integer , intent(in   ) :: n
		integer , intent(in   ) :: indx(n)
		integer , intent(inout) :: a(n)
		integer                 :: i
		integer , allocatable   :: b(:)
		!----------------------------------------------------------------------
		allocate(b(n))
		do i=1, n
			b(i) = a(indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_array_d(n,a,indx)
		!----------------------------------------------------------------------
		implicit none
		!----------------------------------------------------------------------
		integer , intent(in   ) :: n
		integer , intent(in   ) :: indx(n)
		real(DP), intent(inout) :: a(n)
		integer                 :: i
		real(DP), allocatable   :: b(:)
		!----------------------------------------------------------------------
		allocate(b(n))
		do i=1, n
			b(i) = a(indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_array_d3(n,a,indx)
		!----------------------------------------------------------------------
		implicit none
		!----------------------------------------------------------------------
		integer , intent(in   ) :: n
		integer , intent(in   ) :: indx(n)
		real(DP), intent(inout) :: a(3,n)
		integer                 :: i
		real(DP), allocatable   :: b(:,:)
		!----------------------------------------------------------------------
		allocate(b(3,n))
		do i=1, n
			b(:,i) = a(:,indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine sort_matrix(n,a,indx)
		!----------------------------------------------------------------------
		! Example:
		! input  :
		! a      :  2.2  3.2  1.8  5.4
		! indx   :  3    1    4    2
		! output :
		! b      :  3.2  5.4  2.2  1.8
		!
		implicit none
		!----------------------------------------------------------------------
		integer , intent(in   ) :: n
		integer , intent(in   ) :: indx(n)
		real(DP), intent(inout) :: a(n,n)
		real(DP), allocatable   :: b(:,:)
		integer                 :: i
		!----------------------------------------------------------------------
		allocate(b(n,n))
		do i=1, n
			b(:,i) = a(:,indx(i))
		end do
		a=b
		deallocate(b)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	function vn_norm_part(n, v, a, b)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: n, a, b
		real(DP), intent( in) :: v(n)
		integer               :: i
		real(DP)              :: vn_norm_part
		!----------------------------------------------------------------------
		vn_norm_part = 0.D0
		do i=a, b
			vn_norm_part = vn_norm_part + v(i) * v(i)
		end do
		vn_norm_part = sqrt(vn_norm_part)
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function vn_norm(n,v)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: n
		real(DP), intent( in) :: v(n)
		integer               :: i
		real(DP)              :: vn_norm
		!----------------------------------------------------------------------
		vn_norm = 0.D0
		do i=1, n
			vn_norm = vn_norm + v(i) * v(i)
		end do
		vn_norm = sqrt(vn_norm)
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3n_norm(n,v)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: n
		real(DP), intent( in) :: v(3,n)
		integer               :: i
		real(DP)              :: v1(3)
		real(DP)              :: v3n_norm
		!----------------------------------------------------------------------
		v3n_norm = 0.D0
		do i=1, n
			v1 = v(:,i)
			v3n_norm = v3n_norm + dot_product(v1,v1)
		end do
		v3n_norm = sqrt(v3n_norm)
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3n_norm_aver(n,v)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: n
		real(DP), intent( in) :: v(3,n)
		integer               :: i
		real(DP)              :: v1(3)
		real(DP)              :: v3n_norm_aver
		!----------------------------------------------------------------------
		v3n_norm_aver = 0.D0
		do i=1, n
			v1 = v(:,i)
			v3n_norm_aver = v3n_norm_aver + dot_product(v1,v1)
		end do
		v3n_norm_aver = sqrt(v3n_norm_aver) / n
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3_norm(v)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: v(3)
		real(DP)              :: v3_norm
		!----------------------------------------------------------------------
		v3_norm=sqrt(dot_product(v,v))
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function vn_normalize(n,v)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: n
		real(DP), intent( in) :: v(n)
		real(DP)              :: vn_normalize(n)
		!----------------------------------------------------------------------
		vn_normalize=v/sqrt(dot_product(v,v))
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3_normlize(v)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: v(3)
		real(DP)              :: v3_normlize(3)
		!----------------------------------------------------------------------
		v3_normlize=v/sqrt(dot_product(v,v))
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function v3_cross(v1,v2)
		implicit none
		real(DP), intent(in) :: v1(3), v2(3)
		real(DP)             :: v3_cross(3)
		!-----------------------------------------------------------------
		v3_cross(1) = v1(2)*v2(3)-v1(3)*v2(2)
		v3_cross(2) = v1(3)*v2(1)-v1(1)*v2(3)
		v3_cross(3) = v1(1)*v2(2)-v1(2)*v2(1)
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function inverse3(a)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: a(3,3)
		real(DP)              :: inverse3(3,3)
		real(DP), external    :: determinant3
		!-----------------------------------------------------------------
		real(DP)              :: a1(3,3)
		!-----------------------------------------------------------------
		a1(1,1) = a(2,2) * a(3,3) - a(2,3) * a(3,2)
		a1(2,1) = a(2,3) * a(3,1) - a(2,1) * a(3,3)
		a1(3,1) = a(2,1) * a(3,2) - a(2,2) * a(3,1)
		a1(1,2) = a(3,2) * a(1,3) - a(3,3) * a(1,2)
		a1(2,2) = a(3,3) * a(1,1) - a(3,1) * a(1,3)
		a1(3,2) = a(3,1) * a(1,2) - a(3,2) * a(1,1)
		a1(1,3) = a(1,2) * a(2,3) - a(1,3) * a(2,2)
		a1(2,3) = a(1,3) * a(2,1) - a(1,1) * a(2,3)
		a1(3,3) = a(1,1) * a(2,2) - a(1,2) * a(2,1)
		!----------------------------------------------------------------------
		a1 = a1 / determinant3(a)
		inverse3 = a1
		!----------------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function rotmat(q,ixyz)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: ixyz
		real(DP), intent( in) :: q
		real(DP)              :: rotmat(3,3)
		!----------------------------------------------------------------------
		real(DP)              :: c, s, PI
		!----------------------------------------------------------------------
		PI=acos(-1.D0)
		c=cos(q * PI / 180.D0)
		s=sin(q * PI / 180.D0)
		!----------------------------------------------------------------------
		if      (ixyz==1) then
			rotmat(1,1) = 1.D0; rotmat(1,2) = 0.D0; rotmat(1,3) = 0.D0;
			rotmat(2,1) = 0.D0; rotmat(2,2) =    c; rotmat(2,3) =   -s;
			rotmat(3,1) = 0.D0; rotmat(3,2) =    s; rotmat(3,3) =    c;
		else if (ixyz==2) then
		!------------------------------------------------------------------------
			rotmat(1,1) =    c; rotmat(1,2) = 0.D0; rotmat(1,3) =    s;
			rotmat(2,1) = 0.D0; rotmat(2,2) = 1.D0; rotmat(2,3) = 0.D0;
			rotmat(3,1) =   -s; rotmat(3,2) = 0.D0; rotmat(3,3) =    c;
		else if (ixyz==3) then
			rotmat(1,1) =    c; rotmat(1,2) =    -s; rotmat(1,3) = 0.D0;
			rotmat(2,1) =    s; rotmat(2,2) =     c; rotmat(2,3) = 0.D0;
			rotmat(3,1) = 0.D0; rotmat(3,2) =  0.D0; rotmat(3,3) = 1.D0;
		end if
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	subroutine rotvec(r, q, ixyz)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: ixyz
		real(DP), intent( in) :: q
		real(DP), intent(out) :: r(3)
		real(DP)              :: matrix(3,3)
		!----------------------------------------------------------------------
		matrix=rotmat(q,ixyz)
		call rotn(1, matrix, r)
		!----------------------------------------------------------------------
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	function rotmat3(q3,seq)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: seq(3)
		real(DP), intent( in) :: q3(3)
		real(DP)              :: rotmat3(3,3)
		!----------------------------------------------------------------------
		integer               :: i
		real(DP)              :: c(3,3,3)
		!----------------------------------------------------------------------
		do i=1, 3
			c(1:3,1:3,i) = rotmat(q3(i),i)
		end do
		rotmat3=matmul(c(1:3,1:3,seq(1)),matmul(c(1:3,1:3,seq(2)),c(1:3,1:3,seq(3))))
		!----------------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function rotmat3_seq(q3,seq)
		implicit none
		!----------------------------------------------------------------------
		integer , intent( in) :: seq(3)
		real(DP), intent( in) :: q3(3)
		real(DP)              :: rotmat3_seq(3,3)
		!----------------------------------------------------------------------
		integer               :: i
		real(DP)              :: c(3,3,3)
		!----------------------------------------------------------------------
		do i=1, 3
			c(1:3,1:3,i) = rotmat(q3(i),seq(i))
		end do
		rotmat3_seq=matmul(c(1:3,1:3,1),matmul(c(1:3,1:3,2),c(1:3,1:3,3)))
		!-----------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function volume_v3(a)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: a(3,3)
		real(DP)              :: volume_v3
		!----------------------------------------------------------------------
		real(DP)              :: b(3)
		!----------------------------------------------------------------------
		b=v3_cross(a(1,2),a(1,3))
		volume_v3 = abs(dot_product(a(1:3,1),b))
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function area_v2(v1,v2)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: v1(3)
		real(DP), intent( in) :: v2(3)
		real(DP)              :: area_v2
		!----------------------------------------------------------------------
		real(DP)              :: b(3)
		!----------------------------------------------------------------------
		b=v3_cross(v1,v2)
		area_v2 = dot_product(b,b)
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function dis_two_point(a, b)
	implicit none
	!--------------------------------------------------------------------
	real(DP), intent(in) :: a(3)
	real(DP), intent(in) :: b(3)
	real(DP)             :: dis_two_point
	!--------------------------------------------------------------------
	real(DP)             :: c(3)
	!--------------------------------------------------------------------
	c = b - a
	dis_two_point = sqrt(dot_product(c,c))
	!--------------------------------------------------------------------
	end function
	!
	!--------------------------------------------------------------------
	!
	function v2_cos(v1,v2)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: v1(3)
		real(DP), intent( in) :: v2(3)
		real(DP)              :: v2_cos
		!----------------------------------------------------------------------
		real(DP)              :: r1, r2
		!----------------------------------------------------------------------
		r1=sqrt(dot_product(v1,v1))
		r2=sqrt(dot_product(v2,v2))
		!
		v2_cos = ( v1(1) * v2(1) + v1(2) * v2(2) + v1(3) * v2(3) ) / r1 / r2
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	function angle2(x,y)
		implicit none
		!----------------------------------------------------------------------
		real(DP), intent( in) :: x
		real(DP), intent( in) :: y
		real(DP)              :: angle2
		!----------------------------------------------------------------------
		real(DP)              :: x1, y1, r, eps, PI
		!----------------------------------------------------------------------
		eps = 1.D-6
		PI  = acos(-1.D0)
		!----------------------------------------------------------------------
		! 0 ~ 2pi
		!----------------------------------------------------------------------
		r = sqrt(x**2 + y**2)
		if (r<eps) then
			angle2 = 0.D0
			return
		end if
		!----------------------------------------------------------------------
		x1 = x / r
		y1 = y / r
		!----------------------------------------------------------------------
		if (abs(x1-1.D0)<eps .and. abs(y1-0.D0)<eps) then
			angle2 = 0.D0
			return
		end if
		!----------------------------------------------------------------------
		if (y1>=0) then
			angle2 = acos(x1)
		else
			angle2 = PI * 2.0_DP - acos(x1)
		end if
		!----------------------------------------------------------------------
		return
	end function
	!
	!--------------------------------------------------------------------
	!
	recursive subroutine reduction(m,n)
		implicit none
		!----------------------------------------------------------------------
		integer, intent(inout) :: n
		integer, intent(inout) :: m
		!----------------------------------------------------------------------
		integer                :: n1
		integer                :: i
		!----------------------------------------------------------------------
		if (n==m) then
			n=1
			m=1
			return
		else if (n<m) then
			n1    = n
		else
			n1    = m
		end if
		!----------------------------------------------------------------------
		do i=2, n1
			if (mod(n,i) == 0 .and. mod(m,i) == 0) then
				n = n / i
				m = m / i
				call reduction(m,n)
			end if
		end do
		!
		return
	end subroutine
	!
	!--------------------------------------------------------------------
	!
	subroutine diag_dnosy(n,job,t,e,vr,vl,info)
		implicit none
		!----------------------------------------------------------------------
		integer      , intent( in) :: n
		character(1 ), intent( in) :: job
		real     (DP), intent( in) :: t  (n,n)
		complex  (DP), intent(out) :: e  (  n)
		complex  (DP), intent(out) :: vr (n,n)
		complex  (DP), intent(out) :: vl (n,n)
		!----------------------------------------------------------------------
		integer                    :: m
		integer                    :: ilo
		integer                    :: ihi
		integer                    :: info
		integer                    :: lwork
		!----------------------------------------------------------------------
		logical      , allocatable :: select (  :)
		real     (DP), allocatable :: scale  (  :)
		complex  (DP), allocatable :: tau    (  :)
		complex  (DP), allocatable :: work   (  :)
		real     (DP), allocatable :: workr  (  :)
		complex  (DP), allocatable :: a      (:,:)
		complex  (DP), allocatable :: h      (:,:)
		!----------------------------------------------------------------------
		lwork  = n * 16
		allocate(tau    (    n))
		allocate(scale  (    n))
		allocate(select (    n))
		allocate(a      (n,  n))
		allocate(h      (n,  n))
		allocate(workr  (lwork))
		allocate(work   (lwork))
		!----------------------------------------------------------------------
		a   = t
		!----------------------------------------------------------------------
		! Balances A
		!
		call zgebal ("Both", n, a, n, ilo, ihi, scale, info )
		!----------------------------------------------------------------------
		! Reduce A to upper Hessenberg form H = (Q**T)*A*Q
		!
		call zgehrd ( n, ilo, ihi, a, n, tau, work, lwork, info )
		!
		!----------------------------------------------------------------------
		! Copy A to H and VR
		!
		h  = a
		vr = a
		!----------------------------------------------------------------------
		! Form Q explicitly, storing the result in VR
		!
		call zunghr ( n, 1, n, vr, n, tau, work, lwork, info )
		!----------------------------------------------------------------------
		! Calculate the eigenvalues and Schur factorization of A
		!
		call zhseqr ("Schur form","Vectors",n,ilo,ihi,h,n,e,vr,n,work,lwork,info)
		!----------------------------------------------------------------------
		! Calculate the eigenvectors of A, storing the result in VR
		!
		select = .true.
		call ztrevc (job, "Backtransform", select, n, h, n, vl, n, vr, n, n, m, work, workr, info )
		!----------------------------------------------------------------------
		! Transforms eigenvectors of a balanced matrix to those of the original
		! nonsymmetric matrix.
		!
		call zgebak ("Both","Right",n,ilo,ihi,scale,m,vr,n,info )
		!----------------------------------------------------------------------
		deallocate(h      )
		deallocate(tau    )
		deallocate(work   )
		deallocate(workr  )
		deallocate(scale  )
		deallocate(select )
		!----------------------------------------------------------------------
	end subroutine diag_dnosy
end module
