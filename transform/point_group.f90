program point_group
	use kinds, only: DP
	implicit none
	integer                    :: n, m, ng
	integer                    :: i, j, k, info, norder
	real(DP)                   :: PI
	real(DP)                   :: a, ca, sa
	real(DP)                   :: b, cb, sb
	real(DP)     , allocatable :: matrix(:,:,:), v1(:,:), theta(:), da(:,:)
	logical      , allocatable :: ifcal(:,:)
	integer      , allocatable :: product_table(:,:)
	character(80), allocatable :: rot_name(:)
	character(80)              :: fmt, chi3
	!----------------------------------------------------------------------------
	norder=3
	n      = 1000
	ng     = 0
	PI     = acos(-1.D0)
	!----------------------------------------------------------------------------
	allocate(matrix(3,3,n))
	allocate(ifcal(n,n))
	allocate(rot_name(n))
	allocate(v1(3,n))
	allocate(da(3,n))
	allocate(theta(n))
	!----------------------------------------------------------------------------
	matrix = 0.D0
	ifcal=.true.
	!----------------------------------------------------------------------------
	a  = 120.D0
	a  = a * PI / 180.D0
	ca = cos(a)
	sa = sin(a)
	!----------------------------------------------------------------------------
	b  = 180.D0
	b  = b * PI / 180.D0
	cb = cos(b)
	sb = sin(b)
	!----------------------------------------------------------------------------
	! C3(z)
	!
	! 	ng = ng + 1
	! 	matrix(1,1,ng) =  ca
	! 	matrix(1,2,ng) = -sa
	! 	matrix(2,1,ng) =  sa
	! 	matrix(2,2,ng) =  ca
	! 	matrix(3,3,ng) =  1.D0
	!----------------------------------------------------------------------------
	!  C2(x)
	!
	! 	ng = ng + 1
	! 	matrix(1,1,ng) =  1.D0
	! 	matrix(2,2,ng) =  cb
	! 	matrix(2,3,ng) = -sb
	! 	matrix(3,2,ng) =  sb
	! 	matrix(3,3,ng) =  cb
	!----------------------------------------------------------------------------
	!  C2(y)
	!
	ng = ng + 1
	matrix(2,2,ng) =  1.D0
	matrix(3,3,ng) =  cb
	matrix(3,1,ng) = -sb
	matrix(1,3,ng) =  sb
	matrix(1,1,ng) =  cb
	!----------------------------------------------------------------------------
	! sigma(x)
	!
	ng = ng + 1
	matrix(1,1,ng) = -1.D0
	matrix(2,2,ng) =  1.D0
	matrix(3,3,ng) =  1.D0
	!----------------------------------------------------------------------------
	! Sigma_h
	!
	! 	ng = ng + 1
	! 	matrix(1,1,ng) =  1.D0
	! 	matrix(2,2,ng) =  1.D0
	! 	matrix(3,3,ng) = -1.D0
	!----------------------------------------------------------------------------
	m = ng
	!----------------------------------------------------------------------------
	do i=1, ng
		call find_rotation_axis(matrix(1,1,i), da(1,i), v1(1,i), theta(i), rot_name(i))
	end do
	!----------------------------------------------------------------------------
	call generate_matrix(n,m,matrix,rot_name, v1, da, theta,ifcal)
	!----------------------------------------------------------------------------
	write(fmt,'(i0)') m
	j=len(trim(fmt))
	write(fmt,'(i0)') j
	do i=1, m
		write(rot_name(i), '(a,"(",i'//trim(fmt)//'.'//trim(fmt)//',")")') trim(rot_name(i)), i
	end do
	!----------------------------------------------------------------------------
	allocate(product_table(m,m))
	!----------------------------------------------------------------------------
	do i=1, m
		write(*,'("---------------------------------------------------------")')
		write(*, '("matrix", i12, a12, f12.6)') i, trim(rot_name(i)), theta(i)
		write(*, '("vector", 3f12.6  )') v1(:,i)
		write(*, '("angle ", 3f12.6  )') da(:,i)
		write(*,*)
		do j=1, 3
			write(*, '(6x, 3f12.6)') (matrix(j, k, i), k=1, 3)
		end do
	end do
	!----------------------------------------------------------------------------
	!
	call get_product_table(n,m,matrix  , product_table)
	call prn_product_table(n,m,rot_name, product_table)
	!----------------------------------------------------------------------------
	call get_dep_tensor(ng, matrix, norder, chi3)
	!----------------------------------------------------------------------------
	deallocate(matrix       )
	deallocate(ifcal        )
	deallocate(rot_name     )
	deallocate(v1           )
	deallocate(da           )
	deallocate(theta        )
	deallocate(product_table)
	!----------------------------------------------------------------------------
	stop
end
!
!----------------------------------------------------------------------------
!
subroutine get_dep_tensor(ng, matred, ndr, chi3)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent( in)  :: ndr
	integer     , intent( in)  :: ng
	real(DP)    , intent( in)  :: matred(3,3,ng)
	character(*), intent(out)  :: chi3(3,3,3)
	!----------------------------------------------------------------------------
	integer                    :: i, j, k, l, ir, ic, n, ig, m, nrow1, nrow2, ncol, nrow
	integer                    :: iout, jj, ni0, ni1, ni2
	integer                    :: idx_col_n
	integer                    :: ix, iy, iz, imax
	integer                    :: jx, jy, jz, info, nrank
	integer      , allocatable :: idx(:,:), idd(:), id1(:)
	integer      , allocatable :: c0(:), cd0(  :), d0(:)
	integer      , allocatable :: c1(:), cd1(:,:), d1(:)
	integer      , allocatable :: c2(:), cd2(  :), d2(:)
	character(20)              :: xname(3), tmp
	character(20), allocatable :: xnames(:)
	character(20), allocatable :: xnames1(:)
	real(DP)                   :: eps, amax, dr, dc
	real(DP)     , allocatable :: g(:,:,:)
	real(DP)     , allocatable :: c(:,:)
	character(80)              :: fmt
	real(DP)     , external    :: matrix_row_norm
	real(DP)     , external    :: matrix_col_norm
	integer      , external    :: number_of_nonzero_row
	!----------------------------------------------------------------------------
	n    = 3**ndr
	m    = n*ng
	eps  = 1.D-6
	iout = 6
	!----------------------------------------------------------------------------
	allocate(g(n,n,ng))
	allocate(c(m,n))
	allocate(xnames(n))
	allocate(xnames1(n))
	allocate(idx(ndr,n))
	allocate(idd(n))
	allocate(id1(n))
	allocate(c0 (  n))
	allocate(d0 (  n))
	allocate(cd0(  n))
	allocate(c1 (  n))
	allocate(d1 (  n))
	allocate(cd1(n,n))
	allocate(c2 (  n))
	allocate(d2 (  n))
	allocate(cd2(  n))
	!----------------------------------------------------------------------------
	xname(1)="x"
	xname(2)="y"
	xname(3)="z"
	!----------------------------------------------------------------------------
	call get_n_product_index(3, ndr, idx, 1, info)
	!----------------------------------------------------------------------------
	write(fmt,'(i0)') ndr
	xnames=""
	do l=1, n
		do i=1, ndr
			j= idx(i,l)
			xnames(l)=trim(xnames(l))//trim(xname(j))
		end do
	end do
	!----------------------------------------------------------------------------
	g=1.D0
	do ig=1, ng
		do i=1, n
			do j=1, n
				do jj=1, ndr
					ix=idx(jj,i)
					jx=idx(jj,j)
					g(i,j,ig) = g(i,j,ig) * matred(ix,jx,ig)
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	do ig=1, ng
		do i=1, n
			g(i,i,ig) = g(i,i,ig) - 1.D0
		end do
	end do
	!----------------------------------------------------------------------------
	do ig=1, ng
		write(tmp,'("Operation ",i0)') ig
		call print_matrix(iout, 3, 3, matred(1,1,ig), tmp, 3, "f11.6", info)
	end do
	!----------------------------------------------------------------------------
	!do ig=1, ng
		!write(tmp,'("g",i0)') ig
		!call print_matrix_head(iout, n, n, g(1,1,ig), xnames, tmp, 10, "f11.6", info)
	!end do
	!----------------------------------------------------------------------------
	! Copy Operation matrix to c
	!
	l=0
	c=0.D0 
	do ig=1, ng
		do i=1, n
			l=l+1
			do j=1, n
				c(l,j) = g(i,j,ig)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	nrow = m
	ncol = n
	do i=1, n
		idd(i) = i
	end do
	!----------------------------------------------------------------------------
	do i=1, m
		call matrix_del_zero_row(m,n,c,nrow)
		if (i>nrow) exit
		call matrix_del_zero_col(m,n,c,ncol,idd)
		call matrix_move_max(m,n,c,i,nrow,ncol,idd)
		call matrix_minus_row_dn(m,n,c,i,nrow)
	end do
	!----------------------------------------------------------------------------
	do l=1, n
		j= idd(l)
		id1(l) = j
		xnames1(l)=trim(xnames(j))
	end do
	!----------------------------------------------------------------------------
	!call print_matrix_head_del_zero(iout, m, n, c, xnames1, nrow, ncol, "c", 10, "f11.6", info)
	!----------------------------------------------------------------------------
	!do l=1, n
	!	write(iout,'(2(4x, i4, a4))') l, trim(xnames(l)), idd(l), trim(xnames1(l))
	!end do
	!----------------------------------------------------------------------------
	c1  = -1
	ni0 = 0
	ni1 = 0
	d1  = -1
	do i=1, nrow
		j=number_of_nonzero_row(m,n,c,i)
		if (j == 1) then
			ni0     = ni0 + 1
			c0(ni0) = i
			d0(ni0) = j
			call get_nonzero_row1(m,n,c,i,cd0(ni0))
		else if (j >= 2) then
			ni1     = ni1 + 1
			c1(ni1) = i
			d1(ni1) = j
			call get_nonzero_row(m,n,c,i,cd1(1,ni1))
		end if
	end do
	!----------------------------------------------------------------------------
	write(iout,*)
	write(iout,'(2x,"Zero componants:")')
	write(iout,*)
	write(iout, '(3x, "      0 = ",$)')
	do i=1, ni0
		j=id1(cd0(i))
		write(iout,'(2x,a'//trim(fmt)//',$)') xnames(j)
	end do
	write(iout,*)
	write(iout,*)
	!----------------------------------------------------------------------------
	write(iout,'(2x,"Nonzero componants (equations) :")')
	write(iout,*)
	write(fmt,'(i0)') ndr
	do k=1, ni1
		i=c1(k)
		write(iout,'(i4, " :   0 = ", $)') k
		do l=1, d1(k)
			j=id1(cd1(l,k))
			jj=cd1(l,k)
			if      ( abs(c(i,jj) - 1.D0) < eps ) then
				write(iout,'(a4  , "   ", a'//trim(fmt)//' $)') "+"    , trim(xnames(j))
			else if ( abs(c(i,jj) + 1.D0) < eps ) then
				write(iout,'(a4  , "   ", a'//trim(fmt)//' $)') "-"    , trim(xnames(j))
			else
				write(iout,'(f4.1, " * ", a'//trim(fmt)//' $)') c(i,jj), trim(xnames(j))
			end if
		end do
		write(iout,*)
	end do
	write(iout,*)
	!----------------------------------------------------------------------------
	if (ncol<n) then
		write(iout,'(2x,"Nonzero componants (single) :")')
		write(iout,*)
		write(iout, '(3x, "    ",$)')
		do i=ncol+1, n
			write(iout,'(2x,a'//trim(fmt)//',$)') xnames1(i)
		end do
		write(iout,*)
		write(iout,*)
	end if
	!----------------------------------------------------------------------------
	deallocate(g   )
	deallocate(c   )
	deallocate(xnames)
	deallocate(xnames1)
	deallocate(idx)
	deallocate(idd)
	deallocate(id1)
	deallocate(c0 )
	deallocate(d0 )
	deallocate(cd0)
	deallocate(c1 )
	deallocate(d1 )
	deallocate(cd1)
	deallocate(c2 )
	deallocate(d2 )
	deallocate(cd2)
	!----------------------------------------------------------------------------
	return
end subroutine get_dep_tensor
!
!-------------------------------------------------------------------------------
!
subroutine get_nonzero_row1(m,n,matrix,ir,a)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer  :: m, n, ir
	integer  :: a
	real(DP) :: matrix(m,n)
	!----------------------------------------------------------------------------
	integer  :: ic, nz
	real(DP) :: eps
	!----------------------------------------------------------------------------
	eps = 1.D-6
	!----------------------------------------------------------------------------
	do ic=1, n
		if(abs(matrix(ir,ic))>eps) then
			a = ic
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_nonzero_row(m,n,matrix,ir,a)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer  :: m, n, ir
	integer  :: a(n)
	real(DP) :: matrix(m,n)
	!----------------------------------------------------------------------------
	integer  :: ic, nz
	real(DP) :: eps
	!----------------------------------------------------------------------------
	eps = 1.D-6
	!----------------------------------------------------------------------------
	nz = 0
	do ic=1, n
		if(abs(matrix(ir,ic))>eps) then
			nz = nz + 1
			a(nz) = ic
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
function number_of_nonzero_row(m,n,matrix,ir)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer , intent(in) :: m, n, ir
	real(DP), intent(in) :: matrix(m,n)
	!----------------------------------------------------------------------------
	integer              :: ic, nz
	real(DP)             :: eps
	!----------------------------------------------------------------------------
	eps = 1.D-6
	!----------------------------------------------------------------------------
	nz = 0
	do ic=1, n
		if(abs(matrix(ir,ic))>eps) nz = nz + 1
	end do
	number_of_nonzero_row = nz
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
subroutine reduce_matrix(m,n,matrix,nram)
	use kinds, only: DP
	use math , only: same_direction
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n
	integer               :: nram
	integer               :: nrow
	real(DP)              :: matrix(m,n)
	integer               :: i,j,k,m1, ic
	real(DP), allocatable :: r1(:), ma1(:,:), ma2(:,:)
	real(DP), allocatable :: a(:), b(:)
	logical , allocatable :: ifsave(:)
	real(DP)              :: eps, amax
	!----------------------------------------------------------------------------
	eps = 1.D-6
	allocate(r1(m))
	allocate(ifsave(m))
	allocate(ma1(m,n))
	allocate(ma2(m,n))
	allocate(a(n))
	allocate(b(n))
	r1=0.D0
	k=0
	m1=0.D0
	do i=1, m
		do j=1, n
			r1(i) = r1(i) + matrix(i,j)**2
		end do
		r1(i)=sqrt(r1(i))
		if (r1(i)>=eps) then
			k=k+1
			do j=1, n
				ma1(k,j) = matrix(i,j)
			end do
		end if
	end do
	m1=k
	!----------------------------------------------------------------------------
	ifsave    = .false.
	ifsave(1) = .true.
	k=0
	!----------------------------------------------------------------------------
	do i=1, m1
	!----------------------------------------------------------------------------
		if(ifsave(i)) then
			k=k+1
			do ic=1, n
				ma2(k,ic) = ma1(i,ic)
			end do
			!----------------------------------------------------------------------
			do j=i+1, m1
			!----------------------------------------------------------------------
				do ic=1, n
					a(ic)  = ma1(i,ic)
					b(ic)  = ma1(j,ic)
				end do
				!-------------------------------------------------------------------
				ifsave(j) = .not. same_direction(n,a,b)
			end do
			!----------------------------------------------------------------------
		end if
	end do
	nram=k
	matrix=ma2
	!----------------------------------------------------------------------------
	deallocate(r1)
	deallocate(ma1)
	deallocate(ma2)
	deallocate(ifsave)
	deallocate(a)
	deallocate(b)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_minus_row_dn(m,n,matrix,i,nrow)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n,i
	integer               :: nrow
	real(DP)              :: matrix(m,n)
	integer               :: ir, ic
	real(DP)              :: eps, amax, a
	!----------------------------------------------------------------------------
	eps = 1.D-6
	do ic=n, i, -1
		matrix(i,ic) = matrix(i,ic) / matrix(i,i)
	end do
	do ir=1, nrow
		if (ir==i) cycle
		if (abs(matrix(ir,i)) > eps ) then
			!----------------------------------------------------------------------
			! The order should be n, n-1, ... , i
			! It can not be changed to be: i, i+1, ... , n
			! Because that matrix(i,ic) should be the last element to be changed
			!
			a = matrix(ir,i) / matrix(i,i)
			do ic=n, i, -1
				matrix(ir,ic) = matrix(ir,ic) - matrix(i,ic) * a
			end do
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_minus_row_up(m,n,matrix,i,nrow)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n,i
	integer               :: nrow
	real(DP)              :: matrix(m,n)
	integer               :: ir, ic
	real(DP)              :: eps, amax
	!----------------------------------------------------------------------------
	eps = 1.D-6
	do ir=1, i-1
		if (abs(matrix(ir,i)) > eps ) then
			!----------------------------------------------------------------------
			! The order should be n, n-1, ... , i
			! It can not be changed to be: i, i+1, ... , n
			! Because that matrix(i,ic) should be the last element to be changed
			!
			do ic=n, i, -1
				matrix(ir,ic) = matrix(ir,ic) - matrix(i,ic)
			end do
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_move_max(m,n,matrix,i,nrow,ncol,idx)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n,i
	integer               :: nrow, ncol
	integer               :: idx(n)
	real(DP)              :: matrix(m,n)
	real(DP)              :: d
	integer               :: ir, ic, imax, j
	real(DP)              :: eps, amax
	!----------------------------------------------------------------------------
	eps  = 1.D-6
	amax = 0.D0
	imax = -1
	!----------------------------------------------------------------------------
	do ir=i, nrow
		if (amax< abs(matrix(ir,i))) then
			amax = abs(matrix(ir,i))
			imax = ir
		end if
	end do
	!----------------------------------------------------------------------------
	if (amax>eps) then
		if (imax/=i) then
			call swap_row(m, n, i, imax, matrix)
		end if
	else
		!-------------------------------------------------------------------------
		amax = 0.D0
		imax = -1
		!-------------------------------------------------------------------------
		do ic=i, ncol
			if (amax< abs(matrix(i,ic))) then
				amax = abs(matrix(i,ic))
				imax = ic
			end if
		end do
		!-------------------------------------------------------------------------
		if (amax>eps .and. imax/=i) then
			call swap_col(m, n, i, imax, matrix)
			j         = idx(i)
			idx(i)    = idx(imax)
			idx(imax) = j
		end if
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_normlize_row_col(m,n,matrix,i,nrow,ncol,idx)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n,i
	integer               :: nrow, ncol
	integer               :: idx(n)
	real(DP)              :: matrix(m,n)
	real(DP)              :: d
	integer               :: ir, ic, imax, j
	real(DP)              :: eps, amax
	!----------------------------------------------------------------------------
	eps  = 1.D-6
	amax = 0.D0
	imax = -1
	!----------------------------------------------------------------------------
	do ir=i, nrow
		if (abs(matrix(ir,i))>eps) then
			!----------------------------------------------------------------------
			! The order should be ncol, ncol-1, ... , i
			! It can not be changed to be: i, i+1, ... , ncol
			! Because that matrix(ir,i) should be the last element to be changed
			!
			do ic=ncol, i, -1
				matrix(ir,ic) = matrix(ir,ic) / matrix(ir,i)
			end do
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_del_zero_row(m,n,matrix,nrow)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n
	integer               :: nrow
	real(DP)              :: matrix(m,n)
	real(DP), external    :: matrix_row_norm
	real(DP)              :: d
	integer               :: ir, jr, ic
	real(DP), allocatable :: m1(:,:)
	real(DP)              :: eps
	!----------------------------------------------------------------------------
	allocate(m1(m,n))
	eps=1.D-6
	!----------------------------------------------------------------------------
	m1=0.D0
	jr=0
	do ir=1, nrow
		d=matrix_row_norm(m,n,ir,n,matrix)
		if (d > eps) then
			jr=jr+1
			do ic=1, n
				m1(jr,ic) = matrix(ir,ic)
			end do
		end if
	end do
	nrow   = jr
	matrix = m1
	!----------------------------------------------------------------------------
	deallocate(m1)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_del_zero_col(m,n,matrix,ncol,idx)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer               :: m,n
	integer               :: ncol
	integer               :: idx(n)
	real(DP)              :: matrix(m,n)
	real(DP), external    :: matrix_col_norm
	real(DP)              :: d
	integer               :: ir, jc, kc, ic
	integer , allocatable :: id1(:)
	integer , allocatable :: id2(:)
	real(DP), allocatable :: m1(:,:)
	real(DP)              :: eps
	!----------------------------------------------------------------------------
	allocate(m1 (m,n))
	allocate(id1(  n))
	allocate(id2(  n))
	eps=1.D-6
	!----------------------------------------------------------------------------
	m1  = 0.D0
	id1 = 0
	id2 = 0
	jc  = 0
	kc  = 0
	do ic=1, n
		d=matrix_col_norm(m,n,ic,m,matrix)
		if (d > eps) then
			jc=jc+1
			id1(jc) = idx(ic)
			do ir=1, m
				m1(ir,jc) = matrix(ir,ic)
			end do
		else
			kc=kc+1
			id2(kc) = idx(ic)
		end if
	end do
	ncol   = jc
	matrix = m1
	!----------------------------------------------------------------------------
	do ic=1, n
		if (ic<=ncol) then
			idx(ic) = id1(ic)
		else
			idx(ic) = id2(ic-ncol)
		end if
	end do
	!----------------------------------------------------------------------------
	deallocate(m1 )
	deallocate(id1)
	deallocate(id2)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_copy_row(m,n,i,k,ncol,matrix,m1)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer  :: m,n,i,k,ncol
	real(DP) :: matrix(m,n), m1(m,n)
	real(DP) :: matrix_row_norm
	integer  :: j
	!----------------------------------------------------------------------------
	do j=1, ncol
		m1(k,j) = matrix(i,j)
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine matrix_scale_row(m,n,i,ncol,matrix,a)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer  :: m,n,i,ncol
	real(DP) :: matrix(m,n), a
	real(DP) :: matrix_row_norm
	integer  :: j
	real(DP) :: eps
	!----------------------------------------------------------------------------
	eps = 1.D-6
	!----------------------------------------------------------------------------
	if (abs(a) > eps) then
		do j=1, ncol
			matrix(i,j) = matrix(i,j) / a
		end do
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
function matrix_row_norm(m,n,i,ncol,matrix)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer  :: m,n,i,ncol
	real(DP) :: matrix(m,n)
	real(DP) :: matrix_row_norm
	real(DP) :: a
	integer  :: j
	!----------------------------------------------------------------------------
	a=0.D0
	do j=1, ncol
		a=a+matrix(i,j)**2
	end do
	matrix_row_norm=sqrt(a)
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
function matrix_col_norm(m,n,i,nrow,matrix)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	integer  :: m,n,i,nrow
	real(DP) :: matrix(m,n)
	real(DP) :: matrix_col_norm
	real(DP) :: a
	integer  :: j
	!----------------------------------------------------------------------------
	a=0.D0
	do j=1, nrow
		a=a+matrix(j,i)**2
	end do
	matrix_col_norm=sqrt(a)
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
subroutine swap_row(nrow, ncol, i, j, matrix)
	use kinds, only: DP
	implicit none
	integer , intent(in   ) :: nrow, ncol, i, j
	real(DP), intent(inout) :: matrix(nrow, ncol)
	!----------------------------------------------------------------------------
	integer                 :: k
	real(DP)                :: tmp
	!----------------------------------------------------------------------------
	do k=1, ncol
		tmp         = matrix(i,k)
		matrix(i,k) = matrix(j,k)
		matrix(j,k) = tmp
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!----------------------------------------------------------------------------
!
subroutine swap_col(nrow, ncol, i, j, matrix)
	use kinds, only: DP
	implicit none
	integer , intent(in   ) :: nrow, ncol, i, j
	real(DP), intent(inout) :: matrix(nrow, ncol)
	!----------------------------------------------------------------------------
	integer                 :: k
	real(DP)                :: tmp
	!----------------------------------------------------------------------------
	do k=1, nrow
		tmp         = matrix(k,i)
		matrix(k,i) = matrix(k,j)
		matrix(k,j) = tmp
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!----------------------------------------------------------------------------
!
recursive subroutine generate_matrix(n,m,matrix,rot_name,v1,da,theta,ifcal)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer     :: n,m
	real   (DP) :: matrix(3,3,n), v1(3,n), da(3,n), theta(n)
	logical     :: ifcal(n,n)
	character(*):: rot_name(n)
	!----------------------------------------------------------------------------
	integer     :: i, j
	real   (DP) :: mi(3,3)
	real   (DP) :: mj(3,3)
	real   (DP) :: mk(3,3)
	integer     :: info
	integer     :: rot_type
	real   (DP) :: alpha, beta, gamma
	logical     :: debug
	!----------------------------------------------------------------------------
	logical, external :: in_group
	!----------------------------------------------------------------------------
	debug = .false.
	do i=1, m
		mi=matrix(:,:,i)
		do j=1, m
			if (ifcal(i,j)) then
				mj=matrix(:,:,j)
				mk = matmul(mi,mj)
				if (.not. in_group(m,matrix,mk)) then
					m=m+1
					matrix(:,:,m) = mk
					ifcal(i,j) = .false.
					!----------------------------------------------------------------
					call find_rotation_axis(mk, da(1,m), v1(1,m), theta(m), rot_name(m))
					!----------------------------------------------------------------
					call generate_matrix(n,m,matrix,rot_name,v1, da,theta,ifcal)
					return
				end if
				!-------------------------------------------------------------------
				ifcal(i,j) = .false.
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
function in_group(m,matrix,mk)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer  :: m
	real(DP) :: matrix(3,3,m)
	real(DP) :: mk(3,3)
	logical  :: in_group
	logical, external :: same_matrix
	!----------------------------------------------------------------------------
	integer  :: i
	!----------------------------------------------------------------------------
	in_group = .false.
	do i=1, m
		if (same_matrix(mk, matrix(1,1,i))) then
			in_group = .true.
			return
		end if
	end do
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
function same_matrix(mh,mk)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	real(DP), intent(in) :: mk(3,3)
	real(DP), intent(in) :: mh(3,3)
	logical              :: same_matrix
	!----------------------------------------------------------------------------
	real(DP)             :: mc(3,3)
	integer              :: i, j
	real(DP)             :: d
	real(DP)             :: eps
	!----------------------------------------------------------------------------
	eps = 1.D-6
	same_matrix = .false.
	d = 0.D0
	mc = mk - mh
	do i=1, 3
		do j=1, 3
			d = d + mc(i,j)**2
		end do
	end do
	d = sqrt(d)
	if (d<eps) same_matrix = .true.
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
subroutine get_product_table(n,m,matrix, product_table)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer           :: n, m
	real(DP)          :: matrix(3,3,n)
	integer           :: product_table(m,m)
	real(DP)          :: mi(3,3)
	real(DP)          :: mj(3,3)
	real(DP)          :: mk(3,3)
	real(DP)          :: mh(3,3)
	integer           :: i, j, k
	logical, external :: same_matrix
	!----------------------------------------------------------------------------
	do i=1, m
		mi = matrix(:,:,i)
		do j=1, m
			mj = matrix(:,:,j)
			mh = matmul(mi, mj)
			do k=1, m
				mk = matrix(:,:,k)
				if (same_matrix(mh, mk)) then
					product_table(i,j) = k
					exit
				end if
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine prn_product_table(n,m,rot_name, product_table)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer           :: n, m
	character(*)      :: rot_name(m)
	integer           :: product_table(m,m)
	integer           :: i, j, k
	character(80)     :: fmt
	!----------------------------------------------------------------------------
	write(fmt,'(i0)') m
	write(*,*)
	write(*, '(a11, " | ", '//fmt//'a11)') "Operation", (trim(rot_name(j)), j=1, m)
	call write_n_char(6,"-",(m+2)*11)
	!----------------------------------------------------------------------------
	do i=1, m
		write(*, '(a11, " | ", '//fmt//'a11)') trim(rot_name(i)), (trim(rot_name(product_table(i,j))), j=1, m)
	end do
	call write_n_char(6,"-",(m+2)*11)
	write(*,*)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine print_matrix_complex(m, e, v)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	real   (DP) :: m(3,3)
	complex(DP) :: e(  3)
	complex(DP) :: v(3,3)
	integer  :: i, j
	!----------------------------------------------------------------------------
	write(*,*)
	do i=1, 3
		write(*, '("Rotation matrix", 3(16x,f12.6))') (m(i,j), j=1, 3)
	end do
	write(*,*)
	write(*, '("Eigen values   ", 3(4x,2f12.6))') (e(j),j=1,3)
	do i=1, 3
		write(*, '("Eigen vectors  ", 3(4x,2f12.6))') (real(v(i,j)), aimag(v(i,j)), j=1, 3)
	end do
	write(*,*)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine find_rotation_axis(m, da, v1, theta, rot_name)
	use kinds, only: DP
	use math,  only: det3, angle2, rotmat, inverse3, reduction, diag_dnosy
	implicit none
	!----------------------------------------------------------------------------
	real   (DP)   :: m(3,3)
	complex(DP)   :: e(  3)
	complex(DP)   :: v(3,3)
	integer       :: rot_type
	real   (DP)   :: alpha, beta, gamma, theta
	real   (DP)   :: v1(3), da(3), PI
	integer       :: i, j, ntheta, m1, n1, info
	real   (DP)   :: eps, det, phi
	real   (DP)   :: cy(3,3), cz(3,3)
	real   (DP)   :: cy_inv(3,3), cz_inv(3,3)
	real   (DP)   :: c_new(3,3)
	character(*)  :: rot_name
	!----------------------------------------------------------------------------
	call diag_dnosy(3, "R", m, e, v, v, info)
	! call print_matrix_complex(m, e, v)
	!----------------------------------------------------------------------------
	eps = 1.D-6
	PI  = acos(-1.0_DP)
	det = det3(m)
	!----------------------------------------------------------------------------
	if ( abs(real(e(1)) - 1.D0 ) < eps .and.  abs(real(e(2)) - 1.D0 ) < eps .and.  abs(real(e(3)) - 1.D0 ) < eps ) then
		rot_type = 2
		rot_name = "C_1_1"
	else if ( abs(real(e(1)) - 1.D0 ) < eps .and.  abs(real(e(2)) + 1.D0 ) < eps .and.  abs(real(e(3)) + 1.D0 ) < eps .or. &
		& abs(real(e(1)) + 1.D0 ) < eps .and.  abs(real(e(2)) - 1.D0 ) < eps .and.  abs(real(e(3)) + 1.D0 ) < eps .or. &
		& abs(real(e(1)) + 1.D0 ) < eps .and.  abs(real(e(2)) + 1.D0 ) < eps .and.  abs(real(e(3)) - 1.D0 ) < eps ) then
		rot_type = 3
		rot_name = "C_2_1"
	else if (abs(det - 1.D0) < eps) then
		rot_type = 4
		rot_name = "C_n_1"
	else if ( abs(real(e(1)) + 1.D0 ) < eps .and.  abs(real(e(2)) + 1.D0 ) < eps .and.  abs(real(e(3)) + 1.D0 ) < eps ) then
		rot_type = 5
		rot_name = "S_2_1"
	else if ( abs(real(e(1)) - 1.D0 ) < eps .and.  abs(real(e(2)) - 1.D0 ) < eps .and.  abs(real(e(3)) + 1.D0 ) < eps .or. &
		& abs(real(e(1)) - 1.D0 ) < eps .and.  abs(real(e(2)) + 1.D0 ) < eps .and.  abs(real(e(3)) - 1.D0 ) < eps .or. &
		& abs(real(e(1)) + 1.D0 ) < eps .and.  abs(real(e(2)) - 1.D0 ) < eps .and.  abs(real(e(3)) - 1.D0 ) < eps ) then
		rot_type = 6
		rot_name = "S_1_1"
	else if (abs(det+1.D0) < eps) then
		rot_type = 7
		rot_name = "S_n_1"
	else
		rot_type = 1
		rot_name = "NO"
	end if
	!----------------------------------------------------------------------------
	if (rot_type==2) then
		alpha = 0.0_DP
		beta  = 0.0_DP
		gamma = 0.0_DP
		v1    = 0.0_DP
		theta = 0.0_DP
		c_new = 0.0_DP
		c_new(1,1) = 1.0_DP
		c_new(2,2) = 1.0_DP
		c_new(3,3) = 1.0_DP
	else if (rot_type==3 .or. rot_type==4) then
		do i=1, 3
			if ( abs(real(e(i))-1.D0) < eps .and. abs(aimag(e(i))-0.D0) < eps ) then
				v1 = real(v(:,i))
				exit
			end if
		end do
		!-------------------------------------------------------------------------
		v1 = v1 / sqrt(dot_product(v1, v1))
		phi   = angle2(v1(1), v1(2))
		!
		alpha = acos(v1(1)) * 180.0_DP / PI
		beta  = acos(v1(2)) * 180.0_DP / PI
		gamma = acos(v1(3)) * 180.0_DP / PI
		phi   = phi         * 180.0_DP / PI
		!-------------------------------------------------------------------------
		cy     = rotmat(gamma,2)
		cz     = rotmat(phi  ,3)
		cy_inv = inverse3(cy)
		cz_inv = inverse3(cz)
		c_new  = matmul(cz_inv,     m)
		c_new  = matmul(cy_inv, c_new)
		c_new  = matmul(c_new ,    cz)
		c_new  = matmul(c_new ,    cy)
		!-------------------------------------------------------------------------
		theta  = angle2(c_new(1,1), c_new(2,1))
		theta  = theta * 180.0_DP / PI
		m1     = nint(theta)
		n1     = 360
		if (theta /= 0.D0 ) then
			call reduction(m1,n1)
			write(rot_name,'("C_",i0,"_",i0)') n1,m1
		else
			ntheta = 1
		end if
		!-------------------------------------------------------------------------
	else if (rot_type>=6) then
		do i=1, 3
			if ( abs(real(e(i))+1.D0) < eps .and. abs(aimag(e(i))+0.D0) < eps ) then
				v1 = real(v(:,i))
				exit
			end if
		end do
		!-------------------------------------------------------------------------
		v1 = v1 / sqrt(dot_product(v1, v1))
		phi   = angle2(v1(1), v1(2))
		!
		alpha = acos(v1(1)) * 180.0_DP / PI
		beta  = acos(v1(2)) * 180.0_DP / PI
		gamma = acos(v1(3)) * 180.0_DP / PI
		phi   = phi         * 180.0_DP / PI
		!-------------------------------------------------------------------------
		cy     = rotmat(gamma,2)
		cz     = rotmat(phi  ,3)
		cy_inv = inverse3(cy)
		cz_inv = inverse3(cz)
		c_new  = matmul(cz_inv,     m)
		c_new  = matmul(cy_inv, c_new)
		c_new  = matmul(c_new ,    cz)
		c_new  = matmul(c_new ,    cy)
		!-------------------------------------------------------------------------
		theta  = angle2(c_new(1,1), c_new(2,1))
		theta  = theta * 180.0_DP / PI
		m1     = nint(theta)
		n1     = 360
		if (theta /= 0.D0 ) then
			call reduction(m1,n1)
			write(rot_name,'("S_",i0,"_",i0)') n1,m1
		else
			ntheta = 1
		end if
	end if
	!----------------------------------------------------------------------------
	da(1) = alpha
	da(2) = beta
	da(3) = gamma
	!----------------------------------------------------------------------------
	return
end subroutine
