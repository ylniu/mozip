subroutine get_n_product_index(v,m,idx,order_type,info)
	implicit none
	!----------------------------------------------------------------------------
	! E.g. v=3, n=2
	! or
	!      v=3, n=3
	!
	integer, intent( in) :: v, m, order_type
	integer, intent(out) :: idx(m,v**m), info
	!
	integer              :: i, j, k, a, b, n
	integer, allocatable :: npt(:), id1(:)
	!----------------------------------------------------------------------------
	info = 0
	n=m
	allocate(npt(0:n))
	allocate(id1(  n))
	!----------------------------------------------------------------------------
	npt(0)=1
	do i=1, n
		npt(i) = npt(i-1) * v
	end do
	!----------------------------------------------------------------------------
	do a=1, npt(n)
		b   = a - 1
		id1 = 0
		do j=n, 1, -1
			!----------------------------------------------------------------------
			i         = b     / npt(j-1)
			b         = b - i * npt(j-1)
			!----------------------------------------------------------------------
			if (order_type==1) then
				id1(n+1-j) = i + 1
			else
				id1(    j) = i + 1
			end if
			!----------------------------------------------------------------------
		end do
		do k=1, m
			idx(k,a)=id1(k)
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(npt)
	deallocate(id1)
	!----------------------------------------------------------------------------
end subroutine
