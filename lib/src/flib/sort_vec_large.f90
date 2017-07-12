subroutine sort_vec_large(m,n,c)
	use kinds
	!----------------------------------------------------------------------------
	integer , intent(in   ) :: m, n
	real(DP), intent(inout) :: c(m,n)
	!----------------------------------------------------------------------------
	integer                 :: i, j
	real(DP)                :: maxr, maxc
	real(DP), allocatable   :: a(:,:)
	!----------------------------------------------------------------------------
	allocate(a(m,n))
	!----------------------------------------------------------------------------
	if (m<=0 .or. n<=0) return
	do i=1, n
		maxr=0.D0
		do j=1, m
			if ( maxr < abs(c(j,i))  ) then
				maxr=abs(c(j,i))
				maxc=c(j,i)
			end if
		end do
		if (maxc>=0.D0) then
			a(:,i) =   c(:,i)
		else
			a(:,i) = - c(:,i)
		end if
	end do
	c=a
	!----------------------------------------------------------------------------
	deallocate(a)
	!----------------------------------------------------------------------------
	return
end subroutine
