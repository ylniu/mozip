subroutine aswap (n, x, op, jrels,i,j,r)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent(in   ) :: n, i, j
	real(DP), intent(inout) :: r(3,3)
	real(DP), intent(inout) :: x(3,n)
	integer , intent(inout) :: jrels(n,7)
	logical , intent(inout) :: op(7)
	!----------------------------------------------------------------------------
	! Output variables
	!
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                 :: k
	integer                 :: itmp
	real(DP)                :: tmp
	real(DP)                :: r1(3,3)
	logical                 :: op1
	!----------------------------------------------------------------------------
	op1     = op(4-i)
	op(4-i) = op(4-j)
	op(4-j) = op1
	!
	op1     = op(8-i)
	op(8-i) = op(8-j)
	op(8-j) = op1
	!
	! write (6,*) 'swap',i,j
	!----------------------------------------------------------------------------
! 
!	write (6,*) 'SYMM ERROR: swap called but code not in later for it'
!	write (6,*) 'SYMM not used'
!	iswap= 1
! 
! 	**** interchanges coordinate axes i and j ****
	do k= 1, n
		!-------------------------------------------------------------------------
		! edited by niuyingli
		! xz(k,j)    = tmp
		tmp          = x(k,i)
		x(k,i)       = x(k,j)
		x(k,j)       = -tmp
		!-------------------------------------------------------------------------
		itmp         = jrels(k,4-i)
		jrels(k,4-i) = jrels(k,4-j)
		jrels(k,4-j) = itmp
		!-------------------------------------------------------------------------
		itmp         = jrels(k,8-i)
		jrels(k,8-i) = jrels(k,8-j)
		jrels(k,8-j) = itmp
		! write (6,'(i4,3f10.5)') k,(xz(k,kk),kk=1,3)
	end do
	!
	!     *** upgrade rotation transformation ****
	!
	k      = 6 - i - j
	r1     =   0.0_DP
	r1(i,j)=   1.0_DP
	r1(j,i)= - 1.0_DP
	r1(k,k)=   1.0_DP
	r      = matmul(r1, r)
	!----------------------------------------------------------------------------
	return
end subroutine
