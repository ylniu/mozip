subroutine search_op (n, x, nat, mass, symm_op, jrels, op, tol)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent(in ) :: n
	real(DP), intent(in ) :: symm_op(3,3)
	integer , intent(in ) :: nat (n)
	real(DP), intent(in ) :: mass(n)
	real(DP), intent(in ) :: tol
	real(DP), intent(in ) :: x(3, n)
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer,  intent(out) :: jrels(n)
	logical,  intent(out) :: op
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer               :: i, i1
	real(DP)              :: dr
	real(DP)              :: dx(3)
	real(DP)              :: x1(3, n)
	logical               :: find
	!----------------------------------------------------------------------------
	op    = .false.
	jrels = 0
	x1    = x
	!----------------------------------------------------------------------------
	call rotn(n, symm_op, x1)
	!----------------------------------------------------------------------------
	do i1=1, n
		find  = .false.
		do i=1, n
			dx = x1(:,i1) - x(:,i)
			dr = sqrt( dot_product( dx, dx ) )
			if ( dr<= tol .and. mass(i)==mass(i1) .and. nat(i)==nat(i1) ) then
				jrels(i1) = i
				jrels(i ) = i1
				find=.true.
				exit
			end if
		end do
		if (.not. find) then
			jrels = 0
			return
		end if
	end do
	!----------------------------------------------------------------------------
	op = .true.
	!----------------------------------------------------------------------------
	return
end
