subroutine rotn (n,r,v)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	!
	! Input  variables
	!
	integer , intent(in ) :: n
	real(DP), intent(in ) :: r(3,3)

	!----------------------------------------------------------------------------
	!
	! Output variables
	!
	real(DP), intent(out) :: v(3,n)
	!----------------------------------------------------------------------------
	!
	! Local  variables
	!
	integer  :: i, j, k
	real(DP) :: v1(3)
	!----------------------------------------------------------------------------
	do i= 1, n
		v1 = 0.0_DP
		do j=1, 3
			do k=1, 3
				v1(j) = v1(j) + r(j,k) * v(k,i)
			end do
		end do
		v(:,i)= v1
	end do
	!----------------------------------------------------------------------------
	return
end
