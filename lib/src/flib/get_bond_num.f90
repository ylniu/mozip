subroutine get_bond_num(n, bond, bond_num)
	!----------------------------------------------------------------------------
	! Get the number of bonds for each atom in a molecule
	! E.g.
	!
	! If   :
	!
	! bond(1,2) = .True.
	! bond(1,3) = .True.
	! bond(2,3) = .False.
	!
	! Then :
	!
	! bond_num(1) = 2
	! bond_num(2) = 1
	! bond_num(3) = 1
	!
	implicit none
	!----------------------------------------------------------------------------
	integer, intent( in) :: n
	logical, intent( in) :: bond(n,n)
	integer, intent(out) :: bond_num(n)
	!----------------------------------------------------------------------------
	integer              :: i, j
	!----------------------------------------------------------------------------
	if (n<=0) return
	bond_num=0
	do i=1, n
		do j=1, n
			if ( i/=j .and. bond(i,j) ) then
				bond_num(i) = bond_num(i) + 1
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
