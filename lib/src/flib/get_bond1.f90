subroutine get_bond1 (n, x, nat, bond)
	!----------------------------------------------------------------------------
	! x is in the unit of Angstrom
	!
	use kinds
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer,  intent( in) :: n
	integer,  intent( in) :: nat(n)
	real(DP), intent( in) :: x(3,n)
	!----------------------------------------------------------------------------
	! Output variables
	!
	logical,  intent(out) :: bond(n,n)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i, j, k
	real(DP)              :: r(3), d
	!----------------------------------------------------------------------------
	bond = .false.
	!----------------------------------------------------------------------------
	do i= 1, n
		do j= 1, n
			if (i/=j) then
				r = x(:,i) - x(:,j)
				d = sqrt( dot_product(r,r) )
				!-------------------------------------------------------------------
				! For Gerneral bond
				!
				bond(i,j) = bond(i,j) .or. (nat(i)>  1 .and. nat(j)>  1 .and. d < 1.8)
				bond(i,j) = bond(i,j) .or. (nat(i)>  9 .and. nat(j)>  1 .and. d < 2.0)
				!-------------------------------------------------------------------
				! For H-?
				!
				bond(i,j) = bond(i,j) .or. (nat(i)== 1 .and. nat(j)>  1 .and. d < 1.3)
				!-------------------------------------------------------------------
				! For Mg-O
				!
				bond(i,j) = bond(i,j) .or. (nat(i)==12 .and. nat(j)>  8 .and. d < 3.8)
				!-------------------------------------------------------------------
				! For Si-?
				!
				bond(i,j) = bond(i,j) .or. (nat(i)==14 .and. nat(j)>  1 .and. d < 2.0)
				!-------------------------------------------------------------------
				! For Ir-?
				!
				bond(i,j) = bond(i,j) .or. (nat(i)==77 .and. nat(j)>  1 .and. d < 2.2)
				!-------------------------------------------------------------------
			end if
			bond(j,i) = bond(i,j) .or. bond(i,j)
		end do
	end do
	!----------------------------------------------------------------------------
	! For hydrogen Bond
	!
	do i= 1,n
		if (nat(i)==1) then
			do j= 1,n
				if (bond(i,j) .and. (nat(j)==7 .or. nat(j)==8)) then
					do k= 1,n
						if (k/=j .and. (nat(k)==7 .or. nat(k)==8)) then
							r = x(:,k) - x(:,i)
							d = sqrt( dot_product(r,r) )
							if (d< 2.D0) then
								bond(i,k)= .true.
								bond(k,i)= .true.
							end if
						end if
					end do
				end if
			end do
		end if
	end do
	return
end subroutine
