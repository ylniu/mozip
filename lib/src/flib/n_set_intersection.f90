subroutine n_set_intersection(n1, set1, n2, set2, nset)
	implicit none
	!--------------------------------------------------------------------
	integer     , intent( in) :: n1
	integer     , intent( in) :: n2
	character(*), intent( in) :: set1(n1)
	character(*), intent( in) :: set2(n2)
	!--------------------------------------------------------------------
	integer     , intent(out) :: nset
	!--------------------------------------------------------------------
	integer                   :: i1, i2
	!--------------------------------------------------------------------
	nset = 0
	do i1=1, n1
		do i2=1, n2
			if( trim(set1(i1)) == trim(set2(i2)) ) then
				nset = nset + 1
				exit
			end if
		end do
	end do
	!--------------------------------------------------------------------
	return
	!--------------------------------------------------------------------
end subroutine
