function is_number_letter(a)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*) :: a
	!----------------------------------------------------------------------------
	! Output variable
	!
	logical      :: is_number_letter
	!----------------------------------------------------------------------------
	! Local  variable
	!
	integer      :: i, j, n
	!----------------------------------------------------------------------------
	a=trim(adjustl(a))
	n=len(trim(adjustl(a)))
	!----------------------------------------------------------------------------
	if (n==0) then
		is_number_letter=.false.
		return
	end if
	!----------------------------------------------------------------------------
	is_number_letter = .true.
	do i=1, n
		j=iachar(a(i:i))
		if ( j<48 .or. ( j>57 .and. j<65 ) .or. ( j>90 .and. j<97 ) .or. j>122 ) then
			is_number_letter = .false.
			return
		end if
	end do
	!----------------------------------------------------------------------------
	return
end function
