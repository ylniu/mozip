subroutine full_to_short_name(name)
	implicit none
	!----------------------------------------------------------------------------
	! input and output variable
	character(*), intent(inout) :: name
	!----------------------------------------------------------------------------
	! local  variable
	integer                     :: i,j,k,l
	!----------------------------------------------------------------------------
	j=1
	k=len(trim(name))
	!
	do i=len(name),1,-1
		!-------------------------------------------------------------------------
		! if (name(i:i) is not space
		!
		if ( iachar(name(i:i)) /= 32 ) then
			k=i
			exit
		end if
	end do
	do i=len(name),1,-1
		!-------------------------------------------------------------------------
		! if (name(i:i) is "/"
		!
		if ( iachar(name(i:i)) == 47 ) then
			j=i+1
			exit
		end if
	end do
	l=k-j+1
	name(1:l) = name(j:k)
	name(l+1:len(name))=""
	return
end subroutine
