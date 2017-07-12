subroutine del_char(a, c)
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent(inout) :: a
	character(*), intent(in   ) :: c
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer                     :: i, ib, la, k
	character(len=len(a))       :: b
	!----------------------------------------------------------------------------
	la=len(a)
	ib=iachar(c(1:1))
	!
	b=''
	if (la==0) return
	!
	k=0
	do i=1, la
		if (iachar(a(i:i))/=ib) then
			k=k+1
			b(k:k)=a(i:i)
		end if
	end do
	a=b
	!----------------------------------------------------------------------------
	return
end subroutine
