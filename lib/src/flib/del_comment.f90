subroutine del_comment(line)
	implicit none
	!----------------------------------------------------------------------------
	! input and output variable
	character(*), intent(inout) :: line
	!----------------------------------------------------------------------------
	! local  variable
	integer                     :: i,j
	!----------------------------------------------------------------------------
	j=0
	do i=1,len(line)
		if(line(i:i)=="#".or.line(i:i)=="!".or.line(i:i)=="&".or.line(i:i)=="/") then
			j=i
			exit
		end if
	end do
	if(j/=0) line(j:len(line))=""
	return
end subroutine