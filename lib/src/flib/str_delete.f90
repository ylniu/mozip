subroutine str_delete(string,word)
	use String_Utility, only: StrLowCase
	implicit none
	!----------------------------------------------------------------------------
	! Input:
	!   word    = "cd"
	!   string  = "abcdeabcde"
	! Output:
	!   string  = "abeabe"
	!   count   = 2
	! E.g. call str_delete(word, string)
	!----------------------------------------------------------------------------
	character(*), intent(inout) :: string
	character(*), intent(in   ) :: word
	!----------------------------------------------------------------------------
	integer                     :: n1, n2
	integer                     :: i, j
	character(len(string))      :: line
	integer                     :: ntype
	logical                     :: condition
	!----------------------------------------------------------------------------
	ntype=2
	!----------------------------------------------------------------------------
	line=""
	n1=len(trim(string ))
	n2=len(trim(word   ))
	if (n2>n1) return
	i=0
	j=0
	do while (i <= n1 - n2 + 1)
		i=i+1
		j=j+1
		!-------------------------------------------------------------------------
		if (ntype==1) then
			condition=string(i:i+n2-1) == trim(word)
		else
			condition=StrLowCase(string(i:i+n2-1)) == StrLowCase(trim(word))
		end if
		!-------------------------------------------------------------------------
		if ( condition ) then
			i=i+n2
			if (i>n1) exit
		end if
		if (i< n1-n2+1) then
			line(j:j) = string(i:i)
		else if (i==n1-n2+1) then
			line(j:j+n2-1) = string(i:n1)
			exit
		end if
		!-------------------------------------------------------------------------
	end do
	string=line
	!----------------------------------------------------------------------------
	return
end subroutine
