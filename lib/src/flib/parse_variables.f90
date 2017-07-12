subroutine parse_variables(n,str,name,value)
	implicit none
	!--------------------------------------------------------------------
	integer     , intent( in) :: n
	character   , intent( in) :: str(n)
	character(*), intent( in) :: name
	character(*), intent(out) :: value
	!--------------------------------------------------------------------
	integer                   :: ns, i, j, m
	integer                   :: ib, ie
	logical                   :: find
	!--------------------------------------------------------------------
	ns = len(name)
	!--------------------------------------------------------------------
	do i=1, n-ns + 1
		!-----------------------------------------------------------------
		find = .true.
		do j=1, ns
			if (str(i-1+j) /= name(j:j)) then
				find=.false.
			end if
		end do
		!-----------------------------------------------------------------
		if (find) then
			m=i+ns
			do j=m, n
				if (str(j)=="=") then
					ib = j+1
				end if
				if (str(j)==";") then
					ie=j-1
					exit
				end if
			end do
			exit
		end if
		!-----------------------------------------------------------------
	end do
	j=0
	value=""
	do i=ib, ie
		if (iachar(str(i))/=10 .and. iachar(str(i))/=32) then
			j=j+1
			value(j:j) = str(i)
		end if
	end do
	!--------------------------------------------------------------------
	return
end subroutine
