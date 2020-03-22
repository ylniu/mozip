subroutine number_filename(fname,pre,a,n,m)
	implicit none
	!---------------------------------------------------------------------------
	character(*)   :: fname, pre
	real(8)        :: a
	integer        :: n, m
	character(200) :: str, fmt, fmt1, fmt2
	!---------------------------------------------------------------------------
	integer        :: k, j
	!---------------------------------------------------------------------------
	write(fmt,'("f",i0,".",i0)') n,m
	fmt1='("'//trim(adjustl(pre))//'-",'//trim(fmt)//')'
	fmt2='("'//trim(adjustl(pre))//'+",'//trim(fmt)//')'
	!---------------------------------------------------------------------------
	if (a<=0) then
		write(str,fmt1) abs(a)
	else
		write(str,fmt2) abs(a)
	end if
	!---------------------------------------------------------------------------
	k=len(trim(str))
	!---------------------------------------------------------------------------
	do j=1,k
		if(str(j:j)==" ") then
			str(j:j)="0"
		end if
	end do
	fname=trim(str)
	!---------------------------------------------------------------------------
	return
end subroutine