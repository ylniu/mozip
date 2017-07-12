subroutine number_filename_x(fname,pre,iat,ix,a,n,m)
	implicit none
	!---------------------------------------------------------------------------
	character(*)   :: fname, pre
	real(8)        :: a
	integer        :: iat, ix, n, m
	character(  1) :: cx
	character(200) :: str, fmt, fmt1, fmt2
	!---------------------------------------------------------------------------
	integer        :: k, j
	!---------------------------------------------------------------------------
	if (ix==1) then
		cx="x"
	else if(ix==2) then
		cx="y"
	else if(ix==3) then
		cx="z"
	end if
	write(fmt,'("f",i0,".",i0)') n,m
	fmt1='("'//trim(adjustl(pre))//'-",i3,"-",a1,"-",'//trim(fmt)//')'
	fmt2='("'//trim(adjustl(pre))//'-",i3,"-",a1,"+",'//trim(fmt)//')'
	!---------------------------------------------------------------------------
	if (a<=0) then
		write(str,fmt1) iat, cx, abs(a)
	else
		write(str,fmt2) iat, cx, abs(a)
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
