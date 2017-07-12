subroutine write_matrix(fid, n, ncol, matrix, fmt, mname, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent( in) :: fid
	integer     , intent( in) :: n
	integer     , intent( in) :: ncol
	real(DP)    , intent( in) :: matrix(n,n)
	character(*), intent( in) :: mname
	character(*), intent( in) :: fmt
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	integer              :: ib, ie, ic, i, ia, nn, ir, icol, ni
	character( 1)        :: a
	character(80)        :: itmp, a1, a2
	!----------------------------------------------------------------------------
	info=0
	if (ncol>n) then
		icol = n
	else
		icol = ncol
	end if
	!----------------------------------------------------------------------------
	ib=-1
	do i=1, len(trim(fmt))
		a  = fmt(i:i)
		ia = iachar(a)
		if (ib <0 .and. ia>=48 .and. ia<=57) then
			ib=i
		end if
		if (ib>=0 .and. ia==46) then
			ie=i-1
			exit
		end if
	end do
	read(fmt(ib:ie),*) nn
	write(itmp,'("i",i0)') nn
	!----------------------------------------------------------------------------
	call write_n_char(fid,"-",nn*icol+6)
	write(fid, '(2x "Print Array ",a," :")') trim(mname)
	call write_n_char(fid,"-",nn*icol+6)
	!----------------------------------------------------------------------------
	ib=1
	ie=ib+icol-1
	ie=min(ie,n)
	do while(ib<=n)
		ni=ie - ib + 1
		write(a1,'("(a6,",i0,a,")")') ni,trim(itmp)
		write(a2,'("(i6,",i0,a,")")') ni,trim(fmt)
		!-------------------------------------------------------------------------
		write(fid,a1) "col", (ic, ic=ib, ie)
		call write_n_char(fid,"-",nn*ni+6)
		do ir=1, n
			write(fid, a2) ir, (matrix(ir, ic), ic=ib, ie)
		end do
		write(fid,*)
		!-------------------------------------------------------------------------
		ib=ie+1
		ie=ib+icol-1
		ie=min(ie,n)
		!-------------------------------------------------------------------------
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
