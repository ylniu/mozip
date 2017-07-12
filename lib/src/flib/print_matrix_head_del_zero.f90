subroutine print_matrix_head_del_zero(fid, nrow, ncol, m, head, nrow1, ncol1, mname, npcol, fmt, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent( in) :: nrow
	integer     , intent( in) :: ncol
	integer     , intent( in) :: nrow1
	integer     , intent( in) :: ncol1
	integer     , intent( in) :: npcol
	integer     , intent( in) :: fid
	real(DP)    , intent( in) :: m(nrow, ncol)
	character(*), intent( in) :: head(ncol)
	character(*), intent( in) :: mname
	character(*), intent( in) :: fmt
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	integer              :: ib, ie, ic, n, i, ia, nn, ir, ipcol
	character( 1)        :: a
	character(80)        :: itmp, a1, a2
	real(DP)             :: dr, eps
	!----------------------------------------------------------------------------
	info = 0
	eps=1.D-6
	if (npcol>ncol1) then
		ipcol = ncol1
	else
		ipcol = npcol
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
	write(itmp,'("a",i0)') nn
	!----------------------------------------------------------------------------
	call write_n_char(fid,"-",nn*ipcol+6)
	write(fid, '(2x "Print Array ",a," :")') trim(mname)
	call write_n_char(fid,"-",nn*ipcol+6)
	!----------------------------------------------------------------------------
	ib=1
	ie=ib+ipcol-1
	ie=min(ie,ncol1)
	do while(ib<=ncol1)
		n=ie - ib + 1
		write(a1,'("(a6,",i0,a,")")') n,trim(itmp)
		write(a2,'("(i6,",i0,a,")")') n,trim(fmt)
		!-------------------------------------------------------------------------
		write(fid,a1) "col", (trim(head(ic)), ic=ib, ie)
		call write_n_char(fid,"-",nn*n+6)
		do ir=1, nrow1
			dr=0.D0
			do ic=ib,ie
				dr = dr + m(ir,ic)**2
			end do
			dr=sqrt(dr)
			if (dr>eps) then
				write(fid, a2) ir, (m(ir, ic), ic=ib, ie)
			end if
		end do
		write(fid,*)
		!-------------------------------------------------------------------------
		ib=ie+1
		ie=ib+ipcol-1
		ie=min(ie,ncol1)
		!-------------------------------------------------------------------------
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
