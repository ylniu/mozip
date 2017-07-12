program printrn
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	integer               :: i, j, k, l, n, fid, ib, ie, m, nb, info
	real(DP), allocatable :: a(:)
	character(200)        :: finp, tmp, fmt1
	integer , external    :: number_of_lines
	character(300)        :: line
	!--------------------------------------------------------------------
	fid = 1
	call getarg(1, finp)
	call getarg(2, tmp)
	read(tmp, *) k
	!--------------------------------------------------------------------
	open(fid, file=finp, status="old")
		n=number_of_lines(fid)
		rewind(fid)
		allocate(a(n))
		do i=1, n
			if (k==1) then
				read(fid,*) a(i)
			else if (k>=2) then
				read(fid,*) (tmp, j=1, k-1), a(i)
			end if
		end do
	close(fid)
	!--------------------------------------------------------------------
	m=5
	write(*,'("a(",i0,",",i0,")")') 1, n
	write(*,'("data a / &")')
	!--------------------------------------------------------------------
	ib = 1
	ie = ib + m - 1
	ie = min(n, m)
	!--------------------------------------------------------------------
	do while (ib<=n)
		nb = ie - ib + 1
		!-----------------------------------------------------------------
		if (ie==n) then
			write(fmt1,'("(e16.8, ",i0,"("","", es16.8),x,""&"")")') nb-1
		else
			write(fmt1,'("(",i0,"(es16.8,"",""),x,""&"" )")') nb
		end if
		!-----------------------------------------------------------------
		write(line,fmt1) (a(l), l=ib, ie)
		call str_replace("E", "D", line, line, info)
		write(*,'(a)') trim(line)
		!-----------------------------------------------------------------
		ib = ie + 1
		ie = ib + m - 1
		ie = min(n,ie)
	end do
	write(*,'("/")')
	!--------------------------------------------------------------------
	deallocate(a)
	!--------------------------------------------------------------------
	stop
end
