program get_afm
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: n, i, j, k, irecl, nd
	real(DP)      , allocatable :: df(:,:,:)
	character(200), allocatable :: fname(:)
	character(200)              :: cmd, fout, fmt
	!----------------------------------------------------------------------------
	integer       , external    :: number_of_lines
	!----------------------------------------------------------------------------
	cmd="ls -lart df*.dat | wc -l > .tmp"
	call system(cmd)
	!----------------------------------------------------------------------------
	cmd="ls -lart df*.dat | awk '{print $9}' >> .tmp"
	call system(cmd)
	!----------------------------------------------------------------------------
	fid = 1
	open(fid, file=".tmp", status="old")
		read(fid,*) n
		allocate(fname(n))
		do i=1, n
			read(fid,*) fname(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	do i=1, n
		open(fid, file=fname(i), status="old")
			!----------------------------------------------------------------------
			if (i==1) then
				nd = number_of_lines(fid)
				rewind(fid)
				allocate(df(nd,nd,n))
			end if
			!----------------------------------------------------------------------
			do j=1, nd
				read(fid,*) (df(k,j,i), k=1, nd)
			end do
			!----------------------------------------------------------------------
		close(fid)
		!-------------------------------------------------------------------------
	end do
	!----------------------------------------------------------------------------
	irecl=20 * (nd+1)
	write(fmt,*) nd
	do j=1, nd
		write(fout,'("fout_",i3.3,".dat")') j
		open(fid, file=fout, recl=irecl)
		!-------------------------------------------------------------------------
			write(fid,'(a10, '//trim(fmt)//'i20)') "n", (k, k=1, nd)
			do i=1, n
				write(fid,'(i10, '//trim(fmt)//'es20.10)') i, (df(k,j,i), k=1, nd)
			end do
		!-------------------------------------------------------------------------
		close(fid)
	end do
	!----------------------------------------------------------------------------
	cmd="rm .tmp"
	call system(cmd)
	!----------------------------------------------------------------------------
	!----------------------------------------------------------------------------
	deallocate(fname)
	deallocate(df   )
	!----------------------------------------------------------------------------
	stop
end
