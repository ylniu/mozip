program newgjf
	use kinds, only: DP
	use Param
	use file,  only: name_main
	implicit none
	integer                     :: fid, natom, i, j, k, nline, ib, ie, ln
	integer                     :: ios, info
	character(200)              :: tmp, line, fchk, ffchk, flog, fgjf, fout
	character(200)              :: fmt, cmd
	character(200), allocatable :: lines(:)
	character(2  ), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	logical                     :: scanok, find_begin, find_end, if_write
	logical       , external    :: search_word
	!---------------------------------------------------------------------------
	i=iargc()
	if_write=.false.
	if (i==1) then
		call getarg(1,tmp)
		i=len(trim(tmp))
		tmp=trim(name_main(tmp))
		if (tmp(i:i)==".") tmp=tmp(1:i-1)
		fchk  = trim(tmp)//".chk"
		ffchk = trim(tmp)//".fchk"
		fgjf  = trim(tmp)//".com"
		flog  = trim(tmp)//".log"
	else
		write(*,*) "USAGES:"
		write(*,*) "newgjf test.com"
		write(*,*) "Error, stop!"
		stop
	end if
	!---------------------------------------------------------------------------
	fid=1
	cmd="formchk "//trim(fchk)
	call system(cmd, ios)
	!---------------------------------------------------------------------------
	if (ios==0) then
		fout=ffchk
		open(fid, file=ffchk, status="old", iostat=ios)
			if (ios==0) if_write=.true.
			scanok=search_word(fid, 1, 15, "Number of atoms", line)
			if (.not.scanok) then
				write(*,*) "!!! Error !!! Wrong data in fchk file, please check fchk file"
				stop
			end if
			read(line,*) (tmp, i=1, 4), natom
			allocate(x(3,natom))
			allocate(symbol(natom))
			scanok=search_word(fid, 1, 29, "Current cartesian coordinates", line)
			read(fid,*) x
			x = x * au2a
		close(fid)
	else
		cmd="rm -f "//trim(ffchk)//" >& /dev/null"
		call system(cmd)
		write(*,'(/,x,a)') trim(cmd)
		fout=flog
		call qm_file_natom(flog, natom,    info)
		allocate(x     (3,natom))
		allocate(symbol(  natom))
		call qm_file_coord(flog, natom, x, info)
		if (info==0) if_write=.true.
		x = x * au2a
	end if
	!---------------------------------------------------------------------------
	find_begin = .false.
	find_end   = .false.
	!---------------------------------------------------------------------------
	open(fid, file=fgjf, status="old")
		nline=0
		read(fid, '(a)', iostat=ios) line
		if (ios==0) nline = nline + 1
		do while( ios==0)
			read(fid, '(a)', iostat=ios) line
			line=trim(adjustl(line))
			k=iachar(line(1:1))
			ln=len(trim(line))
			if (ios==0) nline = nline + 1
			!
			if ( (.not. find_begin) .and. (k>=48 .and. k<=57) ) then
				find_begin=.true.
				ib=nline+1
			end if
			!
			if ( find_begin .and. (.not. find_end) .and. ln==0 ) then
				find_end=.true.
				ie=nline-1
			end if
		end do
	close(fid)
	allocate(lines(nline))
	!---------------------------------------------------------------------------
	open(fid, file=fgjf, status="old")
	rewind(fid)
	do i=1, nline
		read(fid, '(a)') lines(i)
	end do
	j=0
	do i=ib, ie
		j=j+1
		read(lines(i),*) symbol(j)
		ln=len(trim(symbol(j)))
		if (ln==1) then
			write(lines(i),'(x, a1, 14x, 3f14.8)') trim(symbol(j)), x(:,j)
		else
			write(lines(i),'(   a2, 14x, 3f14.8)') trim(symbol(j)), x(:,j)
		end if
	end do
	close(fid)
	!---------------------------------------------------------------------------
	if (if_write) then
	open(fid, file=fgjf, status="old")
		do i=1, nline
			ln=len(trim(lines(i)))
			write(fmt,*) ln
			if (ln==0) then
				write(fid,*)
			else
				write(fid, '(a'//trim(fmt)//')') trim(lines(i))
			end if
		end do
	close(fid)
	end if
	!---------------------------------------------------------------------------
	write(*,*)
	write(*,'(x,"Insert the optimized coordinates from ''", a, "'' to ''", a, "''")') &
		trim(fout), trim(fgjf)
	write(*,*)
	write(*,'(x,"Transform successful!")')
	write(*,*)
	!---------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	deallocate(lines )
	!---------------------------------------------------------------------------
	stop
end
