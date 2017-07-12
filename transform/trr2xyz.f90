program trr2xyz
	use kinds , only: DP
	use file  , only: name_main
	use string, only: StrLowCase
	use string, only: StrUpCaseFirst
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, i, j, k, it
	integer                     :: natom
	real(DP)      , allocatable :: t(    :)
	real(DP)      , allocatable :: a(:,:,:)
	real(DP)      , allocatable :: x(:,:,:)
	real(DP)      , allocatable :: v(:,:,:)
	real(DP)      , allocatable :: f(:,:,:)
	real(DP)      , allocatable :: mass  (:)
	character(  2), allocatable :: symbol(:)
	character(200), allocatable :: label (:)
	!----------------------------------------------------------------------------
	integer                     :: ntrr
	character(200)              :: ftrr, fgro, fxyz
	character(200)              :: line, tmp, c1, c2
	logical                     :: scanok
	logical       , external    :: is_letter
	logical       , external    :: search_word_free
	logical       , external    :: search_word_free_nline
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	call getarg(1,fgro)
	call getarg(2,ftrr)
	fxyz=trim(name_main(ftrr))//".xyz"
	!----------------------------------------------------------------------------
	open(fid, file=fgro, status="old")
		read(fid, '(a)') line
		read(fid, *) natom
		allocate(mass  (natom))
		allocate(symbol(natom))
		do i=1, natom
			c2=""
			read(fid,*) tmp, c1
			k=0
			do j=1, len(trim(c1))
				if( is_letter(c1(j:j)) ) then
					k=k+1
					c2(k:k) = c1(j:j)
				end if
			end do
			c2 = StrLowCase(c2)
			c2 = StrUpCaseFirst(c2)
			symbol(i) = trim(c2)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	call get_mass(natom,symbol,mass)
	!----------------------------------------------------------------------------
	open(fid, file=ftrr, status="old")
		scanok=search_word_free_nline(fid, "frame", ntrr, line)
		rewind(fid)
		scanok=search_word_free(fid, "natoms", line)
		read(line, *) tmp, natom
		allocate(label(    ntrr))
		allocate(t(        ntrr))
		allocate(a(3,    3,ntrr))
		allocate(x(3,natom,ntrr))
		allocate(v(3,natom,ntrr))
		allocate(f(3,natom,ntrr))
		rewind(fid)
		do it=1, ntrr
			read(fid, '(a)') line
			read(fid, '(44x, e16.7)') t(it)
			write(label(it), '("Time = ", e16.7, 2x, i6, " / ", i6)') t(it), it, ntrr
			read(fid, '(a)') line
			do i=1, 3
				read(fid, '(18x, e12.5, 2(2x,e12.5))') a(:,i,it)
			end do
			read(fid, '(a)') line
			do j=1, natom
				read(fid, '(16x, e12.5, 2(2x,e12.5))') x(:,j,it)
			end do
			read(fid, '(a)') line
			do j=1, natom
				read(fid, '(16x, e12.5, 2(2x,e12.5))') v(:,j,it)
			end do
			read(fid, '(a)') line
			do j=1, natom
				read(fid, '(16x, e12.5, 2(2x,e12.5))') f(:,j,it)
			end do
		end do
	close(fid)
	x = x * 10
	!----------------------------------------------------------------------------
	call write_xyz_v_n(fxyz,ntrr,natom,symbol,label,x,v)
	!----------------------------------------------------------------------------
	write(*,'(2x, "Input  files:", 2(2x, a))') trim(fgro), trim(ftrr)
	write(*,'(2x, "Output files:", 2(2x, a))') trim(fxyz)
	!----------------------------------------------------------------------------
	deallocate(label )
	deallocate(t     )
	deallocate(a     )
	deallocate(x     )
	deallocate(v     )
	deallocate(f     )
	deallocate(mass  )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
