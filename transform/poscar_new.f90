program poscar_new
	use kinds         , only: DP
	use String_Utility, only: StrLowCase
	use math          , only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: fid1, fid2
	integer                      :: i, j, icycle, ncycle
	integer                      :: ntype, natom, npos
	integer        , allocatable :: types(:)
	real(DP)                     :: alat, a(3,3), a1(3,3), inva(3,3), r(3)
	real(DP)       , allocatable :: x(:,:)
	character(1)   , allocatable :: fix(:,:)
	character(200)               :: line, tmp, a_dyn, fmt
	character(200)               :: title, dynam, coord_type
	character(  2) , allocatable :: symbols(:)
	logical                      :: scanok
	!----------------------------------------------------------------------------
	integer        , external    :: number_of_words
	integer        , external    :: number_of_lines_word
	logical        , external    :: search_word_free_last
	logical        , external    :: search_word_free
	logical        , external    :: search_word_free_n
	logical        , external    :: is_number
	!----------------------------------------------------------------------------
	fid1       = 1
	fid2       = 2
	if(iargc()==1) then
		call getarg(1, tmp)
		read(tmp,*) icycle
	end if
	!----------------------------------------------------------------------------
	open(fid1, file="POSCAR", status="old")
		read(fid1,*) title
		read(fid1,*) alat
		read(fid1,*) a(:,1)
		read(fid1,*) a(:,2)
		read(fid1,*) a(:,3)
		read(fid1,'(a)') line
		read(line,*) tmp
		if (.not.is_number(tmp)) then
			ntype=number_of_words(line)
			allocate(symbols(ntype))
			read(line,*) symbols
			read(fid1,'(a)') line
		read(line,*) tmp
		end if
		ntype=number_of_words(line)
		allocate(types(ntype))
		read(line,*) types
		natom=0
		do i=1, ntype
			natom = natom + types(i)
		end do
		read(fid1,'(a)') dynam
		!-------------------------------------------------------------------------
		dynam=trim(dynam)
		a_dyn=StrLowCase(dynam)
		if (a_dyn(1:1)/="s") then
			coord_type=dynam
		else
			read(fid1,'(a)') coord_type
		end if
		!-------------------------------------------------------------------------
		read(fid1,'(a)') line
		backspace(fid1)
		npos=number_of_words(line)
		if (npos/=6) npos=3
		allocate(x(3,natom))
		!-------------------------------------------------------------------------
		if (npos==6) then
			allocate(fix(3,natom))
			do i=1, natom
				read(fid1,'(a)') line
				read(line,*) (x(j,i),j=1,3), (fix(j,i),j=1,3)
			end do
		end if
		!-------------------------------------------------------------------------
	close(fid1)
	!----------------------------------------------------------------------------
	open(fid2, file="OUTCAR", status="old")
		!-------------------------------------------------------------------------
		if (.not. allocated(symbols)) then
			allocate(symbols(ntype))
			do i=1, ntype
				scanok=search_word_free(fid2, "VRHFIN", line)
				tmp = line( index(line,"=")+1 : index(line,":")-1 )
				symbols(i) = trim(tmp)
			end do
		end if
		rewind(fid2)
		!-------------------------------------------------------------------------
		scanok=search_word_free_last(fid2,"VOLUME and BASIS-vectors are now",line)
		scanok=search_word_free_last(fid2,"direct lattice vectors",line)
		do i=1, 3
			read(fid2,*) a1(:,i)
		end do
		!-------------------------------------------------------------------------
		r=0.D0
		do i=1, 3
			do j=1, 3
				r(i) = r(i) + a1(j,i)**2
			end do
			r(i) = sqrt(r(i))
		end do
		!-------------------------------------------------------------------------
		if(iargc()==1) then
			ncycle=number_of_lines_word(fid2,"POSITION")
			if (icycle>0 .and. icycle<=ncycle) then
				scanok=search_word_free_n(fid2,icycle,"POSITION",line)
			else
				write(*,*) "icycle =", icycle
				write(*,*) "ncycle =", ncycle
				write(*,*) "icycle should less than or equal to ncycle"
				write(*,*) "Stop"
				stop
			end if
		else
			scanok=search_word_free_last(fid2,"POSITION",line)
		end if
		!-------------------------------------------------------------------------
		if (scanok) then
			read(fid2,'(a)') line
			do i=1, natom
				read(fid2,*) x(:,i)
			end do
		else
			rewind(fid2)
			scanok=search_word_free_last(fid2,"position of ions in cartesian coordinates  (Angst)",line)
			do i=1, natom
				read(fid2,*) x(:,i)
			end do
		end if
		!-------------------------------------------------------------------------
		coord_type=StrLowCase(coord_type)
		if (trim(StrLowCase(coord_type))=="direct") then
			inva=inverse3(a1)
			call rotn(natom,inva,x)
		end if
		!-------------------------------------------------------------------------
		alat=r(1)
		a=a1/alat
		!-------------------------------------------------------------------------
	close(fid2)
	!----------------------------------------------------------------------------
	open(fid1, file="POSCAR")
		!-------------------------------------------------------------------------
		write(fmt,*) len(trim(title))
		write(fid1,'(a'//trim(fmt)//')') trim(title)
		write(fid1,'(f15.10)') alat
		write(fid1,'(3f15.10)') a(:,1)
		write(fid1,'(3f15.10)') a(:,2)
		write(fid1,'(3f15.10)') a(:,3)
		write(fmt,*) ntype
		write(fid1,'('//trim(fmt)//'a5)') (trim(symbols(i)), i=1, ntype)
		write(fid1,'('//trim(fmt)//'i5)') types
		write(fmt,*) len(trim(dynam))
		if (a_dyn(1:1)=="s") then
			write(fid1,'(a'//trim(fmt)//')') trim(dynam)
		end if
		write(fmt,*) len(trim(coord_type))
		write(fid1,'(a'//trim(fmt)//')') trim(coord_type)
		if      (npos==3) then
			do i=1, natom
				write(fid1,'(3f12.7)') x(:,i)
			end do
		else if (npos==6) then
			do i=1, natom
				write(fid1,'(3f12.7, 3a4)') (x(j,i), j=1, 3), (fix(j,i), j=1, 3)
			end do
		end if
	close(fid1)
	!----------------------------------------------------------------------------
	if (allocated(x  )) deallocate(x  )
	if (allocated(fix)) deallocate(fix)
	!----------------------------------------------------------------------------
	stop
end
