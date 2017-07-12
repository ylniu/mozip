program poscar_move
	use kinds, only: DP
	use String_Utility, only: StrLowCase
	use math,           only: inverse3
	implicit none
	!---------------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: i, j, k, natom
	character(200)              :: fout, line, tmp, keyword, coord_type, xtype, cx(3)
	integer                     :: ntype
	integer       , allocatable :: ions_per_type(:)
	character(1)  , allocatable :: fix(:,:)
	character(2)  , allocatable :: ions_per_symbol(:)
	character(2)  , allocatable :: symbol(:)
	real(DP)                    :: a(3,3), alat, disp(3), b(3,3)
	real(DP)      , allocatable :: x(:,:)
	character(100), allocatable :: masses(:)
	logical                     :: scanok
	integer       , external    :: number_of_lines_word
	integer       , external    :: number_of_words
	logical       , external    :: search_word_free
	logical       , external    :: search_word_del_space
	logical       , external    :: search_word_back_free
	!---------------------------------------------------------------------------------
	fid=1
	fout="POSCAR.cif"
	xtype=''
	do i=1, 3
		call getarg(i, cx(i))
		read(cx(i),*) disp(i)
	end do
	
	open(fid, file="POSCAR", status="old")
		read(fid,'(a)') line
		read(fid,*) alat
		read(fid,*) a(:,1)
		read(fid,*) a(:,2)
		read(fid,*) a(:,3)
		a=a*alat
		read(fid,'(a)') line
		ntype=number_of_words(line)
		allocate(ions_per_type  (ntype))
		allocate(ions_per_symbol(ntype))
		allocate(masses(ntype))
		read(line,*) ions_per_type
		natom=0
		do i=1, ntype
			natom = natom + ions_per_type(i)
		end do
		read(fid,'(a)') tmp
		tmp=StrLowCase(tmp)
		if (trim(tmp)=="direct") then
			coord_type="direct"
		else
			read(fid,'(a)') coord_type
		end if
		allocate(x(3,natom))
		allocate(symbol(natom))
		allocate(fix(3,natom))
		!-------------------------------------------------------------------------
		do i=1, natom
			read(fid,*) x(:,i), fix(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	coord_type=StrLowCase(coord_type)
	b=inverse3(a)
	if (trim(coord_type)=="direct") then
		!call rotn(natom, a, x)
	end if
	!----------------------------------------------------------------------------
	open(fid, file="INCAR", status="old")
		scanok=search_word_free(fid,"POMASS",line)
		read(line,*) tmp, tmp, masses
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file="POTCAR", status="old")
		do i=1, ntype
			write(keyword,*) "POMASS="//trim(masses(i))
			rewind(fid)
			scanok=search_word_del_space(fid,keyword,line)
			scanok=search_word_back_free(fid,"VRHFIN",line)
			j=index(line,"=")+1
			k=index(line,":")-1
			ions_per_symbol(i)=line(j:k)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	k=0
	do i=1, ntype
		do j=1, ions_per_type(i)
			k=k+1
			symbol(k)=ions_per_symbol(i)
		end do
	end do
	!----------------------------------------------------------------------------
	do i=1, natom
		x(:,i) = x(:,i) + disp
	end do
	!----------------------------------------------------------------------------
	!call rotn(natom, b, x)
	!----------------------------------------------------------------------------
	do i=1, natom
		write(*,'(3f12.7, 3a4)') x(:,i), fix(:,i)
	end do
	!---------------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(ions_per_type)
	deallocate(ions_per_symbol)
	deallocate(x)
	!---------------------------------------------------------------------------------
	stop
end
