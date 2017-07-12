program outcar2xyz
	use kinds, only: DP
	implicit none
	!---------------------------------------------------------------------------------
	integer                   :: fid
	integer                   :: i, j, natom, iatom, mi
	character(200)            :: fout, line, tmp
	integer                   :: ncycle, icycle, ntype, itype
	integer     , allocatable :: nat(:)
	integer     , allocatable :: ions_per_type(:)
	character(2), allocatable :: ions_per_symbol(:)
	character(2), allocatable :: symbol(:)
	real(DP)                  :: a(3,3), r(3)
	real(DP)    , allocatable :: x(:,:,:), f(:,:,:)
	logical                   :: scanok
	integer     , external    :: number_of_lines_word
	integer     , external    :: number_of_words
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	!---------------------------------------------------------------------------------
	fid=1
	fout="OUTCAR.xyz"
	mi=0
	r=0.D0
	if(iargc()==1) then
		call getarg(1,tmp)
		read(tmp,*) mi
		if      (mi==1) then
			r(1)=0.5D0
		else if (mi==2) then
			r(2)=0.5D0
		else if (mi==3) then
			r(3)=0.5D0
		end if
	end if
	!---------------------------------------------------------------------------------
	open(fid, file="OUTCAR", status="old")
		!------------------------------------------------------------------------------
		! Get the number of cycles
		!
		ncycle=number_of_lines_word(fid,"POSITION")
		!------------------------------------------------------------------------------
		! Get the number of ions
		!
		rewind(fid)
		scanok=search_word_free(fid,"number of ions",line)
		read(line,*) (tmp,i=1,11), natom
		allocate(nat   (natom))
		allocate(symbol(natom))
		!------------------------------------------------------------------------------
		! Get the number of each type of ions
		!
		rewind(fid)
		scanok=search_word_free(fid,"ions per type",line)
		line=line(20:)
		ntype=number_of_words(line)
		allocate(ions_per_type(ntype))
		allocate(ions_per_symbol(ntype))
		read(line,*) ions_per_type
		!------------------------------------------------------------------------------
		! Get the symbol of each type ions
		!
		rewind(fid)
		do i=1, ntype
			scanok=search_word_free(fid,"TITEL  =",line)
			read(line,*) (tmp, j=1, 3), ions_per_symbol(i)
		end do
		!------------------------------------------------------------------------------
		! Get the symbol of all ions
		!
		i=0
		do itype=1, ntype
			do iatom=1, ions_per_type(itype)
				i=i+1
				symbol(i) = ions_per_symbol(itype)
			end do
		end do
		!------------------------------------------------------------------------------
		! Get the positions and forces
		!
		allocate(x(3,natom,ncycle))
		allocate(f(3,natom,ncycle))
		rewind(fid)
		do icycle=1, ncycle
			scanok=search_word_free(fid,"POSITION",line)
			read(fid,*)
			do iatom=1, natom
				read(fid,*) x(:,iatom,icycle), f(:,iatom,icycle)
			end do
		end do
		!------------------------------------------------------------------------------
		! Get Lattice vectors
		!
		rewind(fid)
		scanok=search_word_free(fid,"Lattice vectors",line)
		if (scanok) then
			read(fid,*)
			read(fid,'(7x,3(f15.10,x))') a(:,1)
			read(fid,'(7x,3(f15.10,x))') a(:,2)
			read(fid,'(7x,3(f15.10,x))') a(:,3)
		else
			rewind(fid)
			scanok=search_word_free_last(fid,"direct lattice vectors",line)
			if (scanok) then
				read(fid,*) a(:,1)
				read(fid,*) a(:,2)
				read(fid,*) a(:,3)
			else
				write(*,*) "Can not find lattice vectors, stop!"
				stop
			end if
		end if
		!------------------------------------------------------------------------------
	close(fid)
	!---------------------------------------------------------------------------------
	call symbol_to_nat(natom, symbol, nat)
	!---------------------------------------------------------------------------------
	if (a(1,1)==0.D0 .and. a(3,3)==0.D0) then
		call swapn(3,a(1,1),a(1,3))
		a=-a
	end if
	call rotn(1,a,r)
	!---------------------------------------------------------------------------------
	!call coord_crys_to_cart(1,a,r)
	!do icycle=1, ncycle
		!call coord_crys_to_cart(natom,a,x(1,1,icycle))
		!call coord_crys_to_cart(natom,a,f(1,1,icycle))
	!end do
	!---------------------------------------------------------------------------------
	call write_xyz_n(fout,ncycle,natom,symbol,x)
	!---------------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(ions_per_type)
	deallocate(ions_per_symbol)
	deallocate(x)
	deallocate(f)
	!---------------------------------------------------------------------------------
	stop
end
