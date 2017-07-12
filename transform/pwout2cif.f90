program pwout2cif
	use kinds, only: DP
	use param, only: au2a
	use file , only: num_file_lines_word, name_main
	use math , only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, info, natom, ncycle, iat, im, nparam
	integer                     :: i, j
	real(DP)                    :: alat, r(3), a(3,3), celldm(6), inva(3,3)
	real(DP)      , allocatable :: x(:,:)
	character(200)              :: fout, fcif, tmp
	character(200)              :: line, ctype
	character(  2), allocatable :: symbol(:)
	logical                     :: scanok
	logical       , external    :: search_word_free_last
	logical       , external    :: search_word_free
	!----------------------------------------------------------------------------
	im    = 0
	r     = 0.D0
	ctype = "none"
	!----------------------------------------------------------------------------
	nparam=iargc()
	call getarg(1, fout)
	fid=1
	fcif=trim(name_main(fout))//".cif"
	!----------------------------------------------------------------------------
	if (iargc()==2) then
		call getarg(2, line)
		read(line,*) ctype
		if (trim(ctype)=="1"   ) r(1)=0.5D0
		if (trim(ctype)=="2"   ) r(2)=0.5D0
		if (trim(ctype)=="3"   ) r(3)=0.5D0
	end if
	!----------------------------------------------------------------------------
	call qm_file_natom(fout, natom, info)
	ncycle=num_file_lines_word(fout,"CELL_PARAMETERS")
	!----------------------------------------------------------------------------
	allocate(x     (3,natom))
	allocate(symbol(  natom))
	!----------------------------------------------------------------------------
	! modified by niuyingli
	!
	ctype="init"
	open(fid, file=fout, status="old")
		if (trim(ctype)=="init") then
			scanok=search_word_free(fid, "celldm(1)", line)
			read(line,*) tmp, celldm(1), tmp, celldm(2), tmp, celldm(3)
			read(fid,'(a)') line
			read(line,*) tmp, celldm(4), tmp, celldm(5), tmp, celldm(6)
			alat = celldm(1)
			scanok=search_word_free(fid, "crystal axes", line)
			do i=1, 3
				read(fid,*) tmp, tmp, tmp, a(:,i)
			end do
			a=a*alat*au2a
			inva=inverse3(a)
			scanok=search_word_free(fid, "site n.", line)
			do iat=1, natom
				read(fid,*) tmp, symbol(iat), tmp, tmp, tmp, tmp, x(:,iat)
			end do
			x=x*alat*au2a
			call rotn(natom,inva,x)
		else
			scanok=search_word_free_last(fid, "CELL_PARAMETERS", line)
			if (scanok) then
				read(line,'(22x,f12.8)') alat
				read(fid,*) a
				read(fid,*)
				read(fid,*)
				do iat=1, natom
					read(fid,*) symbol(iat), x(:,iat)
				end do
			else
				rewind(fid)
				scanok=search_word_free_last(fid, "celldm(1)=", line)
				read(line,*) tmp, alat
				scanok=search_word_free_last(fid, "crystal axes", line)
				do i=1, 3
					read(fid,*) (tmp, j=1, 3), (a(j,i), j=1, 3)
				end do
				rewind(fid)
				scanok=search_word_free_last(fid, "Crystallographic axes", line)
				scanok=search_word_free_last(fid, "site n.     atom", line)
				do iat=1, natom
					read(fid,*) tmp, symbol(iat), (tmp, j=1, 4), x(:,iat)
				end do
			end if
			a=a*alat*au2a
		end if
	close(fid)
	!----------------------------------------------------------------------------
	do iat=1, natom
		x(:,iat) = x(:,iat) + r
	end do
	call incell(natom, x)
	!----------------------------------------------------------------------------
	call write_cif(fcif, natom, symbol, x, a, "CRYSTAL")
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
