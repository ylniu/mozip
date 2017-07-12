program pdb_divide
	use kinds , only: DP
	use string, only: StrUpCase
	use file  , only: name_main
	use string, only: StrLowCase
	use string, only: StrUpCaseFirst
	implicit none
	!------------------------------------------------------------------------------
	integer        :: natom, fid1, fid2, fid3, fid4, iatom
	integer        :: i, j, i1, k, thisi, lasti, jatom, nt
	real(DP)       :: x(3), r(2)
	real(DP)       :: a, b, c, alpha, beta, gamma, aa(3,3)
	!------------------------------------------------------------------------------
	! http://manual.gromacs.org/current/online/gro.html
	!
	! In gromacs, gro file: format:
	!
	! v1(x) v2(y) v3(z) v1(y) v1(z) v2(x) v2(z) v3(x) v3(y)
	!
	! the last 6 values may be omitted (they will be set to zero).
	! Gromacs only supports boxes with v1(y)=v1(z)=v2(z)=0.
	!
	real(DP)       :: v1(3), v2(3), v3(3)
	!------------------------------------------------------------------------------
	character(  7) :: symbol
	character(200) :: fpdb, mol_name, line, tmp, fout, posfix, name1, name2, lastname
	character(200) :: cmd, fout3, fxyz, sym
	!------------------------------------------------------------------------------
	fid1=1
	fid2=2
	fid3=3
	fid4=4
	!------------------------------------------------------------------------------
	call getarg(1, fpdb)
	call getarg(2, mol_name)
	call getarg(3, tmp)
	read(tmp, *) natom
	!------------------------------------------------------------------------------
	fout="new.pdb"
	fout3=trim(name_main(fpdb))//".gro"
	fxyz=trim(name_main(fpdb))//".xyz"
	!------------------------------------------------------------------------------
	open(fid1, file=fpdb, status="old", position="append")
		backspace(fid1)
		backspace(fid1)
		read(fid1, *) tmp, nt
		rewind(fid1)
		read(fid1,*)
		read(fid1,*)
		read(fid1,*) tmp, a, b, c, alpha, beta, gamma
		call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
		!---------------------------------------------------------------------------
		! Angstrom -> nm
		!
		v1 = aa(:,1)/10.D0
		v2 = aa(:,2)/10.D0
		v3 = aa(:,3)/10.D0
	close(fid1)
	!------------------------------------------------------------------------------
	open(fid1, file=fpdb, status="old")
	open(fid2, file=fout)
	open(fid3, file=fout3)
	open(fid4, file=fxyz)
	!------------------------------------------------------------------------------
	write(fid3,'("Title")')
	write(fid3,'(i0)') nt
	!------------------------------------------------------------------------------
	write(fid4,'(i0)') nt
	write(fid4,'("Title")')
	!------------------------------------------------------------------------------
	i=0
	j=1
	k=0
	lastname="none"
	lasti=1
	do while (.true.)
		read(fid1, '(a)') line
		read(line,*) tmp
		if (trim(tmp)=="TER" .or. trim(tmp)=="END") then
			write(fid2,'(a)') "END"
			exit
		else if (trim(tmp)=="ATOM") then
			read(line,'(4x, i7, x, a5, a3, 6x, 4x, 3f8.3, 2f6.2, 10x, a6)') &
				& iatom, symbol, name1, x, r, posfix
			!---------------------------------------------------------------------
			jatom=mod(iatom,natom)
			if(jatom==1) i=i+1
			!---------------------------------------------------------------------
			symbol=StrUpCase(symbol)
			do i1=1, len(trim(symbol))
				if (iachar(symbol(i1:i1))>=48 .and. iachar(symbol(i1:i1))<=57 ) then
					symbol(i1:i1)=" "
				end if
			end do
			!---------------------------------------------------------------------
			name2=trim(symbol)
			sym=adjustl(trim(symbol))
			sym=StrLowCase(sym)
			sym=StrUpCaseFirst(sym)
			if (trim(name2)/=trim(lastname).or. jatom==1 .or. natom==1) then
				j=1
			else
				j=j+1
			end if
			lastname=trim(name2)
			name1=trim(mol_name)
			if(mod(iatom-1, natom)==0) k = k + 1
			write(symbol, '(a,i0)') trim(name2), j
			write(fid2,'("ATOM", i7, 2x, a4, a3, x, a1, i4, 4x, 3f8.3, 2f6.2, 10x, a6)') &
				& iatom, adjustl(symbol), trim(name1), "X", i, x, r, adjustl(posfix)
			write(fid3,'(i5,a3,a7, i5, 3f8.3,i20)') k, adjustl(trim(name1)), &
				& adjustr(symbol), iatom, x/10.D0, natom
			write(fid4,'(a2, 3f8.3)') trim(adjustl(sym)), x
		else
			write(fid2,'(a)') trim(line)
		end if
	end do
	cmd="mv "//trim(fout)//" "//trim(fpdb)
	call system(cmd)
	write(fid3,'(9f10.5)') v1(1), v2(2), v3(3), v1(2), v1(3), v2(1), v2(3), v3(1), v3(2)
	write(*,'("Update", 2x, a,"; Generate ", 2x, 2(a, 2x))') trim(fpdb), trim(fout3), trim(fxyz)
	!------------------------------------------------------------------------------
	close(fid1)
	close(fid2)
	close(fid3)
	close(fid4)
	!------------------------------------------------------------------------------
	stop
end
