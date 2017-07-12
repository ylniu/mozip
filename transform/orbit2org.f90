program oribt2org
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	integer                     :: fid, info
	integer                     :: natom, na, nb, norb, irecl, jatom
	integer       , allocatable :: iat(:)
	character(  2), allocatable :: isymbol(:)
	character( 10), allocatable :: bas_name(:)
	real(DP)                    :: eps
	real(DP)      , allocatable :: ene_a(:)    , ene_b(:)
	real(DP)      , allocatable :: coeff_a(:,:), coeff_b(:,:)
	real(DP)      , allocatable :: tcoeff_a(:,:), tcoeff_b(:,:)
	character( 10), allocatable :: sym_a(:)    , sym_b(:)
	!----------------------------------------------------------------------------
	character(200)              :: finp, fout, fmt, tmp
	!----------------------------------------------------------------------------
	fid=1
	i=iargc()
	eps=0.3D0
	jatom=16
	call getarg(1, finp)
	if (i==2) then
		call getarg(2, tmp)
		read(tmp,*) eps
	end if
	if (i==3) then
		call getarg(2, tmp)
		read(tmp,*) eps
		call getarg(3, tmp)
		read(tmp,*) jatom
	end if
	call qm_file_natom(finp, natom, info)
	call qm_file_nbasis(finp, na, nb, norb, info)
	allocate(ene_a(norb))
	allocate(ene_b(norb))
	allocate(sym_a(norb))
	allocate(sym_b(norb))
	allocate(iat  (norb))
	allocate(isymbol(norb))
	allocate(bas_name(norb))
	allocate(coeff_a(norb,norb))
	allocate(coeff_b(norb,norb))
	allocate(tcoeff_a(natom, norb))
	allocate(tcoeff_b(natom, norb))
	!----------------------------------------------------------------------------
	call qm_file_basis(finp, norb, ene_a, ene_b, sym_a, sym_b, coeff_a, coeff_b, iat, isymbol, bas_name, cinfo)
	!----------------------------------------------------------------------------
	tcoeff_a=0.D0
	tcoeff_b=0.D0
	do i=1, norb
		do j=1, norb
			if ( abs(tcoeff_a(iat(j),i)) < abs(coeff_a(j,i)) ) then
				tcoeff_a(iat(j),i) = coeff_a(j,i)
			end if
			if ( abs(tcoeff_b(iat(j),i)) < abs(coeff_b(j,i)) ) then
				tcoeff_b(iat(j),i) = coeff_b(j,i)
			end if
			!tcoeff_a(iat(j),i) = tcoeff_a(iat(j),i) + coeff_a(j,i)**2.D0
			!tcoeff_b(iat(j),i) = tcoeff_b(iat(j),i) + coeff_b(j,i)**2.D0
		end do
	end do
	jatom=min(jatom, natom)
	!----------------------------------------------------------------------------
	fout="coeffs.dat"
	open(fid, file=fout)
		!-------------------------------------------------------------------------
		write(fid,'("Alpha Orbital Coefficients, eps=", f10.5)') eps
		do i=1, na
			write(fid, '("MO a", 2x, i6, f12.5)') i, ene_a(i)
			do j=1, norb
				if(abs(coeff_a(j,i))>=eps) then
					write(fid,'("a", 3i6, a6, a12, f12.5)') i, j, iat(j), isymbol(j), bas_name(j), coeff_a(j,i)
				end if
			end do
		end do
		!-------------------------------------------------------------------------
		write(fid,'("Beta Orbital Coefficients, eps=", f10.5)') eps
		do i=1, nb
			write(fid, '("MO b", 2x, i6, f12.5)') i, ene_b(i)
			do j=1, norb
				if(abs(coeff_b(j,i))>=eps) then
					write(fid,'("b", 3i6, a6, a12, f12.5)') i, j, iat(j), isymbol(j), bas_name(j), coeff_b(j,i)
				end if
			end do
		end do
		!-------------------------------------------------------------------------
	close(fid)
	!----------------------------------------------------------------------------
	fout="coeff.dat"
	open(fid, file=fout)
		write(fid,'(a6, a12, 2a12)') "n", "sym", "tcoeff", "ene"
		do i=norb, 1, -1
			if (tcoeff_a(jatom,i)>eps) then
				write(fid,'(i6, a12, 2f12.5)') i, sym_a(i), tcoeff_a(jatom,i), ene_a(i)
			end if
		end do
		!-------------------------------------------------------------------------
		write(fid,*)
		write(fid,'(a6, a12, 2a12)') "n", "sym", "tcoeff", "ene"
		do i=norb, 1, -1
			if (tcoeff_b(jatom,i)>eps) then
				write(fid,'(i6, a12, 2f12.5)') i, sym_b(i), tcoeff_b(jatom,i), ene_b(i)
			end if
		end do
	close(fid)
	!----------------------------------------------------------------------------
	fout=trim(name_main(finp))//".dat"
	!----------------------------------------------------------------------------
	fout="orbit_a.dat"
	irecl=norb * 12 + 12
	write(fmt,*) norb
	open(fid, file=fout, recl=irecl)
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 1, ene_a
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 2, ene_a
	close(fid)
	!----------------------------------------------------------------------------
	fout="orbit_b.dat"
	irecl=norb * 12 + 12
	write(fmt,*) norb
	open(fid, file=fout, recl=irecl)
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 3, ene_b
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 4, ene_b
	close(fid)
	!----------------------------------------------------------------------------
	fout="orbit_c.dat"
	irecl=norb * 12 * 2 + 12
	write(fmt,*) norb * 2
	open(fid, file=fout, recl=irecl)
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 5, ene_a, ene_b
		write(fid, '(i2, '//trim(fmt)//'f12.5)') 6, ene_a, ene_b
	close(fid)
	!----------------------------------------------------------------------------
	fout="orbit_n.dat"
	open(fid, file=fout)
		write(fid,'(a6, 2(a10,2x), 2a12)') &
			"n", "Sym(alpha)", "Sym(beta)", "E(alpha)", "E(beta)"
		do i=norb, 1, -1
			write(fid,'(i6, 2(a10, 2x), 2f12.5)') &
				i, adjustr(sym_a(i)), adjustr(sym_b(i)), ene_a(i), ene_b(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(ene_a)
	deallocate(ene_b)
	deallocate(sym_a)
	deallocate(sym_b)
	deallocate(iat  )
	deallocate(isymbol)
	deallocate(bas_name)
	deallocate(coeff_a)
	deallocate(coeff_b)
	deallocate(tcoeff_a)
	deallocate(tcoeff_b)
	!----------------------------------------------------------------------------
	stop
end
