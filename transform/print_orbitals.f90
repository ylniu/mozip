program print_orbitals
	use kinds, only: DP
	use file,  only: name_main
	!-----------------------------------------------------------------------------
	integer               :: fid, info, natom, na, nb, norb
	integer               :: ib, ie, i, j, nfmt
	real(DP), allocatable :: mo(:,:,:)
	!-----------------------------------------------------------------------------
	character(200)        :: finp, fout, line, fmt1, fmt2, fmt3, tmp
	!-----------------------------------------------------------------------------
	logical               :: scanok
	logical , external    :: search_word_free_last
	!-----------------------------------------------------------------------------
	nfmt=3
	fmt1="E18.8"
	!-----------------------------------------------------------------------------
	call getarg(1, finp)
	!-----------------------------------------------------------------------------
	i=iargc()
	!-----------------------------------------------------------------------------
	if (i==2) then
		call getarg(2, line)
		read(line,*) j
		if (j >=1 .and. j<=10) nfmt=j
	end if
	!-----------------------------------------------------------------------------
	fid=1
	fout=trim(name_main(finp))//".mos"
	!-----------------------------------------------------------------------------
	call qm_file_natom (finp, natom       , info)
	call qm_file_nbasis(finp, na, nb, norb, info)
	!-----------------------------------------------------------------------------
	nfmt=min(nfmt,norb)
	!-----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
	!-----------------------------------------------------------------------------
	allocate(mo(norb, norb, 2))
	!-----------------------------------------------------------------------------
	rewind(fid)
	scanok=search_word_free_last(fid, "Alpha Molecular Orbital Coefficients", line)
	ib = 1
	ie = ib + 4
	ie = min(ie, norb)
	do while(ib <= norb)
		read(fid,*)
		read(fid,*)
		read(fid,*)
		do i=1, norb
			read(fid,'(a)') line
			line=line(22:)
			read(line,*) (mo(i,j,1), j=ib, ie)
		end do
		ib = ie + 1
		ie = ib + 4
		ie = min(ie, norb)
	end do
	scanok=search_word_free_last(fid, "Beta Molecular Orbital Coefficients" , line)
	ib = 1
	ie = ib + 4
	ie = min(ie, norb)
	do while(ib <= norb)
		read(fid,*)
		read(fid,*)
		read(fid,*)
		do i=1, norb
			read(fid,'(a)') line
			line=line(22:)
			read(line,*) (mo(i,j,2), j=ib, ie)
		end do
		ib = ie + 1
		ie = ib + 4
		ie = min(ie, norb)
	end do
	!-----------------------------------------------------------------------------
	close(fid)
	!-----------------------------------------------------------------------------
	write(fmt2,'("(",a,",$)")') trim(fmt1)
	write(fmt3,'("(",i0,a,")")') nfmt,trim(fmt1)
	!
	open(fid, file=fout)
		write(fid,'(2x, "norb = ", i10)') norb
		write(fid,'(2x, "na   = ", i10)') na
		write(fid,'(2x, "nb   = ", i10)') nb
		!--------------------------------------------------------------------------
		write(fid,'(2x, "Alpha oribtals")')
		write(fid,'(2x, a)') trim(fmt3)
		do i=1, norb
			write(fid, '(i5)') i
			do j=1, norb
				write(fid, fmt2) mo(j,i,1)
				if (mod(j,nfmt)==0) write(fid,*)
			end do
			if (nfmt /= norb) write(fid,*)
		end do
		!--------------------------------------------------------------------------
		write(fid,'(2x, "Beta oribtals")')
		write(fid,'(2x, a)') trim(fmt3)
		do i=1, norb
			write(fid, '(i5)') i
			do j=1, norb
				write(fid, fmt2) mo(j,i,2)
				if (mod(j,nfmt)==0) write(fid,*)
			end do
			if (nfmt /= norb) write(fid,*)
		end do
		!--------------------------------------------------------------------------
	close(fid)
	!-----------------------------------------------------------------------------
	deallocate(mo)
	!-----------------------------------------------------------------------------
	stop
end
