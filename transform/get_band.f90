program get_band
	use kinds, only: DP
	use file , only: name_main
	implicit none
	!---------------------------------------------------------------------------
	integer               :: nbnd, nks
	integer               :: i, ibnd, iks, irecl
	real(DP), allocatable :: ene(:,:), kp(:,:), wk(:), kr(:)
	real(DP)              :: efermi, r(3)
	character(100)        :: fscf, fband, fout, fout1, fout2
	character(200)        :: line, tmp
	integer               :: fid
	logical               :: scanok
	logical, external     :: search_word_free
	integer, external     :: number_of_lines
	integer, external     :: number_of_words
	character(20)         :: file_type, version
	integer               :: info, ia
	character(100)        :: fmt
	!---------------------------------------------------------------------------
	namelist /plot/ nbnd, nks
	!---------------------------------------------------------------------------
	ia = iargc()
	!---------------------------------------------------------------------------
	call getarg(1,fscf)
	if (ia==2) then
		call getarg(2,fband)
	else
		fband=trim(fscf)
	end if
	call qm_file_type(fscf, file_type, version, info)
	!---------------------------------------------------------------------------
	fid   = 1
	fout  = trim(file_type)//"-bands-org.dat"
	fout2 = trim(file_type)//"-bands-org-sub-fermi.dat"
	fout1 = trim(file_type)//"-bands-all.dat"
	!---------------------------------------------------------------------------
	if (trim(file_type)=="PWOUT") then
		!------------------------------------------------------------------------
		! Read Fermi Energy
		!
		open(fid,file=fscf, status="old")
			scanok=search_word_free(fid, "the Fermi energy is", line)
			read(line,*) (tmp, i=1, 4), efermi
		close(fid)
		!------------------------------------------------------------------------
		open(fid, file=fband, status="old")
			!---------------------------------------------------------------------
			! Read nks
			!
			scanok=search_word_free(fid,"number of k points=", line)
			read(line,*) (tmp, i=1, 4), nks
			!---------------------------------------------------------------------
			! Get nbnd
			!
			rewind(fid)
			scanok=search_word_free(fid,"End of band structure calculation", line)
			do i=1, 4
				read(fid,'(a)') line
			end do
			nbnd=0
			do while ( len(trim(line)) > 0 )
				nbnd=nbnd+number_of_words(line)
				read(fid,'(a)') line
			end do
			!---------------------------------------------------------------------
			! Allocate variables
			!
			allocate(kp (3   , nks))
			allocate(kr (      nks))
			allocate(ene(nbnd, nks))
			allocate(wk (      nks))
			!---------------------------------------------------------------------
			! Get coordinates of k points
			!
			rewind(fid)
			scanok=search_word_free(fid,"number of k points=", line)
			scanok=search_word_free(fid,"cart. coord.", line)
			do iks=1, nks
				read(fid,'(20x,3(f12.7),7x,f12.7)') kp(:,iks),wk(iks)
			end do
			!---------------------------------------------------------------------
			! Read bands
			!
			rewind(fid)
			scanok=search_word_free(fid,"End of band structure calculation", line)
			do iks=1, nks
				do i=1, 3
					read(fid,'(a)') line
				end do
				read(fid,*) ene(:,iks)
			end do
		close(fid)
	else if(trim(file_type)=="VASP") then
		!------------------------------------------------------------------------
		! Read Fermi Energy
		!
		open(fid,file=fscf, status="old")
			scanok=search_word_free(fid, "E-fermi", line)
			read(line,*) (tmp, i=1, 2), efermi
		close(fid)
		!------------------------------------------------------------------------
		open(fid, file=fband, status="old")
			!---------------------------------------------------------------------
			! Get nbnd
			!
			scanok=search_word_free(fid,"Dimension of arrays",line)
			scanok=search_word_free(fid,"NBANDS",line)
			i=index(line,"NBANDS")
			line=line(i+7:)
			read(line,*) nbnd
			!---------------------------------------------------------------------
			! Get nks
			!
			rewind(fid)
			scanok=search_word_free(fid,"k-points in units of",line)
			read(fid,'(a)') line
			nks=0
			do while (len(trim(line))>0)
				nks=nks+1
				read(fid,'(a)') line
			end do
			!------------------------------------------------------------------------
			! Allocate variables
			!
			allocate(kp (3   , nks))
			allocate(kr (      nks))
			allocate(ene(nbnd, nks))
			allocate(wk (      nks))
			!------------------------------------------------------------------------
			! Get coordinates of k points
			!
			rewind(fid)
			scanok=search_word_free(fid,"k-points in units of",line)
			do iks=1, nks
				read(fid,*) kp(:,iks), wk(iks)
			end do
			!------------------------------------------------------------------------
			! Read bands
			!
			rewind(fid)
			scanok=search_word_free(fid, "E-fermi", line)
			do iks=1, nks
				scanok=search_word_free(fid, "band No.  band energies", line)
				do ibnd=1, nbnd
					read(fid,*) tmp, ene(ibnd,iks)
				end do
			end do
			!---------------------------------------------------------------------
		close(fid)
	else if (trim(file_type)=="WIEN2K_spe") then
		open(fid, file=fscf, status="old")
			read(fid, '(a)') line
			nks = 0
			!------------------------------------------------------------------------
			do while(.true.)
				read(fid, *) line
				if (trim(line)/="bandindex:") then
					nks = nks + 1
				else
					exit
				end if
			end do
			!------------------------------------------------------------------------
			rewind(fid)
			i=number_of_lines(fid)
			nbnd=i/(nks+1)
			rewind(fid)
			!------------------------------------------------------------------------
			! Allocate variables
			!
			allocate(kp (3   , nks))
			allocate(kr (      nks))
			allocate(ene(nbnd, nks))
			allocate(wk (      nks))
			!------------------------------------------------------------------------
			rewind(fid)
			do ibnd=1, nbnd
				read(fid,*)
				do iks=1, nks
					read(fid,'(5f10.5)') kp(:,iks), kr(iks), ene(ibnd, iks)
				end do
			end do
			!------------------------------------------------------------------------
		close(fid)
	else
		stop "Unknown file type, stop!"
	end if
	!---------------------------------------------------------------------------
	! Get kr
	!
	do iks=1, nks
		if (iks==1) then
			kr(iks) = 0.D0
		else
			r=kp(:,iks) - kp(:,iks-1)
			kr(iks) = kr(iks-1) + sqrt( dot_product(r,r) )
		end if
	end do
	!---------------------------------------------------------------------------
	irecl=6+nbnd*9+48
	write(fmt,*) nbnd
	open(fid, file=fout, recl=irecl)
		write(fid, '(a6, 4a12, '//trim(fmt)//'i9)') "ik","kr", "kx", "ky", "kz", (i, i=1, nbnd)
		do iks=1, nks
			write(fid,'(i6, 4f12.7, '//trim(fmt)//'f9.4)') iks, kr(iks), kp(:,iks), ( ene(ibnd, iks), ibnd=1, nbnd)
		end do
	close(fid)
	!---------------------------------------------------------------------------
	irecl=6+nbnd*9+48
	write(fmt,*) nbnd
	open(fid, file=fout2, recl=irecl)
		write(fid, '(a6, 4a12, '//trim(fmt)//'i9)') "ik","kr", "kx", "ky", "kz", (i, i=1, nbnd)
		do iks=1, nks
			write(fid,'(i6, 4f12.7, '//trim(fmt)//'f9.4)') iks, kr(iks), kp(:,iks), ( ene(ibnd, iks)-efermi, ibnd=1, nbnd)
		end do
	close(fid)
	!---------------------------------------------------------------------------
	open(fid, file=fout1)
		write(fid,'(4a10, 4a20)') "i","ik","ibnd","ene(eV)","ene-efermi(eV)","wk","1"
		do iks=1, nks
			do ibnd=1, nbnd
				i=(iks-1) * nbnd + ibnd
				write(fid,'(3i10, 4es20.10)') &
					i, iks, ibnd, ene(ibnd, iks), ene(ibnd, iks)-efermi, wk(iks), 1.D0
			end do
		end do
	close(fid)
	write(*,*) "Output file: ", trim(fout), " and ", trim(fout1), " and ", trim(fout2)
	!---------------------------------------------------------------------------
	deallocate(kp )
	deallocate(kr )
	deallocate(ene)
	deallocate(wk )
	!---------------------------------------------------------------------------
	stop
end
