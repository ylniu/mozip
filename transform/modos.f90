program modos
	use kinds , only: DP
	use string, only: number_of_chars
	use file,   only: name_main
	use param,  only: au2ev
	implicit none
	!----------------------------------------------------------------------------
	integer                   :: fid, info, ispin, nmo, natom
	integer                   :: nc, i, irecl
	integer                   :: ii, jj, nwords
	real(DP)    , allocatable :: alpha(:), beta(:)
	character(200)            :: fmol, file_type, version, line, tmp, nf
	character(200)            :: fdos, fmos, fmoa, fmob, a1, a2, a3, cmos
	character( 10)            :: unt
	logical                   :: scanok, is_alpha, is_beta
	!----------------------------------------------------------------------------
	integer                   :: nd
	real(DP)                  :: de, FWHM
	real(DP)                  :: ene_min, ene_max
	real(DP)    , allocatable :: ene_alpha(:), ene_beta(:)
	real(DP)    , allocatable :: dos_alpha(:), dos_beta(:)
	real(DP)    , allocatable :: occ_a(:), occ_b(:)
	!----------------------------------------------------------------------------
	integer     , external    :: number_of_words
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	!----------------------------------------------------------------------------
	fid  = 1
	FWHM = 0.1
	de   = 0.01
	!----------------------------------------------------------------------------
	call getarg(1,fmol)
	unt="au"
	if (iargc()>=2) then
		call getarg(2,tmp)
		read(tmp,*) FWHM
		if (iargc()==3) then
			call getarg(3,tmp)
			if (trim(tmp)=="ev") then
				unt="eV"
			end if
		end if
	end if
	fdos=trim(name_main(fmol))//".dos"
	fmos=trim(name_main(fmol))//".mos"
	fmoa=trim(name_main(fmol))//".moa"
	fmob=trim(name_main(fmol))//".mob"
	call qm_file_type(fmol,file_type,version,info)
	call qm_file_natom(fmol,natom,info)
	if (file_type=="GAUSSIAN LOG") then
		open(fid, file=fmol, status="old")
			!----------------------------------------------------------------------
			scanok=search_word_free(fid,"NBasis=",line)
			read(line,*) tmp, nmo
			!----------------------------------------------------------------------
			allocate(alpha(nmo))
			allocate(beta (nmo))
			allocate(occ_a(nmo))
			allocate(occ_b(nmo))
			occ_a = 0.0_DP
			occ_b = 0.0_DP
			alpha = 0.0_DP
			beta  = 0.0_DP
			!----------------------------------------------------------------------
			rewind(fid)
			if(search_word_free(fid,"Beta  occ",line)) then
				ispin=2
			else
				ispin=1
				rewind(fid)
			end if
			!----------------------------------------------------------------------
			rewind(fid)
			if (natom>=2) then
				scanok=search_word_free_last(fid,"The electronic state",line)
				is_alpha=.true.
				is_beta =.false.
				read(fid,'(a)') line
				!-------------------------------------------------------------------
				i=1
				do while(is_alpha)
					line = line(29:)
					nc   = number_of_chars(trim(line),".")
					write(nf,*) nc
					read(line,'('//trim(adjustl(nf))//'f10.5)') alpha(i:i+nc-1)
					!----------------------------------------------------------------
					i=i+nc
					!----------------------------------------------------------------
					read(fid,'(a)') line
					read(line,*) tmp
					if (trim(tmp)=="Beta") then
						is_beta  = .true.
						is_alpha = .false.
					else if (trim(tmp)/="Alpha" .and. trim(tmp) /= "Beta" ) then
						is_alpha = .false.
					end if
				end do
				!-------------------------------------------------------------------
				if (ispin==1) beta = alpha
				!-------------------------------------------------------------------
				i=1
				do while(is_beta)
					line = line(29:)
					nc   = number_of_chars(trim(line),".")
					write(nf,*) nc
					read(line,'('//trim(adjustl(nf))//'f10.5)') beta(i:i+nc-1)
					!----------------------------------------------------------------
					i=i+nc
					!----------------------------------------------------------------
					read(fid,'(a)') line
					read(line,*) tmp
					if (trim(tmp)/="Beta" ) then
						is_beta = .false.
					end if
				end do
				!-------------------------------------------------------------------
				rewind(fid)
				scanok=search_word_free(fid,"Alpha  occ. eigenvalues",line)
				if (scanok) then
					!----------------------------------------------------------------
					read(line,*) a1, a2, a3
					cmos=line(28:)
					ii=0
					jj=0
					!----------------------------------------------------------------
					do while(trim(a3)=="eigenvalues")
						nwords=number_of_words(cmos)
						if (trim(a1)=="Alpha") then
							read(cmos,*) alpha(ii+1:ii+nwords)
							if (trim(a2)=="occ.") then
								occ_a(ii+1:ii+nwords)=1.0_DP
							else if (trim(a2)=="virt.") then
								occ_a(ii+1:ii+nwords)=0.0_DP
							end if
							ii = ii + nwords
						else if (trim(a1)=="Beta") then
							read(cmos,*) beta(jj+1:jj+nwords)
							if (trim(a2)=="occ.") then
								occ_b(jj+1:jj+nwords)=1.0_DP
							else if (trim(a2)=="virt.") then
								occ_b(jj+1:jj+nwords)=0.0_DP
							end if
							jj = jj + nwords
						end if
						!-------------------------------------------------------------
						read(fid,'(a)') line
						read(line,*) a1, a2, a3
						cmos=line(28:)
						!-------------------------------------------------------------
					end do
				end if
			else if (natom==1) then
				scanok=search_word_free(fid,"Alpha  occ. eigenvalues",line)
				if (scanok) then
					!----------------------------------------------------------------
					read(line,*) a1, a2, a3
					cmos=line(28:)
					ii=0
					jj=0
					!----------------------------------------------------------------
					do while(trim(a3)=="eigenvalues")
						nwords=number_of_words(cmos)
						if (trim(a1)=="Alpha") then
							read(cmos,*) alpha(ii+1:ii+nwords)
							if (trim(a2)=="occ.") then
								occ_a(ii+1:ii+nwords)=1.0_DP
							else if (trim(a2)=="virt.") then
								occ_a(ii+1:ii+nwords)=0.0_DP
							end if
							ii = ii + nwords
						else if (trim(a1)=="Beta") then
							read(cmos,*) beta(jj+1:jj+nwords)
							if (trim(a2)=="occ.") then
								occ_b(jj+1:jj+nwords)=1.0_DP
							else if (trim(a2)=="virt.") then
								occ_b(jj+1:jj+nwords)=0.0_DP
							end if
							jj = jj + nwords
						end if
						!-------------------------------------------------------------
						read(fid,'(a)') line
						read(line,*) a1, a2, a3
						cmos=line(28:)
						!-------------------------------------------------------------
					end do
				end if
			end if
		close(fid)
		!-------------------------------------------------------------------------
	end if
	if (trim(unt)=="eV") then
		alpha = alpha * au2ev
		beta  = beta  * au2ev
		FWHM  = FWHM  * au2ev
	end if
	!----------------------------------------------------------------------------
	if (occ_b(1)==0.0_DP) then
		beta  = alpha
		occ_b = occ_a
	end if
	!----------------------------------------------------------------------------
	ene_min=min(alpha(1  ), beta(1  )) - 10 * FWHM
	ene_max=max(beta (nmo), beta(nmo)) + 10 * FWHM
	!----------------------------------------------------------------------------
	nd=nint((ene_max-ene_min)/de) + 1
	allocate(ene_alpha(nd))
	allocate(dos_alpha(nd))
	allocate(ene_beta (nd))
	allocate(dos_beta (nd))
	!----------------------------------------------------------------------------
	call get_dos_fix(nmo, alpha, ene_min, FWHM, nd, ene_alpha, dos_alpha, de)
	call get_dos_fix(nmo, beta , ene_min, FWHM, nd, ene_beta , dos_beta , de)
	!----------------------------------------------------------------------------
	open(fid, file=fdos)
		write(fid, '(4a15)') "ene_alpha", "dos_alpha", "ene_beta", "dos_beta"
		do i=1, nd
			write(fid, '(2(f15.5,es15.5))') ene_alpha(i), dos_alpha(i), ene_beta(i), dos_beta(i)
		end do 
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fmos)
		write(fid, '(a6, 6a18)') "n", &
			"ene_alpha("//trim(unt)//")", "occ_alpha_e", "occ_alpha_h", &
			"ene_beta("//trim(unt)//")" , "occ_beta_e" , "occ_beta_h"
		do i=1, nmo
			write(fid, '(i6, 6f18.7)') i, &
				alpha(i), occ_a(i), 1.0_DP - occ_a(i), &
				beta(i) , occ_b(i), 1.0_DP - occ_b(i)
		end do 
	close(fid)
	!----------------------------------------------------------------------------
	write(nf,*) nmo
	irecl=10+13*nmo
	!----------------------------------------------------------------------------
	write(*,'("Output files: ")')
	write(*,'(4x,a)') trim(fdos)
	write(*,'(4x,a)') trim(fmos)
	!----------------------------------------------------------------------------
	deallocate(alpha    )
	deallocate(beta     )
	deallocate(ene_alpha)
	deallocate(dos_alpha)
	deallocate(ene_beta )
	deallocate(dos_beta )
	deallocate(occ_a    )
	deallocate(occ_b    )
	!----------------------------------------------------------------------------
	stop
end
