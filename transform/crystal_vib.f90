program crystal_vib
	use kinds, only: DP
	use param, only: AMU_RY, au2a
	implicit none
	!------------------------------------------------------------------------------
	integer                     :: fid, iat, i, j, ipol, i3, ityp
	integer                     :: natom, n3, ntyp, na3
	character(200)              :: finp, framan, fspec, fxyz, fcif, fxsf, faxsfr, faxsfi
	character(  2), allocatable :: symbol(:), sym_type(:), sym_h(:)
	logical                     :: scanok
	character(200)              :: line, tmp, version
	integer                     :: nf, ibrav
	real(DP)                    :: a, b, c, alpha, beta, gamma
	real(DP)                    :: FWHM, freq_min, freq_max, df
	real(DP)                    :: celldm(6), aa(3,3), alat
	real(DP)                    :: aa_h(3,3)
	real(DP)      , allocatable :: x_h(:,:)
	real(DP)      , allocatable :: v_h(:,:,:)
	real(DP)      , allocatable :: freq_spec(:)
	real(DP)      , allocatable :: x(:,:)
	real(DP)      , allocatable :: x_cry(:,:)
	real(DP)      , allocatable :: x_cry_h(:,:)
	real(DP)      , allocatable :: raman_spec(:)
	real(DP)      , allocatable :: mass_au_type(:)
	real(DP)      , allocatable :: mass_au(:)
	real(DP)      , allocatable :: mass   (:)
	real(DP)      , allocatable :: raman_cart(:,:,:,:)
	real(DP)      , allocatable :: raman_mode(:,:,:)
	real(DP)      , allocatable :: raman_I(:)
	real(DP)      , allocatable :: disp_re(:,:,:)
	real(DP)      , allocatable :: disp_im(:,:,:)
	real(DP)      , allocatable :: freq_cm(:)
	logical                     :: if_set_basis
	logical       , external    :: search_word_free
	!------------------------------------------------------------------------------
	fid=1
	framan= "raman_data.dat"
	fspec = "raman_spec.dat"
	fxyz  = "vib.xyz"
	fcif  = "vib.cif"
	fxsf  = "vib.xsf"
	faxsfr= "vibr.axsf"
	faxsfi= "vibi.axsf"
	if_set_basis = .false.
	call getarg(1, finp)
	open(fid, file=finp, status="old")
		!---------------------------------------------------------------------------
		read(fid,'(a)') line
		read(fid,'(a)') line
		read(fid,'(a)') line
		read(line,*) ntyp, natom, ibrav, celldm
		!---------------------------------------------------------------------------
		n3  = natom * 3
		na3 = natom * 3
		!---------------------------------------------------------------------------
		allocate(mass        (      natom))
		allocate(mass_au     (      natom))
		allocate(mass_au_type(       ntyp))
		allocate(raman_cart  (3,3,3,natom))
		allocate(raman_mode  (3,3,     n3))
		allocate(raman_I     (         n3))
		allocate(x           (3,natom    ))
		allocate(x_cry       (3,natom    ))
		allocate(x_cry_h     (3,na3      ))
		allocate(disp_re     (3,natom, n3))
		allocate(disp_im     (3,natom, n3))
		allocate(freq_cm     (         n3))
		allocate(sym_type    (       ntyp))
		allocate(symbol      (      natom))
		allocate(sym_h       (  na3      ))
		allocate(x_h         (3,na3      ))
		allocate(v_h         (3,na3,   n3))
		!---------------------------------------------------------------------------
		read(fid,'(a)') line
		read(line, *) tmp
		if (trim(tmp)=="Basis") then
			if_set_basis=.true.
			read(fid, *) aa(:,1)
			read(fid, *) aa(:,2)
			read(fid, *) aa(:,3)
			aa = aa * celldm(1) * au2a
		else
			backspace(fid)
		end if
		do ityp=1, ntyp
			read(fid,'(a)') line
			read(line, *) tmp
			tmp=line(16:)
			read(tmp,*) sym_type(ityp)
			tmp=line(21:)
			read(tmp,*) mass_au_type(ityp)
		end do
		!---------------------------------------------------------------------------
		do iat=1, natom
			read(fid,*) tmp, j, x(:,iat)
			symbol (iat) = sym_type    (j)
			mass_au(iat) = mass_au_type(j)
		end do
		!---------------------------------------------------------------------------
		mass = mass_au / AMU_RY
		!---------------------------------------------------------------------------
		scanok=search_word_free(fid,"omega",line)
		if (.not. scanok) then
			rewind(fid)
			scanok=search_word_free(fid,"freq",line)
			version="5.3.0"
		else
			version="5.0.2"
		end if
		backspace(fid)
		do i3=1, n3
			read(fid,'(a)') line
			if (trim(version)=="5.0.2") then
				read(line, '(39x,f15.6)') freq_cm(i3)
			else if (trim(version)=="5.3.0") then
				read(line, '(42x,f15.6)') freq_cm(i3)
			end if
			do iat=1, natom
				read(fid,'(a)') line
				line=line(3:62)
				read(line,*) (disp_re(ipol,iat,i3), disp_im(ipol,iat,i3), ipol=1, 3)
			end do
			call change_phase(natom,disp_re(1,1,i3), disp_im(1,1,i3))
		end do
		!---------------------------------------------------------------------------
	close(fid)
	!---------------------------------------------------------------------------
	FWHM     =    10.0_DP ! cm^-1
	freq_min = -1000.0_DP
	freq_max =  4000.0_DP
	df       =     0.1_DP
	nf       = int((freq_max - freq_min) / df) - 1
	!---------------------------------------------------------------------------
	allocate( freq_spec(nf))
	allocate(raman_spec(nf))
	!------------------------------------------------------------------------------
	do i3=1, n3
		write(*,'(2x, "Freq", i10, f15.6)') i3, freq_cm(i3)
		do iat=1, natom
			write(*,'(i6, 3f15.6)') iat, disp_re(:,iat,i3)
		end do
	end do
	!------------------------------------------------------------------------------
	alat=celldm(1) * au2a
	x = x * alat
	call write_xyz_vib(fxyz,n3,natom,symbol,x,disp_re,freq_cm)
	!------------------------------------------------------------------------------
	x_cry=x
	!------------------------------------------------------------------------------
	if (.not.if_set_basis) then
		call celldm_to_lattice_constants(ibrav,celldm,a,b,c,alpha,beta,gamma)
		call celldm_to_a(ibrav,celldm,aa)
	end if
	write(*,'(2x, "Crystal parameters")')
	do i=1, 3
		write(*,'(2x, "v",i0," = ", 3f15.7)') i, aa(:,i)
	end do
	call coord_cart_to_crys(natom, aa, x_cry)
	write(*,'(2x,"Lattice parameters", 6f15.7)') a, b, c, alpha, beta, gamma
	call a_to_lattice_constants(aa, a, b, c, alpha, beta, gamma)
	!------------------------------------------------------------------------------
	call rhom_to_Hex(n3, natom, aa, aa_h, x, x_h, disp_re, v_h, symbol, sym_h)
	!------------------------------------------------------------------------------
	if (ibrav==5) then
		x_cry_h = x_h
		call coord_cart_to_crys(na3, aa_h, x_cry_h)
		call write_cif(fcif, na3, sym_h, x_cry_h, aa_h, "CRYSTAL")
		call write_xsf(fxsf, aa_h, na3, sym_h, x_h)
		call write_xsf_vib(faxsfr, n3, na3, aa_h, sym_h, x_h, v_h)
		call write_xsf_vib(faxsfi, n3, na3, aa_h, sym_h, x_h, v_h)
	else
		call write_cif(fcif, natom, symbol, x_cry, aa, "CRYSTAL")
		call write_xsf(fxsf, aa, natom, symbol, x)
		call write_xsf_vib(faxsfr, n3, natom, aa, symbol, x, disp_re)
		call write_xsf_vib(faxsfi, n3, natom, aa, symbol, x, disp_im)
	end if
	!------------------------------------------------------------------------------
	write(*,'(2x,"Generate files:", 5(2x,a))') &
		trim(fxyz), trim(fcif), trim(fxsf), trim(faxsfr), trim(faxsfi)
	!------------------------------------------------------------------------------
	deallocate(mass      )
	deallocate(raman_cart)
	deallocate(disp_re   )
	deallocate(disp_im   )
	deallocate(freq_cm   )
	deallocate(x         )
	deallocate(x_cry     )
	!------------------------------------------------------------------------------
	stop
end
