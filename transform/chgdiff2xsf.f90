program chgdiff2xsf
	use kinds, only: DP
	use math,  only: inverse3, volume_v3, area_v2
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: fid, fid1, fid2, fid3, fid4
	integer                      :: ntype1, ntype2, ntype3
	integer                      :: natom1, natom2, natom3, ispin, mz
	integer                      :: i, j, k, ix, iy, iz
	integer                      :: nx1, ny1, nz1
	integer                      :: nx2, ny2, nz2
	integer                      :: nx3, ny3, nz3
	real(DP)                     :: alat, a(3,3), b(3,3), vol, lz
	real(DP)                     :: vc(3)
	real(DP)                     :: area, dz
	integer        , allocatable :: natom_per_type1(:)
	integer        , allocatable :: natom_per_type2(:)
	integer        , allocatable :: natom_per_type3(:)
	real(DP)       , allocatable :: x1(:,:)
	real(DP)       , allocatable :: x2(:,:)
	real(DP)       , allocatable :: x3(:,:)
	real(DP)       , allocatable :: tot_dens(:,:,:,:)
	real(DP)       , allocatable :: z_dens(:,:)
	real(DP)       , allocatable :: spin_dens(:,:,:,:)
	real(DP)       , allocatable :: tmp_dens(:,:,:)
	character(2)   , allocatable :: symbol_per_type1(:), symbol1(:)
	character(2)   , allocatable :: symbol_per_type2(:), symbol2(:)
	character(2)   , allocatable :: symbol_per_type3(:), symbol3(:)
	character(200)               :: line, tmp, finp1, finp2, finp3, fvasp
	character(200)               :: line1, line2, line3, middlez
	integer        , external    :: number_of_words
	logical                      :: scanok
	logical        , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid  = 1
	fid1 = 2
	fid2 = 3
	fid3 = 4
	fid4 = 7
	fvasp= "CHG_diff.vasp"
	!----------------------------------------------------------------------------
	call getarg(1, finp1)
	call getarg(2, finp2)
	call getarg(3, finp3)
	call getarg(4, middlez)
	!----------------------------------------------------------------------------
	open(fid, file="INCAR", status="old")
		ispin=1
		scanok=search_word_free(fid,"ISPIN",line)
		if (scanok) then
			read(line,*) tmp, tmp, ispin
		end if
	close(fid)
	!----------------------------------------------------------------------------
	open(fid1, file=finp1, status="old")
	open(fid2, file=finp2, status="old")
	open(fid3, file=finp3, status="old")
	open(fid4, file=fvasp)
		!
		read (fid1 , '(a)') line1
		read (fid2 , '(a)') line2
		read (fid3 , '(a)') line3
		write(fid4 ,     *) trim(line1)
		!
		read (fid1 , '(a)') line1
		read (fid2 , '(a)') line2
		read (fid3 , '(a)') line3
		read (line1,     *) alat
		write(fid4 ,     *) trim(line1)
		!
		read (fid1 , '(a)') line1
		read (line1,     *) a(:,1)
		write(fid4 ,     *) trim(line1)
		read (fid1 , '(a)') line1
		read (line1,     *) a(:,2)
		write(fid4 ,     *) trim(line1)
		read (fid1 , '(a)') line1
		read (line1,     *) a(:,3)
		write(fid4 ,     *) trim(line1)
		
		read (fid2,*) line2
		read (fid2,*) line2
		read (fid2,*) line2
		read (fid3,*) line3
		read (fid3,*) line3
		read (fid3,*) line3
		!-------------------------------------------------------------------------
		a=a*alat
		vc=a(:,3)
		lz=sqrt( dot_product(vc,vc) )
		b=inverse3(a)
		vol=volume_v3(a)
		area=area_v2(a(1,1), a(1,2))
		read (fid1 , '(a)') line1
		write(fid4 ,     *) trim(line1)
		ntype1=number_of_words(line1)
		read (fid2 , '(a)') line2
		ntype2=number_of_words(line2)
		read (fid3 , '(a)') line3
		ntype3=number_of_words(line3)
		backspace(fid1)
		backspace(fid2)
		backspace(fid3)
		allocate(symbol_per_type1(ntype1))
		allocate(symbol_per_type2(ntype2))
		allocate(symbol_per_type3(ntype3))
		allocate(natom_per_type1 (ntype1))
		allocate(natom_per_type2 (ntype2))
		allocate(natom_per_type3 (ntype3))
		read (fid1,*) symbol_per_type1
		read (fid2,*) symbol_per_type2
		read (fid3,*) symbol_per_type3
		read (fid1,'(a)') line1
		read (line1,   *) natom_per_type1
		write(fid4 ,     *) trim(line1)
		read (fid2,*) natom_per_type2
		read (fid3,*) natom_per_type3
		natom1=0
		do i=1, ntype1
			do j=1, natom_per_type1(i)
				natom1=natom1+1
			end do
		end do
		natom2=0
		do i=1, ntype2
			do j=1, natom_per_type2(i)
				natom2=natom2+1
			end do
		end do
		natom3=0
		do i=1, ntype3
			do j=1, natom_per_type3(i)
				natom3=natom3+1
			end do
		end do
		allocate(symbol1(natom1))
		allocate(symbol2(natom2))
		allocate(symbol3(natom3))
		allocate(x1(3,natom1))
		allocate(x2(3,natom2))
		allocate(x3(3,natom3))
		!-------------------------------------------------------------------------
		k=0
		do i=1, ntype1
			do j=1, natom_per_type1(i)
				k=k+1
				symbol1(k)=symbol_per_type1(i)
			end do
		end do
		!-------------------------------------------------------------------------
		k=0
		do i=1, ntype2
			do j=1, natom_per_type2(i)
				k=k+1
				symbol2(k)=symbol_per_type2(i)
			end do
		end do
		!-------------------------------------------------------------------------
		k=0
		do i=1, ntype3
			do j=1, natom_per_type3(i)
				k=k+1
				symbol3(k)=symbol_per_type3(i)
			end do
		end do
		!-------------------------------------------------------------------------
		read (fid1,*)
		read (fid2,*)
		read (fid3,*)
		write(fid4,*)
		!-------------------------------------------------------------------------
		do i=1, natom1
			read (fid1 , '(a)') line1
			read (line1,     *) x1(:,i)
			write(fid4 ,     *) trim(line1)
		end do
		!-------------------------------------------------------------------------
		do i=1, natom2
			read(fid2,*) x2(:,i)
		end do
		!-------------------------------------------------------------------------
		do i=1, natom3
			read(fid3,*) x3(:,i)
		end do
		!-------------------------------------------------------------------------
		call rotn(natom1,a,x1)
		call rotn(natom2,a,x2)
		call rotn(natom3,a,x3)
		read (fid1 , '(a)') line1
		read (fid1 , '(a)') line1
		read (line1,     *) nx1, ny1, nz1
		write(fid4 ,     *) trim(line1)
		read(fid2,*) nx2, ny2, nz2
		read(fid3,*) nx3, ny3, nz3
		if (      nx1/=nx2 .or. nx1/=nx3 &
			& .or. ny1/=ny2 .or. ny1/=ny3 &
			& .or. nz1/=nz2 .or. nz1/=nz3 ) then
			write(*,*) "Error:"
			write(*,*) "nx, ny, nz"
			write(*,*) nx1, ny1, nz1
			write(*,*) nx2, ny2, nz2
			write(*,*) nx3, ny3, nz3
			stop "Stop!"
		end if
		!-------------------------------------------------------------------------
		allocate(tot_dens(nx1, ny1, nz1,4))
		allocate(z_dens(nz1,5))
		read(fid1,*) (((tot_dens(ix, iy, iz,1), ix=1, nx1), iy=1, ny1), iz=1, nz1)
		read(fid2,*) (((tot_dens(ix, iy, iz,2), ix=1, nx1), iy=1, ny1), iz=1, nz1)
		read(fid3,*) (((tot_dens(ix, iy, iz,3), ix=1, nx1), iy=1, ny1), iz=1, nz1)
		do i=1, 3
			tot_dens(:,:,:,i) = tot_dens(:,:,:,i) / vol
		end do
		if (trim(middlez)=="middlez") then
			mz=nz1/2
			allocate(tmp_dens(nx1, ny1, nz1))
			do i=1, natom1
				x1(3,i) = x1(3,i) + real(mz,DP) / nz1 * lz
			end do
			do i=1, natom2
				x2(3,i) = x2(3,i) + real(mz,DP) / nz1 * lz
			end do
			do i=1, natom3
				x3(3,i) = x3(3,i) + real(mz,DP) / nz1 * lz
			end do
			do i=1, 3
				tmp_dens(:,:,nz1-mz+1:nz1   ) = tot_dens(:,:,   1:mz ,i)
				tmp_dens(:,:,       1:nz1-mz) = tot_dens(:,:,mz+1:nz1,i)
				tot_dens(:,:,:,i) = tmp_dens
			end do
		end if
		z_dens=0.D0
		do i=1, 3
			do iz=1, nz1
				do ix=1, nx1
					do iy=1, ny1
						z_dens(iz,i) = z_dens(iz,i) + tot_dens(ix,iy,iz,i)
					end do
				end do
			end do
			z_dens(:,i) = z_dens(:,i) / area
		end do
		tot_dens(:,:,:,4) = tot_dens(:,:,:,1) - tot_dens(:,:,:,2) - tot_dens(:,:,:,3)
		z_dens(:,4) = z_dens(:,1) - z_dens(:,2) - z_dens(:,3)
		z_dens(:,5) = z_dens(:,2) + z_dens(:,3)
		call write_xsf_3dgrid("CHG_diff_tot_den.xsf", &
			& a, natom1, symbol1, x1, nx1, ny1, nz1, tot_dens(1,1,1,4))
		call write_xsf_3dgrid("CHG_1_den.xsf", &
			& a, natom1, symbol1, x1, nx1, ny1, nz1, tot_dens(1,1,1,1))
		call write_xsf_3dgrid("CHG_2_den.xsf", &
			& a, natom2, symbol2, x2, nx1, ny1, nz1, tot_dens(1,1,1,2))
		call write_xsf_3dgrid("CHG_3_den.xsf", &
			& a, natom3, symbol3, x3, nx1, ny1, nz1, tot_dens(1,1,1,3))
		open(fid, file="z_dens.dat")
			write(fid,'(a15, 5a15)') "z", "z_1", "z_2", "z_3", "z_diff", "z_sum23"
			dz=lz/nz1
			do iz=1, nz1
				write(fid,'(f15.7, 5es15.6)') iz * dz, (z_dens(iz,i), i=1, 5)
			end do
		close(fid)
		deallocate(z_dens)
		deallocate(tot_dens)
		if (ispin==2) then
			allocate(spin_dens(nx1, ny1, nz1,4))
			read(fid1,'(a)') line
			read(fid2,'(a)') line
			read(fid3,'(a)') line
			read(fid1,*) (((spin_dens(ix, iy, iz,1), ix=1, nx1), iy=1, ny1), iz=1, nz1)
			read(fid2,*) (((spin_dens(ix, iy, iz,2), ix=1, nx1), iy=1, ny1), iz=1, nz1)
			read(fid3,*) (((spin_dens(ix, iy, iz,3), ix=1, nx1), iy=1, ny1), iz=1, nz1)
			spin_dens(:,:,:,4) = spin_dens(:,:,:,1) - spin_dens(:,:,:,2) - spin_dens(:,:,:,3)
			spin_dens = spin_dens / vol
			if (trim(middlez)=="middlez") then
				do i=1, 3
					tmp_dens(:,:,nz1-mz+1:nz1   ) = spin_dens(:,:,   1:mz ,i)
					tmp_dens(:,:,       1:nz1-mz) = spin_dens(:,:,mz+1:nz1,i)
					spin_dens(:,:,:,i) = tmp_dens
				end do
			end if
			call write_xsf_3dgrid("CHG_diff_spin_den.xsf", &
				& a, natom1, symbol1, x1, nx1, ny1, nz1, spin_dens(1,1,1,4))
			deallocate(spin_dens)
		end if
	!----------------------------------------------------------------------------
	close(fid4)
	deallocate(symbol_per_type1)
	deallocate(symbol_per_type2)
	deallocate(symbol_per_type3)
	deallocate(natom_per_type1)
	deallocate(natom_per_type2)
	deallocate(natom_per_type3)
	deallocate(symbol1)
	deallocate(symbol2)
	deallocate(symbol3)
	deallocate(x1)
	deallocate(x2)
	deallocate(x3)
	!----------------------------------------------------------------------------
	stop
end
