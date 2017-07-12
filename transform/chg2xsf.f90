program chg2xsf
	use kinds, only: DP
	use math,  only: inverse3, volume_v3
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: fid, fid1, ntype, natom, ispin
	integer                      :: i, j, k, nx, ny, nz, ix, iy, iz, mz
	real(DP)                     :: alat, a(3,3), b(3,3), vol, lz, dz, vc(3)
	integer        , allocatable :: natom_per_type(:)
	real(DP)       , allocatable :: x(:,:)
	real(DP)       , allocatable :: dtot_dens(:,:,:)
	real(DP)       , allocatable :: tmp_dens(:,:,:)
	real(DP)       , allocatable :: tot_dens(:,:,:)
	real(DP)       , allocatable :: spin_dens(:,:,:)
	character(2)   , allocatable :: symbol_per_type(:), symbol(:)
	character(200)               :: line, tmp, middlez
	integer        , external    :: number_of_words
	logical                      :: scanok
	logical        , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid  = 1
	fid1 = 2
	!----------------------------------------------------------------------------
	open(fid, file="INCAR", status="old")
		ispin=1
		scanok=search_word_free(fid,"ISPIN",line)
		if (scanok) then
			read(line,*) tmp, tmp, ispin
		end if
	close(fid)
	call getarg(1,middlez)
	!----------------------------------------------------------------------------
	open(fid, file="CHG", status="old")
		read(fid,*) line
		read(fid,*) alat
		read(fid,*) a(:,1)
		read(fid,*) a(:,2)
		read(fid,*) a(:,3)
		a=a*alat
		b=inverse3(a)
		vol=volume_v3(a)
		vc=a(:,3)
		lz=sqrt( dot_product(vc,vc) )
		read(fid,'(a)') line
		backspace(fid)
		ntype=number_of_words(line)
		allocate(symbol_per_type(ntype))
		allocate(natom_per_type(ntype))
		read(fid,*) symbol_per_type
		read(fid,*) natom_per_type
		natom=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				natom=natom+1
			end do
		end do
		allocate(symbol(natom))
		allocate(x(3,natom))
		k=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				k=k+1
				symbol(k)=symbol_per_type(i)
			end do
		end do
		read(fid,*)
		do i=1, natom
			read(fid,*) x(:,i)
		end do
		call rotn(natom,a,x)
		read(fid,*) nx, ny, nz
		allocate(tot_dens(nx, ny, nz))
		allocate(dtot_dens(nx, ny, nz))
		read(fid,*) (((tot_dens(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
		tot_dens = tot_dens / vol
		dz = lz / nz
		if (trim(middlez)=="middlez") then
			mz=nz/2
			allocate(tmp_dens(nx, ny, nz))
			do i=1, natom
				x(3,i) = x(3,i) + real(mz,DP) / nz * lz
			end do
			do i=1, 3
				tmp_dens(:,:,nz-mz+1:nz   ) = tot_dens(:,:,   1:mz)
				tmp_dens(:,:,      1:nz-mz) = tot_dens(:,:,mz+1:nz)
				tot_dens(:,:,:)             = tmp_dens
			end do
			do iz=2, nz
				dtot_dens(:,:,iz) = tot_dens(:,:,iz) - tot_dens(:,:,iz-1)
			end do
			dtot_dens(:,:,1) = dtot_dens(:,:,2)
		end if
		call write_xsf_3dgrid("CHG_dden.xsf"   , a, natom, symbol, x, nx, ny, nz, dtot_dens)
		call write_xsf_3dgrid("CHG_tot_den.xsf", a, natom, symbol, x, nx, ny, nz,  tot_dens)
		deallocate(tot_dens)
		deallocate(dtot_dens)
		if (ispin==2) then
			allocate(spin_dens(nx, ny, nz))
			read(fid,'(a)') line
			read(fid,*) (((spin_dens(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
			spin_dens = spin_dens / vol
			if (trim(middlez)=="middlez") then
				do i=1, 3
					tmp_dens(:,:,nz-mz+1:nz   ) = spin_dens(:,:,   1:mz)
					tmp_dens(:,:,      1:nz-mz) = spin_dens(:,:,mz+1:nz)
					spin_dens(:,:,:)            = tmp_dens
				end do
			end if
			call write_xsf_3dgrid("CHG_spin_den.xsf", a, natom, symbol, x, nx, ny, nz,  spin_dens)
			deallocate(spin_dens)
		end if
	!----------------------------------------------------------------------------
	deallocate(symbol_per_type)
	deallocate(natom_per_type)
	deallocate(symbol)
	deallocate(x)
	if (trim(middlez)=="middlez") then
		deallocate(tmp_dens)
	end if
	!----------------------------------------------------------------------------
	stop
end
