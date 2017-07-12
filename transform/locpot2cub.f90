program locpot2cub
	use kinds, only: DP
	use param, only: au2a
	use param, only: au2ev
	use math,  only: inverse3, volume_v3
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: fid1, fid2, ntype, natom, ispin
	integer                      :: i, j, k, nx, ny, nz, ix, iy, iz, mz
	integer                      :: ngrid(3), totgrids
	real(DP)                     :: alat, a(3,3), b(3,3), vol, lz, dz, vc(3), center(3)
	real(DP)                     :: a1(3,3), coeff, disp(3), xmin, xmax, gsum, dv
	integer        , allocatable :: natom_per_type(:), nat(:)
	real(DP)       , allocatable :: x(:,:)
	real(DP)       , allocatable :: dtot_dens(:,:,:)
	real(DP)       , allocatable :: tmp_dens(:,:,:)
	real(DP)       , allocatable :: tot_dens(:,:,:)
	real(DP)       , allocatable :: spin_dens(:,:,:)
	character(2)   , allocatable :: symbol_per_type(:), symbol(:)
	character(200)               :: line, tmp, middlez, title, caltype
	integer        , external    :: number_of_words
	logical        , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid1   = 1
	fid2   = 2
	ispin  = 1
	coeff  = au2a**3.D0
	disp   = 0.5D0
	center = 0.D0
	xmin   = 1.D99
	xmax   =-1.D99
	gsum   = 0.D0
	!----------------------------------------------------------------------------
	call getarg(1,middlez)
	!----------------------------------------------------------------------------
	open(fid1, file="INCAR", status="old")
		if (search_word_free(fid1,"ISPIN",line)) read(line,*) tmp, tmp, ispin
	close(fid1)
	!----------------------------------------------------------------------------
	open(fid1, file="LOCPOT", status="old")
		!-------------------------------------------------------------------------
		read(fid1,*) title
		read(fid1,*) alat
		read(fid1,*) a(:,1)
		read(fid1,*) a(:,2)
		read(fid1,*) a(:,3)
		!-------------------------------------------------------------------------
		! The unit of a=a*alat is Angstrom
		!
		a=a*alat
		b=inverse3(a)
		!-------------------------------------------------------------------------
		! The unit of vol is Angstrom^3
		!
		vol=volume_v3(a)
		vc=a(:,3)
		lz=sqrt( dot_product(vc,vc) )
		!-------------------------------------------------------------------------
		read(fid1,'(a)') line
		backspace(fid1)
		ntype=number_of_words(line)
		allocate(symbol_per_type(ntype))
		allocate(natom_per_type(ntype))
		read(fid1,*) symbol_per_type
		read(fid1,*) natom_per_type
		natom=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				natom=natom+1
			end do
		end do
		allocate(symbol(natom))
		allocate(x(3,natom))
		allocate(nat(natom))
		k=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				k=k+1
				symbol(k)=symbol_per_type(i)
			end do
		end do
		read(fid1,*)
		do i=1, natom
			read(fid1,*) x(:,i)
		end do
		call rotn(natom,a,x)
		read(fid1,*) nx, ny, nz
		ngrid(1) = nx
		ngrid(2) = ny
		ngrid(3) = nz
		allocate(tot_dens(nx, ny, nz))
		allocate(dtot_dens(nx, ny, nz))
		!-------------------------------------------------------------------------
		totgrids=(ngrid(1)-1) * (ngrid(3)-1) * (ngrid(3)-1)
		!-------------------------------------------------------------------------
		! The electron density is rho
		! sum_{ix,iy,iz} rho(ix,iy,iz) dv = nele
		! tot_dens = rho * vol = rho * dv * N
		! So, sum_{ix,iy,iz} tot_dens(ix,iy,iz) = nele * N
		!
		read(fid1,*) (((tot_dens(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
		do ix=1, nx-1
			do iy=1, ny-1
				do iz=1, nz-1
					gsum = gsum + tot_dens(ix,iy,iz)
				end do
			end do
		end do
		gsum = gsum / totgrids
		dv   = vol  / totgrids
		write(*,*) "gsum=", gsum
		dz = lz / nz
		call rotn(1, a, disp)
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
			deallocate(tmp_dens)
			do iz=2, nz
				dtot_dens(:,:,iz) = tot_dens(:,:,iz) - tot_dens(:,:,iz-1)
			end do
			dtot_dens(:,:,1) = dtot_dens(:,:,2)
		else if (trim(middlez)=="center") then
			do i=1, natom
				x(:,i) = x(:,i) - disp
			end do
			do iz=2, nz
				dtot_dens(:,:,iz) = tot_dens(:,:,iz) - tot_dens(:,:,iz-1)
			end do
			center = center - disp
			dtot_dens(:,:,1) = dtot_dens(:,:,2)
		end if
! 		call write_xsf_3dgrid("CHG_dden.xsf"   , a, natom, symbol, x, nx, ny, nz, dtot_dens)
! 		call write_xsf_3dgrid("CHG_tot_den.xsf", a, natom, symbol, x, nx, ny, nz,  tot_dens)
		caltype = "Electrostatic potential from Total SCF Density"
		call symbol_to_nat(natom,symbol,nat)
		!-------------------------------------------------------------------------
		! Angstrom to Bohr(a.u.)
		!
		center = center / au2a
		a      = a      / au2a
		x      = x      / au2a
		!-------------------------------------------------------------------------
		do i=1, 3
			a1(:,i) = a(:,i) / ngrid(i)
		end do
		!-------------------------------------------------------------------------
		tot_dens = -tot_dens
		!tot_dens = tot_dens / 10.D0
		do ix=1, nx
			do iy=1, ny
				do iz=1, nz
					if (xmin>tot_dens(ix,iy,iz)) xmin=tot_dens(ix,iy,iz)
					if (xmax<tot_dens(ix,iy,iz)) xmax=tot_dens(ix,iy,iz)
				end do
			end do
		end do
		!-------------------------------------------------------------------------
		!tot_dens=tot_dens-xmin
		!write(*,'("From",x,f20.10,x,"to",x,f20.10)') xmin-xmin, xmax-xmin
		write(*,'("From",x,f20.10,x,"to",x,f20.10)') xmin, xmax
		call write_cube("LOCPOT.cube", trim(title),caltype,natom,center, ngrid, a1, nat, x, tot_dens)
		deallocate(tot_dens)
		deallocate(dtot_dens)
		if (ispin==2) then
			allocate(spin_dens(nx, ny, nz))
			read(fid1,'(a)') line
			read(fid1,*) (((spin_dens(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
			spin_dens = spin_dens / vol
			if (trim(middlez)=="middlez") then
				do i=1, 3
					tmp_dens(:,:,nz-mz+1:nz   ) = spin_dens(:,:,   1:mz)
					tmp_dens(:,:,      1:nz-mz) = spin_dens(:,:,mz+1:nz)
					spin_dens(:,:,:)            = tmp_dens
				end do
			end if
			deallocate(tmp_dens)
! 			call write_xsf_3dgrid("CHG_spin_den.xsf", a, natom, symbol, x, nx, ny, nz,  spin_dens)
			deallocate(spin_dens)
		end if
	!----------------------------------------------------------------------------
	deallocate(symbol_per_type)
	deallocate(natom_per_type)
	deallocate(symbol)
	deallocate(x)
	deallocate(nat)
	!----------------------------------------------------------------------------
	stop
end
