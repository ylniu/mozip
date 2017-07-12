program wavecar2xsf
	use math, only: v3_cross, volume_v3, v2_cos, v3_norm, inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer       ,parameter    :: DP=4
	integer                     :: id_inp, iost, id_out, id_chg, id, lband, hband, lk, hk
	integer                     :: nrecl, nspin, nprec, nk, nband, nplane, nwave
	integer                     :: i, j, k, ix, iy, iz
	integer                     :: irec, ik, ispin, ncnt, iband, ig2
	integer                     :: ig2p, ig1, ig1p, iplane, jrec
	integer                     :: ig3, ig3p
	real(8)                     :: xnrecl, xnspin, xnprec, sinphi123
	real(8)                     :: phi12, phi13
	real(8)                     :: xnplane, vtmp(3),sumkg(3)
	real(8)                     :: xnwk, xnband, ecut, a(3,3), a1(9)
	real(8)                     :: b(3,3), PI, bmag(3), vmag, phi123, phi23
	real(8)                     :: re, im
	integer                     :: nbmaxA(3), npmaxAA
	integer                     :: nbmaxB(3), npmaxBB
	integer                     :: nbmaxC(3), npmaxCC
	integer                     :: nbmax(3) , npmaxx
	real(8)                     :: Vcell, c, gtot, etot
	real(8)       , allocatable :: absr  (:)
	integer       , allocatable :: idx   (:,:,:,:)
	complex(4)    , allocatable :: coeff (:,:,:,:)
	complex(4)    , allocatable :: coeff1(:,:,:,:)
	complex(8)    , allocatable :: cener (  :,:,:)
	real(8)       , allocatable :: occ   (  :,:,:)
	real(8)       , allocatable :: wk    (  :,:,:)
	integer       , allocatable :: igall (  :,:,:)
	integer       , allocatable :: igall1(:,:,:,:,:)
	integer       , allocatable :: num   (  :,:,:)
	integer       , allocatable :: nocc  (  :,:,:)
	!----------------------------------------------------------------------------
	integer                     :: ntype, natom, nx, ny, nz
	integer       , allocatable :: natom_per_type(:)
	real(8)                     :: alat, vol, a_inv(3,3), r(3), eps, waveeps
	real(8)                     :: scale_n, scale_wave
	real(8)       , allocatable :: x(:,:)
	real(8)       , allocatable :: gridx(:,:,:,:), grid(:,:,:)
	character(  2), allocatable :: symbol_per_type(:), symbol(:)
	character(200)              :: line, fout, tmp1, tmp2, fmt
	integer       , external    :: number_of_words
	logical                     :: printwave
	logical                     :: if_cal
	!----------------------------------------------------------------------------
	id_inp     = 1
	id_out     = 2
	id_chg     = 3
	id         = 4
	nrecl      = 16
	PI         = acos(-1.D0)
	c          = 0.262465831d0
	eps        = 1.D-8
	printwave  = .true.
	waveeps    = 1.D-3
	scale_n    = 1.0D0
	scale_wave = 1.0D0
	if_cal     = .true.
	hband      = 4
	lband      = 4
	lk         = 1
	hk         = 1
	!----------------------------------------------------------------------------
	open(id_inp, file="WAVECAR", form='unformatted', access='direct',recl=nrecl, iostat=iost, status='old')
		read(id_inp,rec=1) xnrecl,xnspin,xnprec
	close(id_inp)
	nrecl=nint(xnrecl)
	nspin=nint(xnspin)
	nprec=nint(xnprec)
	!----------------------------------------------------------------------------
	if(nprec.eq.45210) then
		write(6,*) '*** error - WAVECAR_double requires complex*16'
		stop
	endif
	write(6,*)
	write(6,'(" ecord length  =", i7)') nrecl
	write(6,'(" spins         =", i7)') nspin
	write(6,'(" prec flag     =", i7)') nprec
	write(6,'(" ")')
	write(6,*) "But in this program, nrecl should be changed to nrecl / 4"
	write(6,*) "If you get wrong number, please modify the code 'nrecl / 4'"
	!----------------------------------------------------------------------------
	open(id_inp,file='WAVECAR', form='unformatted', access='direct',recl=2, iostat=iost, status='old')
		jrec= nrecl / 8
		read(id_inp,rec=jrec+1,iostat=iost) xnwk
		read(id_inp,rec=jrec+2,iostat=iost) xnband
		read(id_inp,rec=jrec+3,iostat=iost) ecut
		i=0
		do irec=jrec+4, jrec+4+8
			i=i+1
			read(id_inp,rec=irec,iostat=iost) a1(i)
		end do
		!-------------------------------------------------------------------------
		nk   = nint(xnwk)
		nband = nint(xnband)
		a     = reshape(a1,(/3,3/))
		!-------------------------------------------------------------------------
		Vcell=volume_v3(a)
		!-------------------------------------------------------------------------
		b(:,1) = v3_cross(a(1,2), a(1,3))
		b(:,2) = v3_cross(a(1,3), a(1,1))
		b(:,3) = v3_cross(a(1,1), a(1,2))
		b      = 2.D0 * PI / Vcell * b
		bmag(1) = sqrt( b(1,1)**2 + b(2,1)**2 + b(3,1)**2 )
		bmag(2) = sqrt( b(1,2)**2 + b(2,2)**2 + b(3,2)**2 )
		bmag(3) = sqrt( b(1,3)**2 + b(2,3)**2 + b(3,3)**2 )
		!-------------------------------------------------------------------------
		write(6,*)
		write(6,'(" no. k points =", i7          )') nk
		write(6,'(" no. bands    =", i7          )') nband
		write(6,'(" max. energy  =", f15.7, " eV")') ecut
		write(6,'(" real space lattice vectors:" )')
		write(6,'(" a1           =", 3f15.7)') (a(j,1),j=1,3)
		write(6,'(" a2           =", 3f15.7)') (a(j,2),j=1,3)
		write(6,'(" a3           =", 3f15.7)') (a(j,3),j=1,3)
		write(6,*)
		!-------------------------------------------------------------------------
		write(6,'(" reciprocal lattice vectors:")')
		write(6,'(" b1           =", 3f15.7)') (b(j,1),j=1,3)
		write(6,'(" b2           =", 3f15.7)') (b(j,2),j=1,3)
		write(6,'(" b3           =", 3f15.7)') (b(j,3),j=1,3)
		write(6,'(" reciprocal lattice vector magnitudes:")')
		write(6,'(" b1, b2, b3   =", 3f15.7)') (bmag(j), j=1, 3)
		write(6,*) ' '
		!-------------------------------------------------------------------------
		allocate(occ  (nband, nk, nspin))
		allocate(nocc (nband, nk, nspin))
		allocate(cener(nband, nk, nspin))
		allocate(wk   (3    , nk, nspin))
		!-------------------------------------------------------------------------
		phi12     = acos(v2_cos(b(:,1),b(:,2)))
		vtmp      = v3_cross(b(:,1),b(:,2))
		vmag      = v3_norm(vtmp)
		sinphi123 = v2_cos(vtmp,b(:,3))
		nbmaxA(1) = int(dsqrt(ecut*c)/(bmag(1)*abs(sin(phi12))))+1
		nbmaxA(2) = int(dsqrt(ecut*c)/(bmag(2)*abs(sin(phi12))))+1
		nbmaxA(3) = int(dsqrt(ecut*c)/(bmag(3)*abs(sinphi123 )))+1
		npmaxAA   = nint(4.D0*pi*nbmaxA(1)*nbmaxA(2)*nbmaxA(3)/3.D0)
		!-------------------------------------------------------------------------
		phi13     = acos(v2_cos(b(:,1),b(:,3)))
		vtmp      = v3_cross(b(:,1),b(:,3))
		vmag      = v3_norm(vtmp)
		sinphi123 = v2_cos(vtmp,b(:,2))
		phi123    = abs(asin(sinphi123))
		nbmaxB(1) = int(dsqrt(ecut*c)/(bmag(1)*abs(sin(phi13))))+1
		nbmaxB(2) = int(dsqrt(ecut*c)/(bmag(2)*abs(sinphi123 )))+1
		nbmaxB(3) = int(dsqrt(ecut*c)/(bmag(3)*abs(sin(phi13))))+1
		npmaxBB   = nint(4.*pi*nbmaxB(1)*nbmaxB(2)*nbmaxB(3)/3.D0)
		!-------------------------------------------------------------------------
		phi23     = acos(v2_cos(b(:,2),b(:,3)))
		vtmp      = v3_cross(b(:,2),b(:,3))
		vmag      = v3_norm(vtmp)
		sinphi123 = v2_cos(vtmp,b(:,1))
		phi123    = abs(asin(sinphi123))
		nbmaxC(1) = int(dsqrt(ecut*c)/(bmag(1)*abs(sinphi123 )))+1
		nbmaxC(2) = int(dsqrt(ecut*c)/(bmag(2)*abs(sin(phi23))))+1
		nbmaxC(3) = int(dsqrt(ecut*c)/(bmag(3)*abs(sin(phi23))))+1
		npmaxCC   = nint(4.*pi*nbmaxC(1)*nbmaxC(2)*nbmaxC(3)/3.D0)
		!-------------------------------------------------------------------------
		nbmax(1)  = max0(nbmaxA(1),nbmaxB(2),nbmaxC(3))
		nbmax(2)  = max0(nbmaxA(1),nbmaxB(2),nbmaxC(3))
		nbmax(3)  = max0(nbmaxA(1),nbmaxB(2),nbmaxC(3))
		npmaxx    = min0(npmaxAA,npmaxBB,npmaxCC)
		allocate (igall (3, npmaxx, nk))
		allocate (num   (        nband, nk, nspin))
		allocate (coeff (npmaxx, nband, nk, nspin))
		allocate (absr  (npmaxx                  ))
		allocate (idx   (npmaxx, nband, nk, nspin))
		!-------------------------------------------------------------------------
		write(6,'(" max. no. G values; 1,2,3 =", 3i7)') nbmax(1),nbmax(2),nbmax(3)
		write(6,*) ' '
		write(*,'(" Reading data in WAVECAR ...")')
		!-------------------------------------------------------------------------
		do ispin=1, nspin
			do ik=1, nk
				! write(6,*)
				! write(6,'(" ******")')
				! write(6,'(" reading spin", i7)')
				!-------------------------------------------------------------------
				irec= nrecl / 4 +1
				read(id_inp,rec=irec) xnplane
				nplane=nint(xnplane)
				do i=1,3
					irec=irec+1
					read(id_inp, rec=irec) wk(i, ik, ispin)
				end do
				!-------------------------------------------------------------------
				! write(6,'(" k point #", i7, " / ", i7)') ik, nk
				! write(6,'(" input no. of plane waves =", i7)') nplane
				! write(6,'(" k value =", 3f15.7)') (wk(j, ik, ispin),j=1,3)
				! write(6,*)
				!-------------------------------------------------------------------
				do iband=1, nband
					irec=irec+1
					read(id_inp, rec=irec) re
					irec=irec+1
					read(id_inp, rec=irec) im
					irec=irec+1
					read(id_inp, rec=irec) occ(iband, ik, ispin)
					nocc(iband, ik, ispin) = nint( occ(iband, ik, ispin) )
					cener(iband, ik, ispin) = dcmplx(re,im)
				end do
				!-------------------------------------------------------------------
				! Calculate plane waves
				ncnt=0
				do ig3=0,2*nbmax(3)
					ig3p=ig3
					if (ig3.gt.nbmax(3)) ig3p=ig3-2*nbmax(3)-1
					do ig2=0,2*nbmax(2)
						ig2p=ig2
						if (ig2.gt.nbmax(2)) ig2p=ig2-2*nbmax(2)-1
						do ig1=0,2*nbmax(1)
							ig1p=ig1
							if (ig1.gt.nbmax(1)) ig1p=ig1-2*nbmax(1)-1
							do j=1,3
								sumkg(j)=(wk(1, ik, ispin)+ig1p)*b(j,1) &
									& +   (wk(2, ik, ispin)+ig2p)*b(j,2) &
									& +   (wk(3, ik, ispin)+ig3p)*b(j,3)
							enddo
							gtot=sqrt(sumkg(1)**2+sumkg(2)**2+sumkg(3)**2)
							etot=gtot**2/c
							if (etot.lt.ecut) then
								ncnt=ncnt+1
								igall(1,ncnt, ik)=ig1p
								igall(2,ncnt, ik)=ig2p
								igall(3,ncnt, ik)=ig3p
							end if
						enddo
					enddo
				enddo
				!-------------------------------------------------------------------
				if (ncnt.ne.nplane) then
					write(6,*) '*** error - computed no. != input no.'
					stop
				endif
				!-------------------------------------------------------------------
				if (ncnt.gt.npmaxx) then
					write(6,*) '*** error - plane wave count exceeds estimate'
					stop
				endif
				!-------------------------------------------------------------------
				!
				do iband=1, nband
					do iplane=1, nplane
						irec=irec+1
						read(id_inp,rec=irec) coeff(iplane,iband,ik,ispin)
					end do
				enddo
			end do ! nk
		end do ! nspin
	close(id_inp)
	write(*,'(" npmaxx = ", i7)') npmaxx
	write(*,'(" nplane = ", i7)') nplane
	nwave=nint(nplane*scale_wave)
	write(6,'(" nwave = ", i7)') nwave
	allocate (igall1(3, nwave, nband, nk, nspin))
	allocate (coeff1(   nwave, nband, nk, nspin))
	!----------------------------------------------------------------------------
	! Reduce wavefunction
	!
	write(6,'(" Ordering Wavefunction")')
!$OMP PARALLELDO
	do ispin=1, nspin
		do ik=1, nk
			do iband=1, nband
				absr=abs(coeff(:,iband, ik, ispin))
				call dorder(nplane,absr,idx(1,iband,ik,ispin), -1)
			enddo ! iband
		end do ! ik
	end do ! ispin
!$OMP END PARALLELDO
!
	!----------------------------------------------------------------------------
	write(6,'(" Reducing Wavefunction")')
!$OMP PARALLELDO
	do ispin=1, nspin
		do ik=1, nk
			do iband=1, nband
				do i=1, nwave
					j=idx(i,iband,ik,ispin)
					igall1(:,i,iband,ik,ispin) = igall(:,j,       ik       )
					coeff1(  i,iband,ik,ispin) = coeff(  j,iband, ik, ispin)
				end do
			enddo ! iband
		end do ! ik
	end do ! ispin
!$OMP END PARALLELDO
	!----------------------------------------------------------------------------
	! Output G values and coefficients
	!
	if (printwave) then
		open(id_out, file="WAVECAR.dat")
		do ispin=1, nspin
			do ik=lk, hk
				write(id_out,'(2a10,2a14)') "iband", "nplane", "energy", "occ"
				do iband=hband,lband,-1
					write(id_out,'( 2i10, f14.6, f14.6)') iband, nplane, real(cener(iband, ik, nspin)), occ(iband, ik, nspin)
					do iplane=1,nplane
						write(id_out,'(3i6,"  ( ",g14.6," , ",g14.6," )")') &
							& (igall(j,iplane, ik),j=1,3), coeff(iplane,iband, ik, nspin)
					end do
				end do ! iband
			end do ! ik
		end do ! ispin
		close(id_out)
	end if
	!----------------------------------------------------------------------------
	write(*,'(" Reading data in CHG ...")')
	open(id_chg, file="CHG", status="old")
		read(id_chg,*) line
		read(id_chg,*) alat
		read(id_chg,*) a(:,1)
		read(id_chg,*) a(:,2)
		read(id_chg,*) a(:,3)
		a     = a*alat
		a_inv = inverse3(a)
		vol   = volume_v3(a)
		read(id_chg,'(a)') line
		backspace(id_chg)
		ntype=number_of_words(line)
		allocate(symbol_per_type(ntype))
		allocate(natom_per_type(ntype))
		read(id_chg,*) symbol_per_type
		read(id_chg,*) natom_per_type
		natom=sum(natom_per_type)
		allocate(symbol(  natom))
		allocate(x     (3,natom))
		k=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				k=k+1
				symbol(k)=symbol_per_type(i)
			end do
		end do
		read(id_chg,*)
		do i=1, natom
			read(id_chg,*) x(:,i)
		end do
		call rotn(natom,a,x)
		read(id_chg,*) nx, ny, nz
	close(id_chg)
	nx = nint(nx * scale_n)
	ny = nint(ny * scale_n)
	nz = nint(nz * scale_n)
	write(6,'(" nx, ny, nz :", 3i10)') nx, ny, nz
	if (.not. if_cal ) then
		write(6,'(" End testing ...")')
		stop
	end if
	!----------------------------------------------------------------------------
	allocate(gridx(3,nx,ny,nz))
	allocate(grid (  nx,ny,nz))
	do ix=1, nx
		do iy=1, ny
			do iz=1, nz
				r(1) = (ix-1.D0) / nx
				r(2) = (iy-1.D0) / ny
				r(3) = (iz-1.D0) / nz
				call rotn(3, a_inv, r)
				gridx(:, ix, iy, iz) = r
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	write(tmp1,'(i0)') nk
	write(tmp1,'(i0)') len(trim(tmp1))
	write(tmp2,'(i0)') nband
	write(tmp2,'(i0)') len(trim(tmp2))
	if (nspin==2) then
		fmt='("wave_k_",i'//trim(tmp1)//'.'//trim(tmp1)//',"_band_",i'//trim(tmp2)//'.'//trim(tmp2)//'_spin_",i1,".xsf")'
	else
		fmt='("wave_k_",i'//trim(tmp1)//'.'//trim(tmp1)//',"_band_",i'//trim(tmp2)//'.'//trim(tmp2)//',".xsf")'
	end if
	write(*,'(" Calculating grid ...")')
	do ispin=1, nspin
		do ik=lk, hk
			do iband=hband, lband, -1
				if (abs(occ(iband, ik, ispin)) >= eps ) then
					write(6,'(" ispin = ", i2, " / ", i2 ," ; ik = ", i4, " / ", i4, " ; iband = ", i4, " / ", i4)') &
						& ispin, nspin, ik, nk, iband, nband
					if (nspin==2) then
						write(fout,fmt) ik, iband, ispin
					else
						write(fout,fmt) ik, iband
					end if
					call get_grid(nx, ny, nz, nwave, gridx, grid, igall1(1,1,iband, ik, ispin), &
						& wk(1, ik, ispin), b, coeff1(1,iband,ik,ispin), Vcell)
					call write_xsf_3dgrid(fout, a, natom, symbol, x, nx, ny, nz, grid)
				end if
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	deallocate(occ   )
	deallocate(nocc  )
	deallocate(cener )
	deallocate(igall )
	deallocate(igall1)
	deallocate(num   )
	deallocate(coeff )
	deallocate(coeff1)
	deallocate(symbol_per_type)
	deallocate(natom_per_type)
	deallocate(gridx )
	deallocate(grid  )
	deallocate(absr  )
	deallocate(idx   )
	!----------------------------------------------------------------------------
	stop
end
!-------------------------------------------------------------------------------
subroutine get_grid(nx, ny, nz, np, gridx, grid, igs, wk, b,coeff,Vcell)
	implicit none
	!----------------------------------------------------------------------------
	complex(8), parameter :: II=dcmplx(0.D0,1.D0), ZONE=dcmplx(0.D0, 0.D0)
	!----------------------------------------------------------------------------
	integer    :: nx, ny, nz, np
	integer    :: igs(3, np)
	real(8)    :: wk, b(3,3), Vcell
	real(8)    :: gridx(3, nx, ny, nz)
	real(8)    :: grid (   nx, ny, nz)
	complex(4) :: coeff(np)
	!----------------------------------------------------------------------------
	integer    :: ix, iy, iz, ip
	integer    :: j
	real(8)    :: x, y, z, sumkg(3), wkg(3)
	complex(8) :: csum
	!----------------------------------------------------------------------------
!$OMP PARALLELDO
	do ix=1, nx
		write(*,'(" ix : ", i4, " / ", i4)') ix, nx
		do iy=1, ny
			do iz=1, nz
				x=gridx(1,ix, iy, iz)
				y=gridx(2,ix, iy, iz)
				z=gridx(3,ix, iy, iz)
				!-------------------------------------------------------------------
				csum=ZONE
				do ip=1, np
					wkg = wk + igs(:,ip)
					do j=1, 3
						sumkg(j) = b(j,1) * wkg(1) + b(j,2) * wkg(2) + b(j,3) * wkg(3)
					end do
					csum = csum + coeff(ip) * exp(II * (sumkg(1)*x+sumkg(2)*y+sumkg(3)*z))
				end do
				csum=csum/dsqrt(Vcell)
				grid(ix,iy,iz)=abs(csum)
			end do
		end do
	end do
!$OMP END PARALLELDO
	!----------------------------------------------------------------------------
	return
end subroutine
