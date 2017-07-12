Program monoclinic_cif_to_pwi
	use kinds, only: DP
	use file,  only: name_main
	use param, only: au2a
	use math,  only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, info, natom, i
	character(200)              :: finp, fout
	real(DP)                    :: alat, latt(6), aa(3,3), v(3,3), invv(3,3), PI
	real(DP)                    :: a, b, c, alpha, beta, gamma
	real(DP)      , allocatable :: x(:,:)
	character(  2), allocatable :: symbol(:)
	!----------------------------------------------------------------------------
	fid=1
	PI=acos(-1.D0)
	call getarg(1,finp)
	fout=trim(name_main(finp))//".pwi"
	call qm_cif_natom(finp, natom, info)
	allocate(x(3,natom))
	allocate(symbol(natom))
	call qm_cif_info(finp, natom, latt, symbol, x, info)
	!----------------------------------------------------------------------------
	a     = latt(1)
	b     = latt(2)
	c     = latt(3)
	alpha = latt(4)
	beta  = latt(5)
	gamma = latt(6)
	alat  = a
	!----------------------------------------------------------------------------
	call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
	!----------------------------------------------------------------------------
	v=0.D0
	v(1,1) =   a / 2.D0
	v(3,1) = - b / 2.D0
	v(1,2) =   c * cos(beta * PI / 180.D0 )
	v(2,2) =   c * sin(beta * PI / 180.D0 )
	v(1,3) =   a / 2.D0
	v(3,3) =   b / 2.D0
	invv=inverse3(v)
	!----------------------------------------------------------------------------
	call rotn(natom, aa  , x)
	!call rotn(natom, invv, x)
	!----------------------------------------------------------------------------
	open(fid, file=fout)
		write(fid, '("alat = ", f15.9, " Bohr")') alat / au2a
		write(fid, '("ibrav      = 13")')
		write(fid, '("celldm(1)  = ", f15.6)') alat / au2a
		write(fid, '("celldm(2)  = ", f15.6)') c/a
		write(fid, '("celldm(3)  = ", f15.6)') b/a
		write(fid, '("celldm(4)  = ", f15.6)') cos(beta * PI / 180.D0)
		write(fid, '(3f15.6)') v(:,1) / a
		write(fid, '(3f15.6)') v(:,2) / a
		write(fid, '(3f15.6)') v(:,3) / a
		write(fid, '("ATOMIC_POSITIONS {angstrom}")')
		do i=1, natom
			write(fid, '(a2, 3f15.7)') symbol(i), x(:,i)
		end do
		write(fid, '("K_POINTS gamma")')
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
