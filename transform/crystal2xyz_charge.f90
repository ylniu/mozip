program crystal2xyz_charge
	use kinds, only: DP
	use math , only: inverse3
	use file , only: name_main
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: na, nb, nc
	character(200)              :: fxyz, fcif, fcharge, fzval, fout, fdat
	integer                     :: i, j, k, ia, ib
	integer                     :: info, fid
	integer                     :: ntype
	integer                     :: natom
	integer                     :: cluster_natom
	real(DP)                    :: alat
	real(DP)                    :: a(3,3)
	real(DP)                    :: aa(3,3)
	real(DP)                    :: cluster_aa(3,3)
	character(200)              :: title, opt, coordtype
	integer       , allocatable :: ntypes         (  :)
	real(DP)      , allocatable :: x              (:,:)
	real(DP)      , allocatable :: cluster_x      (:,:)
	real(DP)      , allocatable :: cluster_x_cart (:,:)
	real(DP)      , allocatable :: zvals          (  :)
	real(DP)      , allocatable :: zval           (  :)
	real(DP)      , allocatable :: charge         (  :)
	real(DP)      , allocatable :: cluster_charge (  :)
	character(  1), allocatable :: fix            (:,:)
	character(  2), allocatable :: symbols        (  :)
	character(  2), allocatable :: symbol         (  :)
	character(  2), allocatable :: cluster_symbol (  :)
	!----------------------------------------------------------------------------
	character(200)              :: tmp
	!----------------------------------------------------------------------------
	fid = 1
	nc  = 1
	!----------------------------------------------------------------------------
	call get_arg(fxyz, fcharge, fzval, fout, na, nb)
	fcif=trim(name_main(fout))//".cif"
	fdat=trim(name_main(fout))//".dat"
	call qm_vasp_poscar_natom(fxyz, natom, ntype, info)
	!----------------------------------------------------------------------------
	allocate(ntypes (  ntype))
	allocate(symbols(  ntype))
	allocate(symbol (  natom))
	allocate(x      (3,natom))
	allocate(fix    (3,natom))
	allocate(zvals  (  ntype))
	allocate(zval   (  natom))
	allocate(charge (  natom))
	!----------------------------------------------------------------------------
	call qm_vasp_poscar_info(fxyz, title, alat, a, ntype, ntypes, symbols, opt, coordtype, info)
	!----------------------------------------------------------------------------
	call qm_vasp_poscar_coord(fxyz, natom, x, fix, info)
	if (trim(symbols(1))=="") then
		call qm_vasp_potcar_symbol("POTCAR", ntype, symbols, info)
	end if
	call qm_vasp_potcar_zval(fzval, ntype, zvals, info)
	!----------------------------------------------------------------------------
	aa = alat * a
	cluster_aa(:,1) = na * aa(:,1)
	cluster_aa(:,2) = nb * aa(:,2)
	cluster_aa(:,3) = nc * aa(:,3)
	!----------------------------------------------------------------------------
	k=0
	do i=1, ntype
		do j=1, ntypes(i)
			k=k+1
			symbol(k) = symbols(i)
			zval  (k) = zvals  (i)
		end do
	end do
	!----------------------------------------------------------------------------
	cluster_natom = na * nb * natom
	allocate(cluster_symbol(   cluster_natom))
	allocate(cluster_charge(   cluster_natom))
	allocate(cluster_x     (3, cluster_natom))
	allocate(cluster_x_cart(3, cluster_natom))
	!----------------------------------------------------------------------------
	open(fid, file=fcharge, status="old")
		read(fid,*)
		read(fid,*)
		do i=1, natom
			read(fid,*) (tmp, j=1, 4), charge(i)
			charge(i) = zval(i) - charge(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	k=0
	do ia=1, na
		do ib=1, nb
			do i=1, natom
				k = k + 1
				cluster_x     (1,k) = x(1,i) + ia-1
				cluster_x     (2,k) = x(2,i) + ib-1
				cluster_x     (3,k) = x(3,i)
				cluster_symbol(  k) = symbol(i)
				cluster_charge(  k) = charge(i)
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	cluster_x_cart = cluster_x
	call coord_crys_to_cart(cluster_natom, aa, cluster_x_cart)
	!----------------------------------------------------------------------------
	call write_xyz(fdat, cluster_natom, cluster_symbol, cluster_x_cart)
	!----------------------------------------------------------------------------
	open(fid, file=fout)
		write(fid, '(i0)') cluster_natom
		write(fid, '("For_AFM_SCAN")')
		do i=1, cluster_natom
			write(fid, '(a2, 2x, 4f16.10)') &
				trim(cluster_symbol(i)), cluster_x_cart(:,i), cluster_charge(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	call write_cif(fcif, cluster_natom, cluster_symbol, cluster_x_cart, cluster_aa, "Cartesian")
	!----------------------------------------------------------------------------
	write(*,'(2x,"Input  file:")')
	write(*,'(2x,4x,a)')  trim(fxyz)
	write(*,'(2x,4x,a)')  trim(fcharge)
	write(*,'(2x,4x,a)')  trim(fzval)
	write(*,'(2x,"Output file:")')
	write(*,'(2x,4x,a)')  trim(fout)
	write(*,'(2x,4x,a)')  trim(fcif)
	write(*,'(2x,4x,a)')  trim(fcif)
	!----------------------------------------------------------------------------
	deallocate(ntypes        )
	deallocate(symbols       )
	deallocate(symbol        )
	deallocate(x             )
	deallocate(fix           )
	deallocate(zvals         )
	deallocate(zval          )
	deallocate(charge        )
	!----------------------------------------------------------------------------
	deallocate(cluster_symbol)
	deallocate(cluster_charge)
	deallocate(cluster_x     )
	deallocate(cluster_x_cart)
	!----------------------------------------------------------------------------
	stop
end

subroutine get_arg(fxyz, fcharge, fzval, fout, na, nb)
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer                     :: na, nb
	integer                     :: iarg, narg
	character(  *)              :: fxyz, fcharge, fzval, fout
	character(200)              :: line
	character(200), allocatable :: carg(:)
	!----------------------------------------------------------------------------
	fxyz     = "POSCAR"
	fcharge  = "ACF.dat"
	fzval    = "POTCAR"
	fout     = "input.xyz"
	!fcharge = "no"
	na       = 4
	nb       = 4
	!----------------------------------------------------------------------------
	if (iargc()==0) then
		write(*,'("Usages:")')
		line="crystal2xyz_charge -fxyz POSCAR -fcharge ACF.dat"
		line=trim(line)//" -fzval POTCAR -fout input.xyz -na 4 -nb 4"
		write(*,'(a)') trim(line)
		stop
	end if
	!----------------------------------------------------------------------------
	narg=iargc()
	allocate(carg(narg))
	!----------------------------------------------------------------------------
	do iarg=1, narg
		call getarg(iarg, carg(iarg))
	end do
	!----------------------------------------------------------------------------
	iarg=1
	do while (iarg<=narg)
		select case (trim(carg(iarg)))
			case ("-fxyz")
				read(carg(iarg+1), *) fxyz
			case ("-fcharge")
				read(carg(iarg+1), *) fcharge
			case ("-fzval")
				read(carg(iarg+1), *) fzval
			case ("-fout")
				read(carg(iarg+1), *) fout
			case ("-na")
				read(carg(iarg+1), *) na
			case ("-nb")
				read(carg(iarg+1), *) nb
			case ("-h")
				write(*,'("Usages:")')
				line="crystal2xyz_charge -fxyz POSCAR -fcharge ACF.dat"
				line=trim(line)//" -fzval POTCAR -fout input.xyz -na 4 -nb 4"
				write(*,'(a)') trim(line)
				stop
			case default
				write(*,'("Error! Do not know the parameter: ", a)') trim(carg(iarg))
				write(*,'("Stop!")')
				stop
		end select
		iarg = iarg + 2
	end do
	!----------------------------------------------------------------------------
	deallocate(carg)
	!----------------------------------------------------------------------------
	return
end subroutine
