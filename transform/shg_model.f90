program shg_model
	use kinds, only: DP
	use math , only: rotmat3_seq
	implicit none
	!----------------------------------------------------------------------------
	integer               :: i, j, k, i1, j1, k1, fid, ilog
	!----------------------------------------------------------------------------
	real(DP)              :: chi (3,3,3)
	real(DP)              :: chi1(3,3,3)
	real(DP)              :: a(4)
	real(DP)              :: b(4)
	real(DP)              :: q3(3)
	real(DP)              :: rot(3,3)
	real(DP)              :: PI
	real(DP)              :: cmin, cmax, dc
	real(DP)              :: eps
	integer               :: nc, ic
	integer               :: seq(3)
	real(DP), allocatable :: c(:)
	real(DP), allocatable :: Intensity(:)
	character(  3)        :: name(3)
	character(  3)        :: chi_name(3,3,3)
	character(200)        :: finp, fout, flog, tmp
	real(DP), external    :: get_Intensity
	!----------------------------------------------------------------------------
	namelist /control/ q3, seq, chi
	!----------------------------------------------------------------------------
	chi     =    0.00_DP
	PI      =    acos(-1.00_DP)
	eps     =    1.D-6
	!----------------------------------------------------------------------------
	name(1) = "x"
	name(2) = "y"
	name(3) = "z"
	!----------------------------------------------------------------------------
	fid     = 1
	ilog    = 2
	cmin    =    0.00_DP
	cmax    =  360.00_DP
	dc      =    1.00_DP
	nc      = (cmax - cmin) / dc
	fout    = "shg.dat"
	flog    = "shg.log"
	!----------------------------------------------------------------------------
	a       =   -1.00_DP
	a(1)    =    1.00_DP
	b       =   -0.50_DP
	b(1)    =    0.50_DP
	!----------------------------------------------------------------------------
	q3(1)   =   10.00_DP
	q3(2)   =    0.00_DP
	q3(3)   =    0.00_DP
	!----------------------------------------------------------------------------
	seq(1)  =    3
	seq(2)  =    2
	seq(3)  =    3
	!----------------------------------------------------------------------------
! 	chi(1,1,1) = a(1)
! 	chi(1,2,2) = a(2)
! 	chi(2,1,2) = a(3)
! 	chi(2,2,1) = a(4)
! 	!----------------------------------------------------------------------------
! 	chi(2,2,2) = b(1)
! 	chi(2,1,1) = b(2)
! 	chi(1,2,1) = b(3)
! 	chi(1,1,2) = b(4)
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	open(fid, file=finp, status="old")
		read(fid,control)
	close(fid)
	!----------------------------------------------------------------------------
	rot=rotmat3_seq(q3,seq)
	!----------------------------------------------------------------------------
	chi1=0.0_DP
	!----------------------------------------------------------------------------
	do i=1, 3
	do j=1, 3
	do k=1, 3
		chi_name(i,j,k)=trim(name(i))//trim(name(j))//trim(name(k))
		do i1=1, 3
		do j1=1, 3
		do k1=1, 3
			chi1(i,j,k) = chi1(i,j,k) + chi(i1,j1,k1) * rot(i1,i) * rot(j1,j) * rot(k1,k)
		end do
		end do
		end do
	end do
	end do
	end do
	chi=chi1
	!----------------------------------------------------------------------------
	open(ilog, file=flog)
	do i=1, 3
		write(ilog, '(3f15.7)') (rot(i,j), j=1, 3)
	end do
	write(ilog,*)
	write(ilog,*)
	do i=1, 2
	do j=1, 2
	do k=1, 2
		if ( abs(chi(i,j,k)) > eps ) then
			write(ilog, '(3i2, x, a4, x, f15.7)') i,j,k,trim(chi_name(i,j,k)), chi(i,j,k)
		end if
	end do
	end do
	end do
	write(ilog,*)
	write(ilog,*)
	do i=1, 3
	do j=1, 3
	do k=1, 3
		if ( abs(chi(i,j,k)) > eps ) then
			write(ilog, '(3i2, x, a4, x, f15.7)') i,j,k,trim(chi_name(i,j,k)), chi(i,j,k)
		end if
	end do
	end do
	end do
	close(ilog)
	!----------------------------------------------------------------------------
	allocate(c        (nc))
	allocate(Intensity(nc))
	!----------------------------------------------------------------------------
	do ic=1, nc
		c(ic) = cmin + (ic-1) * dc
		Intensity(ic) = get_Intensity(chi,c(ic))
	end do
	!----------------------------------------------------------------------------
	open(fid, file=fout)
		write(fid, '(a20, 2x, a20)') "degree", "Intensity"
		do ic=1, nc
			write(fid, '(f20.4,2x,es20.10)') c(ic), Intensity(ic)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(c        )
	deallocate(Intensity)
	!----------------------------------------------------------------------------
	stop
end

function get_Intensity(chi,theta)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	real(DP) :: chi(3,3,3)
	real(DP) :: theta, th
	real(DP) :: get_Intensity
	integer  :: i
	real(DP) :: p(3)
	real(DP) :: ca, sa, ca2, sa2, csa
	real(DP) :: PI
	!----------------------------------------------------------------------------
	p  = 0.0_DP
	PI = acos(-1.D0)
	th = theta * PI / 180.0_DP
	!----------------------------------------------------------------------------
	ca  = cos(th)
	sa  = sin(th)
	ca2 = ca**2
	sa2 = sa**2
	csa = ca * sa
	!----------------------------------------------------------------------------
	do i=1, 3
		p(i) =  chi(i,1,1) * ca2 + (chi(i,1,2)+chi(i,2,1)) * csa + chi(i,2,2) * sa2
	end do
	get_Intensity = (p(1) * ca + p(2) * sa)**2
	!----------------------------------------------------------------------------
	return
end function
