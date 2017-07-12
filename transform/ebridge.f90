module class
	use kinds, only: DP
	!----------------------------------------------------------------------------
	integer                 :: ns, np
	integer, allocatable    :: p    (  :)
	logical, allocatable    :: peven(  :)
	integer, allocatable    :: permu(:,:)
	!----------------------------------------------------------------------------
	integer                 :: iout
	logical                 :: debug
	character(200)          :: flog
	!----------------------------------------------------------------------------
	! Jab( a(1)b(1) | 1/r12 | c(2) d(2) )
	!
	real(DP)                :: Sab(4,4)
	real(DP)                :: Hab(4,4)
	real(DP)                :: Jab(4,4,4,4)
	!----------------------------------------------------------------------------
	type, public:: spin_orbital
		character(1), public :: ob
		integer     , public :: ib
		integer     , public :: sp
	end type spin_orbital
	!----------------------------------------------------------------------------
	type, public:: multi_fun
		type(spin_orbital), public :: func(4)
	end type multi_fun
	!----------------------------------------------------------------------------
contains
	subroutine get_ib(this)
		type(spin_orbital) :: this
		select case(this%ob)
			case("a")
				this%ib = 1
			case("A")
				this%ib = 2
			case("b")
				this%ib = 3
			case("B")
				this%ib = 4
		end select
	end subroutine
	!
	!----------------------------------------------------------------------------
	!
	function same_spin(ti, tj)
		type(multi_fun) :: ti, tj
		logical         :: same_spin
		if ( ti%func(1)%sp == tj%func(1)%sp .and. &
			& ti%func(2)%sp == tj%func(2)%sp .and. &
			& ti%func(3)%sp == tj%func(3)%sp .and. &
			& ti%func(4)%sp == tj%func(4)%sp ) then
			same_spin = .true.
		else
			same_spin = .false.
		end if
		return
	end function
	!----------------------------------------------------------------------------
end module
!
!-------------------------------------------------------------------------------
!
program ebridge
	use class, only: DP, multi_fun, p, debug, iout, ns, Jab, Sab, Hab, flog
	implicit none
	!----------------------------------------------------------------------------
	integer         :: i, j
	integer         :: is
	!----------------------------------------------------------------------------
	real(DP)        :: Smn(2,4,4)
	real(DP)        :: Hmn(2,4,4)
	real(DP)        :: Tmn(2,4,4)
	!----------------------------------------------------------------------------
	real(DP)        :: N  (2)
	real(DP)        :: A21(2)
	real(DP)        :: A31(2)
	real(DP)        :: mu (2)
	real(DP)        :: la (2)
	!----------------------------------------------------------------------------
	real(DP)        :: Vcc(2)
	real(DP)        :: VTB(2)
	!----------------------------------------------------------------------------
	real(DP)        :: kcc(2)
	real(DP)        :: kTB(2)
	real(DP)        :: kTr(2)
	!----------------------------------------------------------------------------
	real(DP)        :: HIJ(2,2,2)
	real(DP)        :: TIJ(2,2,2)
	real(DP)        :: SIJ(2,2,2)
	!----------------------------------------------------------------------------
	type(multi_fun) :: fw(8)
	!----------------------------------------------------------------------------
	character(80)   :: multi_name(2)
	!----------------------------------------------------------------------------
	iout          = 2
	multi_name(1) = "Singlet"
	multi_name(2) = "Triplet"
	!----------------------------------------------------------------------------
	debug = .true.
	!----------------------------------------------------------------------------
	call get_factorial()
	call read_data()
	call init(fw)
	!----------------------------------------------------------------------------
	! calculate singlet H11
	!
	Hmn=0.D0
	Smn=0.D0
	do i=1, 4
		do j=i, 4
! 			if (i==1 .and. j==4) then
			call get_coupling(fw(i), fw(i+4), fw(j), fw(j+4), Hmn(1,i,j), Smn(1,i,j))
			!----------------------------------------------------------------------
			if (i/=j) then
				Hmn(:,j,i) = Hmn(:,i,j)
				Smn(:,j,i) = Smn(:,i,j)
			end if
! 			end if
			!----------------------------------------------------------------------
		end do
	end do
	!----------------------------------------------------------------------------
	do i=1, 4
		do j=1, 4
			Tmn(:,i,j) = Hmn(:,i,j) - Smn(:,i,j) * Hmn(:,1,1)
		end do
	end do
	!----------------------------------------------------------------------------
	A21 = Hmn(:,2,2) - Hmn(:,1,1)
	A31 = Hmn(:,3,3) - Hmn(:,1,1)
	A21 = A21
	A31 = A31
	!----------------------------------------------------------------------------
	la  = -Tmn(:,1,2) / A21
	mu  = -Tmn(:,1,3) / A31
	!----------------------------------------------------------------------------
	N   = 1 + 2 * la * Smn(:,1,2) + 2 * mu * Smn(:,1,3) + la**2 + mu**2 &
		& + 2 * la * mu * Smn(:,2,3)
	do is=1, 2
		N(is)   = 1 + 2 * la(is) * Smn(is,1,2) + 2 * mu(is) * Smn(is,1,3) + la(is)**2 + mu(is)**2 &
			& + 2 * la(is) * mu(is) * Smn(is,2,3)
	end do
	N   = 1.D0 / sqrt(N)
	!----------------------------------------------------------------------------
	SIJ(:,1,1) = 1.D0
	SIJ(:,2,2) = 1.D0
	SIJ(:,1,2) = N**2 * ( Smn(:,1,4) + 2 * la * mu + 2 * mu * Smn(:,1,2) &
		& + 2 * la * Smn(:,1,3) + (la**2 + mu**2) * Smn(:,2,3) )
	SIJ(:,2,1) = SIJ(:,1,2)
	!----------------------------------------------------------------------------
	HIJ(:,1,1) = Hmn(:,1,1) + N**2 * ( la**2*A21 + mu**2*A31 + 2 * la * Tmn(:,1,2) &
		& + 2 * mu * Tmn(:,1,3) + 2 * la * mu * Tmn(:,2,3))
	HIJ(:,2,2) = HIJ(:,1,1)
	HIJ(:,1,2) = SIJ(:,1,2) * Hmn(:,1,1) + N**2 * ( Tmn(:,1,4) + &
		& 2 * la * mu * A21 + 2 * mu * Tmn(:,1,2) + 2 * la * Tmn(:,1,3) + &
		& (la**2 + mu**2) * Tmn(:,2,3) )
	HIJ(:,2,1) = HIJ(:,1,2)
	!----------------------------------------------------------------------------
	do i=1, 2
		do j=1, 2
			!TIJ(:,i,j) = HIJ(:,i,j) - SIJ(:,i,j) * HIJ(:,1,1)
			TIJ(:,i,j) = HIJ(:,i,j) - SIJ(:,i,j) * Hmn(:,1,1)
		end do
	end do
	!----------------------------------------------------------------------------
	Vcc = Hmn(:,1,4)
	VTB = TIJ(:,1,2) - Vcc
	!----------------------------------------------------------------------------
	kcc = Vcc**2
	kTr = TIJ(:,1,2)**2
	kTB = kTr - kcc
	!----------------------------------------------------------------------------
	do is=1, 2
		write(iout,'("--------------------------------------------------------------")')
		write(iout,'(2x,a)') trim(multi_name(is))
		write(iout,*)
		write(iout,'(2x,i0,"N      = ", f25.17)') is, N   (is)
		write(iout,'(2x,i0,"A21    = ", f25.17)') is, A21 (is)
		write(iout,'(2x,i0,"A31    = ", f25.17)') is, A31 (is)
		write(iout,'(2x,i0,"mu     = ", f25.17)') is, mu  (is)
		write(iout,'(2x,i0,"lambda = ", f25.17)') is, la  (is)
		write(iout,'(2x,i0,"Vcc    = ", f25.17)') is, Vcc (is)
		write(iout,'(2x,i0,"VTB    = ", f25.17)') is, VTB (is)
		write(iout,'(2x,i0,"TRP    = ", f25.17)') is, TIJ (is,1,2)
		write(iout,'(2x,i0,"kcc    = ", f25.17)') is, kcc (is)
		write(iout,'(2x,i0,"kTB    = ", f25.17)') is, kTB (is)
		write(iout,'(2x,i0,"kTr    = ", f25.17)') is, kTr (is)
		write(iout,*)
		write(iout,'(2x,"Hmn")')
		do i=1, ns
			write(iout, '(4f15.7)') (Hmn(is, i, j), j=1, ns)
		end do
		write(iout,'(2x,"Smn")')
		do i=1, ns
			write(iout, '(4f15.7)') (Smn(is, i, j), j=1, ns)
		end do
		write(iout,'(2x,"Tmn")')
		do i=1, ns
			write(iout, '(4f15.7)') (Tmn(is, i, j), j=1, ns)
		end do
		write(iout,'(2x,"HIJ")')
		do i=1, 2
			write(iout, '(4f15.7)') (HIJ(is, i, j), j=1, 2)
		end do
		write(iout,'(2x,"SIJ")')
		do i=1, 2
			write(iout, '(4f15.7)') (SIJ(is, i, j), j=1, 2)
		end do
		write(iout,'(2x,"TIJ")')
		do i=1, 2
			write(iout, '(4f15.7)') (TIJ(is, i, j), j=1, 2)
		end do
	end do
	write(iout,*)
	write(iout,'(2x,"J0                   = ", f15.7)') Jab(2,1,1,2) * 2
	write(iout,'(2x,"  J(33,44)-J(11,22)  = ", f15.7)') Jab(3,3,4,4) -Jab (4,4,3,3)
	write(iout,'(2x,"2(J(11,44)-J(22,33)) = ", f15.7)') (Jab(1,1,4,4) -Jab (2,2,3,3)) * 2
	write(iout,'(2x,"A3 - A1              = ", f15.7)') A31(2) - A31(1)
	write(iout,'(2x,"h44 - h22            = ", f15.7)') Hab(  4,4) - Hab(  2,2)
	write(iout,'(2x,"H1(1,1) - H3(1,1)    = ", f15.7)') Hmn(1,1,1) - Hmn(2,1,1)
	write(iout,'(2x,"H3(2,2) - H1(2,2)    = ", f15.7)') Hmn(2,2,2) - Hmn(1,2,2)
	write(iout,'(2x,"H1(2,2) - H1(1,1)    = ", f15.7)') Hmn(1,2,2) - Hmn(1,1,1)
	write(iout,'(2x,"H3(2,2) - H3(1,1)    = ", f15.7)') Hmn(2,2,2) - Hmn(2,1,1)
	write(iout,'(2x,"h(a|A)               = ", f15.7)') Hab(1,2)
	write(iout,'(2x,"h(a|B)               = ", f15.7)') Hab(1,4)
	write(iout,'(2x,"s(a|A)               = ", f15.7)') Sab(1,2)
	write(iout,'(2x,"s(a|B)               = ", f15.7)') Sab(1,4)
	write(iout,'(2x,"J(aA|Aa)             = ", f15.7)') Jab(1,2,2,1)
	write(iout,'(2x,"J(aB|Ba)             = ", f15.7)') Jab(1,4,4,1)
	write(iout,'(2x,"J(aA|bB)             = ", f15.7)') Jab(1,2,3,4)
	write(iout,'(2x,"J(Aa|bB)             = ", f15.7)') Jab(2,1,3,4)
	!----------------------------------------------------------------------------
	close(iout)
	!----------------------------------------------------------------------------
	call deallocate_var()
	!----------------------------------------------------------------------------
	stop
end
subroutine deallocate_var()
	use class, only: p, peven, permu
	!----------------------------------------------------------------------------
	deallocate(p    )
	deallocate(peven)
	deallocate(permu)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_factorial()
	use math , only: factorial
	use class, only: ns, np, p, peven, permu
	integer :: i
	!----------------------------------------------------------------------------
	ns = 4
	np = factorial(ns)
	!----------------------------------------------------------------------------
	allocate(p     (    np))
	allocate(peven (    np))
	allocate(permu (ns, np))
	!----------------------------------------------------------------------------
	call testnexper(ns, np, permu, peven)
	do i=1, np
		select case(peven(i))
			case (.true.)
				p(i) =  1
			case (.false.)
				p(i) = -1
		end select
	end do
	!----------------------------------------------------------------------------
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_coupling(fw1, fw2, fw3, fw4, Hmn, Smn)
	use class, only: DP, multi_fun, iout
	!----------------------------------------------------------------------------
	real(DP)        :: Hmn(2)
	real(DP)        :: Smn(2)
	real(DP)        :: Hpq(4)
	real(DP)        :: Spq(4)
	type(multi_fun) :: fw1, fw2, fw3, fw4
	real(DP)        :: const
	!----------------------------------------------------------------------------
	Hpq=0.D0
	Spq=0.D0
	call single_coupling(fw1, fw3, Hpq(1), Spq(1))
	call single_coupling(fw2, fw3, Hpq(2), Spq(2))
	call single_coupling(fw1, fw4, Hpq(3), Spq(3))
	call single_coupling(fw2, fw4, Hpq(4), Spq(4))
	!----------------------------------------------------------------------------
	const = 1.D0 / 2.D0
	!----------------------------------------------------------------------------
	Hmn(1) = (Hpq(1) - Hpq(2) - Hpq(3) + Hpq(4)) * const
	Smn(1) = (Spq(1) - Spq(2) - Spq(3) + Spq(4)) * const
	!----------------------------------------------------------------------------
	Hmn(2) = (Hpq(1) + Hpq(2) + Hpq(3) + Hpq(4)) * const
	Smn(2) = (Spq(1) + Spq(2) + Spq(3) + Spq(4)) * const
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine single_coupling(fi, fj, Hmn, Smn)
	use class, only: DP, multi_fun
	use math, only: factorial
	!----------------------------------------------------------------------------
	real(DP)             :: Hmn
	real(DP)             :: Smn
	type(multi_fun)      :: fi, fj
	!----------------------------------------------------------------------------
	real(DP)             :: H1, H2
	!----------------------------------------------------------------------------
	call get_overlap(fi, fj, Smn)
	call get_single (fi, fj, H1 )
	call get_double (fi, fj, H2 )
	Hmn = H1 + H2
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_overlap(fi, fj, S)
	use class, only: DP, multi_fun, ns, np, p, permu, iout, debug, same_spin, Sab
	!----------------------------------------------------------------------------
	real(DP)        :: S
	type(multi_fun) :: fi, fj
	type(multi_fun) :: gi, gj
	!----------------------------------------------------------------------------
	real(DP)        :: prod
	integer         :: is
	integer         :: ip, jp
	integer         :: i(ns), j(ns)
	!----------------------------------------------------------------------------
	S = 0.D0
	do ip=1, np
		i=permu(:,ip)
		call get_order(ns,i,fi,gi)
		do jp=1, np
			j=permu(:,jp)
			call get_order(ns,j,fj,gj)
			if ( same_spin(gi,gj) ) then
				prod=1.D0
				do is=1, ns
					prod = prod * Sab( gi%func(is)%ib, gj%func(is)%ib )
				end do
				prod = prod / np
				prod = prod * p(ip) * p(jp)
				S = S + prod
! 				if (debug) then
! 					write(iout,'(2(2x, i4, 2x, 4i4, 2x, 4(2x, a,i0)), 4x, 4("(",i0,",",i0,")"), 2f15.7)')  &
! 						& ip, i, (gi%func(is)%ob,gi%func(is)%sp, is=1, ns), &
! 						& jp, j, (gj%func(is)%ob,gj%func(is)%sp, is=1, ns), &
! 						(gi%func(is)%ib,gj%func(is)%ib, is=1, ns), prod, S
! 				end if
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_single(fi, fj, H1)
	use class, only: DP, multi_fun, ns, np, p, permu, iout, debug, same_spin, Hab, Sab
	!----------------------------------------------------------------------------
	real(DP)        :: H1
	type(multi_fun) :: fi, fj
	type(multi_fun) :: gi, gj
	!----------------------------------------------------------------------------
	real(DP)        :: prod
	integer         :: is
	integer         :: ip, jp
	integer         :: i(ns), j(ns)
	!----------------------------------------------------------------------------
	H1 = 0.D0
	do ip=1, np
		i=permu(:,ip)
		call get_order(ns,i,fi,gi)
		do jp=1, np
			j=permu(:,jp)
			call get_order(ns,j,fj,gj)
			!----------------------------------------------------------------------
			if ( same_spin(gi,gj) ) then
				do m=1, ns
					prod=1.D0
					do is=1, ns
						if (is/=m) then
							prod = prod * Sab( gi%func(is)%ib, gj%func(is)%ib )
						end if
					end do
					prod = prod * Hab( gi%func(m)%ib, gj%func(m)%ib )
					prod = prod / np
					prod = prod * p(ip) * p(jp)
					H1 = H1 + prod
! 					if (debug) then
! 						write(iout,'(2(2x, i4, 2x, 4i4, 2x, 4(2x, a,i0)), 4x, 4("(",i0,",",i0,")"), 2f15.7)')  &
! 							& ip, i, (gi%func(is)%ob,gi%func(is)%sp, is=1, ns), &
! 							& jp, j, (gj%func(is)%ob,gj%func(is)%sp, is=1, ns), &
! 							(gi%func(is)%ib,gj%func(is)%ib, is=1, ns), prod, H1
! 					end if
				end do
			end if
			!----------------------------------------------------------------------
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_double(fi, fj, H2)
	use class, only: DP, multi_fun, ns, np, p, permu, iout, debug, same_spin, Sab, Jab
	!----------------------------------------------------------------------------
	real(DP)        :: H2
	type(multi_fun) :: fi, fj
	type(multi_fun) :: gi, gj
	!----------------------------------------------------------------------------
	real(DP)        :: prod
	integer         :: is
	integer         :: ip, jp
	integer         :: i(ns), j(ns)
	!----------------------------------------------------------------------------
	H2 = 0.D0
	do ip=1, np
		i=permu(:,ip)
		call get_order(ns,i,fi,gi)
		do jp=1, np
			j=permu(:,jp)
			call get_order(ns,j,fj,gj)
			!----------------------------------------------------------------------
			if ( same_spin(gi,gj) ) then
				do m=1, ns-1
					do n=m+1, ns
						prod=1.D0
						do is=1, ns
							if (is/=m .and. is/=n) then
								prod = prod * Sab( gi%func(is)%ib, gj%func(is)%ib )
							end if
						end do
						prod = prod * Jab( gi%func(m)%ib, gj%func(m)%ib, gi%func(n)%ib, gj%func(n)%ib )
						prod = prod / np
						prod = prod * p(ip) * p(jp)
						H2 = H2 + prod
! 						if (debug .and. abs(prod) > 1.D-6) then
! 							write(iout,'(2(2x, i4, 2x, 4i4, 2x, 4(2x, a,i0)),$)')  &
! 								& ip, i, (gi%func(is)%ob,gi%func(is)%sp, is=1, ns), &
! 								& jp, j, (gj%func(is)%ob,gj%func(is)%sp, is=1, ns)
! 							write(iout,'(4x,"(",a,a,"|",a,a,")", $)') &
! 								& gi%func(m)%ob,gj%func(m)%ob, gi%func(n)%ob,gj%func(n)%ob
! 							do is=1, ns
! 								if (is/=m .and. is/=n) then
! 									write(iout,'("(",a,",",a,")", $)')  &
! 										gi%func(is)%ob,gj%func(is)%ob
! 								end if
! 							end do
! 							write(iout,'(4x,"(",i0,i0,"|",i0,i0,")", $)') &
! 								& gi%func(m)%ib,gj%func(m)%ib, gi%func(n)%ib,gj%func(n)%ib
! 							do is=1, ns
! 								if (is/=m .and. is/=n) then
! 									write(iout,'("(",i0,",",i0,")", $)')  &
! 										gi%func(is)%ib,gj%func(is)%ib
! 								end if
! 							end do
! 							write(iout, '(2f15.11)') prod, H2
! 						end if
					end do
				end do
			end if
			!----------------------------------------------------------------------
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_order(ns,i,f,g)
	use class, only: DP, multi_fun
	!----------------------------------------------------------------------------
	integer         :: ns
	integer         :: i(ns)
	type(multi_fun) :: f
	type(multi_fun) :: g
	!----------------------------------------------------------------------------
	integer         :: k, j
	!----------------------------------------------------------------------------
	do j=1, ns
		k=i(j)
		g%func(j)%ob = f%func(k)%ob
		g%func(j)%ib = f%func(k)%ib
		g%func(j)%sp = f%func(k)%sp
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine read_data()
	use class, only: Sab, Hab, Jab, flog, iout
	use kinds, only: DP
	use file , only: name_main
	!----------------------------------------------------------------------------
	integer        :: fid
	integer        :: i,j,k,l
	integer        :: ios
	character(200) :: febt, line
	!----------------------------------------------------------------------------
	call getarg(1, febt)
	fid = 1
	!----------------------------------------------------------------------------
	flog=trim(name_main(febt))//".log"
	!----------------------------------------------------------------------------
	open(iout, file=flog)
	write(iout, '(2x,"Energy transfer")')
	!----------------------------------------------------------------------------
	open(fid, file=febt, status="old")
		read(fid,*)
		do i=1, 4
			read(fid,*) (Sab(i,j), j=1, 4)
		end do
		read(fid,*)
		do i=1, 4
			read(fid,*) (Hab(i,j), j=1, 4)
		end do
		read(fid, '(a)', iostat=ios) line
		read(fid, '(a)', iostat=ios) line
		do while(ios==0)
			read(line,*) i,j,k,l, Jab(i,j,k,l)
			call get_full_Jab(i,j,k,l, Jab)
			read(fid, '(a)', iostat=ios) line
		end do
		call check_AB()
	close(fid)
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine check_AB()
	use class, only: DP, Hab, Sab, Jab
	!----------------------------------------------------------------------------
	integer  :: i, j, k, l, ord(4)
	real(DP) :: Hab_new(4,4)
	real(DP) :: Sab_new(4,4)
	real(DP) :: Jab_new(4,4,4,4)
	!----------------------------------------------------------------------------
	if ( abs(Jab(1,2,2,1)) < abs(Jab(1,4,4,1)) ) then
		ord(1) = 1
		ord(2) = 4
		ord(3) = 3
		ord(4) = 2
		!-------------------------------------------------------------------------
		do i=1, 4
			do j=1, 4
				Hab_new(i,j) = Hab(ord(i), ord(j))
				Sab_new(i,j) = Sab(ord(i), ord(j))
			end do
		end do
		!-------------------------------------------------------------------------
		do i=1, 4
			do j=1, 4
				do k=1, 4
					do l=1, 4
						Jab_new(i,j,k,l) = Jab(ord(i), ord(j), ord(k), ord(l))
					end do
				end do
			end do
		end do
		!-------------------------------------------------------------------------
		Hab = Hab_new
		Sab = Sab_new
		Jab = Jab_new
		!-------------------------------------------------------------------------
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine get_full_Jab(i,j,k,l, Jab)
	use class, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer  :: i,j,k,l
	real(DP) :: Jab(4,4,4,4)
	!----------------------------------------------------------------------------
	Jab(j,i,k,l) = Jab(i,j,k,l)
	Jab(i,j,l,k) = Jab(i,j,k,l)
	Jab(j,i,l,k) = Jab(i,j,k,l)
	!
	Jab(k,l,i,j) = Jab(i,j,k,l)
	Jab(k,l,j,i) = Jab(i,j,k,l)
	Jab(l,k,i,j) = Jab(i,j,k,l)
	Jab(l,k,j,i) = Jab(i,j,k,l)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine init(fw)
	use class, only: multi_fun, get_ib
	!----------------------------------------------------------------------------
	type(multi_fun) :: fw(8)
	integer         :: i, j
	!----------------------------------------------------------------------------
	! state 1
	!
	fw(1)%func(1)%ob = "a"
	fw(1)%func(2)%ob = "A"
	fw(1)%func(3)%ob = "b"
	fw(1)%func(4)%ob = "b"
	!----------------------------------------------------------------------------
	fw(1)%func(1)%sp =  1
	fw(1)%func(2)%sp =  2
	fw(1)%func(3)%sp =  1
	fw(1)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	fw(5)%func(1)%ob = "a"
	fw(5)%func(2)%ob = "A"
	fw(5)%func(3)%ob = "b"
	fw(5)%func(4)%ob = "b"
	!----------------------------------------------------------------------------
	fw(5)%func(1)%sp =  2
	fw(5)%func(2)%sp =  1
	fw(5)%func(3)%sp =  1
	fw(5)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	! state 2
	!
	fw(2)%func(1)%ob = "a"
	fw(2)%func(2)%ob = "B"
	fw(2)%func(3)%ob = "b"
	fw(2)%func(4)%ob = "b"
	!----------------------------------------------------------------------------
	fw(2)%func(1)%sp =  1
	fw(2)%func(2)%sp =  2
	fw(2)%func(3)%sp =  1
	fw(2)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	fw(6)%func(1)%ob = "a"
	fw(6)%func(2)%ob = "B"
	fw(6)%func(3)%ob = "b"
	fw(6)%func(4)%ob = "b"
	!----------------------------------------------------------------------------
	fw(6)%func(1)%sp =  2
	fw(6)%func(2)%sp =  1
	fw(6)%func(3)%sp =  1
	fw(6)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	! state 3
	!
	fw(3)%func(1)%ob = "b"
	fw(3)%func(2)%ob = "A"
	fw(3)%func(3)%ob = "a"
	fw(3)%func(4)%ob = "a"
	!----------------------------------------------------------------------------
	fw(3)%func(1)%sp =  1
	fw(3)%func(2)%sp =  2
	fw(3)%func(3)%sp =  1
	fw(3)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	fw(7)%func(1)%ob = "b"
	fw(7)%func(2)%ob = "A"
	fw(7)%func(3)%ob = "a"
	fw(7)%func(4)%ob = "a"
	!----------------------------------------------------------------------------
	fw(7)%func(1)%sp =  2
	fw(7)%func(2)%sp =  1
	fw(7)%func(3)%sp =  1
	fw(7)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	! state 4
	!
	fw(4)%func(1)%ob = "b"
	fw(4)%func(2)%ob = "B"
	fw(4)%func(3)%ob = "a"
	fw(4)%func(4)%ob = "a"
	!----------------------------------------------------------------------------
	fw(4)%func(1)%sp =  1
	fw(4)%func(2)%sp =  2
	fw(4)%func(3)%sp =  1
	fw(4)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	fw(8)%func(1)%ob = "b"
	fw(8)%func(2)%ob = "B"
	fw(8)%func(3)%ob = "a"
	fw(8)%func(4)%ob = "a"
	!----------------------------------------------------------------------------
	fw(8)%func(1)%sp =  2
	fw(8)%func(2)%sp =  1
	fw(8)%func(3)%sp =  1
	fw(8)%func(4)%sp =  2
	!----------------------------------------------------------------------------
	do i=1, 8
		do j=1, 4
			call get_ib(fw(i)%func(j))
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
