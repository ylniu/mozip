module spins
	implicit none
	!----------------------------------------------------------------------------
	type, public:: spin
		integer     , public :: sp
		character(1), public :: nm
		integer     , public :: ci
		integer     , public :: cr
	end type
	!----------------------------------------------------------------------------
contains
	!----------------------------------------------------------------------------
	subroutine get_name(this)
		type(spin) :: this
		if (this%sp==1) then
			this%nm="a"
		else if (this%sp==2) then
			this%nm="b"
		end if
		return
	end subroutine
	!----------------------------------------------------------------------------
	subroutine init(this)
		type(spin) :: this
		this%sp=1
		this%cr=1
		this%ci=0
		call get_name(this)
		return
	end subroutine
	!----------------------------------------------------------------------------
	subroutine spin_set(this, sp, cr, ci)
		type(spin) :: this
		integer    :: sp
		integer    :: cr
		integer    :: ci
		!-------------------------------------------------------------------------
		this%sp=sp
		this%cr=cr
		this%ci=ci
		call get_name(this)
		return
	end subroutine
	!----------------------------------------------------------------------------
	subroutine spin_get(this, sp, cr, ci)
		type(spin) :: this
		integer    :: sp
		integer    :: cr
		integer    :: ci
		!-------------------------------------------------------------------------
		sp=this%sp
		cr=this%cr
		ci=this%ci
		call get_name(this)
		return
	end subroutine
	!----------------------------------------------------------------------------
	subroutine spin_rot(this,j)
		type(spin) :: this
		integer    :: sp
		integer    :: cr
		integer    :: ci
		integer    :: cr0
		integer    :: ci0
		integer    :: j
		!-------------------------------------------------------------------------
		sp  = this%sp
		cr0 = this%cr
		ci0 = this%ci
		!-------------------------------------------------------------------------
		if (sp==1) then
			if (j==1) then
				sp =  2
				cr =  1
				ci =  0
			else if (j==2) then
				sp =  2
				cr =  0
				ci =  1
			else if (j==3) then
				sp =  1
				cr =  1
				ci =  0
			end if
		else if(sp==2) then
			if (j==1) then
				sp =  1
				cr =  1
				ci =  0
			else if (j==2) then
				sp =  1
				cr =  0
				ci = -1
			else if (j==3) then
				sp =  2
				cr = -1
				ci =  0
			end if
		end if
		!-------------------------------------------------------------------------
		this%sp = sp
		this%cr = cr0 * cr - ci0 * ci
		this%ci = cr0 * ci + ci0 * cr
		!-------------------------------------------------------------------------
		call get_name(this)
		return
	end subroutine
	!----------------------------------------------------------------------------
end module
!
!-------------------------------------------------------------------------------
!
program spin_function
	use kinds, only:DP
	use math,  only: factorial
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: i, j, k, l, fid, info, iout
	integer                      :: idet, iorb, jorb
	integer                      :: norb, np, ndet, nn, ngen, nmax
	integer        , allocatable :: fun1(:,:)
	integer        , allocatable :: fun2(:,:)
	integer        , allocatable :: permu(:,:)
	integer        , allocatable :: orb(:,:,:)
	integer        , allocatable :: det_sign(:)
	integer        , allocatable :: fun(:,:,:)
	integer        , allocatable :: fun_gen(:,:,:)
	character(  1) , allocatable :: sname(:,:)
	character(  1) , allocatable :: signn(  :)
	character(200)               :: finp, fout, fmt
	logical        , allocatable :: peven(:)
	logical                      :: condition
	logical                      :: debug, if_eigen
	integer                      :: eigen
	character(  1)               :: sp_name(2)
	!----------------------------------------------------------------------------
	sp_name(1) = "a"
	sp_name(2) = "b"
	!----------------------------------------------------------------------------
	debug = .false.
	fid   = 1
	iout  = 2
	fout  = "orbital.out"
	!----------------------------------------------------------------------------
	open(iout, file=fout)
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		read(fid,*) ndet
		read(fid,*) norb
		!-------------------------------------------------------------------------
		np=factorial(norb)
		nmax = np*ndet*10
		!-------------------------------------------------------------------------
		allocate(orb      (2,norb  ,   ndet  ))
		allocate(det_sign (            ndet  ))
		allocate(fun      (  norb,3,np*ndet  ))
		allocate(fun_gen  (  norb,3,nmax     ))
		allocate(fun1     (  norb,3          ))
		allocate(fun2     (  norb,3          ))
		allocate(sname    (  norb,  np*ndet  ))
		allocate(signn    (         np*ndet  ))
		allocate(peven    (np                ))
		allocate(permu    (  norb,       np  ))
		!-------------------------------------------------------------------------
		write(fmt,*) norb
		!-------------------------------------------------------------------------
		do idet=1, ndet
			read(fid,*)
			read(fid,*) det_sign(idet)
			do iorb=1, norb
				read(fid,*) orb(:,iorb, idet)
			end do
		end do
		!-------------------------------------------------------------------------
	close(fid)
	!----------------------------------------------------------------------------
	j=0
	fun=0
	do idet=1, ndet
		!-------------------------------------------------------------------------
		call TESTNEXPER(norb,np, permu,peven)
		!-------------------------------------------------------------------------
		do i=1, np
			j=j+1
			if (peven(i)) then
				fun(1,1,j) =  1 * det_sign(idet)
			else
				fun(1,1,j) = -1 * det_sign(idet)
			end if
			!----------------------------------------------------------------------
			write(iout,'(i4, 4x, '//trim(fmt)//'i4)') i, permu(:,i)
			do k=1, norb
				fun(k,2,j) = orb(1, permu(k,i), idet)
				fun(k,3,j) = orb(2, permu(k,i), idet)
			end do
			!----------------------------------------------------------------------
		end do
	end do
	nn=j
	!----------------------------------------------------------------------------
	do i=1, nn-1
		do j=i+1, nn
			condition = .true.
			do iorb=1, norb
				condition = condition .and. fun(iorb,2,i) == fun(iorb,2,j)
				condition = condition .and. fun(iorb,3,i) == fun(iorb,3,j)
			end do
			if (condition .and. debug) then
				write(iout,'("equal", 4x, 2(2i4, 4x, '//trim(fmt)//'(4x, i4, i4)))') &
					& i, fun(1,1,i), (fun(k,2,i), fun(k,3,i), k=1, norb), &
					& j, fun(1,1,j), (fun(k,2,j), fun(k,3,j), k=1, norb)
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	ngen    = 0
	do i=1, nn
		fun1=fun(:,:,i)
		fun2=fun(:,:,i)
		do iorb=1, norb
			do jorb=1, norb
				do j=1, 3
					call spin_act(norb, iorb, jorb, j, fun1, fun2, iout, debug)
					call fun_add(nmax, norb, ngen, fun_gen, fun2, iout, debug)
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	do j=1, nn
		if (fun(1,1,j) == 1) then
				signn(j) = "+"
			else
				signn(j) = "-"
			end if
		do iorb=1, norb
			if (fun(iorb,3,j) == 1) then
				sname(iorb,j) = "a"
			else
				sname(iorb,j) = "b"
			end if
		end do
	end do
	!----------------------------------------------------------------------------
! 	do idet=1, ndet
! 		write(iout,*)
! 		do iorb=1, norb
! 			write(iout,*) orb(:,iorb, idet)
! 		end do
! 	end do
	!----------------------------------------------------------------------------
	if (ngen==0) then
		eigen = 0
		if_eigen = .true.
	else
		call find_eigen(nn, nmax, norb, ngen, fun, fun_gen, eigen, if_eigen, iout, debug)
	end if
	!----------------------------------------------------------------------------
	write(iout,'("nn = ", i4, 4x, "ngen = ", i4)') nn, ngen
	
	do j=1, nn
		write(iout,'(2i4, '//trim(fmt)//'(4x, i4, a4))') j,  fun(1,1,j), (fun(k,2,j), trim(sname(k,j)), k=1, norb)
	end do
	write(iout,*)
	do j=1, ngen
		write(iout,'(2i4, '//trim(fmt)//'(4x, i4, a4))') j,  fun_gen(1,1,j), (fun_gen(k,2,j), sp_name(fun_gen(k,3,j)), k=1, norb)
	end do
	!----------------------------------------------------------------------------
	if (if_eigen) then
		write(iout,'(2x, "Eigen value: ", i4)') eigen
	else
		write(iout,'(2x, "Not eigen state")')
	end if
	!----------------------------------------------------------------------------
	close(iout)
	!----------------------------------------------------------------------------
	deallocate(orb     )
	deallocate(fun     )
	deallocate(det_sign)
	deallocate(fun_gen )
	deallocate(fun1    )
	deallocate(fun2    )
	deallocate(peven   )
	deallocate(permu   )
	deallocate(sname   )
	deallocate(signn   )
	!----------------------------------------------------------------------------
	stop
end
!
!-------------------------------------------------------------------------------
!
subroutine spin_act(norb, iorb, jorb, j, fun1, fun2, iout, debug)
	use spins
	implicit none
	integer        :: iout
	integer        :: norb, iorb, jorb, j
	integer        :: i
	integer        :: fun1(norb, 3)
	integer        :: fun2(norb, 3)
	type(spin)     :: ispin
	logical        :: debug
	character(  2) :: sp_name(2)
	character(200) :: fmt
	sp_name(1) = "a"
	sp_name(2) = "b"
	!----------------------------------------------------------------------------
	fun2 = fun1
	write(fmt,*) norb
	!----------------------------------------------------------------------------
	! Step 1
	!----------------------------------------------------------------------------
	if (debug) then
		write(iout,'("-------------------------------------------------------------------")')
	end if
	call spin_act_single(norb, iorb, j, fun2, iout, debug)
	call spin_act_single(norb, jorb, j, fun2, iout, debug)
	if (debug) then
		write(iout,'(2x, "Multi-spin-State :", " coeff     =", 2i4, " ; spin = ", '//trim(fmt)//'a4)') &
			& fun2(1:2,1), (sp_name(fun2(i,3)), i=1, norb)
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine spin_act_single(norb, iorb, j, fun, iout, debug)
	use spins
	implicit none
	integer                    :: iout
	integer                    :: norb, iorb, j
	integer                    :: fun(norb, 3)
	integer                    :: i
	type(spin)                 :: ispin
	logical                    :: debug
	character( 2)              :: op_name(3)
	character( 2)              :: sp_name(2)
	character(40)              :: fmt
	character( 2), allocatable :: ops(:)
	!----------------------------------------------------------------------------
	write(fmt,*) norb
	op_name(1) = "Sx"
	op_name(2) = "Sy"
	op_name(3) = "Sz"
	sp_name(1) = "a"
	sp_name(2) = "b"
	!----------------------------------------------------------------------------
	allocate(ops(norb))
	ops="  "
	ops(iorb) = op_name(j)
	!----------------------------------------------------------------------------
	call spin_set(ispin, fun(iorb,3), fun(1,1), fun(2,1))
	!----------------------------------------------------------------------------
	if (debug) then
		write(iout,'(2x, "Orbital    =", i4, " ; coeff     =", 2i4, " ; spin = ", '//trim(fmt)//'a4)') &
			& iorb, fun(1:2,1), (sp_name(fun(i,3)), i=1, norb)
		write(iout,'(2x, "Opertation =", 35x, '//trim(fmt)//'a4)') ops
	end if
	!----------------------------------------------------------------------------
	call spin_rot(ispin,j)
	!----------------------------------------------------------------------------
	fun(   1, 1) = ispin%cr
	fun(   2, 1) = ispin%ci
	fun(iorb, 3) = ispin%sp
	!----------------------------------------------------------------------------
	deallocate(ops)
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine fun_add(nmax, norb, ngen, fun_gen, fun, iout, debug)
	use spins
	implicit none
	integer       :: iout
	integer       :: nmax, norb, ngen
	integer       :: fun    (norb, 3)
	integer       :: fun_gen(norb, 3, nmax)
	integer       :: i, j, k, n, iorb
	logical       :: debug
	logical       :: find
	character(20) :: fmt
	!----------------------------------------------------------------------------
	n = 0
	write(fmt,*) norb
	!----------------------------------------------------------------------------
	do i=1, ngen
		find = .true.
		do j=2,3
			do k=1, norb
				find = find .and. fun(k,j) == fun_gen(k,j,i)
				if (.not.find) exit
			end do
			if (.not.find) exit
		end do
		if (find) then
			iorb = i
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	if (ngen==0) find = .false.
	!----------------------------------------------------------------------------
! 	if (ngen>=0 .and. .not. find) then
! 		write(iout,*)
! 		write(iout,'("fun1 ", i4, 4x, 2i4, 8x, 2(4x, '//trim(fmt)//'i4))') &
! 			& ngen, fun(1:2,1), fun(:,2), fun(:,3)
! 	end if
	!----------------------------------------------------------------------------
	if (find) then
		fun_gen(:,1,iorb) = fun_gen(:,1,iorb) + fun(:,1)
		if (fun_gen(1,1,iorb)==0 .and. fun_gen(2,1,iorb)==0) then
			call fun_del(nmax, norb, ngen, fun_gen, iorb, iout, debug)
		end if
	else
		ngen = ngen + 1
		fun_gen(:,:,ngen) = fun
	end if
	!----------------------------------------------------------------------------
! 	if (ngen>0 .and. .not. find) then
! 		write(iout,'("fun2 ", i4, 4x, 2i4, 8x, 2(4x, '//trim(fmt)//'i4))') &
! 			& ngen, fun_gen(1:2,1,ngen), fun_gen(:,2,ngen), fun_gen(:,3,ngen)
! 	end if
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine fun_del(nmax, norb, ngen, fun_gen, iorb, iout, debug)
	use spins
	implicit none
	integer       :: iout
	integer       :: nmax, norb, ngen
	integer       :: iorb
	integer       :: fun_gen(norb, 3, nmax)
	logical       :: debug
	!----------------------------------------------------------------------------
	fun_gen(:,:,iorb:ngen-1) = fun_gen(:,:,iorb+1:ngen)
	fun_gen(:,:,ngen:) = 0
	ngen = ngen - 1
	!----------------------------------------------------------------------------
	return
end subroutine
!
!-------------------------------------------------------------------------------
!
subroutine find_eigen(nn, nmax, norb, ngen, fun, fun_gen, eigen0, if_eigen, iout, debug)
	use spins
	implicit none
	integer              :: nn, iout
	integer              :: nmax, norb, ngen
	integer              :: fun(norb, 3, nn)
	integer              :: fun_gen(norb, 3, nmax)
	integer              :: eigen, eigen0, icheck
	logical              :: debug, if_eigen, find
	logical, allocatable :: checked(:)
	character(  1)       :: sp_name(2)
	character(200)       :: fmt
	!----------------------------------------------------------------------------
	integer              :: i, j, k, l
	!----------------------------------------------------------------------------
	sp_name(1) = "a"
	sp_name(2) = "b"
	write(fmt,*) norb
	!----------------------------------------------------------------------------
	if_eigen = .true.
	if (nn/=ngen) then
		if_eigen = .false.
		return
	end if
	!----------------------------------------------------------------------------
	allocate(checked(nn))
	!----------------------------------------------------------------------------
	checked = .false.
	icheck  = 0
	do i=1, nn
		do l=1, nn
			if (.not.checked(l)) then
				!-------------------------------------------------------------------
				eigen = fun_gen(1,1,i) / fun(1,1,l)
				!-------------------------------------------------------------------
				find = .true.
				do j=2,3
					do k=1, norb
						find = find .and. fun_gen(k,j,i) == fun(k,j,l)
						if (.not.find) exit
					end do
					if (.not.find) exit
				end do
				!-------------------------------------------------------------------
				if (find) then
					icheck     = icheck + 1
					checked(l) = .true.
					if (icheck == 1) then
						eigen0 = eigen
					else
						if (eigen /= eigen0) then
							if_eigen = .false.
							return
						end if
					end if
					exit
				end if
				!-------------------------------------------------------------------
			end if
		end do
		if (.not. find) then
			if_eigen = .false.
			return
		end if
	end do
	!----------------------------------------------------------------------------
	deallocate(checked)
	!----------------------------------------------------------------------------
	return
end subroutine