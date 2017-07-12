subroutine point_group1(n, x, nat, mass, c, nr_out, ir_lw, ir_hi, ir_name_out, pgrp, mirror, id3, id9, tol)
	use kinds    , only: DP
	use symmetry1, only: ir_name, npg, pg_name, nr, nrep, op
	use param,     only: au2a
	implicit none
	!--------------------------------------------------------------------
	! Input variables
	!
	integer     , intent(in ) :: n
	integer     , intent(in ) :: nat(n)
	real(DP)    , intent(in ) :: x(3,n)
	real(DP)    , intent(in ) :: mass(3,n)
	real(DP)    , intent(in ) :: c(n*3,n*3)
	real(DP)    , intent(in ) :: tol
	!--------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: nr_out
	logical     , intent(out) :: mirror(3)
	integer     , intent(out) :: id3(3,3,n)
	integer     , intent(out) :: id9(5,3,n,3,n)
	integer     , intent(out) :: ir_lw(8)
	integer     , intent(out) :: ir_hi(8)
	character(3), intent(out) :: ir_name_out(8)
	character(6), intent(out) :: pgrp
	!--------------------------------------------------------------------
	! Logical variables
	!
	integer                   :: j, k, l, n3, ir, info, ip
	integer                   :: ia, ix
	integer                   :: ja, jx, js
	integer                   :: ka, kx, ks, k3
	integer                   :: la, lx, ls, l3
	integer                   :: n_symm
	integer                   :: n_symm2
	integer                   :: jreln(n,8)
	integer                   :: jreln3(n*3,8)
	integer                   :: jreln9(n*3,n*3,8)
	integer     , allocatable :: nrs(:)
	integer     , allocatable :: idx_symm (  :)
	integer     , allocatable :: idx_symm2(:,:)
	real(DP)                  :: dx
	real(DP)    , allocatable :: x0(:,:)
	real(DP)    , allocatable :: x_symm(:,:)
	character(1)              :: cxyz(3)
	character(2), allocatable :: symbol(:)
	character(8), allocatable :: name_n3(:)
	logical                   :: std
	!--------------------------------------------------------------------
	call check_x(n, x, info)
	if (info/=0) then
		write(*,*)
		write(*,*) "Error! Wrong coordinate in subroutine point_group1"
		write(*,*)
		stop
	end if
	!--------------------------------------------------------------------
	std = .false.
	n3  = n*3
	allocate(x0       (      3,n ))
	allocate(x_symm   (      3,n ))
	allocate(idx_symm (        n ))
	allocate(idx_symm2(    2,n*n ))
	allocate(symbol   (        n ))
	allocate(nrs      (        n ))
	allocate(name_n3  (        n3))
	!--------------------------------------------------------------------
	x0=x
	!--------------------------------------------------------------------
	cxyz(1)="x"
	cxyz(2)="y"
	cxyz(3)="z"
	do ia=1, n
		do j=1,3
			k=(ia-1)*3+j
			write(name_n3(k),'(i0,"(",a,")")') ia, cxyz(j)
		end do
	end do
	!--------------------------------------------------------------------
	! transform into inertial coordinate system
	!
	! call inertm(n, mass, x0)
	!--------------------------------------------------------------------
	call search_op1 (n, x0, nat, mass, jreln, jreln3, jreln9, nrs, tol)
	!--------------------------------------------------------------------
	if (std) then
		call std_orient1(n, x0)
		call search_op1 (n, x0, nat, mass, jreln, jreln3, jreln9, nrs, tol)
	end if
	!--------------------------------------------------------------------
	mirror = .false.
	if (op(6)) mirror(3) = .true.
	if (op(7)) mirror(2) = .true.
	if (op(8)) mirror(1) = .true.
	!--------------------------------------------------------------------
	nr=nrep(npg)
	nr_out=nr
	ir_name_out=""
	do ir=1, nr
		ir_name_out(ir)=ir_name(ir,npg)
	end do
	!--------------------------------------------------------------------
	call search_op3 (n3, jreln3, name_n3, ir_lw, ir_hi, c)
	!--------------------------------------------------------------------
	call nat_to_symbol(n,nat,symbol)
	!call write_gjf("1.gjf", n, symbol, x0)
	!call write_xyz("1.xyz", n, symbol, x0)
	pgrp=pg_name(npg)
	!--------------------------------------------------------------------
! 	n_symm      = 1
! 	idx_symm(1) = 1
! 	!--------------------------------------------------------------------
! 	do ix=1, 3
! 		id3(1,ix,1) = 1
! 		id3(2,ix,1) = ix
! 		id3(3,ix,1) = 1
! 	end do
	n_symm = 0
	!--------------------------------------------------------------------
	do ia=1, n
		call in_coord_basis(n, n_symm, jreln, idx_symm, ia, ja, ip)
		if (ja==0) then
			n_symm = n_symm + 1
			idx_symm(n_symm) = ia
			do ix=1, 3
				id3(1,ix,ia) = ia
				id3(2,ix,ia) = ix
				id3(3,ix,ia) = 1
			end do
		else
			do ix=1, 3
				k3 = (ja-1)*3 + ix
				k  = sign(abs(jreln3(k3,ip)) - (ia-1)*3, jreln3(k3,ip))
				jx = abs(k)
				js = sign(1,k)
				id3(1,ix,ia) = ja
				id3(2,ix,ia) = jx
				id3(3,ix,ia) = js
			end do
		end if
	end do
	!--------------------------------------------------------------------
	n_symm2 = 0
	!--------------------------------------------------------------------
	do ia=1, n
		do ja=1, n
			call in_coord_basis2(n, n_symm2, jreln, idx_symm2, ia, ja, ka, la, ip)
			if (ka==0) then
				n_symm2 = n_symm2 + 1
				idx_symm2(1,n_symm2) = ia
				idx_symm2(2,n_symm2) = ja
				!-----------------------------------------------------------
				do ix=1, 3
					do jx=1, 3
						id9(1,jx,ja,ix,ia) = ia
						id9(2,jx,ja,ix,ia) = ix
						id9(3,jx,ja,ix,ia) = ja
						id9(4,jx,ja,ix,ia) = jx
						id9(5,jx,ja,ix,ia) = 1
					end do
				end do
				!-----------------------------------------------------------
			else
				!-----------------------------------------------------------
				do ix=1, 3
					!--------------------------------------------------------
					k3 = (ka-1)*3 + ix
					k  = sign(abs(jreln3(k3,ip)) - (ia-1)*3, jreln3(k3,ip))
					kx = abs(k)
					ks = sign(1,k)
					!--------------------------------------------------------
					do jx=1, 3
						!-----------------------------------------------------
						l3 = (la-1)*3 + jx
						l  = sign(abs(jreln3(l3,ip)) - (ja-1)*3, jreln3(l3,ip))
						lx = abs(l)
						ls = sign(1,l)
						!-----------------------------------------------------
						id9(1,jx,ja,ix,ia) = ka
						id9(2,jx,ja,ix,ia) = kx
						id9(3,jx,ja,ix,ia) = la
						id9(4,jx,ja,ix,ia) = lx
						id9(5,jx,ja,ix,ia) = ks * ls
						!-----------------------------------------------------
					end do
				end do
			end if
		end do
	end do
	!--------------------------------------------------------------------
	dx = 0.D0
	do ia=1, n
		do ix=1, 3
			ja = id3(1,ix,ia)
			jx = id3(2,ix,ia)
			js = id3(3,ix,ia)
			x_symm(ix,ia) = x(jx,ja) * js
			dx = dx + (x(ix,ia)-x_symm(ix,ia))**2
		end do
	end do
	!--------------------------------------------------------------------
	dx = dx / n3
	dx = sqrt(dx)
	!--------------------------------------------------------------------
! 	do ia=1, n
! 		do ix=1, 3
! 			do ja=1, n
! 				do jx=1, 3
! 					id9(1,jx,ja,ix,ia) = id3(1,ix,ia)
! 					id9(2,jx,ja,ix,ia) = id3(2,ix,ia)
! 					id9(3,jx,ja,ix,ia) = id3(1,jx,ja)
! 					id9(4,jx,ja,ix,ia) = id3(2,jx,ja)
! 					id9(5,jx,ja,ix,ia) = id3(3,ix,ia) * id3(3,jx,ja)
! 					! write(*,'(2(2i4,2x), 4x, 2(2i4,2x), 4x, i4)') ia,ix,ja,jx, id9(:,jx,ja,ix,ia)
! 				end do
! 			end do
! 		end do
! 	end do
	!--------------------------------------------------------------------
	if (dx > tol) then
		write(*,*) "x_symm is not the same as x, stop!"
		stop
	end if
	!--------------------------------------------------------------------
	deallocate(x0       )
	deallocate(x_symm   )
	deallocate(idx_symm )
	deallocate(idx_symm2)
	deallocate(nrs      )
	deallocate(symbol   )
	deallocate(name_n3  )
	!--------------------------------------------------------------------
	return
end subroutine
