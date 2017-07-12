program cube_product
	use kinds, only: DP
	use file , only: name_main
	use param, only: au2a
	implicit none
	!------------------------------------------------------------------------------
	integer                     :: fid, i, j, ix, iy, iz
	integer                     :: natom, n(3)
	real(DP)                    :: a(3,3)
	real(DP)                    :: x0(3)
	real(DP)                    :: dip(3)
	real(DP)                    :: dx(3), dv, chat, coc(3), r(3), norm(2), scal
	integer       , allocatable :: nat(:)
	real(DP)      , allocatable :: charge(:)
	real(DP)      , allocatable :: x(:,:)
	real(DP)      , allocatable :: grid1(:,:,:)
	real(DP)      , allocatable :: grid2(:,:,:)
	real(DP)      , allocatable :: grid3(:,:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: fname1, fname2, fout, line1, line2, fxyz, tmp
	!------------------------------------------------------------------------------
	fid = 1
	call getarg(1, fname1)
	call getarg(2, fname2)
	call getarg(3, fout)
	i=iargc()
	scal=1.0
	if (i==4) then
		call getarg(4, tmp)
		read(tmp, *) scal
	end if
	fxyz=trim(name_main(fout))//".xyz"
	!------------------------------------------------------------------------------
	open(fid, file=fname1, status="old")
		!---------------------------------------------------------------------------
		read(fid, '(a)') line1
		read(fid, '(a)') line2
		!---------------------------------------------------------------------------
		read(fid,*) natom, x0
		do i=1, 3
			read(fid,*) n(i), a(:,i)
		end do
		!---------------------------------------------------------------------------
		natom=abs(natom)
		!---------------------------------------------------------------------------
		allocate(nat   (  natom         ))
		allocate(symbol(  natom         ))
		allocate(charge(  natom         ))
		allocate(x     (3,natom         ))
		allocate(grid1 (n(1), n(2), n(3)))
		allocate(grid2 (n(1), n(2), n(3)))
		allocate(grid3 (n(1), n(2), n(3)))
		!---------------------------------------------------------------------------
		do i=1, 3
			dx(i) = a(i,i)
		end do
		dv = dx(1) * dx(2) * dx(3)
		!---------------------------------------------------------------------------
		do i=1, natom
			read(fid,*) nat(i), charge(i), x(:,i)
		end do
		!---------------------------------------------------------------------------
		do ix=1, n(1)
			do iy=1, n(2)
				read(fid,*) (grid1(ix,iy,iz), iz=1, n(3))
			end do
		end do
		!---------------------------------------------------------------------------
	close(fid)
	!------------------------------------------------------------------------------
	open(fid, file=fname2, status="old")
		!---------------------------------------------------------------------------
		read(fid, '(a)') line1
		read(fid, '(a)') line2
		!---------------------------------------------------------------------------
		read(fid,*) natom, x0
		do i=1, 3
			read(fid,*) n(i), a(:,i)
		end do
		!---------------------------------------------------------------------------
		natom=abs(natom)
		!---------------------------------------------------------------------------
		do i=1, natom
			read(fid,*) nat(i), charge(i), x(:,i)
		end do
		!---------------------------------------------------------------------------
		do ix=1, n(1)
			do iy=1, n(2)
				read(fid,*) (grid2(ix,iy,iz), iz=1, n(3))
			end do
		end do
		!---------------------------------------------------------------------------
	close(fid)
	!------------------------------------------------------------------------------
	grid3 = grid1 * grid2
	!------------------------------------------------------------------------------
	dip  = 0.D0
	norm = 0.D0
	do ix=1, n(1)
		r(1)=x0(1) + (ix-1) * dx(1) + dx(1)
		do iy=1, n(2)
			r(2)=x0(2) + (iy-1) * dx(2) + dx(2)
			do iz=1, n(3)
				r(3)=x0(3) + (iz-1) * dx(3) + dx(3)
				norm(1) = norm(1) + grid1(ix,iy,iz) * grid1(ix,iy,iz)
				norm(2) = norm(2) + grid2(ix,iy,iz) * grid2(ix,iy,iz)
				dip = dip + grid3(ix,iy,iz) * r
			end do
		end do
	end do
	norm = 1.D0 / sqrt(norm)
	dip = dip * dv * norm(1) * norm(2)
	!------------------------------------------------------------------------------
	grid3 = grid3 * scal
	dip   = dip * scal
	!------------------------------------------------------------------------------
	open(fid, file=fout)
		write(fid, '(x,a)') trim(adjustl(line1))
		write(fid, '(x,a)') trim(adjustl(line2))
		!---------------------------------------------------------------------------
		write(fid, '(i5,3f12.6,i5)') -natom, x0, 1
		!---------------------------------------------------------------------------
		do i=1, 3
			write(fid, '(i5, 3f12.6)') n(i), a(:,i)
		end do
		!---------------------------------------------------------------------------
		do i=1, natom
			write(fid, '(i5, 4f12.6)') nat(i), charge(i), x(:,i)
		end do
		write(fid, '(2i5)') 1, 1
		!---------------------------------------------------------------------------
		do ix=1, n(1)
			do iy=1, n(2)
				write(fid,'(6e13.5)') (grid3(ix,iy,iz), iz=1, n(3))
			end do
		end do
		write(fid,*)
		!---------------------------------------------------------------------------
	close(fid)
	!------------------------------------------------------------------------------
	chat = 0.D0
	coc  = 0.D0
	do i=1, natom
		chat = chat   + charge(i)
		coc  = x(:,i) * charge(i)
	end do
	coc = coc / chat
	!------------------------------------------------------------------------------
	call nat_to_symbol(natom, nat, symbol)
	!------------------------------------------------------------------------------
	open(fid, file=fxyz)
		write(fid,'(i4)') natom + 1
		write(fid,'("title")')
		do i=1, natom
			write(fid,'(a4,6f16.10)') symbol(i), (x(j,i)*au2a,j=1,3),(0.D0,j=1,3)
		end do
		write(fid,'(a4,6f16.10)') "X ", (coc(j)*au2a,j=1,3), (dip(j),j=1,3)
	close(fid)
	!------------------------------------------------------------------------------
	deallocate(nat    )
	deallocate(symbol )
	deallocate(charge )
	deallocate(x      )
	deallocate(grid1  )
	deallocate(grid2  )
	deallocate(grid3  )
	!------------------------------------------------------------------------------
	stop
end
