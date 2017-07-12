program gro_gather
	use kinds, only: DP
	use file , only: name_main
	use string, only: StrLowCase
	use string, only: StrUpCaseFirst
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid1, fid2, fid3, ia, ib, nf
	integer                     :: i, j, k, im, ires0, ires0_old, ires
	integer                     :: natoms, iat
	integer       , allocatable :: natom(:)
	real(DP)                    :: x(3), xmin(3), xmax(3), border
	real(DP)                    :: xcenter(3), ccenter(3), dcenter(3)
	real(DP)                    :: dv1(3), dv2(3), dv3(3)
	real(DP)      , allocatable :: v1(:,:), v2(:,:), v3(:,:)
	character(  3)              :: res
	character(  7)              :: symbol, sym
	character(200)              :: fout, fxyz, tmp, line, fmt
	character(200), allocatable :: fname(:)
	logical                     :: lcenter, lborder
	logical       , external    :: is_number
	!----------------------------------------------------------------------------
	fid1   = 1
	fid2   = 2
	fid3   = 3
	lcenter = .false.
	lborder = .false.
	!----------------------------------------------------------------------------
	ib=iargc()
	!----------------------------------------------------------------------------
	call getarg(ib-1,tmp)
	if(trim(tmp)=="border") then
		!lcenter=.true.
		lborder=.true.
		call getarg(ib,tmp)
		read(tmp,*) border
		ia = ib - 2
	else
		ia = ib
	end if
	nf = ia - 1
	!----------------------------------------------------------------------------
	allocate(fname(   nf))
	allocate(natom(   nf))
	allocate(v1   (3, nf))
	allocate(v2   (3, nf))
	allocate(v3   (3, nf))
	do i=1, ia-1
		call getarg(i, fname(i))
	end do
	call getarg(ia, fout)
	fxyz=trim(name_main(fout))//".xyz"
	!----------------------------------------------------------------------------
	open(fid1, file=fout)
	open(fid3, file=fxyz)
		!-------------------------------------------------------------------------
		! Read natom
		!
		natoms = 0
		do i=1, ia-1
			open(fid2, file=fname(i), status="old")
				read(fid2, *)
				read(fid2, *) natom(i)
			close(fid2)
			natoms = natoms + natom(i)
		end do
		!-------------------------------------------------------------------------
		write(fid1, '("Title")')
		write(fid1, '(i0)') natoms
		!-------------------------------------------------------------------------
		write(fid3, '(i0)') natoms
		write(fid3, '("Title")')
		!-------------------------------------------------------------------------
		k         = 0
		ires      = 0
		ires0_old = 0
		!-------------------------------------------------------------------------
		xmin= 1.D99
		xmax=-1.D99
		do i=1, nf
			open(fid2, file=fname(i), status="old")
				read(fid2, *)
				read(fid2, *)
				do j=1, natom(i)
					read(fid2,'(a)') line
					read(line, '(i5, a3, a7, 5x, 3f8.3, i20)') &
						& ires0, res, symbol, x, iat
					if (xmin(1) > x(1) ) xmin(1) = x(1)
					if (xmin(2) > x(2) ) xmin(2) = x(2)
					if (xmin(3) > x(3) ) xmin(3) = x(3)
					if (xmax(1) < x(1) ) xmax(1) = x(1)
					if (xmax(2) < x(2) ) xmax(2) = x(2)
					if (xmax(3) < x(3) ) xmax(3) = x(3)
				end do
				read(fid2,'(9f10.5)') &
					& v1(1,i), v2(2,i), v3(3,i), &
					& v1(2,i), v1(3,i), v2(1,i), &
					& v2(3,i), v3(1,i), v3(2,i)
				!-------------------------------------------------------------------
			close(fid2)
		end do
		!-------------------------------------------------------------------------
		! xcenter = (xmin + xmax) / 2.D0
		! ccenter = (v1(:,nf) + v2(:,nf) + v3(:,nf)) / 2.D0
		! dcenter = ccenter - xcenter
		dv1 = v1(:,nf) * (border - 1.D0)
		dv2 = v2(:,nf) * (border - 1.D0)
		dv3 = v3(:,nf) * (border - 1.D0)
		dcenter = (dv1 + dv2 + dv3) / 2.D0
		if(lborder) write(*,'("lcenter = .true., move ", 3f15.7)') dcenter
		!-------------------------------------------------------------------------
		do i=1, nf
			open(fid2, file=fname(i), status="old")
				read(fid2, *)
				read(fid2, *)
				do j=1, natom(i)
					read(fid2,'(a)') line
					read(line, '(i5, a3, a7, 5x, 3f8.3, i20)') &
						& ires0, res, symbol, x, iat
					if (iat==1) then
						ires = ires + 1
					else
						if (ires0/=ires0_old) then
							ires = ires + 1
						end if
					end if
					!----------------------------------------------------------------
					sym=trim(adjustl(symbol))
					do k=1, len(trim(sym))
						if (is_number(sym(k:k))) then
							sym(k:k)=" "
						end if
					end do
					sym=trim(adjustl(sym))
					sym=StrLowCase(sym)
					sym=StrUpCaseFirst(sym)
					!----------------------------------------------------------------
					ires0_old = ires0
					k = k + 1
					if (lborder) x = x + dcenter
					!write(fid1,'(i5, a3, a7, i5, 3f8.3, i20)') &
					!	& ires, res, symbol, k, x, iat
					write(fid1,'(i5, a3, a7, i5, 3f8.3)') &
						& ires, res, symbol, k, x
					write(fid3,'(a2, 2x, 3f15.7)') trim(sym), x * 10.D0
				end do
			close(fid2)
		end do
		!-------------------------------------------------------------------------
		write(fid1,'(9f10.5)') &
			& v1(1,nf), v2(2,nf), v3(3,nf), &
			& v1(2,nf), v1(3,nf), v2(1,nf), &
			& v2(3,nf), v3(1,nf), v3(2,nf)
	close(fid3)
	close(fid1)
	!----------------------------------------------------------------------------
	write(fmt,*) ia-1
	write(*,'('//trim(fmt)//'(a,2x),"  ->  ",2(a,2x))') &
		(trim(fname(j)), j=1, nf), trim(fout), trim(fxyz)
	!----------------------------------------------------------------------------
	deallocate(fname)
	deallocate(natom)
	deallocate(v1   )
	deallocate(v2   )
	deallocate(v3   )
	!----------------------------------------------------------------------------
	stop
	!----------------------------------------------------------------------------
end
