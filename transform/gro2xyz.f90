program gro2xyz
	use kinds , only: DP
	use file  , only: name_main
	use string, only: StrLowCase
	use string, only: StrUpCaseFirst
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, i, j, k
	integer                     :: natom
	real(DP)      , allocatable :: x(:,:)
	real(DP)      , allocatable :: v(:,:)
	character(  2), allocatable :: symbol(:)
	!----------------------------------------------------------------------------
	character(200)              :: fgro, fxyz
	character(200)              :: line, tmp, c1, c2
	logical       , external    :: is_letter
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	call getarg(1,fgro)
	fxyz=trim(name_main(fgro))//".xyz"
	!----------------------------------------------------------------------------
	open(fid, file=fgro, status="old")
		read(fid, '(a)') line
		read(fid, *) natom
		allocate(x     (3,natom))
		allocate(v     (3,natom))
		allocate(symbol(  natom))
		do i=1, natom
			c2=""
			read(fid,*) tmp, c1, tmp, x(:,i), v(:,i)
			k=0
			do j=1, len(trim(c1))
				if( is_letter(c1(j:j)) ) then
					k=k+1
					c2(k:k) = c1(j:j)
				end if
			end do
			c2 = StrLowCase(c2)
			c2 = StrUpCaseFirst(c2)
			symbol(i) = trim(c2)
		end do
	close(fid)
	x = x * 10
	!----------------------------------------------------------------------------
	call write_xyz_v(fxyz,natom,symbol,x,v)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(v     )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
