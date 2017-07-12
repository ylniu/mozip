program charge2xyz
	implicit none
	integer :: fid, n, i, j, nl
	real(8), allocatable :: charge(:), ch(:)
	character(  2), allocatable :: ss(:)
	real(8), allocatable :: x(:,:)
	character(100) :: fname
	character(  2), allocatable :: symbol(:)
	!----------------------------------------------------------------------------
	fname="charge.dat"
	i=iargc()
	if (i==1) call getarg(1,fname)
	open(fid, file=fname, status="old")
		read(fid,*) n
		allocate(symbol(n))
		allocate(charge(n))
		do i=1, n
			read(fid,*) symbol(i), charge(i)
		end do
	close(fid)
	
	open(fid, file="input.xyz", status="old")
		read(fid,*) nl
		allocate(ss(nl))
		allocate(x(3,nl))
		allocate(ch(nl))
		do i=1, nl
			read(fid,*) ss(i), x(:,i)
			do j=1, n
				if (trim(ss(i))==trim(symbol(j))) then
					ch(i) = charge(j)
					exit
				end if
			end do
			! if (trim(ss(i))=="Na") x(3,i) = x(3,i) - 0.5
		end do
	close(fid)
	!
	open(fid, file="input.xyz")
		write(fid,'(i0)') nl
		do i=1, nl
			write(fid,'(a2, 3f19.9, f15.7)') ss(i), x(:,i), ch(i)
		end do
	close(fid)
stop
end
