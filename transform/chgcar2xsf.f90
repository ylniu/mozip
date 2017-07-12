program chgcar2xsf
	use kinds, only: DP
	use math,  only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer                      :: fid, ntype, natom
	integer                      :: i, j, k, nx, ny, nz, ix, iy, iz
	real(DP)                     :: alat, a(3,3), b(3,3)
	integer        , allocatable :: natom_per_type(:)
	real(DP)       , allocatable :: grid(:,:,:), x(:,:)
	character(2)   , allocatable :: symbol_per_type(:), symbol(:)
	character(200)               :: line
	integer        , external    :: number_of_words
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	open(fid, file="CHGCAR", status="old")
		read(fid,*) line
		read(fid,*) alat
		read(fid,*) a(:,1)
		read(fid,*) a(:,2)
		read(fid,*) a(:,3)
		a=a*alat
		b=inverse3(a)
		read(fid,'(a)') line
		backspace(fid)
		ntype=number_of_words(line)
		allocate(symbol_per_type(ntype))
		allocate(natom_per_type(ntype))
		read(fid,*) symbol_per_type
		read(fid,*) natom_per_type
		natom=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				natom=natom+1
			end do
		end do
		allocate(symbol(natom))
		allocate(x(3,natom))
		k=0
		do i=1, ntype
			do j=1, natom_per_type(i)
				k=k+1
				symbol(k)=symbol_per_type(i)
			end do
		end do
		read(fid,*)
		do i=1, natom
			read(fid,*) x(:,i)
		end do
		call rotn(natom,a,x)
		read(fid,*) nx, ny, nz
		allocate(grid(nx, ny, nz))
		read(fid,*) (((grid(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
	close(fid)
	open(fid, file="CHGCAR.xsf")
		write(fid,*) "SLAB"
		write(fid,*) "PRIMVEC"
		write(fid,'(3(f15.10,x))') a(:,1)
		write(fid,'(3(f15.10,x))') a(:,2)
		write(fid,'(3(f15.10,x))') a(:,3)
		write(fid,*) "PRIMCOORD"
		write(fid,*) natom,1
		do i=1, natom
			write(fid,'(a3,6f12.6)') symbol(i), x(:,i)
		end do
		write(fid,'("BEGIN_BLOCK_DATAGRID_3D")')
		write(fid,'("3D_datagrid")')
		write(fid,'("BEGIN_DATAGRID_3D_this_is_3Dgrid#1")')
		write(fid,'(3i6)') nx, ny, nz
		write(fid,'(3f15.7)') 0.0, 0.0, 0.0
		write(fid,'(3f15.7)') a(:,1)
		write(fid,'(3f15.7)') a(:,2)
		write(fid,'(3f15.7)') a(:,3)
		write(fid, '(5e19.11)') (((grid(ix, iy, iz), ix=1, nx), iy=1, ny), iz=1, nz)
		write(fid,'("END_DATAGRID_3D")')
		write(fid,'("END_BLOCK_DATAGRID_3D")')
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(symbol_per_type)
	deallocate(natom_per_type)
	deallocate(symbol)
	deallocate(x)
	deallocate(grid)
	!----------------------------------------------------------------------------
	stop
end
