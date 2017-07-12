program afm_slice
	use kinds, only: DP
	use file , only: name_main
	implicit none
	!----------------------------------------------------------------------------
	character(200)        :: finp, fout
	character(200)        :: dir_x, dir_f
	integer               :: nx, ny, nz, n3(3)
	real(DP)              :: grid(3,3)
	real(DP)              :: dx(3)
	real(DP), allocatable :: coord(:,:,:,:)
	real(DP), allocatable :: force(:,:,:,:)
	integer               :: i, j, ix, iy, iz, ios, fid, fx
	integer               :: num, irecl
	character(200)        :: line, tmp, outformat
	!----------------------------------------------------------------------------
	call getarg(1, finp  )
	call getarg(2, dir_x )
	call getarg(3, dir_f )
	call getarg(4, tmp   )
	read(tmp,*) num
	!----------------------------------------------------------------------------
	if (trim(dir_f)=="x") then
		fx=1
	else if (trim(dir_f)=="y") then
		fx=2
	else if (trim(dir_f)=="z") then
		fx=3
	end if
	outformat="matrix"
	outformat="new"
	outformat="line"
	!----------------------------------------------------------------------------
	fid = 1
	open(fid, file=finp, status="old")
		read(fid,*)
		read(fid,*) nx, ny, nz
		n3(1) = nx
		n3(2) = ny
		n3(3) = nz
		allocate(coord(3, 0:nx-1, 0:ny-1, 0:nz-1))
		allocate(force(3, 0:nx-1, 0:ny-1, 0:nz-1))
		read(fid,*)
		do i=1, 3
			read(fid, *) (grid(i,j), j=1, 3)
			dx(i) = grid(i,i) / n3(i)
		end do
		!-------------------------------------------------------------------------
		read(fid,'(a)', iostat=ios) line
		do while (ios==0)
			read(line,*) ix, iy, iz, coord(:,ix,iy,iz), force(:,ix,iy,iz)
			read(fid,'(a)', iostat=ios) line
		end do
	close(fid)
	!----------------------------------------------------------------------------
	write(fout,'(a,"_",a,"_",a,"_",i0,".dat")') &
		trim(name_main(finp)), trim(dir_x), trim(dir_f), num
	!----------------------------------------------------------------------------
	if (trim(outformat)=="new") then
		open(fid, file=fout)
		if (trim(dir_x)=="x") then
			ix=num
			do iz=0, nz-1
				do iy=0, ny-1
					write(fid, '(2f16.8, es20.10)') &
						coord(1, ix, iy, iz), coord(2, ix, iy, iz), force(fx, ix, iy, iz)
				end do
			end do
		else if (trim(dir_x)=="y") then
			iy=num
			do iz=0, nz-1
				do ix=0, nx-1
					write(fid, '(2f16.8, es20.10)') &
						coord(1, ix, iy, iz), coord(2, ix, iy, iz), force(fx, ix, iy, iz)
				end do
			end do
		else if (trim(dir_x)=="z") then
		iz=num
			do ix=0, nx-1
				do iy=0, ny-1
					write(fid, '(2f16.8, es20.10)') &
						coord(1, ix, iy, iz), coord(2, ix, iy, iz), force(fx, ix, iy, iz)
				end do
			end do
		end if
	else if (trim(outformat)=="matrix") then
		if (trim(dir_x)=="x") then
			irecl = (nx + 1) * 16
			ix=num
			open(fid, file=fout, recl=irecl)
				do iz=0, nz-1
					write(fid,'(f15.6, $)') iz * dx(3)
					do iy=0, ny-1
						write(fid,'(es16.6,$)') force(fx, ix, iy, iz)
					end do
					write(fid,*)
				end do
			close(fid)
		else if (trim(dir_x)=="y") then
			irecl = (nx + 1) * 16
			iy=num
			open(fid, file=fout, recl=irecl)
				do iz=0, nz-1
					write(fid,'(f15.6, $)') iz * dx(3)
					do ix=0, nx-1
						write(fid,'(es16.6,$)') force(fx, ix, iy, iz)
					end do
					write(fid,*)
				end do
			close(fid)
		else if (trim(dir_x)=="z") then
			irecl = (nx + 1) * 16
			iz=num
			open(fid, file=fout, recl=irecl)
				do iy=0, ny-1
					write(fid,'(f15.6, $)') iy * dx(2)
					do ix=0, nx-1
						write(fid,'(es16.6,$)') force(fx, ix, iy, iz)
					end do
					write(fid,*)
				end do
			close(fid)
		end if
	end if
	!----------------------------------------------------------------------------
	write(*,*)
	write(*,'(2x,"Input  file:", 2x, a)') trim(finp)
	write(*,'(2x,"Output file:", 2x, a)') trim(fout)
	write(*,*)
	!----------------------------------------------------------------------------
	deallocate(coord)
	deallocate(force)
	!----------------------------------------------------------------------------
	stop
end
