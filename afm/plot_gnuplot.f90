subroutine plot_gnuplot(bas_n, bas_nat, bas_x, mf, delete, fix_range)
	use kinds     , only: DP
	use omp_lib
	use cmd_progress
	use force_grid, only: OBJ_FGRID
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	type(OBJ_FGRID)             :: mf
	integer      , intent( in)  :: bas_n
	integer      , intent( in)  :: delete
	integer      , intent( in)  :: fix_range
	integer      , intent( in)  :: bas_nat(bas_n)
	real(DP)     , intent( in)  :: bas_x(3,bas_n)
	!--------------------------------------------------------------------
	! Local  variables
	!
	type(cls_cmd_progress)      :: progress
	integer                     :: fid, m, i, j, k, nf, nth, delay
	integer                     :: ix, iy, iz
	integer                     :: nx, ny, nz
	integer                     :: scn_n(3)
	real(DP)                    :: sxmin(3)
	real(DP)                    :: sxmax(3)
	real(DP)      , allocatable :: f(:,:,:)
	character( 64)              :: prefix
	character(200)              :: fman, fxy, cmd, fcon, fanim
	character(200)              :: linestyle
	character(200)              :: terminal
	character(200)              :: s1, s2, s3
	character(200), allocatable :: fdat(:)
	character(200), allocatable :: fplt(:)
	character(200), allocatable :: fgif(:)
	logical       , allocatable :: bas_bond(:,:)
	real(DP)                    :: xmin, xmax, figx
	real(DP)                    :: ymin, ymax, figy
	real(DP)                    :: dx(3)
	real(DP)                    :: fmin, fmax, df, ratio
	real(DP)      , allocatable :: x(:,:,:,:)
	!--------------------------------------------------------------------
	scn_n = mf%n
	allocate(f(0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
	sxmin = mf%xmin
	sxmax = mf%xmax
	f     = mf%f
	!--------------------------------------------------------------------
	nx   = scn_n(1)
	ny   = scn_n(2)
	nz   = scn_n(3)
	allocate(x(3,0:nx, 0:ny, 0:nz))
	dx = (sxmax - sxmin) / scn_n
	do ix=0, nx
		do iy=0, ny
			do iz=0, nz
				x(1, ix, iy, iz) = sxmin(1) + ix * dx(1)
				x(2, ix, iy, iz) = sxmin(2) + iy * dx(2)
				x(3, ix, iy, iz) = sxmin(3) + iz * dx(3)
			end do
		end do
	end do
	!--------------------------------------------------------------------
	! Get parallel parameters
	!
	!$omp parallel
	nth=omp_get_num_threads()
	!$omp end parallel
	!--------------------------------------------------------------------
	if (nth==1) then
		write(*,'(2x, "Processing in ", i0, " thread" )') nth
	else
		write(*,'(2x, "Processing in ", i0, " threads")') nth
	end if
	!--------------------------------------------------------------------
	allocate(fdat(0:nz))
	allocate(fplt(0:nz))
	allocate(fgif(0:nz))
	allocate(bas_bond(bas_n, bas_n))
	!--------------------------------------------------------------------
	call get_bond(bas_n, bas_x, bas_nat, bas_bond)
	!--------------------------------------------------------------------
	do iz=0, nz
		write(fman, '("plan_",i4.4)') iz
		fdat(iz)=trim(fman)//".dat"
		fplt(iz)=trim(fman)//".plt"
		fgif(iz)=trim(fman)//".gif"
	end do
	!--------------------------------------------------------------------
	fxy       = "xy.dat"
	fcon      = "convert.sh"
	fanim     = "scan.gnu.gif"
	prefix    = "Writing data"
	xmin      =  0.0_DP
	ymin      =  0.0_DP
	xmax      = 30.0_DP
	ymax      = 30.0_DP
	nf        = 10
	figx      = 800.0_DP
	figy      = 600.0_DP
	delay     = 50
	!--------------------------------------------------------------------
	xmin      = sxmin(1) 
	ymin      = sxmin(2) 
	xmax      = sxmax(1) 
	ymax      = sxmax(2) 
	!--------------------------------------------------------------------
	ratio     = ( ymax - ymin ) / ( xmax - xmin )
	if (ratio >= 3.0_DP / 4.0_DP ) then
		figx   = figy / ratio
	else
		figy   = figx * ratio
	end if
	!--------------------------------------------------------------------
	write(*,'(2x, "Generate animation gif file",x,a,x,"using",x,a)') &
		trim(fanim), "gnuplot"
	!--------------------------------------------------------------------
	linestyle  = "with lines"
	!terminal   = "set terminal gif font ""arial"" 14 size 800,600"
	terminal   = "set terminal gif size 800,600"
	!write(terminal,'(a,x,"animate delay ",i0)') trim(terminal), delay
	!--------------------------------------------------------------------
	!f    = log(f)
	fmin = minval(f)
	fmax = maxval(f)
	df   = (fmax - fmin) / nf
	!--------------------------------------------------------------------
	call progress%set(n=nz+1, L=25, prefix=prefix)
	!--------------------------------------------------------------------
	open(newunit=fid, file=fxy)
	do i=1, bas_n
		write(fid,*) bas_x(1:2,i)
	end do
	close(fid)
	!--------------------------------------------------------------------
	!$omp parallel do private(m, ix, iy, fid)
	do iz=0, nz
		fid = 10 + iz
		!-----------------------------------------------------------------
		open(fid, file=fdat(iz))
			write(fid, *)
			write(fid, '("#Surface 0 of 1 surfaces")')
			do iy=0, ny
				write(fid, *)
				write(fid, '("#IsoCurve ",i0,", ",i0," points")') iy, nx
				write(fid, '("#x y z type")')
				do ix=0, nx
					write(fid, '(3f20.10, 2x, "i")') x(1:2, ix, iy, iz), f(ix,iy,iz)
				end do
			end do
		close(fid)
		!-----------------------------------------------------------------
		call progress%put(1)
	end do
	!$omp end parallel do
	!--------------------------------------------------------------------
	!
	prefix    = "Call gnuplot"
	call progress%set(n=nz+1, L=25, prefix=prefix)
	!--------------------------------------------------------------------
	!$omp parallel do private(ix, iy, fid, i, j, k, s1, s2, s3, cmd)
	!--------------------------------------------------------------------
	do iz=nz, 0, -1
		fid = 10 + iz
		!-----------------------------------------------------------------
		open(newunit=fid, file=fplt(iz))
			write(fid, '("set terminal gif size ",i0,",",i0,"")') nint(figx), nint(figy)
			write(fid, '("set output """,a,"""")') trim(fgif(iz))
			!write(fid, '("set cntrparam level incremental ", f15.7, 2(",",f15.7))')&
			!	fmin, df, fmax
			if (fix_range==1) then
				write(fid, '("set cbrange [",f15.7," : ",f15.7,"]")') fmin, fmax
			end if
			write(fid, '("set  xrange [",f15.7," : ",f15.7,"]")') xmin, xmax
			write(fid, '("set  yrange [",f15.7," : ",f15.7,"]")') ymin, ymax
			!
			!--------------------------------------------------------------
			!
			write(fid, '("set style line 12 lc rgb ""#808080"" lt 0 lw 1")')
			write(fid, '("set grid back ls 12")')
			write(fid, '("set xtics 1")')
			write(fid, '("set ytics 1")')
			!--------------------------------------------------------------
			write(s3, '("plot """,a,""""    )') trim(fdat(iz))
			write(s3, '(a, x, "with image"  )') trim(s3)
			write(s3, '(a,x,"title",x,"""",a,"""")') trim(s3),trim(fdat(iz))
			write(fid, '(a)') trim(s3)
			!--------------------------------------------------------------
			k=0
			do i=1, bas_n
				do j=1, bas_n
					if(bas_bond(i,j)) then
						k=k+1
						write(s2,  '("set arrow ",i0," from ",f15.7,",",f15.7)') k, bas_x(1:2,i)
						write(s2,  '(a," to ", f15.7, ",", f15.7)') trim(s2), bas_x(1:2,j)
						write(s2,  '(a," lt 2 lc rgb ""red"" lw 3")') trim(s2)
						write(s2,  '(a," nohead")') trim(s2)
						write(fid, '(a)') trim(s2)
					end if
				end do
			end do
			!--------------------------------------------------------------
			write(s1, '("replot """,a,""""    )') trim(fxy)
			!write(s1, '(a,x,"with circles"    )') trim(s1)
			write(s1, '(a,x,"with points"    )') trim(s1)
			write(s1, '(a,x,"pointsize 2"    )') trim(s1)
			write(s1, '(a,x,"lc rgb ""blue""")') trim(s1)
			write(s1, '(a,x,"pointtype 7"    )') trim(s1)
			write(fid, '(a)') trim(s1)
			!--------------------------------------------------------------
		close(fid)
		!-----------------------------------------------------------------
		cmd = "gnuplot "//trim(fplt(iz))//" >& /dev/null"
		call system(cmd)
		!-----------------------------------------------------------------
		call progress%put(1)
	end do
	!--------------------------------------------------------------------
	!$omp end parallel do
	!--------------------------------------------------------------------
	open(newunit=fid, file=fcon)
		write(fid,'("#!/bin/sh")')
		write(fid,'("convert -delay ",i0," -loop 0 \")') delay
		do iz=nz, 0, -1
			write(fid, '(a," \")') trim(fgif(iz))
		end do
		write(fid,'(a)') trim(fanim)
	close(fid)
	!--------------------------------------------------------------------
	cmd="/bin/sh ./"//trim(fcon)
	call system(cmd)
	!--------------------------------------------------------------------
	! Delete dat files
	!
	if (delete==1) then
		do iz=0, nz
			open(newunit=fid, file=fdat(iz))
			close(fid, status='delete')
			open(newunit=fid, file=fgif(iz))
			close(fid, status='delete')
			open(newunit=fid, file=fplt(iz))
			close(fid, status='delete')
		end do
		open(newunit=fid, file=fxy)
		close(fid, status='delete')
		open(newunit=fid, file=fcon)
		close(fid, status='delete')
	end if
	!--------------------------------------------------------------------
	write(*, '(2x, "Install ImageMagick")')
	write(*, '(2x, "http://www.imagemagick.org/script/install-source.php#windows")')
	write(*, '(2x, "view with: display",x,a)') trim(fanim)
	write(*, '(2x, "Terminated Normally!")')
	!--------------------------------------------------------------------
	deallocate(f   )
	deallocate(x   )
	deallocate(fdat)
	deallocate(fplt)
	deallocate(fgif)
	return
	!--------------------------------------------------------------------
end subroutine
