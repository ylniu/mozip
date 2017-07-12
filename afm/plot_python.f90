subroutine plot_python(bas_n, bas_nat, bas_x, mf, delete, fix_range)
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
	integer                     :: fid, i, j, nf, nth
	integer                     :: ix, iy, iz, irecl
	integer                     :: nx, ny, nz
	integer                     :: delay, lw, nbnd
	integer                     :: scn_n(3)
	real(DP)      , allocatable :: f(:,:,:)
	real(DP)                    :: radius, z
	real(DP)                    :: sxmin(3)
	real(DP)                    :: sxmax(3)
	character( 64)              :: prefix
	character( 64)              :: color
	character( 64)              :: bcolor
	character(200)              :: fman, fxy, cmd, fcon, intp, fanim
	character(200)              :: fbnd
	character(200), allocatable :: fpy (:)
	character(200), allocatable :: fdat(:)
	character(200), allocatable :: fpng(:)
	logical       , allocatable :: bas_bond(:,:)
	real(DP)                    :: xmin, xmax, figx
	real(DP)                    :: ymin, ymax, figy
	real(DP)                    :: fmin, fmax, df, ratio
	real(DP)                    :: scale_range
	character(200)              :: fixz
	!--------------------------------------------------------------------
	scn_n = mf%n
	allocate(f(0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
	sxmin = mf%xmin
	sxmax = mf%xmax
	f     = mf%f
	!--------------------------------------------------------------------
	nx = scn_n(1)
	ny = scn_n(2)
	nz = scn_n(3)
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
	allocate(fpy (0:nz))
	allocate(fdat(0:nz))
	allocate(fpng(0:nz))
	allocate(bas_bond(bas_n, bas_n))
	!--------------------------------------------------------------------
	call get_bond(bas_n, bas_x, bas_nat, bas_bond)
	!--------------------------------------------------------------------
	do iz=0, nz
		write(fman, '("plan_",i4.4)') iz
		fpy (iz)=trim(fman)//".py"
		fdat(iz)=trim(fman)//".dat"
		fpng(iz)=trim(fman)//".png"
	end do
	!--------------------------------------------------------------------
	fxy         = "xy.dat"
	fcon        = "convert.sh"
	fbnd        = "bond.dat"
	!intp       = "bicubic"
	intp        = "nearest"
	fanim       = "scan.py.gif"
	xmin        =  0.0_DP
	ymin        =  0.0_DP
	xmax        = 30.0_DP
	ymax        = 30.0_DP
	nf          = 10
	figx        = 16.0_DP
	figy        = 12.0_DP
	delay       = 10
	radius      = 50.0_DP
	color       = "blue"
	bcolor      = "y-"
	lw          = 2
	scale_range = 0.1
	!--------------------------------------------------------------------
	if(fix_range==1) then
		fixz=", vmin=zmin, vmax=zmax"
	else
		fixz=""
	end if
	!--------------------------------------------------------------------
	write(*,'(2x, "Generate animation gif file",x,a,x,"using",x,a)') & 
		trim(fanim), "python"
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
	fmin = minval(f)
	fmax = maxval(f)
	df   = (fmax - fmin) / nf
	!--------------------------------------------------------------------
	open(newunit=fid, file=fxy)
		do i=1, bas_n
			write(fid,'(2f15.7, f15.7, 2x, a10)') &
				bas_x(1:2,i), radius, trim(color)
		end do
	close(fid)
	!--------------------------------------------------------------------
	nbnd=0
	open(newunit=fid, file=fbnd)
		do i=1, bas_n-1
			do j=i, bas_n
				if (bas_bond(i,j)) then
					nbnd = nbnd + 1
					write(fid, '(4f15.7, 2x, i10, 2x,  a)') &
						bas_x(1,i), bas_x(1,j), &
						bas_x(2,i), bas_x(2,j), &
						lw, trim(bcolor)
				end if
			end do
		end do
	close(fid)
	!--------------------------------------------------------------------
	!
	irecl  = 20 * (nx+1)
	prefix = "Writing   data"
	call progress%set(n=nz+1, L=25, prefix=prefix)
	!--------------------------------------------------------------------
	!$omp parallel do private(ix, iy, fid)
	!--------------------------------------------------------------------
	do iz=nz, 0, -1
		fid = 10 + iz
		open(fid, file=fdat(iz), recl=irecl)
			do iy=0, ny
				do ix=0, nx
					write(fid, '(es18.10,$)') f(ix,iy,iz)
				end do
				write(fid,*)
			end do
		close(fid)
		call progress%put(1)
	end do
	!--------------------------------------------------------------------
	!$omp end parallel do
	!--------------------------------------------------------------------
	!
	prefix = "Writing python"
	call progress%set(n=nz+1, L=25, prefix=prefix)
	!--------------------------------------------------------------------
	!$omp parallel do private(ix, iy, fid,z)
	!--------------------------------------------------------------------
	do iz=nz, 0, -1
		fid = 10 + iz
		z=mf%xmin(3)+iz * mf%dx(3)
		!-----------------------------------------------------------------
		open(fid, file=fpy(iz))
			write(fid, '("import numpy as np")')
			write(fid, '("import os")')
			write(fid, '("import warnings")')
			write(fid, '("import numpy as np")')
			write(fid, '("import matplotlib as mpl")')
			write(fid, '("if os.environ.get(""DISPLAY"","""") == """":")')
			write(fid, '(2x, "#print(""no display found. Using non-interactive Agg backend"")")')
			write(fid, '(2x, "mpl.use(""Agg"")")')
			write(fid, '("import matplotlib.pyplot as plt")')
			write(fid, '("warnings.simplefilter(""ignore"")")')
			write(fid, *)
			write(fid, '("xmin   = ", es18.10)') xmin
			write(fid, '("xmax   = ", es18.10)') xmax
			write(fid, '("ymin   = ", es18.10)') ymin
			write(fid, '("ymax   = ", es18.10)') ymax
			write(fid, '("zmin   = ", es18.10)') fmin * scale_range
			write(fid, '("zmax   = ", es18.10)') fmax * scale_range
			write(fid, '("figx   = ",  f18.10)') figx
			write(fid, '("figy   = ",  f18.10)') figy
			write(fid, '("nx     = ", i18    )') nx
			write(fid, '("ny     = ", i18    )') ny
			write(fid, '("na     = ", i18    )') bas_n
			write(fid, '("nbnd   = ", i18    )') nbnd
			write(fid, '("z      = ",  f18.10)') z
			write(fid, '("extent = [xmin,xmax,ymin,ymax]")')
			write(fid, *)
			write(fid, '("with open(""",a,""", ""r"") as fig:")') trim(fbnd)
			write(fid, '(2x, "xline  = np.zeros((nbnd,2))")')
			write(fid, '(2x, "yline  = np.zeros((nbnd,2))")')
			write(fid, '(2x, "lw     = np.zeros( nbnd   )")')
			write(fid, '(2x, "bcolor = []")')
			write(fid, '(2x, "i = 0")')
			write(fid, '(2x, "for line in fig:")')
			write(fid, '(2x, 2x, "data     = line.split()")')
			write(fid, '(2x, 2x, "xline[i] = [data[0], data[1]]")')
			write(fid, '(2x, 2x, "yline[i] = [data[2], data[3]]")')
			write(fid, '(2x, 2x, "lw   [i] =           data[4] ")')
			write(fid, '(2x, 2x, "bcolor.append(       data[5])")')
			write(fid, '(2x, 2x, "i=i+1")')
			write(fid, *)
			write(fid, '("with open(""",a,""", ""r"") as fig:")') trim(fxy)
			write(fid, '(2x, "x = np.zeros(na)")')
			write(fid, '(2x, "y = np.zeros(na)")')
			write(fid, '(2x, "s = np.zeros(na)")')
			write(fid, '(2x, "c = []")')
			write(fid, '(2x, "i = 0")')
			write(fid, '(2x, "for line in fig:")')
			write(fid, '(2x, 2x, "data = line.split()")')
			write(fid, '(2x, 2x, "x[i]=data[0]"     )')
			write(fid, '(2x, 2x, "y[i]=data[1]"     )')
			write(fid, '(2x, 2x, "s[i]=data[2]"     )')
			write(fid, '(2x, 2x, "c.append(data[3])")')
			write(fid, '(2x, 2x, "i=i+1")')
			write(fid, *)
			write(fid, '("#------------------------------------------------")')
			write(fid, '("#Plot AFM image")')
			write(fid, '("#")')
			write(fid, '("with open(""",a,""", ""r"") as fig:")') trim(fdat(iz))
			write(fid, '(2x, "fz = np.zeros((ny+1,nx+1))")')
			write(fid, '(2x, "i  = 0")')
			write(fid, '(2x, "for line in fig:")')
			write(fid, '(2x, 2x, "data = line.split()")')
			write(fid, '(2x, 2x, "fz[i]=data")')
			write(fid, '(2x, 2x, "i=i+1")')
			write(fid, *)
			write(fid, '("#------------------------------------------------")')
			write(fid, '("#Plot molecular bond")')
			write(fid, '("#")')
			write(fid, '("fig     = plt.figure(figsize=(figx, figy))")')
			write(fid, '("i=0")')
			write(fid, '("while i < nbnd:")')
			write(fid, '(2x, "plt.plot(xline[i], yline[i], bcolor[i], lw=lw[i], alpha=0.3, zorder=2)")')
			write(fid, '(2x, "i=i+1")')
			write(fid, *)
			write(fid, '("#------------------------------------------------")')
			write(fid, '("#Plot atoms")')
			write(fid, '("#")')
			write(fid, '("ax      = fig.add_subplot(111)")')
			write(fid, '("props   = dict(alpha=0.5, edgecolors=""face"", zorder=3)")')
			write(fid, '("handles = []")')
			write(fid, '("handles.append(ax.scatter(x, y, c=c, s=s, **props))")')
			write(fid, *)
			write(fid, '("#------------------------------------------------")')
			write(fid, '("#Plot AFM")')
			write(fid, '("#http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.imshow")')
			write(fid, '("#")')
			write(fid, '("plt.grid()")')
			write(fid, '("im=plt.imshow(fz,origin=""lower"",interpolation=""", a, &
				& """, cmap=plt.cm.gray_r, extent=extent",a,")")') trim(intp), trim(fixz)
			write(fid, '("cbar_ax = fig.add_axes([0.91, 0.11, 0.04, 0.78])")')
			write(fid, '("plt.xlabel(r"" Tip_x $\AA$"")")')
			write(fid, '("plt.ylabel(r"" Tip_y $\AA$"")")')
			write(fid, '("plt.title( r""df Tip_z = %2.2f $\AA$"" %z )")')
			write(fid, '("fig.colorbar(im, cax=cbar_ax)")')
			write(fid, '("plt.savefig(""",a,""", bbox_inches=""tight"")")') trim(fpng(iz))
			write(fid, '("#plt.show()")')
		close(fid)
		cmd="python "//trim(fpy(iz))
		call system(cmd)
		!-----------------------------------------------------------------
		call progress%put(1)
	end do
	!--------------------------------------------------------------------
	!$omp end parallel do
	!--------------------------------------------------------------------
	open(newunit=fid, file=fcon)
		write(fid,'("#!/bin/sh")')
		write(fid,'("convert -delay ,",i0,", -loop 0 \")') delay
		do iz=nz, 0, -1
			write(fid, '(a," \")') trim(fpng(iz))
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
		write(*,'("Deleting files ...")')
		do iz=0, nz
			open(newunit=fid, file=fpy(iz))
			close(fid, status='delete')
			open(newunit=fid, file=fdat(iz))
			close(fid, status='delete')
			open(newunit=fid, file=fpng(iz))
			close(fid, status='delete')
		end do
		open(newunit=fid, file=fxy)
		close(fid, status='delete')
		open(newunit=fid, file=fbnd)
		close(fid, status='delete')
		open(newunit=fid, file=fcon)
		close(fid)
	end if
	!--------------------------------------------------------------------
	write(*, '(2x, "Install ImageMagick")')
	write(*, '(2x, "http://www.imagemagick.org/script/install-source.php#windows")')
	write(*, '(2x, "view with: display",x,a)') trim(fanim)
	write(*, '(2x, "Terminated Normally!")')
	!--------------------------------------------------------------------
	deallocate(f   )
	deallocate(fpy )
	deallocate(fdat)
	deallocate(fpng)
	!--------------------------------------------------------------------
	return
	!--------------------------------------------------------------------
end subroutine
