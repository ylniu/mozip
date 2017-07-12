program plot_force
	use kinds, only: DP
	use file , only: name_main
	use param, only: au2a
	use force_grid, only: OBJ_FGRID
	use omp_lib
	implicit none
	!--------------------------------------------------------------------
	type(OBJ_FGRID)           :: scn_fz1
	type(OBJ_FGRID)           :: scn_fz1_av
	type(OBJ_FGRID)           :: scn_dfz1
	type(OBJ_FGRID)           :: scn_dfz1_av
	!--------------------------------------------------------------------
	integer                   :: fid, i, j
	integer                   :: nf, nv
	integer                   :: delay, plot_type, delete, fix_range
	real(DP)                  :: f0, zv
	character(200)            :: fbin, fxyz, flog, tmp
	!--------------------------------------------------------------------
	integer                   :: bas_n
	integer     , allocatable :: bas_nat (  :)
	real(DP)    , allocatable :: bas_x   (:,:)
	!--------------------------------------------------------------------
	integer                   :: tip_n
	integer     , allocatable :: tip_nat (  :)
	real(DP)    , allocatable :: tip_x   (:,:)
	character(2), allocatable :: tip_sym (  :)
	!--------------------------------------------------------------------
	integer                   :: scn_n(3)
	integer                   :: scn_i(3)
	integer                   :: ixmin(3)
	integer                   :: ixmax(3)
	real(DP)                  :: sxmin(3)
	real(DP)                  :: sxmax(3)
	real(DP)                  :: pxmin(3)
	real(DP)                  :: pxmax(3)
	character(1)              :: key, dirc(3)
	logical                   :: isdev
	!--------------------------------------------------------------------
	real(DP)    , allocatable :: scn_xp  (:,:,:,:,:)
	real(DP)    , allocatable :: scn_fz  (    :,:,:)
	!real(DP)   , allocatable :: scn_f   (:,:,:,:,:)
	!real(DP)   , allocatable :: scn_fb  (:,:,:,:,:)
	!real(DP)   , allocatable :: scn_ft  (:,:,:,:,:)
	!-------------------------------------------------------------------
	isdev = .true.
	!--------------------------------------------------------------------
	! Get command arguments
	!
	i=1
	call get_command_argument(i, fbin)
	i=i+1
	call get_command_argument(i, tmp)
	read(tmp,*) plot_type
	i=i+1
	call get_command_argument(i, tmp)
	read(tmp,*) delete
	i=i+1
	call get_command_argument(i, tmp)
	read(tmp,*) fix_range
	i=i+1
	call get_command_argument(i, tmp)
	read(tmp,*) key
	!--------------------------------------------------------------------
	i=i+1
	call get_command_argument(i, tmp )
	if (key=="f") then
		read(tmp,*) pxmin(1)
	else if (key=="i") then
		read(tmp,*) ixmin(1)
	end if
	i=i+1
	call get_command_argument(i, tmp )
	read(tmp,*) pxmin(2)
	if (key=="f") then
		read(tmp,*) pxmin(2)
	else if (key=="i") then
		read(tmp,*) ixmin(2)
	end if
	i=i+1
	call get_command_argument(i, tmp )
	read(tmp,*) pxmin(3)
	if (key=="f") then
		read(tmp,*) pxmin(3)
	else if (key=="i") then
		read(tmp,*) ixmin(3)
	end if
	i=i+1
	call get_command_argument(i, tmp )
	read(tmp,*) pxmax(1)
	if (key=="f") then
		read(tmp,*) pxmax(1)
	else if (key=="i") then
		read(tmp,*) ixmax(1)
	end if
	i=i+1
	call get_command_argument(i, tmp )
	read(tmp,*) pxmax(2)
	if (key=="f") then
		read(tmp,*) pxmax(2)
	else if (key=="i") then
		read(tmp,*) ixmax(2)
	end if
	i=i+1
	call get_command_argument(i, tmp )
	read(tmp,*) pxmax(3)
	if (key=="f") then
		read(tmp,*) pxmax(3)
	else if (key=="i") then
		read(tmp,*) ixmax(3)
	end if
	!--------------------------------------------------------------------
	! Initialization
	!
	delay     = 10
	nf        = 10
	f0        = 0.0_DP
	zv        = 1.0_DP ! Angstrom
	!--------------------------------------------------------------------
	fxyz      = trim(name_main(fbin))//".xyz"
	flog      = "plot.log"
	!--------------------------------------------------------------------
	write(*,'(2x, "Reading ", a, " ...")') trim(fbin)
	open(newunit=fid, file=fbin, form='unformatted', access='stream')
		read(fid) bas_n
		read(fid) tip_n
		read(fid) scn_n
		read(fid) sxmin
		read(fid) sxmax
		!-----------------------------------------------------------------
		allocate(bas_nat  (       bas_n))
		allocate(bas_x    (    3, bas_n))
		!-----------------------------------------------------------------
		allocate(tip_nat  (       tip_n))
		allocate(tip_sym  (       tip_n))
		allocate(tip_x    (    3, tip_n))
		!-----------------------------------------------------------------
		allocate(scn_xp  (3, tip_n, 0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
		allocate(scn_fz  (          0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
		!-----------------------------------------------------------------
		!allocate(scn_f   (3, tip_n, 0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
		!allocate(scn_fb  (3, tip_n, 0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
		!allocate(scn_ft  (3, tip_n, 0:scn_n(1), 0:scn_n(2), 0:scn_n(3)))
		!-----------------------------------------------------------------
		read(fid) bas_nat
		read(fid) bas_x
		read(fid) tip_nat
		read(fid) tip_x
		read(fid) scn_fz
		read(fid) scn_xp
		!read(fid) scn_fb
		!read(fid) scn_ft
	close(fid)
	!--------------------------------------------------------------------
	sxmin  = sxmin  * au2a 
	sxmax  = sxmax  * au2a 
	bas_x  = bas_x  * au2a
	tip_x  = tip_x  * au2a
	scn_xp = scn_xp * au2a
	!--------------------------------------------------------------------
	call scn_fz1%alloc(scn_n)
	call scn_fz1%setdx(sxmin, sxmax)
	scn_fz1%f = scn_fz
	!--------------------------------------------------------------------
	nv = nint(zv / (scn_fz1%dx(3)))
	nv = min(nv, scn_fz1%n(3))
	!--------------------------------------------------------------------
	if (key=="i") then
		do i=1, 3
			ixmin(i) = max(ixmin(i),           0 )
			ixmin(i) = min(ixmin(i), scn_fz1%n(i))
			ixmax(i) = max(ixmax(i),           0 )
			ixmax(i) = min(ixmax(i), scn_fz1%n(i))
		end do
		scn_i = ixmax - ixmin + 1
		!-----------------------------------------------------------------
		pxmin = scn_fz1%xmin + ixmin * scn_fz1%dx
		pxmax = scn_fz1%xmin + ixmax * scn_fz1%dx
	end if
	!--------------------------------------------------------------------
	do i=1, 3
		pxmin(i) = max(pxmin(i), sxmin(i))
		pxmin(i) = min(pxmin(i), sxmax(i))
		pxmax(i) = max(pxmax(i), sxmin(i))
		pxmax(i) = min(pxmax(i), sxmax(i))
	end do
	!--------------------------------------------------------------------
	if (isdev) then
		call scn_fz1_av  % reduce  (scn_fz1 , nv)
		call scn_dfz1    % deriv   (scn_fz1     )
		call scn_dfz1_av % reduce  (scn_dfz1, nv)
	end if
	!--------------------------------------------------------------------
	open(newunit=fid, file=flog)
		write(fid, '(2x, "Scan points    :", 3i10  )') scn_n
		write(fid, '(2x, "Output scan    :", 2i10  )') ixmin(1), ixmax(1)
		write(fid, '(2x, "Output scan    :", 2i10  )') ixmin(2), ixmax(2)
		write(fid, '(2x, "Output scan    :", 2i10  )') ixmax(3), ixmin(3)
		write(fid, '(2x, "Number of atom :",  i10  )') tip_n + bas_n
		write(fid, '(2x, "Number of frame:",  i10  )') scn_i(1) * scn_i(2) * scn_i(3)
		!-----------------------------------------------------------------
		dirc(1) = "x"
		dirc(2) = "y"
		dirc(3) = "z"
		!-----------------------------------------------------------------
		do i=1, 3
			write(fid, '(2x, "Corresponding: ",a)') dirc(i)
			do j=0, scn_fz1%n(i)
				write(fid, '(2x, a, i10, f15.7)') dirc(i), j, scn_fz1%xmin(i) + j * scn_fz1%dx(i)
			end do
		end do
		!-----------------------------------------------------------------
	close(fid)
	!--------------------------------------------------------------------
	Write(*,'(2x,"plot_type = ", i0)') plot_type
	!
	if (plot_type==1) then
		call plot_xyz    (bas_n, bas_nat, bas_x, tip_n, tip_nat, scn_fz1, scn_n, scn_xp, pxmin, pxmax)
	else if (plot_type==2) then
		!call plot_python (bas_n, bas_nat, bas_x, scn_fz1, delete, fix_range)
		!call plot_python (bas_n, bas_nat, bas_x, scn_dfz1_av, delete, fix_range)
		call plot_python (bas_n, bas_nat, bas_x, scn_fz1, delete, fix_range)
	else if (plot_type==3) then
		!call plot_gnuplot(bas_n, bas_nat, bas_x, scn_fz1, delete, fix_range)
		call plot_gnuplot(bas_n, bas_nat, bas_x, scn_dfz1_av, delete, fix_range)
	end if
	!--------------------------------------------------------------------
	call scn_fz1%dealloc()
	!--------------------------------------------------------------------
	deallocate(bas_nat)
	deallocate(bas_x  )
	!-----------------------------------------------------------------
	deallocate(tip_nat)
	deallocate(tip_sym)
	deallocate(tip_x  )
	!-----------------------------------------------------------------
	deallocate(scn_xp)
	deallocate(scn_fz)
	!deallocate(scn_f)
	!deallocate(scn_fb)
	!deallocate(scn_ft)
	!--------------------------------------------------------------------
	call exit(0)
end
