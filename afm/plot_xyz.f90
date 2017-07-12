subroutine plot_xyz(bas_n, bas_nat, bas_x, tip_n, tip_nat, mf, scn_n, scn_xp, pxmin, pxmax)
	use kinds     , only: DP
	use force_grid, only: OBJ_FGRID
	implicit none
	!--------------------------------------------------------------------
	type(OBJ_FGRID)             :: mf
	integer       , intent( in) :: bas_n
	integer       , intent( in) :: tip_n
	integer       , intent( in) :: bas_nat( bas_n)
	integer       , intent( in) :: tip_nat( tip_n)
	integer       , intent( in) :: scn_n(3)
	real(DP)      , intent( in) :: bas_x(3, bas_n)
	real(DP)      , intent( in) :: scn_xp(3,tip_n,0:scn_n(1),0:scn_n(2),0:scn_n(3))
	real(DP)      , intent( in) :: pxmin(3)
	real(DP)      , intent( in) :: pxmax(3)
	!--------------------------------------------------------------------
	integer                     :: fid, i, j, k, ia
	integer                     :: scn_i(3)
	integer                     :: ixmin(3)
	integer                     :: ixmax(3)
	real(DP)                    :: dx(3)
	real(DP)                    :: sxmin(3)
	real(DP)                    :: sxmax(3)
	character(  2), allocatable :: bas_sym(:)
	character(  2), allocatable :: tip_sym(:)
	character(200)              :: flog, fxyz
	!--------------------------------------------------------------------
	sxmin = mf%xmin
	sxmax = mf%xmax
	!--------------------------------------------------------------------
	dx = (sxmax - sxmin) / scn_n
	do i=1, 3
		ixmin(i) = nint( (pxmin(i) - sxmin(i)) / dx(i) )
		ixmax(i) = nint( (pxmax(i) - sxmin(i)) / dx(i) )
	end do
	scn_i = ixmax - ixmin + 1
	!--------------------------------------------------------------------
	allocate(bas_sym(bas_n))
	allocate(tip_sym(tip_n))
	!--------------------------------------------------------------------
	call nat_to_symbol(bas_n, bas_nat, bas_sym)
	call nat_to_symbol(tip_n, tip_nat, tip_sym)
	!--------------------------------------------------------------------
	flog="plot.log"
	fxyz="scan.xyz"
	!--------------------------------------------------------------------
	write(*,'(2x, "Generate animation xyz file",x,a)') trim(fxyz)
	write(*,'(2x, "Writing ", a, " ...")') trim(fxyz)
	open(newunit=fid, file=fxyz)
		do i=ixmin(1), ixmax(1)
			do j=ixmin(2), ixmax(2)
				do k=ixmax(3), ixmin(3), -1
					write(fid, '(i10)') tip_n + bas_n
					write(fid, '("scan", 2x, i0, "/", i0, 2(",", i0, "/", i0))') &
					i, scn_n(1), j, scn_n(2), scn_n(3)-k, scn_n(3)
					do ia=1, tip_n
						write(fid,'(a4, 3f15.7)') tip_sym(ia), scn_xp(:,ia,i,j,k)
					end do
					do ia=1, bas_n
						write(fid,'(a4, 3f15.7)') bas_sym(ia), bas_x(:,ia)
					end do
				end do
			end do
		end do
	close(fid)
	!--------------------------------------------------------------------
	if (ixmin(2)==ixmax(2) .and. ixmin(3) == ixmax(3)) then
		j=ixmin(2)
		k=ixmin(3)
		open(newunit=fid, file="x.dat")
			do i=0, mf%n(1)
				write(fid, '(f15.7, es20.10, f15.7)') mf%xmin(1) + i * mf%dx(1), mf%f(i,j,k), scn_xp(3,6,i,j,k)
			end do
		close(fid)
	end if
	!--------------------------------------------------------------------
	!open(newunit=fid, file=flog)
	!	write(fid, '(2x, "Scan points    :", 3i10  )') scn_n
	!	write(fid, '(2x, "Output scan    :", 2i10  )') ixmin(1), ixmax(1)
	!	write(fid, '(2x, "Output scan    :", 2i10  )') ixmin(2), ixmax(2)
	!	write(fid, '(2x, "Output scan    :", 2i10  )') ixmax(3), ixmin(3)
	!	write(fid, '(2x, "Number of atom :",  i10  )') tip_n + bas_n
	!	write(fid, '(2x, "Number of frame:",  i10  )') scn_i(1) * scn_i(2) * scn_i(3)
	!close(fid)
	write(*, '(2x, "Open file ",a," with jmol")') trim(fxyz)
	write(*, '(2x, "Terminated Normally!")')
	!--------------------------------------------------------------------
	deallocate(bas_sym)
	deallocate(tip_sym)
	!--------------------------------------------------------------------
end subroutine
