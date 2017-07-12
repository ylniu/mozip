subroutine relaxTipStroke()
	use module_afm, only: scn, tip
	!use module_afm, only: f_eps
	!use module_afm, only: bas
	use module_afm, only: debug_imin
	use module_afm, only: debug_imax
	use module_afm, only: debug_xy
	use cmd_progress
	use omp_lib
	use param     , only: au2a
	implicit none
	!--------------------------------------------------------------------
	type(cls_cmd_progress) :: progress
	integer                :: ix, iy, i, j, n, nx, ny, m, fid
	integer                :: para_type
	character(64)          :: prefix
	logical                :: debug_loc
	integer                :: imin(3), imax(3)
	!--------------------------------------------------------------------
	imin = 0
	imax = scn%n
	if(debug_xy) then
		do i=1, 3
			debug_imin(i) = max(debug_imin(i),       0 )
			debug_imax(i) = max(debug_imax(i),       0 )
			debug_imin(i) = min(debug_imin(i), scn%n(i))
			debug_imax(i) = min(debug_imax(i), scn%n(i))
		end do
		imin = debug_imin
		imax = debug_imax
	end if
	para_type = 2
	debug_loc = .false.
	prefix    = "AFM    scanning"
	!--------------------------------------------------------------------
	if (para_type==1) then
		nx = scn%n(1) + 1
		ny = scn%n(2) + 1
		n  = nx * ny
		!$omp parallel do private(m, i, ix, iy)
		do i=0, n-1
			ix = mod(i, nx)
			iy = i / nx
			!--------------------------------------------------------------
			if (debug_loc) then
				m=omp_get_thread_num()
				write(*,'("Thread: ",i4," , ix = ",i4," / ",i4, " ; iy = ", i4," / ", i4)') &
					m, ix, scn%n(1), iy, scn%n(2)
			end if
			!--------------------------------------------------------------
			call relaxProbe(ix, iy)
		end do
		!$omp end parallel do
	else if (para_type==2) then
		call progress%set(n=imax(1)-imin(1)+1, L=25, prefix=prefix)
		!$omp parallel do private(m, ix, iy)
		do ix=imin(1), imax(1)
			!--------------------------------------------------------------
			m=omp_get_thread_num()
			!--------------------------------------------------------------
			do iy=imin(2), imax(2)
				call relaxProbe(ix, iy)
			end do
			call progress%put(1)
		end do
		!$omp end parallel do
	end if
	if (debug_xy) then
		!-----------------------------------------------------------------
		open(newunit=fid, file="force_dbg.dat")
		write(fid, '(5a20)') "n", "x", "fz", "fza", "cycles"
		do i=imax(3), imin(3), -1
			write(fid, '(i20, f20.10, 2es20.10, i20)') i, scn%x(3,imin(1),imin(2),i) * au2a, &
			scn%Fz(imin(1),imin(2),i), &
			scn%Fa(imin(1),imin(2),i), &
			scn%cy(imin(1),imin(2),i)
		end do
		!-----------------------------------------------------------------
		open(newunit=fid, file="force_dbg.xyz")
			do i=imax(3), imin(3), -1
				write(fid, '(i10)') tip%n
				write(fid, '("z", i6, " /", i6)') i, scn%n(3)
				do j=1, tip%n
					write(fid, '(a4, 6f20.10)') tip%symbol(j), &
						scn%xp(:,j,imin(1),imin(2),i) * au2a, &
						(scn%ft(:,j,imin(1),imin(2),i) + scn%fb(:,j,imin(1),imin(2),i))
				end do
			end do
		close(fid)
		!-----------------------------------------------------------------
		write(*, '(2x, "Debug mode finished! Stop")')
		call exit(1)
	end if
	!--------------------------------------------------------------------
	return
end subroutine
