subroutine get_force_field
	use kinds     , only: DP
	use module_afm, only: FF
	use cmd_progress
	use omp_lib
	implicit none
	!--------------------------------------------------------------------
	type(cls_cmd_progress) :: progress
	integer                :: i, ix, iy, iz
	integer                :: n, nx, ny, nz, nxy
	integer                :: para_type
	character(64)          :: prefix
	!--------------------------------------------------------------------
	para_type = 2
	prefix    = "Get force field"
	!--------------------------------------------------------------------
	if (para_type==1) then
		nx    = FF%n(1) + 1
		ny    = FF%n(2) + 1
		nz    = FF%n(3) + 1
		nxy   = nx * ny
		n     = nxy * nz
		!-----------------------------------------------------------------
		!$omp parallel do private(i, iy, iz)
		do i=0, n-1
			ix = mod(i,nx)
			iy = mod(i,nxy) / nx
			iz = i / nxy
			!--------------------------------------------------------------
			call getCoulombFF    (ix,iy,iz)
			call getLenardJonesFF(ix,iy,iz)
		end do
		!$omp end parallel do
	else if (para_type==2) then
		!-----------------------------------------------------------------
		call progress%set(n=FF%n(1)+1, L=25, prefix=prefix)
		!-----------------------------------------------------------------
		!$omp parallel do private(iy, iz)
		do ix=0, FF%n(1)
			do iy=0, FF%n(2)
				do iz=0, FF%n(3)
					!--------------------------------------------------------
					call getCoulombFF    (ix,iy,iz)
					call getLenardJonesFF(ix,iy,iz)
					!--------------------------------------------------------
				end do
			end do
			call progress%put(1)
		end do
		!$omp end parallel do
	end if
	!--------------------------------------------------------------------
	FF%T = FF%C + FF%L
	!--------------------------------------------------------------------
end subroutine
