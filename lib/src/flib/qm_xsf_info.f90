subroutine qm_xsf_info(fxsf, natom, ngrid, center, a, nat, x, grid, info)
	use kinds, only: DP
	implicit none
	integer     , intent( in) :: natom, ngrid(3)
	character(*), intent( in) :: fxsf
	!----------------------------------------------------------------------------
	integer     , intent(out) :: nat(natom), info
	real(DP)    , intent(out) :: center(3), a(3,3)
	real(DP)    , intent(out) :: x(3,natom)
	real(DP)    , intent(out) :: grid(ngrid(1), ngrid(2), ngrid(3))
	!----------------------------------------------------------------------------
	integer                   :: fid
	integer                   :: nx, ny, nz, ix, iy, iz, i, j
	character(200)            :: line, tmp
	character(2), allocatable :: symbol(:)
	!----------------------------------------------------------------------------
	logical                   :: scanok
	logical     , external    :: search_word_free, is_number
	!----------------------------------------------------------------------------
	info = -1
	nx   = ngrid(1)
	ny   = ngrid(2)
	nz   = ngrid(3)
	call get_free_fid(fid)
	allocate(symbol(natom))
	open(fid, file=fxsf, status="old")
		!-------------------------------------------------------------------------
		center=0.D0
		!-------------------------------------------------------------------------
		if (search_word_free(fid, "PRIMVEC", line)) then
			do i=1, 3
				read(fid,*) (a(j,i), j=1, 3)
			end do
		end if
		!-------------------------------------------------------------------------
		scanok=search_word_free(fid, "ATOMS", line)
		if (scanok) then
			do i=1, natom
				read(fid,'(a)') line
				read(line,*) tmp
				if (is_number(trim(tmp))) then
					read(line,*) nat(i), x(:,i)
				else
					read(line,*) symbol(i), x(:,i)
					call symbol_to_nat(1,symbol(i),nat(i))
				end if
			end do
		else
			rewind(fid)
			scanok=search_word_free(fid, "PRIMCOORD", line)
			if (scanok) then
				read(fid,*)
				do i=1, natom
					read(fid,'(a)') line
					read(line,*) tmp
					if (is_number(trim(tmp))) then
						read(line,*) nat(i), x(:,i)
					else
						read(line,*) symbol(i), x(:,i)
						call symbol_to_nat(1,symbol(i),nat(i))
					end if
				end do
			else
				rewind(fid)
				
			end if
		end if
		!-------------------------------------------------------------------------
		scanok=search_word_free(fid, "BEGIN_BLOCK_DATAGRID3D", line)
		if (scanok) then
			if (search_word_free(fid, "DATAGRID_3D", line)) then
				read(fid,*)
				read(fid,*) center
				do i=1, 3
					read(fid,*) (a(j,i), j=1, 3)
				end do
				read(fid,*) (((grid(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
			end if
		else
			rewind(fid)
			scanok=search_word_free(fid, "BEGIN_BLOCK_DATAGRID_3D", line)
			if (scanok) then
				if (search_word_free(fid, "DATAGRID_3D", line)) then
					read(fid,*)
					read(fid,*) center
					do i=1, 3
						read(fid,*) (a(j,i), j=1, 3)
					end do
					read(fid,*) (((grid(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
				end if
			end if
		end if
	close(fid)
	info=0
	!----------------------------------------------------------------------------
	deallocate(symbol)
	return
end subroutine
