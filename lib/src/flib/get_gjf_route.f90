subroutine get_gjf_route(fname, n, m, route, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fname
	integer     , intent( in) :: n
	character(*), intent(out) :: route(n)
	integer     , intent(out) :: m
	integer     , intent(out) :: info
	!----------------------------------------------------------------------------
	integer           :: i, fid
	!----------------------------------------------------------------------------
	info = 0
	call get_gjf_atom_linenumber(fname,m)
	m = m - 1
	!----------------------------------------------------------------------------
	fid = 1
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
		do i=1, m
			read(fid,'(a)') route(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
