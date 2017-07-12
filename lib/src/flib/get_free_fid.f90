	subroutine get_free_fid(fid)
	!---------------------------------------------------------------------------
	! This subroutine retures a integer fid, which is not used as a file handle.
	!
	implicit none
	!----------------------------------------------------------------------------
	integer, intent(inout) :: fid
	logical                :: if_open
	!----------------------------------------------------------------------------
	fid=100
	inquire(unit=fid, opened=if_open)
	if ( .not. if_open) return
	do while (if_open)
		fid=fid+1
		inquire(unit=fid, opened=if_open)
	end do
	return
	!----------------------------------------------------------------------------
end subroutine