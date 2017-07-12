subroutine qm_wien2k_psink_ngrid(fpsink, ngrid, info)
	!----------------------------------------------------------------------------
	integer      :: ngrid(3), info
	character(*) :: fpsink
	!----------------------------------------------------------------------------
	integer      :: fid, i
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info=-1
	open(fid, file=fpsink, status="old")
		read(fid,*)
		do i=1, 3
			read(fid,*) ngrid(i)
		end do
	close(fid)
	info=1
	!----------------------------------------------------------------------------
	return
end subroutine
