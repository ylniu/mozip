subroutine file_delete(fname)
	implicit none
	!--------------------------------------------------------------------
	character(*) :: fname
	!--------------------------------------------------------------------
	integer      :: fid
	integer      :: stat
	!--------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fname, iostat=stat, status="old")
	if (stat==0) then
		close(fid, status="delete")
	else
		close(fid) 
	end if
	!--------------------------------------------------------------------
	return
end subroutine
