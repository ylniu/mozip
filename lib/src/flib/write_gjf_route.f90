subroutine write_gjf_route(fgjf, natom, symbol, x, n, route, info)
	use kinds, only: DP
	use file,  only: name_main
	implicit none
	integer        :: natom, n, info
	real(DP)       :: x   (3,natom)
	character(  *) :: symbol(natom), fgjf
	integer        :: fid, i
	character(  *) :: route(n)
	!----------------------------------------------------------------------------
	info = 0
	call get_free_fid(fid)
	open(fid, file=fgjf)
		do i=1, n
			call str_delete(route(i), "geom=connectivity")
			write(fid, '(a)') trim(route(i))
		end do
		do i=1, natom
			write(fid,'(a3, 3f17.8)') symbol(i), x(:,i)
		end do
		write(fid,*)
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
