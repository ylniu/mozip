subroutine rewrite_input(fid,new_fid,new_filename)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	integer      ,   intent( in) :: fid, new_fid
	!----------------------------------------------------------------------------
	! output variable
	character(*) ,   intent(out) :: new_filename
	!----------------------------------------------------------------------------
	! local  variable
	integer                      :: i, n
	integer        , external    :: number_of_lines
	character(400)               :: line, tmp_filename
	!----------------------------------------------------------------------------
	! Example: new_filename="/dir/input"
	inquire(unit=fid, name=tmp_filename)
	!----------------------------------------------------------------------------
	! Example: new_filename="input"
	call full_to_short_name(tmp_filename)
	!----------------------------------------------------------------------------
	! Example: new_filename=".input"
	new_filename="."//trim(tmp_filename)
	!----------------------------------------------------------------------------
	! calculation the number of lines of the file with the id "fid"
	n = number_of_lines(fid)
	!----------------------------------------------------------------------------
	rewind(fid)
	!
	open(new_fid,file=trim(new_filename))
	do i=1,n
		read(fid,'(a)') line
		line=adjustl(line)
		call del_comment(line)
		if ( len(trim(line)) /= 0 ) then
			write(new_fid,'(x,a)') trim(line)
		end if
	end do
	close(new_fid)
	return
end subroutine
