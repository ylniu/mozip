subroutine extract_input(finp,fout,bc,ec)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	!
	character(*), intent( in) :: finp
	character(*), intent( in) :: fout
	character(*), intent( in) :: bc
	character(*), intent( in) :: ec
	!----------------------------------------------------------------------------
	! local  variable
	!
	integer                   :: istat
	integer                   :: fid_inp
	integer                   :: fid_out
	character(400)            :: line
	!----------------------------------------------------------------------------
	call get_free_fid(fid_inp)
	open(fid_inp, file=finp)
	!----------------------------------------------------------------------------
	call get_free_fid(fid_out)
	open(fid_out, file=fout)
	!----------------------------------------------------------------------------
	read(fid_inp,'(a)', iostat=istat) line
	!----------------------------------------------------------------------------
	do while(istat==0)
		!-------------------------------------------------------------------------
		! Trim the space
		! Delete line beginning with "#", "!"
		! Delete the part of line after "#" and "!"
		!
		call del_comment1(line)
		!-------------------------------------------------------------------------
		if ( len_trim(line)==0 ) then
			read(fid_inp,'(a)', iostat=istat) line
			cycle
		end if
		!-------------------------------------------------------------------------
		if ( trim(line) == bc ) then
			!----------------------------------------------------------------------
			! Read data between bc and ec
			!
			read(fid_inp,'(a)', iostat=istat) line
			call del_comment1(line)
			!----------------------------------------------------------------------
			! Write data to fout
			!
			do while(istat==0 .and. trim(adjustl(line)) /= trim(ec) )
				if ( len_trim(line)==0 ) then
					read(fid_inp,'(a)', iostat=istat) line
					cycle
				end if
				write(fid_out, '(x,a)') trim(line)
				read (fid_inp, '(a)', iostat=istat) line
				call del_comment1(line)
			end do
			exit
		end if
		read(fid_inp,'(a)') line
	end do
	!----------------------------------------------------------------------------
	close(fid_out)
	close(fid_inp)
	!----------------------------------------------------------------------------
	return
end subroutine
