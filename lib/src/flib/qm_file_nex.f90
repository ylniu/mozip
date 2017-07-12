subroutine qm_file_nex(fname, nex, info)
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	integer     , intent(out) :: nex
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, ios, i
	character(200)            :: line, tmp, file_type, version
	logical                   :: scanok, is_number
	logical     , external    :: search_word_free_last
	!----------------------------------------------------------------------------
	nex = 0
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		!-------------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
			if (search_word_free_last(fid, "Excited State", line)) then
				read(line,*) tmp, tmp, tmp
				call del_char(tmp,":")
				read(tmp,*) nex
				close(fid)
				info=0
				return
			end if
			!----------------------------------------------------------------------
		case default
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
