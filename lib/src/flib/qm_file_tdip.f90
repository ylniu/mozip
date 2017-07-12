subroutine qm_file_tdip(fname, nex, tdip, osci, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: nex
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: tdip(3, nex)
	real(DP)    , intent(out) :: osci(nex)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i
	character(200)            :: line, tmp, file_type, version
	logical     , external    :: search_word_free_last
	!----------------------------------------------------------------------------
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
			if (search_word_free_last(fid, "1PDM for each excited state", line)) then
				read(fid,*) 
				read(fid,*) 
				read(fid,*)
				do i=1, nex
					read(fid,*) tmp, tdip(:,i), tmp, osci(i)
				end do
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
