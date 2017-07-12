!-------------------------------------------------------------------------------
! Subroutine : qm_file_charge
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_file_charge(fname, natom, charge, info)
	use kinds
	use Param, only: au2a
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: charge(natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j
	character(200)            :: line, tmp, file_type, version
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	!----------------------------------------------------------------------------
	if (natom<=0) then
		write(*,'(2x,"Error!")')
		write(*,'(2x,"natom =", i20)') natom
		write(*,'(2x,"Stop!")')
		stop
	end if
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN
			!----------------------------------------------------------------------
			if (search_word_free_last(fid, "Condensed to atoms (all electrons)",line)) then
				if (search_word_free(fid, "Mulliken charges:",line)) then
					read(fid,*)
					do i=1, natom
						read(fid,*) (tmp, j=1, 2), charge(i)
					end do
					info=0
				end if
			else
				close(fid)
				return
			end if
		!-------------------------------------------------------------------------
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
