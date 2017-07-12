!-------------------------------------------------------------------------------
! Subroutine : qm_file_tdipd
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_file_tdipd(fname, natom, dip0, tdipd, info)
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
	real(DP)    , intent(out) :: dip0(3)
	real(DP)    , intent(out) :: tdipd(3,3,natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j
	character(200)            :: line, tmp, file_type, version
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	logical     , external    :: search_word
	logical     , external    :: search_word_last_del_space
	logical     , external    :: search_word_first_number
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
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			if (search_word_free(fid,"Transition dipole moment derivative (Atomic unit)", line)) then
				read(fid,*) tmp, tmp, tmp, tmp, dip0
				do i=1, natom
					do j=1, 3
						read(fid,*) tmp, tmp, tmp, tmp, tdipd(:,j,i)
					end do
				end do
				info=0
			end if
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
