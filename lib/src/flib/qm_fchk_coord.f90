!-------------------------------------------------------------------------------
! Subroutine : qm_fchk_coord
! Input      : fname
! Input      : natom
! Output     : x (atomic unit)
! Output     : info
!-------------------------------------------------------------------------------
subroutine qm_fchk_coord(fname, natom, x, info)
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
	real(DP)    , intent(out) :: x(3,natom)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j
	character(200)            :: line
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
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	if (search_word_last_del_space(fid, "Current cartesian coordinates",line)) then
		read(fid,*) ((x(i,j),i=1,3),j=1,natom)
		info=0
	else
		write(*,'("Error!")')
		write(*,'("Can not find coordinate in Gaussian fchk file, stop!")')
		write(*,*) trim(fname)
		stop
	end if
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
