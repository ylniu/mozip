subroutine qm_file_force_orbital_fid(fid,fname,natom,nf,force,info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	integer     , intent( in) :: fid
	integer     , intent( in) :: nf
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: force(3,natom,nf)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: i, j, istat
	character(200)            :: line, file_type, version
	logical                   :: scanok
	logical     , external    :: search_word_last_del_space
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	force=0.D0
	!----------------------------------------------------------------------------
	call qm_file_type_fid(fid, fname, file_type, version, info)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old", iostat=istat)
	if (istat/=0) return
	select case(trim(file_type))
		!-------------------------------------------------------------------------
		case ("CHEMSHELL")
			!
		case ("GAUSSIAN LOG")
			return
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
! 			rewind(fid)
! 			scanok=search_word_last_del_space(fid,"Center Atomic Forces",line)
! 			if (scanok) then
! 				info=0
! 			else
! 				return
! 			end if
! 			scanok=search_word_first_number(fid,line)
! 			backspace(fid)
! 			do i=1, natom
! 				read(fid,*) tmp, tmp, force(:,i,1)
! 			end do
			!----------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			rewind(fid)
			j=0
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"nuclear repulsion gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"weighted density gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"kinetic energy gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"2-electron gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
			!scanok=search_word_last_del_space(fid,"2-electron1gradient",line)
			!if (scanok) then
			!	info=0
			!else
			!	return
			!end if
			!j=j+1
			!do i=1, natom
			!	read(fid,*) force(:,i,j)
			!end do
			!----------------------------------------------------------------------
			!scanok=search_word_last_del_space(fid,"2-electron2gradient",line)
			!if (scanok) then
			!	info=0
			!else
			!	return
			!end if
			!j=j+1
			!do i=1, natom
			!	read(fid,*) force(:,i,j)
			!end do
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"DFT CD+XC gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
			scanok=search_word_last_del_space(fid,"total DFT gradient",line)
			if (scanok) then
				info=0
			else
				return
			end if
			j=j+1
			do i=1, natom
				read(fid,*) force(:,i,j)
			end do
			!----------------------------------------------------------------------
		case default
			!----------------------------------------------------------------------
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
