subroutine qm_vasp_chgcar_info(fchg, natom, n, ntype)
	use kinds, only: DP
	use array, only: array_sum
	implicit none
	!----------------------------------------------------------------------------
	character(*)  , intent( in) :: fchg
	integer       , intent(out) :: natom
	integer       , intent(out) :: n(3)
	!----------------------------------------------------------------------------
	character(200)              :: title
	character(200)              :: coord_type
	real(DP)                    :: alat
	real(DP)                    :: a(3,3)
	integer                     :: ntype
	character(  2), allocatable :: symbols(:)
	integer       , allocatable :: natoms(:)
	real(DP)      , allocatable :: x(:,:)
	!----------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: i
	character(200)              :: line, tmp
	integer       , external    :: number_of_words
	logical       , external    :: is_number
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fchg, status="old")
		read(fid,*) title
		read(fid,*) alat
		do i=1, 3
			read(fid,*) a(:,i)
		end do
		read(fid,'(a)') line
		ntype = number_of_words(line)
		!-------------------------------------------------------------------------
		allocate(symbols(ntype))
		allocate(natoms (ntype))
		!-------------------------------------------------------------------------
		read(line,*) tmp
		if (is_number(tmp)) then
			symbols=""
		else
			read(line,*) symbols
			read(fid,'(a)') line
		end if
		read(line,*) natoms
		!-------------------------------------------------------------------------
		read(fid,*) coord_type
		natom = array_sum(ntype, natoms)
		allocate(x(3,natom))
		do i=1, natom
			read(fid,*) x(:,i)
		end do
		read(fid,*)
		read(fid,*) n
	close(fid)
	!----------------------------------------------------------------------------
	!----------------------------------------------------------------------------
	deallocate(symbols)
	deallocate(natoms )
	deallocate(x      )
	!----------------------------------------------------------------------------
	return
end subroutine