program chgsum
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: natom(2), n(3,2), ntype(2)
	!----------------------------------------------------------------------------
	character(200)              :: fchg(3)
	logical                     :: condition
	!----------------------------------------------------------------------------
	integer                     :: i
	character(  2), allocatable :: symbols(:,:)
	integer       , allocatable :: natoms(:,:)
	real(DP)      , allocatable :: x(:,:,:)
	character(200)              :: title(2)
	character(200)              :: coord_type(2)
	real(DP)                    :: alat(2)
	real(DP)                    :: a(3,3,2)
	real(DP)      , allocatable :: grid(:,:,:,:)
	!----------------------------------------------------------------------------
	if (iargc()==0) then
		call help()
	else if (iargc()==1) then
		call help()
	else if (iargc()==2) then
		call getarg(1, fchg(1))
		call getarg(2, fchg(2))
		fchg(3)="CHGCAR_sum"
	else if (iargc()==3) then
		call getarg(1, fchg(1))
		call getarg(2, fchg(2))
		call getarg(3, fchg(3))
	else
		write(*,*) "Error, too many arguments!"
		stop
	end if
	!----------------------------------------------------------------------------
	call qm_vasp_chgcar_info(fchg(1), natom(1), n(1,1), ntype(1))
	call qm_vasp_chgcar_info(fchg(2), natom(2), n(1,2), ntype(2))
	!----------------------------------------------------------------------------
	condition = .true.
	condition = condition .and. natom(1) == natom(2)
	condition = condition .and. n  (1,1) == n  (1,2)
	condition = condition .and. n  (2,1) == n  (2,2)
	condition = condition .and. n  (3,1) == n  (3,2)
	condition = condition .and. ntype(1) == ntype(2)
	!----------------------------------------------------------------------------
	if (.not.condition) then
		write(*,*) "Error! Please check natom, na, nb, nc, ntype in two files:"
		write(*,*) trim(fchg(1)), " and ", trim(fchg(2))
		write(*,*) "Stop"
		stop
	end if
	!----------------------------------------------------------------------------
	allocate(symbols            (ntype(1),2))
	allocate(natoms             (ntype(1),2))
	allocate(x                (3,natom(1),2))
	allocate(grid(n(1,1), n(2,1), n(3,1), 3))
	!----------------------------------------------------------------------------
	do i=1, 2
		call qm_vasp_chgcar_data(fchg(i), natom(i), n(1,i), ntype(i), &
			symbols(1,i), natoms(1,i), x(1,1,i), title(i), alat(i), a(1,1,i), &
			coord_type(i), grid(1,1,1,i))
	end do
	!----------------------------------------------------------------------------
	grid(:,:,:,3) = grid(:,:,:,1) + grid(:,:,:,2)
	!----------------------------------------------------------------------------
	call write_chgcar(fchg(3), natom(1), n(1,1), ntype(1), &
			symbols(1,1), natoms(1,1), x(1,1,1), title(1), alat(1), a(1,1,1), &
			coord_type(1), grid(1,1,1,3))
	!----------------------------------------------------------------------------
	deallocate(symbols)
	deallocate(natoms )
	deallocate(x      )
	deallocate(grid   )
	!----------------------------------------------------------------------------
	stop
	!----------------------------------------------------------------------------
end
!
!-------------------------------------------------------------------------------
!
subroutine help()
	implicit none
	!----------------------------------------------------------------------------
	write(*,'("Usages:")')
	write(*,'("chgsum CHGCAR1 CHGCAR2 CHGCAR_sum")')
	stop
	!----------------------------------------------------------------------------
	return
end subroutine
