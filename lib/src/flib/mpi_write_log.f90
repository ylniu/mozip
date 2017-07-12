subroutine MPI_WRITE_LOG(ilog,numprocs,myid,message)
	use mpi
	implicit none
	!----------------------------------------------------------------------------
	! input variables
	integer      , intent(in   ) :: ilog
	integer      , intent(in   ) :: numprocs
	integer      , intent(in   ) :: myid
	character(*) , intent(inout) :: message(0:numprocs-1)
	!----------------------------------------------------------------------------
	! local variables
	integer                   :: i, n, ierr
	!----------------------------------------------------------------------------
	n=len(message(0))
	CALL MPI_GATHER (message(myid), n, MPI_CHARACTER, message, n, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
	!----------------------------------------------------------------------------
	if (myid==0) then
		do i=0,numprocs - 1
			write(ilog,'(a)') trim( message(i) )
		end do
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
