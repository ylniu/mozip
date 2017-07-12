module mpi_kind
	use mpi
	use kinds, only: DP
	integer :: MPI_KIND_REAL
	integer :: MPI_KIND_COMPLEX
	contains
		subroutine get_mpi_kind()
			if     (DP==8) then
				MPI_KIND_REAL=MPI_DOUBLE_PRECISION
				MPI_KIND_COMPLEX=MPI_DOUBLE_COMPLEX
			else if(DP==16) then
				MPI_KIND_REAL=MPI_LONG_DOUBLE
				MPI_KIND_COMPLEX=MPI_COMPLEX32
			end if
		end subroutine
end module
