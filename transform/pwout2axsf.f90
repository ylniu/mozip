program pwout2axsf
	use kinds, only: DP
	use file , only: num_file_lines_word
	implicit none
	!----------------------------------------------------------------------------
	integer               :: fid, info, natom, ncycle
	character(200)        :: finp, fout
	logical               :: scanok
	logical , external    :: search_word_free
	character(200)        :: line
	real(DP), allocatable :: a(:,:,:), x(:,:,:)
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	fid=1
	!----------------------------------------------------------------------------
	call qm_file_natom(finp, natom, info)
	ncycle=num_file_lines_word(finp,"CELL_PARAMETERS")
	!----------------------------------------------------------------------------
	open(fid, file=fout, status="old")
! 		if (search_word_free(fid, "CELL_PARAMETERS", line)) then
! 		end if
	close(fid)
	!----------------------------------------------------------------------------
	!----------------------------------------------------------------------------
	!----------------------------------------------------------------------------
	stop
end
