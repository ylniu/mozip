subroutine get_gjf_natom_onion_H(fname, natom)
	use kinds , only: DP
	use string, only: last_word
	implicit none
	!----------------------------------------------------------------------------
	character(*)  , intent( in) :: fname
	integer       , intent(out) :: natom
	!----------------------------------------------------------------------------
	integer                     :: i, j, fid
	integer                     :: nlines
	integer                     :: atom_begin
	integer                     :: atom_end
	logical                     :: findroute
	!----------------------------------------------------------------------------
	integer       , external    :: number_of_lines
	logical       , external    :: is_number
	!----------------------------------------------------------------------------
	character(200), allocatable :: lines(:)
	!----------------------------------------------------------------------------
	integer       , external    :: number_of_words
	!----------------------------------------------------------------------------
	findroute=.false.
	!----------------------------------------------------------------------------
	call get_gjf_atom_linenumber(fname,atom_begin)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
		nlines=number_of_lines(fid)
		allocate(lines(nlines))
		rewind(fid)
		do i=1, nlines
			read(fid,'(a)') lines(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	! find atom_end
	!
	do i=atom_begin, nlines
		j=len(trim(adjustl(lines(i))))
		if (j==0) then
			atom_end = i-1
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	natom = 0
	do i=atom_begin, atom_end
		if(trim(last_word(lines(i)))=="H") natom = natom + 1
	end do
	!----------------------------------------------------------------------------
	deallocate(lines)
	!----------------------------------------------------------------------------
	return
end subroutine
