subroutine get_gjf_atom_linenumber(fgjf,lnumber)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	character(*)  , intent( in) :: fgjf
	integer       , intent(out) :: lnumber
	!----------------------------------------------------------------------------
	integer                     :: i, j, fid
	integer                     :: nlines
	integer                     :: line_title
	logical                     :: findroute
	!----------------------------------------------------------------------------
	integer       , external    :: number_of_lines
	!----------------------------------------------------------------------------
	character(200)              :: tmp
	character(200), allocatable :: lines(:)
	!----------------------------------------------------------------------------
	findroute=.false.
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fgjf, status="old")
		nlines=number_of_lines(fid)
		allocate(lines(nlines))
		rewind(fid)
		do i=1, nlines
			read(fid,'(a)') lines(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	! find line title
	!
	do i=1, nlines
		tmp=trim(adjustl(lines(i)))
		if (.not.findroute) then
			if (tmp(1:1)=="#") findroute=.true.
		else
			if (len(trim(adjustl(tmp)))==0) then
				line_title=i+1
				exit
			end if
		end if
	end do
	!----------------------------------------------------------------------------
	! find lnumber
	!
	do i=line_title+1, nlines
		j=len(trim(adjustl(lines(i))))
		if (j==0) then
			lnumber=i+2
			exit
		end if
	end do
	return
end subroutine
