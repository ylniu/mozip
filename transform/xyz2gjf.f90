program xyz2gjf
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	integer                     :: info, natom, i, fid, nl
	real(DP)                    :: a(3,3)
	character(200)              :: fxyz, fgjf, file_type, version
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	character(200)              :: line
	integer       , external    :: number_of_words
	integer       , external    :: number_of_lines
	!----------------------------------------------------------------------------
	fid=1
	call getarg(1, fxyz)
	open(fid, file=fxyz, status="old")
		read(fid,'(a)') line
		if (number_of_words(line)==1) then
			read(line,*) natom
		else
			rewind(fid)
			natom = number_of_lines(fid)
			rewind(fid)
		end if
		!-------------------------------------------------------------------------
		allocate(symbol(natom))
		allocate(x   (3,natom))
		!-------------------------------------------------------------------------
		read(fid,'(a)') line
		do while(number_of_words(line)<4)
			read(fid,'(a)') line
		end do
		backspace(fid)
		!-------------------------------------------------------------------------
		do i=1, natom
			read(fid,*) symbol(i), x(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	fgjf=trim(name_main(fxyz))//".gjf"
	!----------------------------------------------------------------------------
	call write_gjf(fgjf, natom, symbol, x)
	!----------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	!----------------------------------------------------------------------------
	stop
end
