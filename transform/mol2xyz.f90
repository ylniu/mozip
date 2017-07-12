program mol2xyz
	use kinds, only: DP
	use file,  only: name_main
	integer                     :: fid, natom, i
	real(DP)      , allocatable :: x(:,:)
	character( 20), allocatable :: symbol(:)
	character(200)              :: fmol, fxyz, line, tmp
	logical                     :: scanok
	logical       , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid=1
	call getarg(1, fmol)
	fxyz=trim(name_main(fmol))//".xyz"
	!----------------------------------------------------------------------------
	open(fid, file=fmol, status="old")
		read(fid,'(a)') tmp
		read(fid,'(a)') tmp
		read(fid,'(a)') tmp
		read(fid,'(i3)') natom
		if (natom==0) then
			scanok=search_word_free(fid,"END ATOM",line)
			backspace(fid)
			backspace(fid)
			read(fid,*) tmp, tmp, natom
			allocate(symbol(   natom))
			allocate(x     (3, natom))
			rewind(fid)
			scanok=search_word_free(fid,"BEGIN ATOM",line)
			do i=1, natom
				read(fid,*) tmp, tmp, tmp, symbol(i), x(:,i)
			end do
		else
			allocate(symbol(   natom))
			allocate(x     (3, natom))
			do i=1, natom
				read(fid,*) x(:,i), symbol(i)
			end do
		end if
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fxyz)
		write(fid, '(i0)') natom
		do i=1, natom
			write(fid, '(a2, 3(4x, f15.9))') trim(symbol(i)), x(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	write(*,'("Transform ",a," to ",a)') trim(fmol), trim(fxyz)
	!----------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	!----------------------------------------------------------------------------
	stop
end
