program pdb2xyz
	use kinds, only: DP
	use file,  only: name_main
	integer                     :: fid, natom, i, j
	real(DP)      , allocatable :: x(:,:)
	character( 20), allocatable :: symbol(:)
	character(200)              :: fpdb, fxyz, line, tmp
	logical                     :: scanok
	logical       , external    :: search_word_free
	logical       , external    :: search_word_free_last
	logical       , external    :: is_number
	!----------------------------------------------------------------------------
	fid=1
	call getarg(1, fpdb)
	fxyz=trim(name_main(fpdb))//".xyz"
	!----------------------------------------------------------------------------
	open(fid, file=fpdb, status="old")
		scanok=search_word_free_last(fid,"ATOM",line)
		read(line,*) tmp, natom
		allocate(symbol(   natom))
		allocate(x     (3, natom))
		rewind(fid)
		scanok=search_word_free(fid,"ATOM",line)
		backspace(fid)
		do i=1, natom
			read(fid,*) tmp, tmp, symbol(i), tmp, tmp, x(:,i)
			do j=1, len(trim(symbol(i)))
				if (is_number(symbol(i)(j:j))) then
					symbol(i)(j:j)=''
				end if
			end do
			symbol(i) = trim(adjustl(symbol(i)))
		end do
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fxyz)
		write(fid, '(i0)') natom
		do i=1, natom
			write(fid, '(a2, 3(4x, f15.9))') trim(symbol(i)), x(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	write(*,'("Transform ",a," to ",a)') trim(fpdb), trim(fxyz)
	!----------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	!----------------------------------------------------------------------------
	stop
end
