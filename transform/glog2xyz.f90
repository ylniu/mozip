program glog2xyz
	use kinds, only: DP
	use file,  only: name_main
	!------------------------------------------------------------------------------
	implicit none
	!------------------------------------------------------------------------------
	integer                     :: fid, natom
	integer                     :: i, j
	integer       , allocatable :: nat(:)
	real(DP)      , allocatable :: charge(:)
	real(DP)      , allocatable :: x(:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: flog, fxyz, line, tmp
	!------------------------------------------------------------------------------
	logical        :: scanok
	logical        :: search_word_free
	logical        :: search_word_free_last
	!------------------------------------------------------------------------------
	fid=1
	call getarg(1, flog)
	fxyz=trim(name_main(flog))//".xyz"
	!------------------------------------------------------------------------------
	open(fid, file=flog, status="old")
		scanok=search_word_free(fid, "NAtoms=", line)
		read(line,*)  tmp, natom
		!---------------------------------------------------------------------------
		allocate(nat   (  natom))
		allocate(symbol(  natom))
		allocate(x     (3,natom))
		allocate(charge(  natom))
		!---------------------------------------------------------------------------
		rewind(fid)
		scanok=search_word_free_last(fid, "orientation:", line)
		do i=1, 4
			read(fid,*)
		end do
		do i=1, natom
			read(fid,*) tmp, nat(i), tmp, x(:,i)
		end do
		call nat_to_symbol(natom, nat, symbol)
		!---------------------------------------------------------------------------
		rewind(fid)
		scanok=search_word_free_last(fid, "Mulliken charges:", line)
		read(fid,*)
		do i=1, natom
			read(fid,*) tmp, tmp, charge(i)
		end do
		!---------------------------------------------------------------------------
	close(fid)
	!------------------------------------------------------------------------------
	open(fid, file=fxyz)
		write(fid,'(i0)') natom
		do i=1, natom
			write(fid,'(a2, 3f15.7,2x, f15.7)') symbol(i), x(:,i), charge(i)
		end do
	close(fid)
	!------------------------------------------------------------------------------
	deallocate(nat   )
	deallocate(symbol)
	deallocate(charge)
	deallocate(x     )
	!------------------------------------------------------------------------------
	stop
end
