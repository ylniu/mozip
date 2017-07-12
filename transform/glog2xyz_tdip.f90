program glog2xyz_tdip
	use kinds, only: DP
	use param, only: au2a
	use file , only: name_main
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer                     :: info, natom, nex, fid
	integer                     :: i, j, k
	real(DP)      , allocatable :: x(:,:)
	real(DP)      , allocatable :: tdip(:,:)
	real(DP)      , allocatable :: osci(  :)
	character(  2), allocatable :: symbol(:)
	character(200)              :: flog, fxyz
	logical       , external    :: search_word_back_free
	!--------------------------------------------------------------------
	call getarg(1, flog)
	fxyz=trim(name_main(flog))//".xyz"
	call qm_file_natom(flog, natom, info)
	call qm_file_nex  (flog, nex  , info)
	allocate(symbol(   natom))
	allocate(x     (3, natom))
	allocate(tdip  (3, nex  ))
	allocate(osci  (   nex  ))
	!--------------------------------------------------------------------
	call qm_file_symbol(flog, natom, symbol,       info)
	call qm_file_coord (flog, natom, x     ,       info)
	call qm_file_tdip  (flog, nex  , tdip  , osci, info)
	x = x * au2a
	open(newunit=fid, file=fxyz)
		do i=1, nex
			write(fid, '(i0)') natom+1
			write(fid, '("Osc. Strength", f15.4)') osci(i)
			do j=1, natom
				write(fid, '(a4, 3f15.7)') symbol(j), x(:,j)
			end do
			write(fid, '(a4, 6f15.7)') "X", (0.0_DP, k=1,3), tdip(:,i)
		end do
	close(fid)
	!--------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	deallocate(tdip  )
	deallocate(osci  )
	!--------------------------------------------------------------------
end
