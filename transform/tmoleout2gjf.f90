program tmoleout2gjf
	use kinds, only: DP
	use file , only: name_main
	use param, only: au2a
	implicit none
	!--------------------------------------------------------------------
	integer                   :: fid, info
	integer                   :: natom
	integer     , allocatable :: nat (  :)
	real(DP)    , allocatable :: x   (:,:)
	character(2), allocatable :: symbol(:)
	!--------------------------------------------------------------------
	character(200) :: finp, fout
	!--------------------------------------------------------------------
	fid = 1
	!--------------------------------------------------------------------
	call getarg(1, finp)
	fout=trim(name_main(finp))//".gjf"
	!--------------------------------------------------------------------
	call qm_file_natom(finp, natom,      info)
	!--------------------------------------------------------------------
	allocate(x     (3, natom))
	allocate(nat   (   natom))
	allocate(symbol(   natom))
	!--------------------------------------------------------------------
	call qm_file_nat  (finp, natom, nat, info  )
	call qm_file_coord(finp, natom, x  , info  )
	call nat_to_symbol(      natom, nat, symbol)
	!--------------------------------------------------------------------
	x = x * au2a
	!--------------------------------------------------------------------
	call write_gjf    (fout, natom, symbol, x  )
	!--------------------------------------------------------------------
	deallocate(x     )
	deallocate(nat   )
	deallocate(symbol)
	!--------------------------------------------------------------------
	call exit(0)
end
