program xsf2gjf
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	integer                     :: info, natom, i, fid
	real(DP)                    :: a(3,3)
	character(200)              :: fxsf, fgjf, file_type, version, line
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	logical                     :: scanok
	logical       , external    :: search_word_free
	!----------------------------------------------------------------------------
	call getarg(1, fxsf)
	!call qm_file_type(fxsf, file_type, version, info)
	!if (info<0 .or. trim(file_type) /= "CIF") then
		!write(*,*) trim(fxsf)//" is not cif file, stop!"
		!stop
	!end if
	!----------------------------------------------------------------------------
	!call qm_file_natom(fxsf, natom, info)
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	open(fid, file=fxsf, status="old")
		scanok=search_word_free(fid,"PRIMVEC",line)
		read(fid, *) a(:,1)
		read(fid, *) a(:,2)
		read(fid, *) a(:,3)
		scanok=search_word_free(fid,"PRIMCOORD",line)
		read(fid, *) natom
		allocate(symbol(natom))
		allocate(x   (3,natom))
		do i=1, natom
			read(fid, *) symbol(i), x(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	!call rotn(natom, a, x)
	fgjf=trim(name_main(fxsf))//".gjf"
	!----------------------------------------------------------------------------
	call write_gjf(fgjf, natom, symbol, x)
	!----------------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	!----------------------------------------------------------------------------
	stop
end
