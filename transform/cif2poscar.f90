program cif2poscar
	use kinds, only: DP
	use file , only: name_main
	use math , only: sort_array
	implicit none
	!--------------------------------------------------------------------
	integer                     :: info, natom, fid, i, nfix
	real(DP)                    :: a(3,3)
	character(200)              :: fcif, fpos, file_type, version, tmp
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x   (:,:)
	character(  1), allocatable :: fix (:,:)
	character(200)              :: title, fnewcif
	integer       , allocatable :: idx(:)
	!--------------------------------------------------------------------
	fid  =  1
	nfix = -1
	i=iargc()
	call getarg(1, fcif)
	if (i>=2) then
		call getarg(2,tmp)
		read(tmp,*) nfix
	end if
	!--------------------------------------------------------------------
	call qm_file_type(fcif, file_type, version, info)
	!--------------------------------------------------------------------
	if (info<0 .or. trim(file_type) /= "CIF") then
		write(*,*) trim(fcif)//" is not cif file, stop!"
		stop
	end if
	!--------------------------------------------------------------------
	call qm_file_natom(fcif, natom, info)
	!--------------------------------------------------------------------
	if (nfix<=0 .or. nfix >natom) then
		nfix = natom
	end if
	!--------------------------------------------------------------------
	allocate(symbol(natom))
	allocate(fix (3,natom))
	allocate(x   (3,natom))
	allocate(idx (  natom))
	fix="F"
	!--------------------------------------------------------------------
	do i=nfix+1, natom
		fix(:,i) = "T"
	end do
	!--------------------------------------------------------------------
	call qm_file_crystal(fcif, natom, a, symbol, x, info)
	fpos    = trim(name_main(fcif))//".pos"
	fnewcif = trim(name_main(fcif))//"_sort.cif"
	!--------------------------------------------------------------------
	call sort_array_string(natom, symbol, idx, 1)
	!--------------------------------------------------------------------
	call sort_array       (natom, symbol, idx   )
	call sort_array       (natom, x     , idx   )
	call sort_array       (natom, fix   , idx   )
	!--------------------------------------------------------------------
	title="POSCAR_from_cif"
	call write_poscar(fpos, title, a, natom, symbol, x, fix)
	call write_cif(fnewcif, natom, symbol, x, a, "CRYSTAL")
	!--------------------------------------------------------------------
	deallocate(symbol)
	deallocate(x     )
	deallocate(fix   )
	deallocate(idx   )
	!--------------------------------------------------------------------
	stop
end
