program nw2gjf
	use kinds, only: DP
	use file,  only: name_main
	!----------------------------------------------------------------------------
	implicit none
	integer                     :: natom, i, j, k, i1, fid, nl
	integer                     :: atom_begin_mol, atom_end_mol
	character(200)              :: fxyz, fgjf
	character(  2), allocatable :: symbol(:)
	real(DP)      , allocatable :: x(:,:)
	character(200)              :: tmp
	integer       , external    :: number_of_words
	integer       , external    :: number_of_lines
	character(200), allocatable :: lines_mol(:)
	!----------------------------------------------------------------------------
	fid=1
	call getarg(1, fxyz)
	open(fid, file=fxyz, status="old")
		nl=number_of_lines(fid)
		allocate(lines_mol(nl))
		rewind(fid)
		do i=1, nl
			read(fid,'(a)') lines_mol(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	do i=1, nl
		if (index(lines_mol(i), "geometry")>0) then
			atom_begin_mol=i+1
			do j=i+1,nl
				k=number_of_words(lines_mol(j))
				if (k>=4) then
				!-------------------------------------------------------------------
					do i1=j, nl
						read(lines_mol(i1),*) tmp
						if (trim(tmp)=="end") then
							atom_end_mol=i1-1
							exit
						end if
					end do
				end if
			end do
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	natom = atom_end_mol - atom_begin_mol + 1
	allocate(symbol(   natom))
	allocate(x     (3, natom))
	!----------------------------------------------------------------------------
	j=0
	do i=atom_begin_mol, atom_end_mol
		j=j+1
		read(lines_mol(i), *) symbol(j), x(:,j)
	end do
	!----------------------------------------------------------------------------
	fgjf=trim(name_main(fxyz))//".gjf"
	!----------------------------------------------------------------------------
	call write_gjf(fgjf, natom, symbol, x)
	!----------------------------------------------------------------------------
	deallocate(symbol   )
	deallocate(x        )
	deallocate(lines_mol)
	!----------------------------------------------------------------------------
	stop
end
