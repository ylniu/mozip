subroutine get_gjf_coord_intx(fname, natom, x, symbol)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	character(*)   , intent( in) :: fname
	integer        , intent( in) :: natom
	real(DP)       , intent(out) :: x(3,natom)
	character(*)   , intent(out) :: symbol(natom)
	!----------------------------------------------------------------------------
	integer                      :: i, j, k, l, fid, fid_tmp, info
	integer                      :: nlines
	integer                      :: atom_begin
	integer                      :: atom_end
	!----------------------------------------------------------------------------
	integer                      :: nvar
	integer                      :: b_var_line, e_var_line
	real(DP)                     :: pi
	character(200)               :: ftmp_name
	!
	character( 50) , allocatable :: var_name (:)
	character(200) , allocatable :: zmat_tmp(:,:)
	character(200)               :: tmp_name
	character(200)               :: cmd
	!
	character( 50) , allocatable :: var_value(:)
	real(DP)       , allocatable :: zmat(:,:)
	!
	integer        , allocatable :: zrel(:,:)
	!----------------------------------------------------------------------------
	logical                      :: is_var
	integer        , external    :: find_int_crd
	integer        , external    :: number_of_lines
	logical        , external    :: is_number
	logical        , external    :: is_letter
	!----------------------------------------------------------------------------
	character(200)               :: tmp
	character(200) , allocatable :: lines(:)
	!----------------------------------------------------------------------------
	pi = acos(-1.D0)
	x  = 0.D0
	!----------------------------------------------------------------------------
	allocate(zrel     (3,natom))
	allocate(zmat_tmp (3,natom))
	allocate(zmat     (3,natom))
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	open(fid, file=fname, status="old")
		nlines=number_of_lines(fid)
		allocate(lines(nlines))
		rewind(fid)
		do i=1, nlines
			read(fid,'(a)') lines(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	! find atom_begin
	!
	do i=1, nlines
		tmp=trim(adjustl(lines(i)))
		if ( is_number(tmp(1:1)) .or. tmp(1:1)=="-" ) then
			atom_begin=i+1
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	! find atom_end
	!
	do i=atom_begin, nlines
		j=len(trim(adjustl(lines(i))))
		if (j==0) then
			atom_end = i-1
			exit
		end if
	end do
	!
	read(lines(atom_begin+1),*) tmp, tmp, tmp
	if (is_letter(tmp(1:1))) is_var=.true.
	!----------------------------------------------------------------------------
	if (.not.is_var) then
		write(*,*) "Not implemented!"
		write(*,*) "Stop!"
		stop
	end if
	!----------------------------------------------------------------------------
	b_var_line=atom_end+2
	!----------------------------------------------------------------------------
	!
	do i=b_var_line, nlines
		j=len(trim(adjustl(lines(i))))
		if (j==0) then
			e_var_line = i-1
			exit
		end if
	end do
	!----------------------------------------------------------------------------
	nvar = e_var_line - b_var_line + 1
	allocate(var_name (nvar))
	allocate(var_value(nvar))
	!----------------------------------------------------------------------------
	zmat_tmp=''
	zrel=0
	do i=atom_begin, atom_end
		j=i-atom_begin+1
		if (j==1) then
			read(lines(i),*) symbol(j)
		else if (j==2) then
			read(lines(i),*) symbol(j), zrel(1,j), zmat_tmp(1,j)
		else if (j==3) then
			read(lines(i),*) symbol(j), zrel(1,j), zmat_tmp(1,j), &
												 zrel(2,j), zmat_tmp(2,j)
		else
			read(lines(i),*) symbol(j), zrel(1,j), zmat_tmp(1,j), &
												 zrel(2,j), zmat_tmp(2,j), &
												 zrel(3,j), zmat_tmp(3,j)
		end if
	end do
	!----------------------------------------------------------------------------
	do i=b_var_line, e_var_line
		j=i-b_var_line+1
		read(lines(i),*) var_name(j), var_value(j)
	end do
	!----------------------------------------------------------------------------
	zmat=0.D0
	!----------------------------------------------------------------------------
	do j=2, natom
		do l=1, 3
			if ( (j==2.and.l==2) .or. (j==2.and.l==3) .or. (j==3.and.l==3) ) cycle
			do k=1, nvar
				!-------------------------------------------------------------------
				! replace B1, A1, D1 with the number
				!
				call str_replace(trim(var_name(k)), trim(var_value(k)), zmat_tmp(l,j), tmp_name, info)
				!-------------------------------------------------------------------
				! Calculate the internal coordinates, e.g.
				! -B1,  B2+B3, ...
				!
				if (info>0) then
					ftmp_name="."//trim(var_name(k))
					cmd="/bin/echo 'scale=8;"//trim(tmp_name)//"' | /usr/bin/bc -l > "//trim(ftmp_name)
					call system(cmd)
					call get_free_fid(fid_tmp)
					open(fid_tmp, file=ftmp_name, status="old")
						read(fid_tmp,*) tmp_name
					close(fid_tmp)
					cmd="/bin/rm -f "//trim(ftmp_name)
					call system(cmd)
					!-------------------------------------------------------------------
					! get the number of internal coordinates
					!
					read(tmp_name,*) zmat(l,j)
					!-------------------------------------------------------------------
				end if
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	! get internal coordinates
	!
	call zmat_to_xyz(natom, zrel, zmat, x)
	!----------------------------------------------------------------------------
	deallocate(zrel     )
	deallocate(zmat_tmp )
	deallocate(zmat     )
	deallocate(lines    )
	deallocate(var_name )
	deallocate(var_value)
	!----------------------------------------------------------------------------
	return
end subroutine
