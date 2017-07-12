program polar_inert
	use kinds, only: DP
	use file,  only: name_main
	use param, only: au2a
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: natom, info, fid
	integer                     :: i, j, n3
	real(DP)                    :: polar(3,3), eval(3), vector(3,3), x0(3), masst
	real(DP)                    :: vmax
	real(DP)      , allocatable :: x(:,:), mass(:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: flog, fxyz, tmp, line
	logical                     :: scanok
	logical       , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid=1
	!----------------------------------------------------------------------------
	call getarg(1, flog)
	call qm_file_natom(flog, natom, info)
	!----------------------------------------------------------------------------
	n3=natom*3
	!----------------------------------------------------------------------------
	allocate(x     (3,natom))
	allocate(mass  (  natom))
	allocate(symbol(  natom))
	!----------------------------------------------------------------------------
	call qm_file_coord (flog, natom, x     , info)
	call qm_file_mass  (flog, natom, mass  , info)
	call qm_file_symbol(flog, natom, symbol, info)
	!----------------------------------------------------------------------------
	x0    = 0.D0
	masst = 0.D0
	do i=1, natom
		do j=1, 3
			x0(j) = x0(j) + mass(i) * x(j,i)
		end do
		masst = masst + mass(i)
	end do
	x0 = x0 / masst
	!----------------------------------------------------------------------------
	write(*,*)
	open(fid, file=flog, status="old")
		if ( search_word_free(fid,"SCF Polarizability", line) ) then
			write(*,'(a)') "Find 'SCF Polarizability'"
			read(fid,*) tmp
			read(fid,*) tmp, polar(1,1)
			read(fid,*) tmp, polar(2,1), polar(2,2)
			read(fid,*) tmp, polar(3,1), polar(3,2), polar(3,3)
		else
			rewind(fid)
			if ( search_word_free(fid,"Exact polarizability",line)) then
				write(*,'(a)') "Find 'Exact polarizability'"
				read(line,'(23x, 6f8.3)') polar(1,1), polar(2,1), polar(2,2), polar(3,1), polar(3,2), polar(3,3)
			else
				stop "Can not find 'SCF Polarizability' and 'Exact polarizability'"
			end if
		end if
		polar(1,2) = polar(2,1)
		polar(1,3) = polar(3,1)
		polar(2,3) = polar(3,2)
	close(fid)
	!----------------------------------------------------------------------------
	write(*,'("------------------------------------------------------------------------")')
	write(*,'("Polarizability matrix : ", 3f15.7)')
	write(*,'(24x, 3f15.7)') polar
	write(*,'("------------------------------------------------------------------------")')
	!----------------------------------------------------------------------------
	call diag_symm (3,polar,eval,vector,info)
	write(*,'("Eigen values          : ", 3f15.7)') eval
	write(*,'("------------------------------------------------------------------------")')
	write(*,'("Eigen vectors         : ", 3i15  )') 1, 2, 3
	write(*,'("------------------------------------------------------------------------")')
	do i=1, 3
		write(*,'(24x, 3f15.7)') (vector(i,j), j=1, 3)
	end do
	vmax=1.D-99
	do i=1, 3
		if (vmax < eval(i)) vmax = eval(i)
	end do
	do i=1, 3
		vector(:,i) = vector(:,i) * eval(i) / vmax * 10.D0
	end do
	!----------------------------------------------------------------------------
	fxyz=trim(name_main(flog))//".xyz"
	!----------------------------------------------------------------------------
	x  = x  * au2a
	x0 = x0 * au2a
	open(fid,file=fxyz,status="unknown")
		write(fid,'(i4)') natom+3
		write(fid,'("polar:", 3f12.5)') eval
		do j=1,natom
			write(fid,'(a4,3f16.10)') symbol(j),(x(i,j),i=1,3)
		end do
		write(fid,'(a3,x,3f16.10,3f12.6)') "X", x0, vector(:,1)
		write(fid,'(a3,x,3f16.10,3f12.6)') "X", x0, vector(:,2)
		write(fid,'(a3,x,3f16.10,3f12.6)') "X", x0, vector(:,3)
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(mass  )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
