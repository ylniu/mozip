subroutine loadAtomscolor()
	use module_afm, only: iout, fcolor, ncolor, icolor, cymbol, bas
	use string, only: StrLowCase
	implicit none
	!----------------------------------------------------------------------------
	integer        :: fid, i, j
	character(200) :: w1, w2
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fcolor, status="old")
		read(fid,*) ncolor
		if (.not. allocated(icolor)) allocate( icolor(ncolor) )
		if (.not. allocated(cymbol)) allocate( cymbol(ncolor) )
		do i=1, ncolor
			read(fid,*) cymbol(i), icolor(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	bas%acolor="black"
	do i=1, ncolor
		w1=adjustl(StrLowCase(cymbol(i)))
		do j=1, bas%n
			w2=adjustl(StrLowCase(bas%symbol(j)))
			if (trim(w1)==trim(w2)) then
				bas%acolor(j) = icolor(i)
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	write(iout,*)
	write(iout,'(2x,"Origin coordinates")')
	write(iout,'(2x,"Number of atoms : ", i10)') bas%n
	write(iout,*)
	do j=1, bas%n
		write(iout,'(2x,a2,6x,3f18.7, a10)') &
			trim(bas%symbol(j)), bas%x(:,j), trim(bas%acolor(j))
	end do
	write(iout,*)
	!----------------------------------------------------------------------------
	return
end subroutine
