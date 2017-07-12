program read_ao_info
	use kinds, only: DP
	use file , only: name_main
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: nbas
	integer                     :: ib, ie, jb
	integer                     :: i, j, k, l, im
	character(200)              :: fname, tmp, line, fout
	real(DP)      , allocatable :: s(:,:)
	real(DP)      , allocatable :: mo(:,:)
	real(DP)      , allocatable :: dm(:,:)
	real(DP)      , allocatable :: norm(:,:)
	real(DP)      , allocatable :: dm_cal(:,:)
	!----------------------------------------------------------------------------
	logical                     :: scanok
	logical       , external    :: search_word_free
	!----------------------------------------------------------------------------
	fid = 1
	!----------------------------------------------------------------------------
	call getarg(1, fname)
	fout=trim(name_main(fname))//".out"
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
		!-------------------------------------------------------------------------
		scanok=search_word_free(fid, "NBasis", line)
		read(line,*) tmp, tmp, nbas
		!-------------------------------------------------------------------------
		allocate(s (nbas, nbas))
		allocate(mo(nbas, nbas))
		allocate(dm(nbas, nbas))
		allocate(norm(nbas, nbas))
		allocate(dm_cal(nbas, nbas))
		!-------------------------------------------------------------------------
		! read MO
		!
		rewind(fid)
		scanok=search_word_free(fid, "Molecular Orbital Coefficients", line)
		ib = 1
		ie = min(ib+4,nbas)
		do while (ib<=nbas)
			read(fid,*)
			read(fid,*)
			read(fid,*)
			do i=1, nbas
				read(fid,'(a)') line
				line=line(18:)
				read(line,*) (mo(i,im), im=ib, ie)
			end do
			ib = ib + 5
			ie = min(ib+4,nbas)
		end do
		!-------------------------------------------------------------------------
		!-------------------------------------------------------------------------
		rewind(fid)
		scanok=search_word_free(fid, "*** Overlap ***", line)
		ib = 1
		ie = min(ib+4,nbas)
		do while (ib<=nbas)
			read(fid,'(a)') line
			do i=ib, nbas
				jb = min(i, ie)
				read(fid,'(a)') line
				line=line(8:)
				read(line,*) (s(i,im), im=ib, jb)
			end do
			ib = ib + 5
			ie = min(ib+4,nbas)
		end do
		do i=1, nbas-1
			do j=i, nbas
				s(i,j) = s(j,i)
			end do
		end do
		!-------------------------------------------------------------------------
	close(fid)
	!----------------------------------------------------------------------------
	norm=0.D0
	do k=1, nbas
		do l=1, nbas
			do i=1, nbas
				do j=1, nbas
					norm(k,l) = norm(k,l) + mo(i,k) * mo(j,l) * s(i,j)
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	dm_cal=0.D0
	do k=1, nbas
		do l=1, nbas
			do i=1, nbas
				do j=1, nbas
					dm_cal(k,l) = dm_cal(k,l) + mo(i,k) * mo(j,l)
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	open(fid, file=fout)
		write(fid,'(2x,"Normlization")')
		do i=1, nbas
			write(fid,'(2x,"vector", i10)') i
			write(fid,'(10f15.7)') norm(:,i)
		end do
		write(fid,'(2x,"Density Matrix")')
		do i=1, nbas
			write(fid,'(2x,"vector", i10)') i
			write(fid,'(10f15.7)') dm_cal(:,i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(s )
	deallocate(mo)
	deallocate(dm)
	deallocate(norm)
	deallocate(dm_cal)
	!----------------------------------------------------------------------------
	stop
end
