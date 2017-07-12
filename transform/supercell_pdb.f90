program supercell_pdb
	use kinds, only: DP
	use file , only: name_main
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid1, fid2, na, nb, nc, natom, nconnect
	integer                     :: fid3, fid4, fid5
	integer                     :: i, j, k, ia, ib, ic, iat, iconn
	real(DP)                    :: aa1(3,3), a1, b1, c1, alpha, beta, gamma
	real(DP)                    :: aa2(3,3), a2, b2, c2
	real(DP)                    :: bb1(3,3)
	real(DP)                    :: bb2(3,3)
	real(DP)                    :: r(3)
	integer       , allocatable :: conn0(:,:)
	integer       , allocatable :: conn(:,:), cnum(:)
	real(DP)      , allocatable :: x0(:,:)
	real(DP)      , allocatable :: x(:,:)
	character(  5), allocatable :: symbol(:)
	character(  3), allocatable :: res(:)
	character(  2), allocatable :: posfix(:)
	character( 50)              :: fmt
	character(200)              :: fsuper, fpdb, fcoord, fconnect, cmd, fxyz
	character(200)              :: line, tmp, tmp1, tmp2
	!----------------------------------------------------------------------------
	logical       , external    :: search_word_free
	integer       , external    :: number_of_lines_word
	integer       , external    :: number_of_words
	!----------------------------------------------------------------------------
	fid1 = 1
	fid2 = 2
	fid3 = 3
	fid4 = 4
	fid5 = 7
	fcoord   = ".tmp.xyz"
	fconnect = ".tmp.connect"
	!----------------------------------------------------------------------------
	call getarg(1, fpdb)
	call getarg(2, tmp)
	read(tmp,*) na
	call getarg(3, tmp)
	read(tmp,*) nb
	call getarg(4, tmp)
	read(tmp,*) nc
	!----------------------------------------------------------------------------
	write(fsuper,'(a,3("-",i0),".pdb")') trim(name_main(fpdb)),na,nb,nc
	write(fxyz,'(a,3("-",i0),".xyz")') trim(name_main(fpdb)),na,nb,nc
	!----------------------------------------------------------------------------
	open(fid1, file=fpdb, status="old")
		natom = number_of_lines_word(fid1, "ATOM")
		rewind(fid1)
		nconnect = number_of_lines_word(fid1, "CONECT")
	close(fid1)
	!----------------------------------------------------------------------------
	allocate(x0    (3,    natom))
	allocate(x     (3,    natom))
	allocate(symbol(      natom))
	allocate(res   (      natom))
	allocate(posfix(      natom))
	!----------------------------------------------------------------------------
	allocate(conn0 (4, nconnect))
	allocate(conn  (4, nconnect))
	allocate(cnum  (   nconnect))
	!----------------------------------------------------------------------------
	open(fid2, file=fsuper)
	open(fid1, file=fpdb, status="old")
		!-------------------------------------------------------------------------
		do i=1, 2
			read (fid1,'(a)') line
			write(fid2,'(a)') trim(line)
		end do
		!-------------------------------------------------------------------------
		read (fid1,*) tmp1, a1, b1, c1, alpha, beta, gamma, tmp2
		!-------------------------------------------------------------------------
		a2 = a1 * na
		b2 = b1 * nb
		c2 = c1 * nc
		!-------------------------------------------------------------------------
		call lattice_constants_to_a(aa1, a1, b1, c1, alpha, beta, gamma)
		call lattice_constants_to_a(aa2, a2, b2, c2, alpha, beta, gamma)
		!-------------------------------------------------------------------------
		call a2b(aa1, bb1)
		call a2b(aa2, bb2)
		!-------------------------------------------------------------------------
		write(fid2,'("CRYST1", 3f9.3, 3f7.2," P2")') a2, b2, c2, alpha, beta, gamma
		!-------------------------------------------------------------------------
		do i=1, 3
			read (fid1,'(a)') line
			write(fid2,'(a)') trim(line)
		end do
		!-------------------------------------------------------------------------
		do i=1, 3
			read (fid1,'(a)') line
			write(fid2,'("SCALE",i0,4x,3f10.6,5x,f10.5)') i, bb2(:,i), 0.D0
		end do
		!-------------------------------------------------------------------------
		do i=1, natom
			read(fid1,*) tmp, tmp, symbol(i), res(i), tmp, x0(:,i), tmp, tmp, posfix(i)
		end do
		read(fid1,*)
		do i=1, nconnect
			read(fid1, '(a)') line
			cnum(i)=number_of_words(line) - 1
			read(line,*) tmp, (conn0(j,i), j=1, cnum(i))
		end do
		!-------------------------------------------------------------------------
		open(fid3, file=fcoord)
		open(fid4, file=fconnect)
		open(fid5, file=fxyz)
			write(fid5,'(i0)') natom * na * nb * nc
			write(fid5,'("Title")')
			j=0
			k=0
			do ia=1, na
				do ib=1, nb
					do ic=1, nc
						j=j+1
						r = (ia-1)*aa1(:,1) + (ib-1)*aa1(:,2) + (ic-1)*aa1(:,3)
						x = x0
						call disp (natom,r,x)
						!-------------------------------------------------------------
						k=0
						do iat=1, natom
							k=k+1
							write(fid3, '("ATOM", i7, a5, x, a3, i6, 4x, 3f8.3, 2f6.2, 10x, a2)') &
								& k, adjustr(symbol(iat)), res(iat), j, x(:,iat), &
								& 1.D0, 0.D0, adjustr(posfix(iat))
							write(fid5, '(a2,4x, 3f15.7)') adjustl(posfix(iat)), x(:,iat)
						end do
						!-------------------------------------------------------------
						conn=conn0+(j-1)*natom
						do iconn=1, nconnect
							write(fmt,*) cnum(iconn)
							write(fid4, '("CONECT",'//trim(fmt)//'i5)') (conn(k, iconn), k=1, cnum(iconn))
						end do
						!-------------------------------------------------------------
					end do
				end do
			end do
			write(fid3,'("TER",i0)') j+1
			write(fid4,'("END")')
		close(fid3)
		close(fid4)
		close(fid5)
		!-------------------------------------------------------------------------
		cmd="cat "//trim(fcoord)//" >> "//trim(fsuper)
		call system(cmd)
		cmd="cat "//trim(fconnect)//" >> "//trim(fsuper)
		call system(cmd)
		cmd="rm -r "//trim(fcoord)//" "//trim(fconnect)
		call system(cmd)
		!-------------------------------------------------------------------------
	close(fid1)
	close(fid2)
	!----------------------------------------------------------------------------
	write(*,'(a, " -> ", a, 2x, a)') trim(fpdb), trim(fsuper), trim(fxyz)
	!----------------------------------------------------------------------------
	deallocate(x0    )
	deallocate(x     )
	deallocate(symbol)
	deallocate(res   )
	deallocate(posfix)
	deallocate(conn0 )
	deallocate(conn  )
	deallocate(cnum  )
	!----------------------------------------------------------------------------
	stop
end
