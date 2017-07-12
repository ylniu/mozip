program ff2afm
	use kinds, only: DP
	use file , only: name_main
	use param, only: kJmol_to_eV
	!----------------------------------------------------------------------------
	integer                     :: fid, nline, i, j, ios
	character(  1)              :: c
	character(200)              :: finp, fout, line, tmp
	integer       , allocatable :: idx(:)
	integer       , allocatable :: nat(:)
	real(DP)      , allocatable :: natd(:)
	real(DP)      , allocatable :: mass(:)
	real(DP)      , allocatable :: charge(:)
	real(DP)      , allocatable :: sigma_nm(:)
	real(DP)      , allocatable :: epsil_kJmol(:)
	real(DP)      , allocatable :: epsil_eV(:)
	real(DP)      , allocatable :: rm_a(:)
	character(200), allocatable :: symbol(:)
	character(200), allocatable :: ptype(:)
	logical       , external    :: is_number
	!----------------------------------------------------------------------------
	call getarg(1,finp)
	!----------------------------------------------------------------------------
	fout  = trim(name_main(finp))//".aff"
	fid   = 1
	nline = 0
	i     = 0
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		!-------------------------------------------------------------------------
		read(fid, '(a)', iostat=ios) line
		do while(ios==0)
			line=adjustl(line)
			c=line(1:1)
			if (.not.(c==";" .or. c=="[" .or. c=="#")) then
				nline = nline + 1
			end if
			read(fid, '(a)', iostat=ios) line
		end do
		!-------------------------------------------------------------------------
		allocate( idx        (nline) )
		allocate( nat        (nline) )
		allocate( natd       (nline) )
		allocate( mass       (nline) )
		allocate( charge     (nline) )
		allocate( sigma_nm   (nline) )
		allocate( epsil_kJmol(nline) )
		allocate( epsil_eV   (nline) )
		allocate( rm_a       (nline) )
		allocate( symbol     (nline) )
		allocate( ptype      (nline) )
		!-------------------------------------------------------------------------
		rewind(fid)
		read(fid, '(a)', iostat=ios) line
		do while(ios==0)
			line=adjustl(line)
			c=line(1:1)
			if (.not.(c==";" .or. c=="[" .or. c=="#")) then
				i = i + 1
				line=trim(line)
				read(line,*) tmp, tmp
				c=tmp(1:1)
				if (is_number(c)) then
					read(line,*) symbol(i), nat(i), mass(i), charge(i), ptype(i), &
						sigma_nm(i), epsil_kJmol(i)
				else
					read(line,*) tmp, symbol(i), nat(i), mass(i), charge(i), ptype(i), &
						sigma_nm(i), epsil_kJmol(i)
				end if
				rm_a(i)     = sigma_nm(i) * 10.D0 * 2.D0**(1.D0/6.D0) / 2.D0
				epsil_eV(i) = epsil_kJmol(i) * kJmol_to_eV
				natd(i) = real(nat(i),DP)
			end if
			read(fid, '(a)', iostat=ios) line
		end do
	close(fid)
	!----------------------------------------------------------------------------
	call sort_array(nline, natd, idx, 1)
	!----------------------------------------------------------------------------
	open(fid, file=fout, status="unknown")
		write(fid,'(a, 3a12, a8, 2a14)') &
			"name", "at.num", "mass", "charge", "ptype", "Rm(A)", "epsilon(eV)"
		do j=1, nline
			i=idx(j)
			write(fid,'(a10,i6,f12.3,f12.3, a8, f14.4, f14.9)') &
				adjustl(symbol(i)), nat(i), mass(i), charge(i), trim(ptype(i)), &
				rm_a(i), epsil_eV(i)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate( idx         )
	deallocate( nat         )
	deallocate( natd        )
	deallocate( mass        )
	deallocate( charge      )
	deallocate( sigma_nm    )
	deallocate( epsil_kJmol )
	deallocate( epsil_eV    )
	deallocate( rm_a        )
	deallocate( symbol      )
	deallocate( ptype       )
	!----------------------------------------------------------------------------
	stop
end
