subroutine readbas()
	use kinds     , only: DP
	use module_afm, only: fxyz, bas
	use module_bas, only: allocate_bas
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	integer           :: fid, ios, nw, i
	character(200)    :: line
	integer, external :: number_of_words
	!--------------------------------------------------------------------
	call get_free_fid(fid)
	!--------------------------------------------------------------------
	open(fid, file=fxyz, status="old")
		read(fid, '(a)', iostat=ios) line
		bas%n = 0
		do while(ios==0)
			nw=number_of_words(line)
			if(nw>=4) bas%n = bas%n + 1
			read(fid, '(a)', iostat=ios) line
		end do
		!-----------------------------------------------------------------
		call allocate_bas(bas)
		!-----------------------------------------------------------------
		bas%symbol = "X"
		bas%charge = 0.0_DP
		bas%x      = 0.0_DP
		!-----------------------------------------------------------------
		rewind(fid)
		i = 0
		read(fid, '(a)', iostat=ios) line
		do while(ios==0)
			nw=number_of_words(line)
			if(nw==4) then
				i = i + 1
				read(line,*) bas%symbol(i), bas%x(:,i)
			else if (nw==5) then
				i = i + 1
				read(line,*) bas%symbol(i), bas%x(:,i), bas%charge(i)
			end if
			read(fid, '(a)', iostat=ios) line
		end do
		!-----------------------------------------------------------------
	close(fid)
	!--------------------------------------------------------------------
	call symbol_to_nat(bas%n, bas%symbol, bas%nat)
	!--------------------------------------------------------------------
	return
end subroutine
