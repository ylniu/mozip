subroutine readtip()
	use module_afm, only: ftip, tip, iout
	use module_tip, only: allocate_tip,  allocate_tip_type
	use string    , only: StrLowCase, StrUpCase
	use array     , only: array_group_number, array_group
	use param     , only: au2a, MassAmu
	implicit none
	!------------------------------------------------------------------------
	integer           :: fid, ios, nw, i, j
	character(200)    :: line, tmp
	integer, external :: number_of_words
	!------------------------------------------------------------------------
	call get_free_fid(fid)
	!------------------------------------------------------------------------
	open(fid, file=ftip, status="old")
		read(fid, '(a)', iostat=ios) line
		tip%n = 0
		do while(ios==0)
			nw=number_of_words(line)
			read(line,*) tmp
			tmp=StrLowCase(tmp)
			if(nw>=4 .and. trim(tmp)/="center") tip%n = tip%n + 1
			read(fid, '(a)', iostat=ios) line
		end do
		!---------------------------------------------------------------------
		call allocate_tip(tip)
		!---------------------------------------------------------------------
		rewind(fid)
		read(fid, '(a)', iostat=ios) line
		i=0
		do while(ios==0)
			nw=number_of_words(line)
			read(line,*) tmp
			tmp=StrLowCase(tmp)
			if(nw>=4 .and. trim(tmp)/="center") Then
				i=i+1
				read(line, *) tip%symbol(i), tip%x(:,i), tip%charge(i), tip%opt(i), tip%opt_sum(i)
				tip%opt    (i) = StrUpCase(tip%opt    (i))
				tip%opt_sum(i) = StrUpCase(tip%opt_sum(i))
			else if (trim(tmp)=="center") then
				read(line, *) tmp, tip%center
			end if
			read(fid, '(a)', iostat=ios) line
		end do
	close(fid)
	!------------------------------------------------------------------------
	tip%x      = tip%x / au2a
	tip%center = tip%center / au2a
	!------------------------------------------------------------------------
	do i=1, tip%n
		tip%x(:,i) = tip%x(:,i) - tip%center
	end do
	!------------------------------------------------------------------------
	call symbol_to_nat(tip%n, tip%symbol, tip%nat )
	call nat_to_mass  (tip%n, tip%nat   , tip%mass)
	tip%mass = tip%mass * MassAmu
	do i=1, tip%n
		do j=1, 3
			tip%m3(j,i) = tip%mass(i)
		end do
	end do
	tip%type_n = array_group_number(tip%n, tip%nat)
	call allocate_tip_type(tip)
	call array_group(tip%n, tip%nat, tip%type_n, tip%idx_n, tip%idx_nat)
	do i=1, tip%n
		do j=1, tip%type_n
			if ( tip%nat(i) == tip%idx_nat(j) ) then
				tip%idx(i) = j
				exit
			end if
		end do
	end do
	!------------------------------------------------------------------------
	call nat_to_symbol(tip%type_n, tip%idx_nat, tip%idx_sym)
	!------------------------------------------------------------------------
	write(iout, '(/,2x,"Reading tip xyz ......",/)')
	do i=1, tip%n
		write(iout,'(2x, a2, 6x, 3f18.7)') tip%symbol(i), tip%x(:,i)
	end do
	write(iout,'(2x, a6, 2x, 3f18.7)') "Center", tip%center
	write(iout, '(/,2x,"In group ......",/)')
	do i=1, tip%type_n
		write(iout,'(2x, a2, i4)') tip%idx_sym(i), tip%idx_nat(i)
	end do
	!------------------------------------------------------------------------
	return
end subroutine
