subroutine qm_file_basis(fname, norb, ene_a, ene_b, sym_a, sym_b, coeff_a, coeff_b, iat, isymbol, bas_name, info)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: norb
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	integer     , intent(out) :: iat(norb)
	real(DP)    , intent(out) :: ene_a(norb)
	real(DP)    , intent(out) :: ene_b(norb)
	real(DP)    , intent(out) :: coeff_a(norb, norb)
	real(DP)    , intent(out) :: coeff_b(norb, norb)
	character(*), intent(out) :: sym_a(norb)
	character(*), intent(out) :: sym_b(norb)
	character(*), intent(out) :: isymbol(norb)
	character(*), intent(out) :: bas_name(norb)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, ib, ie, i, j
	character( 10)            :: alpha, nc, nsym, mc, msym
	character(200)            :: line, file_type, version, fmt
	logical                   :: scanok
	logical     , external    :: search_word_free
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	info = -1
	!----------------------------------------------------------------------------
	! Check if Turbomole freq file
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		!-------------------------------------------------------------------------
		case ("CHEMSHELL")
			!----------------------------------------------------------------------
			! CHEMSHELL
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
			if(search_word_free(fid,"Molecular Orbital Coefficients",line)) then
				!-------------------------------------------------------------------
				read(line,*) alpha
				!-------------------------------------------------------------------
				ib=1
				ie=min(norb,5)
				write(fmt,*) ie-ib+1
				do while (ib<=norb)
					scanok=search_word_free(fid,"Eigenvalues",line)
					read(line,'(21x,'//trim(fmt)//'f10.5)') ene_a(ib:ie)
					!----------------------------------------------------------------
					backspace(fid)
					backspace(fid)
					read(fid,'(21x,'//trim(fmt)//'a10)') sym_a(ib:ie)
					read(fid,*)
					!----------------------------------------------------------------
					if (ib==1) then
						do i=1, norb
							read(fid,'(4x, a5, a2, a10, '//trim(fmt)//'f10.5)') &
								nc, nsym, bas_name(i), (coeff_a(i,j), j=ib, ie)
							if (trim(nc)/="") then
								mc   = nc
								msym = nsym
							end if
							read(mc  ,*) iat(i)
							read(msym,*) isymbol(i)
						end do
					else
						do i=1, norb
							read(fid,'(21x, '//trim(fmt)//'f10.5)') &
								(coeff_a(i,j), j=ib, ie)
						end do
					end if
					!----------------------------------------------------------------
					ib=ib+5
					ie=min(ie+5, norb)
					write(fmt,*) ie-ib+1
				end do
				!-------------------------------------------------------------------
				if (trim(alpha)=="Alpha") then
					!----------------------------------------------------------------
					ib=1
					ie=min(norb,5)
					write(fmt,*) ie-ib+1
					do while (ib<=norb)
						scanok=search_word_free(fid,"Eigenvalues",line)
						read(line,'(21x,'//trim(fmt)//'f10.5)') ene_b(ib:ie)
						!-------------------------------------------------------------
						backspace(fid)
						backspace(fid)
						read(fid,'(21x,'//trim(fmt)//'a10)') sym_b(ib:ie)
						read(fid,*)
						!-------------------------------------------------------------
						do i=1, norb
							read(fid,'(21x, '//trim(fmt)//'f10.5)') &
								(coeff_b(i,j), j=ib, ie)
						end do
						!-------------------------------------------------------------
						ib=ib+5
						ie=min(ie+5, norb)
						write(fmt,*) ie-ib+1
					end do
					!----------------------------------------------------------------
				else
					ene_b   = ene_a
					sym_b   = sym_a
					coeff_b = coeff_a
				end if
				!-------------------------------------------------------------------
				close(fid)
				return
			end if
			!----------------------------------------------------------------------
		case ("GAUSSIAN FCHK")
			!----------------------------------------------------------------------
			! GAUSSIAN FCHK
			!----------------------------------------------------------------------
			!----------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			!scanok=search_word_del_space(fid,"type atoms prim cont basis",line)
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			!----------------------------------------------------------------------
		case ("CIF")
			!----------------------------------------------------------------------
			! CIF
			!----------------------------------------------------------------------
		case ("PWOUT")
			!----------------------------------------------------------------------
			! PWOUT
			!----------------------------------------------------------------------
		case default
	end select
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
