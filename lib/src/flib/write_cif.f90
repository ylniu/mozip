subroutine write_cif(fcif, natom, symbol, xin, aa, xtype)
	use kinds, only: DP
	use math,  only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent(in) :: natom
	real(DP)    , intent(in) :: xin(3,natom)
	real(DP)    , intent(in) :: aa(3,3)
	character(*), intent(in) :: fcif
	character(*), intent(in) :: symbol(natom)
	character(*), intent(in) :: xtype
	!----------------------------------------------------------------------------
	integer                  :: fid, la, lb, lc, lalpha, lbeta, lgamma
	integer                  :: date_time(8)
	character(10)            :: bb(3), date
	!----------------------------------------------------------------------------
	integer                  :: i
	real(DP)                 :: a, b, c, alpha, beta, gamma
	real(DP)                 :: inva(3,3)
	real(DP)    , allocatable:: x(:,:)
	character(8)             :: ca, cb, cc, calpha, cbeta, cgamma
	character(80)            :: fmt, tmp
	character(12)            :: symbol_num(natom)
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	inva=inverse3(aa)
	allocate(x(3,natom))
	x=xin
	if (trim(xtype)/="CRYSTAL") call rotn(natom, inva, x)
	!----------------------------------------------------------------------------
	call date_and_time(bb(1), bb(2), bb(3), date_time)
	!----------------------------------------------------------------------------
	write(date ,'(i4.4,"-",i2.2,"-",i2.2)') date_time(1), date_time(2), date_time(3)
	do i=1, natom
		write(tmp,*) i
		symbol_num(i) = trim(adjustl(symbol(i)))//trim(adjustl(tmp))
	end do
	!----------------------------------------------------------------------------
	call a_to_lattice_constants(aa, a, b, c, alpha, beta, gamma)
	!----------------------------------------------------------------------------
	write(ca    ,'(f8.4)') a
	write(cb    ,'(f8.4)') b
	write(cc    ,'(f8.4)') c
	write(calpha,'(f8.4)') alpha
	write(cbeta ,'(f8.4)') beta
	write(cgamma,'(f8.4)') gamma
	!----------------------------------------------------------------------------
	la     = len(ca    )
	lb     = len(cb    )
	lc     = len(cc    )
	lalpha = len(calpha)
	lbeta  = len(cbeta )
	lgamma = len(cgamma)
	!----------------------------------------------------------------------------
	open(fid, file=fcif)
		write(fid,'("data_new")')
		write(fid,'("_audit_creation_date",14x,a10)') date
		write(fid,'("_audit_creation_method            ''pwscf2cif''")')
		write(fid,'("_symmetry_space_group_name_H-M    ''P1''")')
		write(fid,'("_symmetry_Int_Tables_number       1")')
		write(fid,'("_symmetry_cell_setting            triclinic")')
		write(fid,'("loop_")')
		write(fid,'("_symmetry_equiv_pos_as_xyz")')
		write(fid,'("  x,y,z")')
		write(fmt,*) la
		write(fid,'("_cell_length_a   ",17x,a'//trim(fmt)//')') ca
		write(fmt,*) lb
		write(fid,'("_cell_length_b   ",17x,a'//trim(fmt)//')') cb
		write(fmt,*) lc
		write(fid,'("_cell_length_c   ",17x,a'//trim(fmt)//')') cc
		write(fmt,*) lalpha
		write(fid,'("_cell_angle_alpha",17x,a'//trim(fmt)//')') calpha
		write(fmt,*) lbeta
		write(fid,'("_cell_angle_beta ",17x,a'//trim(fmt)//')') cbeta
		write(fmt,*) lgamma
		write(fid,'("_cell_angle_gamma",17x,a'//trim(fmt)//')') cgamma
		write(fid,'("loop_")')
		write(fid,'("_atom_site_label")')
		write(fid,'("_atom_site_type_symbol")')
		write(fid,'("_atom_site_fract_x")')
		write(fid,'("_atom_site_fract_y")')
		write(fid,'("_atom_site_fract_z")')
		write(fid,'("_atom_site_U_iso_or_equiv")')
		write(fid,'("_atom_site_adp_type")')
		write(fid,'("_atom_site_occupancy")')
		do i=1, natom
			write(fid,'(a8,a2,3f10.5,"   0.00000  Uiso   1.00")') &
				adjustl(symbol_num(i)), adjustl(symbol(i)), x(:,i)
		end do
	close(fid)
	deallocate(x)
	!----------------------------------------------------------------------------
	return
end subroutine
