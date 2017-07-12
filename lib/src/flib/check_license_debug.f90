subroutine check_license_debug(info)
	use cipher, only: decrypt_information, key
	implicit none
	!--------------------------------------------------------------------
	integer                      :: fid
	integer       , intent(out)  :: info
	!--------------------------------------------------------------------
	integer                      :: stat
	integer                      :: fs
	integer                      :: n
	integer                      :: nline
	integer                      :: i, j, iline
	integer                      :: nmac_lic
	integer                      :: nmac_loc
	integer                      :: nset
	integer                      :: date_rel_tmp
	integer                      :: date_mnt_tmp
	integer                      :: date_loc_tmp
	integer        , allocatable :: code(:)
	character(   1), allocatable :: str  (:)
	character(  50), allocatable :: mac_lic(:)
	character(  50), allocatable :: mac_loc(:)
	character(5000), allocatable :: lines(:)
	character(  20)              :: date_rel
	character(  20)              :: date_mnt
	character(   8)              :: date_loc
	character(5000)              :: line
	character(5000)              :: product_version
	character(5000)              :: mac_address
	character(5000)              :: cmd, tmp
	integer        , external    :: number_of_lines
	logical                      :: success
	!--------------------------------------------------------------------
	character(5000)              :: momap_license
	!--------------------------------------------------------------------
	success = .false.
	info    = 0
	!--------------------------------------------------------------------
	call get_free_fid(fid)
	!--------------------------------------------------------------------
	call getenv("MOMAP_LICENSE", momap_license)
	open(96, file="/export/home/ylniu/mm.dat")
	write(96,*) "MOMAP_LICENSE"
	write(96,*) trim(momap_license)
	write(96,*) len_trim(adjustl(momap_license))
	close(96)
	!--------------------------------------------------------------------
	if ( len_trim(adjustl(momap_license)) == 0 ) then
		write(*,'("Please set environment variable MOMAP_LICENSE: ")')
		write(*,'("export MOMAP_LICENSE= the_full_path_of_MOMAP_license_file ")')
		info=1
		return
	end if
	!--------------------------------------------------------------------
	open(fid, file=momap_license, status="old", iostat=stat)
	close(fid)
	!--------------------------------------------------------------------
	if (stat/=0) then
		write(*,'("Can not open license file!")')
		write(*,'("Please check:")')
		write(*,'(a)') trim(momap_license)
		info=1
		return
	end if
	!--------------------------------------------------------------------
	inquire(file=momap_license, size=fs)
	if (fs==0) then
		write(*,'("License file is empty!")')
		write(*,'("Please check:")')
		write(*,'(a)') trim(momap_license)
		info=1
		return
	end if
	!--------------------------------------------------------------------
	n = fs / 8
	allocate(code(n))
	allocate(str (n))
	!--------------------------------------------------------------------
	open(fid, file=momap_license, status="old", iostat=stat)
		do i=1, n
			read(fid,'(z8,$)') code(i)
		end do
	close(fid)
	!--------------------------------------------------------------------
	call decrypt_information(code,key,n)
	!--------------------------------------------------------------------
	nline=0
	do i=1, n
		if (code(i)==10) nline=nline+1
	end do
	if (code(n)/=10) nline=nline+1
	!--------------------------------------------------------------------
	allocate(lines(nline))
	!--------------------------------------------------------------------
	j=0
	lines=''
	line=''
	iline=1
	do i=1, n
		str(i) = char(code(i))
		if (code(i)==10) then
			j=0
			iline=iline+1
		else
			j=j+1
			lines(iline)(j:j) = char(code(i))
		end if
	end do
	!--------------------------------------------------------------------
	call parse_variables(n,str,"licenseInput.product_version",product_version)
	open(97, file="/export/home/ylniu/tt.dat")
	write(97,*) "kkk:", trim(momap_license)
	write(97,*) str
	write(97,*) "kkk", product_version
	call parse_variables(n,str,"licenseInput.computer_MAC",mac_address)
	write(97,*) str
	write(97,*) "ggg", mac_address
	!--------------------------------------------------------------------
	mac_address= trim(adjustl(mac_address))
	nmac_lic=0
	do i=1, len_trim(mac_address)
		if (mac_address(i:i)==",") nmac_lic = nmac_lic + 1
	end do
	nmac_lic = nmac_lic + 1
	!--------------------------------------------------------------------
	allocate(mac_lic(nmac_lic))
	!--------------------------------------------------------------------
	do i=1, len_trim(mac_address)
		if (mac_address(i:i)=="," .or. mac_address(i:i)=="}" &
			.or. mac_address(i:i)=="{" .or. mac_address(i:i)=="'") &
			mac_address(i:i) = " "
	end do
	read(mac_address,*) mac_lic
	!--------------------------------------------------------------------
	cmd="/sbin/ifconfig -a | grep HWaddr > .mac.dat"
	call system(cmd)
	open(fid, file=".mac.dat", status="old")
		nmac_loc=number_of_lines(fid)
		rewind(fid)
		allocate(mac_loc(nmac_loc))
		do i=1, nmac_loc
			read(fid,*) (tmp, j=1, 4), mac_loc(i)
		end do
	close(fid, status="delete")
	call n_set_intersection(nmac_lic, mac_lic, nmac_loc, mac_loc, nset)
	write(97,*) "ttt", mac_loc
	write(97,*) "nset=", nset
	close(97)
	if (nset>0)  then 
		success = .true.
	else 
		write(*,'("License is invalid due to mismatch machine information.")')
		write(*,'("Program teminates abnormally.")')
		info=1
		return
	end if
	!--------------------------------------------------------------------
	call parse_variables(n,str,"licenseInput.license_releaseDate",date_rel)
	call parse_variables(n,str,"licenseInput.license_maintenanceDate",date_mnt)
	do i=1, len_trim(date_rel)
		if (date_rel(i:i)=="'") date_rel(i:i) = " "
	end do
	do i=1, len_trim(date_mnt)
		if (date_mnt(i:i)=="'") date_mnt(i:i) = " "
	end do
	date_rel= trim(adjustl(date_rel))
	date_mnt= trim(adjustl(date_mnt))
	!--------------------------------------------------------------------
   call date_and_time(DATE=date_loc)
	!--------------------------------------------------------------------
   read(date_rel,*) date_rel_tmp
   read(date_mnt,*) date_mnt_tmp
   read(date_loc,*) date_loc_tmp
	!--------------------------------------------------------------------
	if ((date_loc_tmp >= date_rel_tmp).and.(date_loc_tmp <= date_mnt_tmp)) then
		success = .true.
	else 
		write(*,'("License is invalid due to expired date information.")')
		write(*,'("Program teminates abnormally.")')
		info=1
		return
   end if
	!--------------------------------------------------------------------
	deallocate(mac_lic  )
	deallocate(str      )
	deallocate(code     )
	deallocate(lines    )
	!--------------------------------------------------------------------
	return
end subroutine
