subroutine check_license(info)
	use cipher, only: decrypt_information, key
	use string, only: StrLowCase
	use file  , only: get_pid
	implicit none
	!--------------------------------------------------------------------
	integer                       :: fid
	integer        , intent(out)  :: info
	!--------------------------------------------------------------------
	integer                       :: stat
	integer                       :: pid
	integer                       :: fs
	integer                       :: n
	integer                       :: nline
	integer                       :: i, j, iline
	integer                       :: nmac_lic
	integer                       :: nmac_loc
	integer                       :: nset
	integer                       :: date_rel_tmp
	integer                       :: date_mnt_tmp
	integer                       :: date_loc_tmp
	integer         , allocatable :: code(:)
	character(    1), allocatable :: str  (:)
	character(   50), allocatable :: mac_lic(:)
	character(   50), allocatable :: mac_loc(:)
	character( 5000), allocatable :: lines(:)
	character(   20)              :: date_rel
	character(   20)              :: date_mnt
	character(    8)              :: date_loc
	character(50000)              :: line
	character(  200)              :: fmac
	character(  200)              :: fmac_local
	character(  200)              :: fmac_remote
	character(  200)              :: fmac_master
	character(  200)              :: product_version
	character(  200)              :: license_type
	character(  500)              :: license_node
	character( 5000)              :: mac_address
	character(  200)              :: cmd
	character(   17)              :: this_mac
	character(   17)              :: that_mac
	integer         , external    :: number_of_lines
	logical                       :: success
	logical                       :: is_license_node
	!--------------------------------------------------------------------
	character( 5000)              :: momap_license
	!--------------------------------------------------------------------
	is_license_node = .false.
	success = .false.
	info    = 0
	!--------------------------------------------------------------------
	call get_free_fid(fid)
	!--------------------------------------------------------------------
	call getenv("MOMAP_LICENSE", momap_license)
	!--------------------------------------------------------------------
	if ( len_trim(adjustl(momap_license)) == 0 ) then
		write(*,'("Please set environment variable MOMAP_LICENSE: ")')
		write(*,'("export MOMAP_LICENSE= the_full_path_of_MOMAP_license_file ")')
		info=1
		return
	end if
	!--------------------------------------------------------------------
	open(fid, file=momap_license, status="old", iostat=stat)
		read(fid, '(a)', iostat=stat) line
	close(fid)
	!--------------------------------------------------------------------
	if (stat/=0) then
		write(*,'("Can not open license file!")')
		write(*,'("Please check:")')
		write(*,'(a, i10)') trim(momap_license), stat
		write(*,*) trim(line)
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
			read(fid,'(z8)',advance='no') code(i)
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
	call parse_variables(n,str,"licenseInput.computer_MAC",mac_address)
	call parse_variables(n,str,"licenseInput.license_type",license_type)
	read(license_type,*) license_type
	license_type=StrLowCase(license_type)
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
	do i=1, nmac_lic
		mac_lic(i)=StrLowCase(mac_lic(i))
	end do
	!--------------------------------------------------------------------
	! Read mac address with ifconfig command
	!
	pid=get_pid()
	call get_first_mac(this_mac)
	call str_replace_same(":", "-", this_mac, info)
	!--------------------------------------------------------------------
	write(fmac_local , '(".mac.loc.",a,"-",i0,".dat")') trim(this_mac), pid
	write(fmac_master, '(".mac.mas.",a,"-",i0,".dat")') trim(this_mac), pid
	fmac_remote="/tmp/"//trim(fmac_master)
	!--------------------------------------------------------------------
	if ( trim(license_type) == 'single' ) then
		!----------------------------------------------------------------
		! license_type = "single", Get Mac address from localhost
		!
		fmac=fmac_local
#ifdef WIN 
		cmd="C:\Windows\System32\ipconfig /all > "//trim(fmac_local)
#else
		cmd="/sbin/ifconfig -a 2> /dev/null > "//trim(fmac_local)
#endif
		call execute_command_line(cmd, exitstat=i)
		is_license_node = .true.
	!-------------------------------------------------------------------
	else if ( trim(license_type) == 'network' ) then
	!-------------------------------------------------------------------
		fmac=fmac_master
		!----------------------------------------------------------------
#ifdef WIN 
		write(*,'(2x,"This platform is Windows, the type of license is network,")')
		write(*,'(2x,"which has not been developed! Stop!")')
		info = 1
		return
#endif
		!----------------------------------------------------------------
		! license_type = "network", Get Mac address from license node 
		!
		call getenv("MOMAP_LICENSE_NODE", license_node)
		if ( len_trim(adjustl(license_node)) == 0 ) then
			write(*,'("Your License type is ""Network""")')
			write(*,'("Please set environment variable MOMAP_LICENSE_NODE: ")')
			write(*,'("export MOMAP_LICENSE_NODE=''the_name_of_license_node''")')
			write(*,'("or")')
			write(*,'("export MOMAP_LICENSE_NODE=''the_IP_of_license_node''")')
			info=1
			return
		end if
		!----------------------------------------------------------------
		! debug
		!
		cmd="ssh "//trim(license_node)//" 'echo test' > /dev/null"
		call execute_command_line(cmd, exitstat=i)
		!-----------------------------------------------------------------
		if (i/=0) then
			write(*,'("Can not connect to ",a)') trim(license_node)
			write(*,'("Please set environment variable MOMAP_LICENSE_NODE correctly!")')
			info=1
			return
		end if
		!-----------------------------------------------------------------
		cmd="ssh "//trim(license_node)//" '/sbin/ifconfig -a 2> /dev/null > "//trim(fmac_remote)//"'"
		call execute_command_line(cmd)
		!
		cmd="scp "//trim(license_node)//":"//trim(fmac_remote)//" ./"//trim(fmac_master)//" > /dev/null"
		call execute_command_line(cmd)
		!
		call get_first_mac_file(fmac_master, that_mac)
		call str_replace_same(":", "-", that_mac, info)
		!
		if (trim(this_mac)==trim(that_mac)) is_license_node = .true.
		!
		cmd="ssh "//trim(license_node)//" 'rm -rf "//trim(fmac_remote)//"' > /dev/null"
		call execute_command_line(cmd)
	else
		write(*,'("License type should be ''Single'' or ''Network''!")')
		write(*,'("Please contact MOMAP development team to apply for a legal license file!")')
		info=1
		return
	end if
	!--------------------------------------------------------------------
	call get_mac_num(fmac, nmac_loc)
	!
	allocate(mac_loc(nmac_loc))
	call get_mac_array(fmac, nmac_loc, mac_loc)
	do i=1, nmac_loc
		mac_loc(i)=StrLowCase(mac_loc(i))
	end do
	!
	open(fid, file=fmac, status="old")
	close(fid, status="delete")
	!--------------------------------------------------------------------
	call n_set_intersection(nmac_lic, mac_lic, nmac_loc, mac_loc, nset)
	if (nset>0)  then 
		success = .true.
	else
		write(*,'("License is invalid due to mismatch machine information.")')
		write(*,'("Please contact MOMAP development team to apply for a legal license file!")')
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
	deallocate(mac_loc  )
	deallocate(mac_lic  )
	deallocate(str      )
	deallocate(code     )
	deallocate(lines    )
	!--------------------------------------------------------------------
	return
end subroutine
