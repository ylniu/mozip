subroutine write_gjf(fgjf, natom, symbol, x)
	use kinds, only: DP
	use file,  only: name_main
	implicit none
	integer        :: natom
	real(DP)       :: x   (3,natom)
	character(  *) :: symbol(natom), fgjf
	integer        :: fid, i
	character(200) :: fchk, route
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	fchk=trim(name_main(fgjf))//".chk"
	open(fid, file=fgjf)
		write(fid,'(a)') "%chk="//trim(fchk)
		write(fid,'("%mem=1GB")')
		write(fid,'("%nprocs=1")')
		write(fid,*)
		route="#p b3lyp/6-31G"
		route="#p opt cam-b3lyp/6-31g(d,p)/auto scrf=(pcm,solvent=Toluene) EmpiricalDispersion=GD3BJ"
		write(fid,'(a)') trim(route)
		write(fid,*)
		write(fid,'(a)') trim(name_main(fgjf))
		write(fid,*)
		write(fid,'("0 1")')
		do i=1, natom
			write(fid,'(a3, 3f17.8)') symbol(i), x(:,i)
		end do
		write(fid,*)
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
