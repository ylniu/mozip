program gjf2cif
	use kinds, only: DP
	use file,  only: name_main
	implicit none
	!----------------------------------------------------------------------------
	integer                     :: fid, natom, i
	real(DP)                    :: a(3,3), a1(3), a2(3), a3(3), disp(3), ascale
	real(DP)      , allocatable :: x(:,:)
	character(  2), allocatable :: symbol(:)
	character(200)              :: finp, fgjf, fcif, xtype
	!----------------------------------------------------------------------------
	namelist /control/ fgjf, a1, a2, a3, ascale, disp
	!----------------------------------------------------------------------------
	fid    = 1
	ascale = 1.D0
	xtype  = ""
	call getarg(1,finp)
	open(fid, file=finp, status="old")
		read(fid,control)
	close(fid)
	!----------------------------------------------------------------------------
	a1 = a1 * ascale
	a2 = a2 * ascale
	a3 = a3 * ascale
	a(:,1) = a1
	a(:,2) = a2
	a(:,3) = a3
	!----------------------------------------------------------------------------
	call rotn(1, a, disp)
	!----------------------------------------------------------------------------
	call get_gjf_natom(fgjf, natom)
	allocate(x     (3,natom))
	allocate(symbol(  natom))
	call get_gjf_coord(fgjf, natom, x, symbol)
	do i=1, natom
		x(:,i) = x(:,i) + disp
	end do
	fcif=trim(name_main(fgjf))//".cif"
	call write_cif(fcif, natom, symbol, x, a, xtype)
	!----------------------------------------------------------------------------
	deallocate(x     )
	deallocate(symbol)
	!----------------------------------------------------------------------------
	stop
end
