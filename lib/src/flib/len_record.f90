subroutine len_record(fname, nw)
	!----------------------------------------------------------------------------
	! This subroutine returns the length of the record in the file
	!----------------------------------------------------------------------------
	implicit none
	character(*), intent( in) :: fname
	integer     , intent(out) :: nw
	!
	integer                   :: fid, i, stat, irecl
	character(4)              :: c
	logical                   :: find
	!----------------------------------------------------------------------------
	fid=1
	!
	open(fid,file=fname,status="old",form="unformatted",recl=1,access="direct")
		read(fid,rec=1,iostat=stat) c
		if (stat/=0) then
			! gfortran
			irecl=4
		else if (stat==0) then
			! ifort
			irecl=1
		end if
	close(fid)
	!----------------------------------------------------------------------------
	open(fid,file=fname,status="old",form="unformatted",recl=irecl,access="direct")
	nw = 0
	i  = 1
	find = .false.
	do while ( .not. find)
		read(fid,rec=i) c
		if      (iachar(c(1:1))==10) then
			find = .true.
			exit
		else if (iachar(c(2:2))==10) then
			find = .true.
			nw=nw+1
			exit
		else if (iachar(c(3:3))==10) then
			find = .true.
			nw=nw+2
			exit
		else if (iachar(c(4:4))==10) then
			find = .true.
			nw=nw+3
			exit
		end if
		i=i+1
		nw=nw+4
	end do
	close(fid)
	!----------------------------------------------------------------------------
	return
end subroutine
