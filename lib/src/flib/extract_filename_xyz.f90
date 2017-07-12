subroutine extract_filename_xyz(fname, x)
	!----------------------------------------------------------------------------
	! input : fname = "coord-001.500+002.000-000.500.log"
	! output: x(1)  = -001.500
	! output: x(2)  = +002.000
	! output: x(3)  = -000.500
	!----------------------------------------------------------------------------
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	character(*), intent( in) :: fname
	real(DP)    , intent(out) :: x(3)
	!----------------------------------------------------------------------------
	integer                   :: i, lf, lx, nx
	integer                   :: bx, ex
	integer                   :: by, ey
	integer                   :: bz, ez
	character(1)              :: c
	logical                   :: first
	logical                   :: second
	!----------------------------------------------------------------------------
	lf=len(trim(fname))
	!----------------------------------------------------------------------------
	first  = .false.
	second = .false.
	nx     = 0
	x      = 0.D0
	!----------------------------------------------------------------------------
	! e.g. fname = coord+0001.200 or coord+0001.200.chk
	!
	c = fname(lf:lf)
	if (iachar(c)<48 .or. iachar(c)>57) then
		!-------------------------------------------------------------------------
		! e.g. fname = coord+0001.200.chk
		! lf = len(trim(coord+0001.200))
		!
		lf=index(fname,".",back=.true.)-1
	end if
	!----------------------------------------------------------------------------
	ex = lf
	!----------------------------------------------------------------------------
	do i=1, lf
		c=fname(i:i)
		if ( iachar(c)==43 .or. iachar(c)==45 ) then
			nx=nx+1
		end if
	end do
	do i=1, lf
		c=fname(i:i)
		if ( iachar(c)==43 .or. iachar(c)==45 ) then
			if (.not. first .and. .not. second) then
				bx     = i
				first  = .true.
			else if ( first .and. .not. second) then
				ex     = i-1
				second = .true.
				exit
			end if
		end if
	end do
	if (nx==3) then
		lx = ex-bx
		by = ex+1
		ey = by+lx
		bz = ey+1
		ez = bz+lx
		read(fname(bx:ex), *) x(1)
		read(fname(by:ey), *) x(2)
		read(fname(bz:ez), *) x(3)
	else if (nx==1) then
		read(fname(bx:ex), *) x(1)
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
