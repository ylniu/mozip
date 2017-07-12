subroutine plotModel
	use kinds     , only: DP
	use param     , only: au2a
	use module_afm, only: FF
	use module_afm, only: bas
	use module_afm, only: border
	use module_afm, only: scanmin, scanmax
	implicit none
	!--------------------------------------------------------------------
	real(DP)    , allocatable :: x1(:,:)
	character(2), allocatable :: s1(  :)
	!--------------------------------------------------------------------
	allocate(x1  (3,bas%n+16))
	allocate(s1  (  bas%n+16))
	!--------------------------------------------------------------------
	x1(:,1:bas%n ) = bas%x
	x1(:,bas%n+ 1) = FF%x(:,     1,      1,      1 )
	x1(:,bas%n+ 2) = FF%x(:,FF%n(1),     1 ,     1 )
	x1(:,bas%n+ 3) = FF%x(:,     1 ,FF%n(2),     1 )
	x1(:,bas%n+ 4) = FF%x(:,FF%n(1),FF%n(2),     1 )
	x1(:,bas%n+ 5) = FF%x(:,     1 ,     1 ,FF%n(3))
	x1(:,bas%n+ 6) = FF%x(:,FF%n(1),     1 ,FF%n(3))
	x1(:,bas%n+ 7) = FF%x(:,     1 ,FF%n(2),FF%n(3))
	x1(:,bas%n+ 8) = FF%x(:,FF%n(1),FF%n(2),FF%n(3))
	!--------------------------------------------------------------------
	scanmin (1)    = FF%x(1,     1 ,     1 ,      1 ) + border / 2.0_DP
	scanmin (2)    = FF%x(2,     1 ,     1 ,      1 ) + border / 2.0_DP
	scanmax (1)    = FF%x(1,FF%n(1),FF%n(2) ,FF%n(3)) - border / 2.0_DP
	scanmax (2)    = FF%x(2,FF%n(1),FF%n(2) ,FF%n(3)) - border / 2.0_DP
	x1(1,bas%n+ 9) = scanmin(1)
	x1(2,bas%n+ 9) = scanmin(2)
	x1(3,bas%n+ 9) = scanmin(3)
	!
	x1(1,bas%n+10) = scanmax(1)
	x1(2,bas%n+10) = scanmin(2)
	x1(3,bas%n+10) = scanmin(3)
	!
	x1(1,bas%n+11) = scanmax(1)
	x1(2,bas%n+11) = scanmax(2)
	x1(3,bas%n+11) = scanmin(3)
	!
	x1(1,bas%n+12) = scanmin(1)
	x1(2,bas%n+12) = scanmax(2)
	x1(3,bas%n+12) = scanmin(3)
	!
	x1(1,bas%n+13) = scanmin(1)
	x1(2,bas%n+13) = scanmin(2)
	x1(3,bas%n+13) = scanmax(3)
	!
	x1(1,bas%n+14) = scanmax(1)
	x1(2,bas%n+14) = scanmin(2)
	x1(3,bas%n+14) = scanmax(3)
	!
	x1(1,bas%n+15) = scanmin(1)
	x1(2,bas%n+15) = scanmax(2)
	x1(3,bas%n+15) = scanmax(3)
	!
	x1(1,bas%n+16) = scanmax(1)
	x1(2,bas%n+16) = scanmax(2)
	x1(3,bas%n+16) = scanmax(3)
	!--------------------------------------------------------------------
	x1   = x1 * au2a
	s1(      1:bas%n   ) = bas%symbol
	s1(bas%n+1:bas%n+ 8) = "X"
	s1(bas%n+9:bas%n+16) = "B"
	call write_gjf("bas.gjf", bas%n+16, s1, x1)
	call write_xyz("bas.xyz", bas%n+16, s1, x1)
	!--------------------------------------------------------------------
	deallocate(x1  )
	deallocate(s1  )
	!--------------------------------------------------------------------
end subroutine
