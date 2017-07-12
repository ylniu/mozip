! a) Input: Add lattice parameters units: au or Ang
! b) Output: More info printed out
! c) Output: Additional output file with E+PV
!
subroutine eos_fitting(au_unit,bravais,istat,npt,a,v0, etot, efit,fileout)
	use kinds, only: DP
!
!      fit of E(v) to an equation of state (EOS)
!
!      Interactive input:
!         au or Ang
!         structure
!         equation of state
!         input data file
!         output data file
!
!      Input data file format for cubic systems:
!         a0(1)  Etot(1)
!         ...
!         a0(n)  Etot(n)
!      where a0 is the lattice parameter (a.u. or Ang)
!      Input data file format for noncubic (e.g. hexagonal) systems:
!         V0(1)  Etot(1)
!         ...
!         V0(n)  Etot(n)
!      where V0 is the unit-cell volume (a.u.^3 or Ang^3)
!      e.g. for an hexagonal cell,
!         V0(i)  = sqrt(3)/2 * a^2 * c    unit-cell volume
!         Etot(i)= min Etot(c)   for the given volume V0(i)
!      Etot in atomic (Rydberg) units
!
!      Output data file format  for cubic systems:
!      # a0=... a.u., K0=... kbar, dk0=..., d2k0=... kbar^-1, Emin=... Ry
!      # a0=... Ang,  K0=... GPa , V0=... (a.u.)^3, V0 = Ang^3
!         a0(1)  Etot(1) Efit(1)  Etot(1)-Efit(1)  Pfit(1)  Enth(1)
!         ...
!         a0(n)  Etot(n) Efit(n)  Etot(n)-Efit(n)  Pfit(n)  Enth(n)
!      Output data file format  for noncubic systems:
!      # V0=...(a.u.)^3, K0=... kbar, dk0=..., d2k0=... kbar^-1, Emin=... Ry
!      # V0=...Ang^3,  K0=... GPa
!         V0(1)  Etot(1) Efit(1)  Etot(1)-Efit(1)  Pfit(1)  Enth(1)
!         ...
!         V0(n)  Etot(n) Efit(n)  Etot(n)-Efit(n)  Pfit(n)  Enth(n)
!      where
!            a0(i), V0(i), Etot(i) as in input
!            Efit(i) is the fitted value from the EOS
!            Pfit(i) is the corresponding pressure from the EOS (GPa)
!            Enth(i)=Efit(i)+Pfit(i)*V0(i) is the enthalpy (Ry)
!!
      use constants, only : bohr_radius_angs, ry_kbar
      !use ev_xml,    only : write_evdata_xml
      !use mp,        only : mp_start
      !use mp_global, only : mp_global_end, nproc, mpime
      implicit none
      integer, parameter  :: nmaxpar=4, nseek=10000, nmin=4
      integer             :: npar,npt,ios
		integer             :: i
		integer             :: istat
		character(*)        :: au_unit
		character(*)        :: bravais
      real(DP)            :: par(nmaxpar), deltapar(nmaxpar), parmin(nmaxpar)
      real(DP)            :: parmax(nmaxpar)
		real(DP)            :: a(npt)
		real(DP)            :: v0(npt)
		real(DP)            :: etot(npt)
		real(DP)            :: efit(npt)
      real(DP)            :: fac, emin, chisq
      real(DP), parameter :: gpa_kbar = 10.0_dp
      logical             :: in_angstrom
      character(*)        :: fileout
		integer, external   :: number_of_lines
	!----------------------------------------------------------------------------
	in_angstrom = au_unit=='Ang' .or. au_unit=='ANG' .or. au_unit=='ang'
	!----------------------------------------------------------------------------
	! Enter type of bravais lattice (fcc, bcc, sc, hex)
	!
	if(bravais=='fcc'.or.bravais=='FCC') then
		fac = 0.25d0
	else if(bravais=='bcc'.or.bravais=='BCC') then
		fac = 0.50d0
	else if(bravais=='sc'.or.bravais=='SC') then
		fac = 1.0d0
	else if(bravais=='hex'.or.bravais=='HEX') then
		! fac = sqrt(3d0)/2d0 ! not used
		fac = 0.0_DP ! not used
	else
		print '(5x,"ev: unexpected lattice ",a3)', bravais
		stop
	endif
	!----------------------------------------------------------------------------
	! Enter type of equation of state :
	! 1=birch1, 2=birch2, 3=keane, 4=murnaghan
	if(istat==1 .or. istat==4) then
		!npar=3
		npar=4
	else if(istat==2 .or. istat==3) then
		npar=4
	else
		print '(5x,"Unexpected eq. of state ",i2)', istat
		stop
	endif
	!----------------------------------------------------------------------------
	if (in_angstrom) then
		a  = a   / bohr_radius_angs
		v0 = v0 / bohr_radius_angs**3
	end if
	!v0 = fac * v0
	!----------------------------------------------------------------------------
	emin=1d10
	do i=1, npt
		if (bravais=='hex'.or.bravais=='HEX') then
			!if (in_angstrom) v0(i)=v0(i)/bohr_radius_angs**3
		else
			!if (in_angstrom) a(i) = a(i)/bohr_radius_angs
			!v0(i) = fac*a(i)**3
		endif
		if(etot(i)<emin) then
			par(1) = v0(i)
			emin = etot(i)
		endif
	end do
	!----------------------------------------------------------------------------
	! par(1) = V, Volume of the unit cell in (a.u.^3)
	! par(2) = B, Bulk Modulus (in KBar)
	! par(3) = dB/dP (adimensional)
	! par(4) = d^2B/dP^2 (in KBar^(-1), used only by 2nd order formulae)
	!----------------------------------------------------------------------------
	!
	par(2)    = 500.0d0
	par(3)    = 5.0d0
	par(4)    = -0.01d0
	!
	parmin(1) = 0.0d0
	parmin(2) = 0.0d0
	parmin(3) = 1.0d0
	parmin(4) = -1.0d0
	!
	parmax(1) = 100000.d0
	parmax(2) = 100000.d0
	parmax(3) = 15.0d0
	parmax(4) = 0.0d0
	!
	deltapar(1) = 0.1d0
	deltapar(2) = 100.d0
	deltapar(3) = 1.0d0
	deltapar(4) = 0.01d0
	!
	call find_minimum(npar,par,deltapar,parmin,parmax,nseek,nmin,chisq)
	!
	call write_results(npt,in_angstrom,fac,v0,etot,efit,istat,par,npar,emin,chisq,fileout)
	!
	! call write_evdata_xml(npt,fac,v0,etot,efit,istat,par,npar,emin,chisq,fileout)
	return
	contains
	!----------------------------------------------------------------------------
	subroutine eqstate(npar,par,chisq)
		use kinds, only: DP
		!-------------------------------------------------------------------------
		implicit none
		integer , intent(in) :: npar
		real(DP), intent(in) :: par(npar)
		real(DP), intent(out):: chisq
		integer  :: i
		real(DP) :: k0, dk0, d2k0, c0, c1, x, vol0, ddk
		!-------------------------------------------------------------------------
		vol0 = par(1)
		k0   = par(2)/ry_kbar ! converts k0 to Ry atomic units...
		dk0  = par(3)
		d2k0 = par(4)*ry_kbar ! and d2k0/dp2 to (Ry a.u.)^(-1)
		!-------------------------------------------------------------------------
		if(istat==1.or.istat==2) then
			if(istat==1) then
				c0 = 0.0d0
			else
				c0 = ( 9.d0*k0*d2k0 + 9.d0*dk0**2-63.d0*dk0+143.d0 )/48.d0
			endif
			c1 = 3.d0*(dk0-4.d0)/8.d0
			do i=1,npt
				x = vol0/v0(i)
				efit(i) = 9.d0*k0*vol0*( (-0.5d0+c1-c0)*x**(2.d0/3.d0)/2.d0 &
								+( 0.50-2.d0*c1+3.d0*c0)*x**(4.d0/3.d0)/4.d0 &
								+(       c1-3.d0*c0)*x**(6.d0/3.d0)/6.d0 &
								+(            c0)*x**(8.d0/3.d0)/8.d0 &
								-(-1.d0/8.d0+c1/6.d0-c0/8.d0) )
			end do
		else
			if(istat==3) then
				ddk = dk0 + k0*d2k0/dk0
			else
				ddk = dk0
			endif
			do i=1,npt
				efit(i) = - k0*dk0/ddk*vol0/(ddk-1.d0) &
				+ v0(i)*k0*dk0/ddk**2*( (vol0/v0(i))**ddk/(ddk-1.d0)+1.d0) &
				- k0*(dk0-ddk)/ddk*( v0(i)*log(vol0/v0(i)) + v0(i)-vol0 )
			end do
		endif
		!-------------------------------------------------------------------------
		! emin = equilibrium energy obtained by minimizing chi**2
		!
		emin = 0.0d0
		do i = 1,npt
			emin = emin + etot(i)-efit(i)
		end do
		emin = emin/npt
		!
		chisq = 0.0d0
		do i = 1,npt
			efit(i) = efit(i)+emin
			chisq   = chisq + (etot(i)-efit(i))**2
		end do
		chisq = chisq/npt
		!
		return
	end subroutine eqstate
!
!-----------------------------------------------------------------------
      subroutine write_results &
            (npt,in_angstrom,fac,v0,etot,efit,istat,par,npar,emin,chisq, &
             filout)
!-----------------------------------------------------------------------
!
      implicit none
      integer, intent(in) :: npt, istat, npar
      real(DP), intent(in):: v0(npt), etot(npt), efit(npt), emin, chisq, fac
      real(DP), intent(inout):: par(npar)
      real(DP), EXTERNAL :: keane, birch
      logical, intent(in) :: in_angstrom
      character(*), intent(inout) :: filout
      !
      real(DP) :: p(npt), epv(npt)
      integer :: i, iun
      logical :: exst

      if(filout/=' ') then
         iun=8
         INQUIRE(file=filout,exist=exst)
         if (exst) print '(5x,"Beware: file ",A," will be overwritten")',&
                  trim(filout)
         OPEN(unit=iun,file=filout,form='formatted',status='unknown', &
              iostat=ios)
         if (ios /= 0) then
            print '(5x,"Cannot open file ",A)',trim(filout)
            stop
         endif
      else
         iun=6
      endif

      if(istat==1) then
         WRITE(iun,'("# equation of state: birch 1st order.  chisq = ", d12.4)') chisq
      else if(istat==2) then
         WRITE(iun,'("# equation of state: birch 3rd order.  chisq = ", d12.4)') chisq
      else if(istat==3) then
         WRITE(iun,'("# equation of state: keane.            chisq = ", d12.4)') chisq
      else if(istat==4) then
         WRITE(iun,'("# equation of state: murnaghan.        chisq = ", d12.4)') chisq
      endif

      if(istat==1 .or. istat==4) par(4) = 0.0d0

      if(istat==1 .or. istat==2) then
         do i=1,npt
            p(i)=birch(v0(i)/par(1),par(2),par(3),par(4))
         end do
      else
         do i=1,npt
            p(i)=keane(v0(i)/par(1),par(2),par(3),par(4))
         end do
      endif

      do i=1,npt
         epv(i) = etot(i) + p(i)*v0(i) / ry_kbar
      end do

      if ( fac /= 0.0_dp ) then
! cubic case
         WRITE(iun,'("# a0 =",f8.4," a.u., k0 =",i5," kbar, dk0 =", &
                    &f6.2," d2k0 =",f7.3," emin =",f11.5)') &
            (par(1)/fac)**(1d0/3d0), int(par(2)), par(3), par(4), emin
         WRITE(iun,'("# a0 =",f9.5," Ang, k0 =", f6.1," GPa,  V0 = ", &
                  & f7.3," (a.u.)^3,  V0 =", f7.3," A^3 ",/)') &
           & (par(1)/fac)**(1d0/3d0)*bohr_radius_angs, par(2)/gpa_kbar, &
             par(1), par(1)*bohr_radius_angs**3

        WRITE(iun,'(80("#"))')
        WRITE(iun,'("# Lat.Par", 7x, "E_calc", 8x, "E_fit", 7x, &
             & "E_diff", 8x, "Pressure", 6x, "Enthalpy")')
        if (in_angstrom) then
           WRITE(iun,'("# Ang", 13x, "Ry", 11x, "Ry", 12x, &
             & "Ry", 12x, "GPa", 11x, "Ry")')
           WRITE(iun,'(80("#"))')
           WRITE(iun,'(f9.5,2x,f12.5, 2x,f12.5, f12.5, 3x, f12.3, 3x,f12.5)') &
              & ( (v0(i)/fac)**(1d0/3d0)*bohr_radius_angs, etot(i), efit(i),  &
              & etot(i)-efit(i), p(i)/gpa_kbar, epv(i), i=1,npt )
        else
           WRITE(iun,'("# a.u.",12x, "Ry", 11x, "Ry", 12x, &
             & "Ry", 8x, "GPa", 11x, "Ry")')
           WRITE(iun,'(73("#"))')
           WRITE(iun,'(f9.5,2x,f12.5, 2x,f12.5, f12.5, 3x, f8.2, 3x,f12.5)') &
              & ( (v0(i)/fac)**(1d0/3d0), etot(i), efit(i),  &
              & etot(i)-efit(i), p(i)/gpa_kbar, epv(i), i=1,npt )
        endif

      else
! noncubic case
         WRITE(iun,'("# V0 =",f8.2," a.u.^3,  k0 =",i5," kbar,  dk0 =", &
                    & f6.2,"  d2k0 =",f7.3,"  emin =",f11.5)') &
                    & par(1), int(par(2)), par(3), par(4), emin

         WRITE(iun,'("# V0 =",f8.2,"  Ang^3,  k0 =",f6.1," GPa"/)') &
                    & par(1)*bohr_radius_angs**3, par(2)/gpa_kbar

        WRITE(iun,'(74("#"))')
        WRITE(iun,'("# Vol.", 8x, "E_calc", 8x, "E_fit", 7x, &
             & "E_diff", 4x, "Pressure", 6x, "Enthalpy")')
        if (in_angstrom) then
          WRITE(iun,'("# Ang^3", 9x, "Ry", 11x, "Ry", 12x, &
             & "Ry", 8x, "GPa", 11x, "Ry")')
          WRITE(iun,'(74("#"))')
           WRITE(iun,'(f8.2,2x,f12.5, 2x,f12.5, f12.5, 3x, f8.2, 3x,f12.5)') &
              ( v0(i)*bohr_radius_angs**3, etot(i), efit(i),  &
               etot(i)-efit(i), p(i)/gpa_kbar, epv(i), i=1,npt )
         else
          WRITE(iun,'("# a.u.^3",8x, "Ry", 11x, "Ry", 12x, &
             & "Ry", 8x, "GPa", 11x, "Ry")')
          WRITE(iun,'(74("#"))')
           WRITE(iun,'(f8.2,2x,f12.5, 2x,f12.5, f12.5, 3x, f8.2, 3x,f12.5)') &
              ( v0(i), etot(i), efit(i),  &
               etot(i)-efit(i), p(i)/gpa_kbar, epv(i), i=1,npt )
         end if


      endif

      if(filout/=' ') CLOSE(unit=iun)
      return
    end subroutine write_results
!
!-----------------------------------------------------------------------
	subroutine find_minimum(npar,par,deltapar,parmin,parmax,nseek,nmin,chisq)
	!-----------------------------------------------------------------------
	! Minimization
	!
	use random_numbers, only : randy
	implicit none
	integer maxpar, nseek, npar, nmin, n,j,i
	parameter (maxpar=4)
	real(DP) :: par(npar), deltapar(npar), parmin(npar)
	real(DP) :: parmax(npar), parnew(maxpar), chisq, chinew
	!-----------------------------------------------------------------------
	! various initializations
	!
	chisq = 1.0d30
	chinew= 1.0d30
	call eqstate(npar,par,chisq)
	do j = 1,nmin
		do i = 1,nseek
			do n = 1,npar
				parnew(n) = par(n) + (0.5d0 - randy())*deltapar(n)
				do while( parnew(n)>parmax(n) .and. parnew(n)<parmin(n) )
					parnew(n) = par(n) + (0.5d0 - randy())*deltapar(n)
				end do
			end do
			!
			call eqstate(npar,parnew,chinew)
			!
			if(chinew<chisq) then
				do n = 1,npar
					par(n) = parnew(n)
				end do
				chisq = chinew
			endif
		end do
		do n = 1,npar
			deltapar(n) = deltapar(n)/10.d0
		end do
	end do
	!
	call eqstate(npar,par,chisq)
	!
	return
	end subroutine find_minimum
	!----------------------------------------------------------------------------
	end subroutine
	
	function birch(x,k0,dk0,d2k0)
	!
	use kinds, only : DP
	implicit none
	real(DP) birch, x, k0,dk0, d2k0
	real(DP) c0, c1
	
	if(d2k0/=0.d0) then
		c0 = (9.d0*k0*d2k0 + 9.d0*dk0**2 - 63.d0*dk0 + 143.d0 )/48.d0
	else
		c0 = 0.0d0
	end if
	c1 = 3.d0*(dk0-4.d0)/8.d0
	birch = 3.d0*k0*( (-0.5d0+  c1-  c0)*x**( -5.d0/3.d0) &
		+( 0.5d0-2.d0*c1+3.0d0*c0)*x**( -7.d0/3.d0) &
		+(       c1-3*c0)*x**( -9.0d0/3d0) &
		+(            c0)*x**(-11.0d0/3d0) )
	return
	end function birch
	
	function keane(x,k0,dk0,d2k0)
	use kinds, only : DP
	implicit none
	real(DP) keane, x, k0, dk0, d2k0, ddk
	
	ddk = dk0 + k0*d2k0/dk0
	keane = k0*dk0/ddk**2*( x**(-ddk) - 1d0 ) + (dk0-ddk)/ddk*log(x)
	
	return
	end function keane
