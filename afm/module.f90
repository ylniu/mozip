module module_prg
	implicit none
	!------------------------------------------------------------------------
	type, public :: OBJ_PRG
		integer :: n
		integer :: i
		integer, allocatable :: p(:)
	end type
	!------------------------------------------------------------------------
	contains
		!---------------------------------------------------------------------
		subroutine allocate_prg(this)
			type(OBJ_PRG) :: this
			if (.not. allocated(this%p)) allocate(this%p(0:this%n))
		end subroutine
		!---------------------------------------------------------------------
		subroutine deallocate_prg(this)
			type(OBJ_PRG) :: this
			if (allocated(this%p)) deallocate(this%p)
		end subroutine
	!------------------------------------------------------------------------
end module

module module_LJp
	use kinds, only:DP
	implicit none
	!------------------------------------------------------------------------
	type, public                :: OBJ_LJp
		integer                  :: n
		real(DP), allocatable    :: r0(:)
		real(DP), allocatable    :: ep(:)
	end type
	!------------------------------------------------------------------------
	contains
	!------------------------------------------------------------------------
	subroutine allocate_LJp(this)
		type(OBJ_LJp) :: this
		allocate(this%r0(this%n))
		allocate(this%ep(this%n))
	end subroutine
	!------------------------------------------------------------------------
	subroutine deallocate_LJp(this)
		type(OBJ_LJp) :: this
		deallocate(this%r0)
		deallocate(this%ep)
	end subroutine
	!------------------------------------------------------------------------
end module module_LJp
!
!===========================================================================
!
module module_obj
	use kinds, only:DP
	implicit none
	!------------------------------------------------------------------------
	type, public                :: OBJ_FF
		integer                  :: n(3)
		real(DP), allocatable    :: x(:,:,:,:  )
		real(DP), allocatable    :: L(:,:,:,:,:)
		real(DP), allocatable    :: C(:,:,:,:,:)
		real(DP), allocatable    :: T(:,:,:,:,:)
		real(DP)                 :: dCell(3,3)
! 		contains
! 			procedure, public :: makeConsistent => makeConsist
	end type
	contains
	subroutine allocate_obj(this, tip_natom)
		type(OBJ_FF) :: this
		integer      :: tip_natom
		!---------------------------------------------------------------------
		integer      :: n1, n2, n3, m
		!---------------------------------------------------------------------
		n1 = this%n(1)
		n2 = this%n(2)
		n3 = this%n(3)
		m  = tip_natom
		!---------------------------------------------------------------------
		if (.not. allocated(this%X)) allocate(this%X(3, 0:n1, 0:n2, 0:n3   ))
		if (.not. allocated(this%L)) allocate(this%L(3, 0:n1, 0:n2, 0:n3, m))
		if (.not. allocated(this%C)) allocate(this%C(3, 0:n1, 0:n2, 0:n3, m))
		if (.not. allocated(this%T)) allocate(this%T(3, 0:n1, 0:n2, 0:n3, m))
		!---------------------------------------------------------------------
	end subroutine
	!------------------------------------------------------------------------
	subroutine deallocate_obj(this)
		type(OBJ_FF) :: this
		!---------------------------------------------------------------------
		if (allocated(this%X)) deallocate(this%X)
		if (allocated(this%L)) deallocate(this%L)
		if (allocated(this%C)) deallocate(this%C)
		if (allocated(this%T)) deallocate(this%T)
		!---------------------------------------------------------------------
	end subroutine
end module module_obj
!
!===========================================================================
!
module module_tip
	use kinds     , only: DP
	implicit none
	!------------------------------------------------------------------------
	type, public                 :: OBJ_TIP
		integer                   :: Tip_type
		!---------------------------------------------------------------------
		real(DP)                  :: rPP0   (3)  ! equilibirum bending position
		real(DP)                  :: kSpring(3)  ! bending stiffness ( z component usually zero )
		real(DP)                  :: lRadial     ! radial PP-tip distance
		real(DP)                  :: kRadial     ! radial PP-tip stiffness
		!---------------------------------------------------------------------
		integer                   :: n
		integer                   :: type_n
		real(DP)                  :: ft(3)
		real(DP)                  :: fz
		real(DP)                  :: center (  3)
		integer     , allocatable :: idx    (  :)
		integer     , allocatable :: idx_n  (  :)
		integer     , allocatable :: idx_nat(  :)
		integer     , allocatable :: nat    (  :)
		real(DP)    , allocatable :: x      (:,:)
		real(DP)    , allocatable :: f      (:,:)
		real(DP)    , allocatable :: charge (  :)
		real(DP)    , allocatable :: mass   (  :)
		real(DP)    , allocatable :: m3     (:,:)
		real(DP)    , allocatable :: r0     (  :)
		real(DP)    , allocatable :: ep     (  :)
		logical     , allocatable :: relax  (  :)
		character(1), allocatable :: opt    (  :)
		character(1), allocatable :: opt_sum(  :)
		character(2), allocatable :: symbol (  :)
		character(2), allocatable :: idx_sym(  :)
	end type
	!------------------------------------------------------------------------
	contains
	!------------------------------------------------------------------------
	subroutine allocate_tip(this)
		type(OBJ_TIP) :: this
		integer       :: n
		n=this%n
		!---------------------------------------------------------------------
		if (.not. allocated(this%x       ) ) allocate(this%x      (3,n))
		if (.not. allocated(this%x       ) ) allocate(this%f      (3,n))
		if (.not. allocated(this%nat     ) ) allocate(this%nat    (  n))
		if (.not. allocated(this%mass    ) ) allocate(this%mass   (  n))
		if (.not. allocated(this%m3      ) ) allocate(this%m3     (3,n))
		if (.not. allocated(this%idx     ) ) allocate(this%idx    (  n))
		if (.not. allocated(this%charge  ) ) allocate(this%charge (  n))
		if (.not. allocated(this%r0      ) ) allocate(this%r0     (  n))
		if (.not. allocated(this%ep      ) ) allocate(this%ep     (  n))
		if (.not. allocated(this%opt     ) ) allocate(this%opt    (  n))
		if (.not. allocated(this%opt_sum ) ) allocate(this%opt_sum(  n))
		if (.not. allocated(this%relax   ) ) allocate(this%relax  (  n))
		if (.not. allocated(this%symbol  ) ) allocate(this%symbol (  n))
		!---------------------------------------------------------------------
	end subroutine
	!------------------------------------------------------------------------
	subroutine allocate_tip_type(this)
		type(OBJ_TIP) :: this
		integer       :: n
		n=this%type_n
		!---------------------------------------------------------------------
		if (.not. allocated(this%idx_n   ) ) allocate(this%idx_n  (  n))
		if (.not. allocated(this%idx_nat ) ) allocate(this%idx_nat(  n))
		if (.not. allocated(this%idx_sym ) ) allocate(this%idx_sym(  n))
		!---------------------------------------------------------------------
	end subroutine
	!------------------------------------------------------------------------
	subroutine deallocate_tip(this)
		type(OBJ_TIP) :: this
		!---------------------------------------------------------------------
		if ( allocated(this%x       ) ) deallocate(this%x      )
		if ( allocated(this%f       ) ) deallocate(this%f      )
		if ( allocated(this%nat     ) ) deallocate(this%nat    )
		if ( allocated(this%mass    ) ) deallocate(this%mass   )
		if ( allocated(this%m3      ) ) deallocate(this%m3     )
		if ( allocated(this%idx     ) ) deallocate(this%idx    )
		if ( allocated(this%charge  ) ) deallocate(this%charge )
		if ( allocated(this%r0      ) ) deallocate(this%r0     )
		if ( allocated(this%ep      ) ) deallocate(this%ep     )
		if ( allocated(this%opt     ) ) deallocate(this%opt    )
		if ( allocated(this%opt_sum ) ) deallocate(this%opt_sum)
		if ( allocated(this%relax   ) ) deallocate(this%relax  )
		if ( allocated(this%symbol  ) ) deallocate(this%symbol )
		!-----------------------------------------------------------------
	end subroutine
	!--------------------------------------------------------------------
	subroutine deallocate_tip_type(this)
		type(OBJ_TIP) :: this
		!-----------------------------------------------------------------
		if ( allocated(this%idx_n   ) ) deallocate(this%idx_n  )
		if ( allocated(this%idx_nat ) ) deallocate(this%idx_nat)
		if ( allocated(this%idx_sym ) ) deallocate(this%idx_sym)
		!-----------------------------------------------------------------
	end subroutine
	!--------------------------------------------------------------------
end module
!
!=======================================================================
!
module module_bas
	use kinds, only: DP
	!--------------------------------------------------------------------
	type, public                 :: OBJ_BAS
		integer                   :: n
		integer      , allocatable :: nat   (  :)
		real(DP)     , allocatable :: charge(  :)
		real(DP)     , allocatable :: r0    (  :)
		real(DP)     , allocatable :: ep    (  :)
		real(DP)     , allocatable :: x     (:,:)
		real(DP)     , allocatable :: xout  (:,:)
		character(20), allocatable :: acolor(  :)
		character( 2), allocatable :: symbol(  :)
	end type
	!--------------------------------------------------------------------
	contains
	!--------------------------------------------------------------------
	subroutine allocate_bas(this)
		type(OBJ_BAS) :: this
		integer       :: n
		n=this%n
		if(.not. allocated(this%symbol)) allocate(this%symbol(  n))
		if(.not. allocated(this%nat   )) allocate(this%nat   (  n))
		if(.not. allocated(this%charge)) allocate(this%charge(  n))
		if(.not. allocated(this%r0    )) allocate(this%r0    (  n))
		if(.not. allocated(this%ep    )) allocate(this%ep    (  n))
		if(.not. allocated(this%x     )) allocate(this%x     (3,n))
		if(.not. allocated(this%xout  )) allocate(this%xout  (3,n))
		if(.not. allocated(this%acolor)) allocate(this%acolor(  n))
	end subroutine allocate_bas
	!--------------------------------------------------------------------
	subroutine deallocate_bas(this)
		type(OBJ_BAS) :: this
		if(allocated(this%symbol)) deallocate(this%symbol)
		if(allocated(this%nat   )) deallocate(this%nat   )
		if(allocated(this%charge)) deallocate(this%charge)
		if(allocated(this%r0    )) deallocate(this%r0    )
		if(allocated(this%ep    )) deallocate(this%ep    )
		if(allocated(this%x     )) deallocate(this%x     )
		if(allocated(this%xout  )) deallocate(this%xout  )
		if(allocated(this%acolor)) deallocate(this%acolor)
	end subroutine deallocate_bas
	!--------------------------------------------------------------------
end module
!
!=======================================================================
!
module module_scn
	use kinds, only: DP
	!------------------------------------------------------------------------
	type, public                :: OBJ_SCN
		integer                  :: n(3)
		real(DP)                 :: dx
		real(DP)                 :: dy
		real(DP)                 :: dz
		integer , allocatable    :: cy(  :,:,:)
		real(DP), allocatable    :: xTip(:)
		real(DP), allocatable    :: yTip(:)
		real(DP), allocatable    :: zTip(:)
		real(DP), allocatable    :: x (:,:,:,:)
		real(DP), allocatable    :: xp(:,:,:,:,:)
		real(DP), allocatable    :: ft(:,:,:,:,:)
		real(DP), allocatable    :: fb(:,:,:,:,:)
		real(DP), allocatable    :: F (:,:,:,:)
		real(DP), allocatable    :: Fz(  :,:,:)
		real(DP), allocatable    :: Fa(  :,:,:)
		real(DP), allocatable    :: Fr(  :,:,:)
		logical , allocatable    :: conv(:,:,:)
	end type
	contains
	subroutine allocate_scn(this,n)
		type(OBJ_SCN) :: this
		integer       :: n
		integer       :: n1, n2, n3
		!---------------------------------------------------------------------
		n1 = this%n(1)
		n2 = this%n(2)
		n3 = this%n(3)
		!---------------------------------------------------------------------
		if (.not. allocated(this%xTip)) allocate(this%xTip(      0:n1            ))
		if (.not. allocated(this%yTip)) allocate(this%yTip(            0:n2      ))
		if (.not. allocated(this%zTip)) allocate(this%zTip(                  0:n3))
		if (.not. allocated(this%X   )) allocate(this%X   (3,    0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%xp  )) allocate(this%xp  (3, n, 0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%fb  )) allocate(this%fb  (3, n, 0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%ft  )) allocate(this%ft  (3, n, 0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%F   )) allocate(this%F   (3,    0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%Fz  )) allocate(this%Fz  (      0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%Fa  )) allocate(this%Fa  (      0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%Fr  )) allocate(this%Fr  (      0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%cy  )) allocate(this%cy  (      0:n1, 0:n2, 0:n3))
		if (.not. allocated(this%conv)) allocate(this%conv(      0:n1, 0:n2, 0:n3))
		!---------------------------------------------------------------------
	end subroutine
	!------------------------------------------------------------------------
	subroutine deallocate_scn(this)
		type(OBJ_SCN) :: this
		!---------------------------------------------------------------------
		if (allocated(this%xTip)) deallocate(this%xTip)
		if (allocated(this%yTip)) deallocate(this%yTip)
		if (allocated(this%zTip)) deallocate(this%zTip)
		if (allocated(this%xp  )) deallocate(this%xp  )
		if (allocated(this%fb  )) deallocate(this%fb  )
		if (allocated(this%ft  )) deallocate(this%ft  )
		if (allocated(this%X   )) deallocate(this%X   )
		if (allocated(this%F   )) deallocate(this%F   )
		if (allocated(this%Fz  )) deallocate(this%Fz  )
		if (allocated(this%Fa  )) deallocate(this%Fa  )
		if (allocated(this%Fr  )) deallocate(this%Fr  )
		if (allocated(this%cy  )) deallocate(this%cy  )
		if (allocated(this%conv)) deallocate(this%conv)
		!---------------------------------------------------------------------
	end subroutine
end module
!
!=======================================================================
!
module module_afm
	use kinds     , only: DP
	use module_obj, only: OBJ_FF
	use module_tip, only: OBJ_TIP
	use module_LJp, only: OBJ_LJp
	use module_bas, only: OBJ_BAS
	use module_scn, only: OBJ_SCN
	use module_prg, only: OBJ_PRG
	implicit none
	!------------------------------------------------------------------------
	real(DP)      , parameter   :: eVA_Nm =  16.0217657D0
	!------------------------------------------------------------------------
	integer                     :: fid
	integer                     :: iout
	!------------------------------------------------------------------------
	character(200)              :: fpara
	character(200)              :: fout
	character(200)              :: fxyz
	character(200)              :: ftip
	character(200)              :: fcolor
	character(800)              :: path_exe
	!------------------------------------------------------------------------
	integer                     :: ncolor
	character( 20), allocatable :: icolor(:)
	character(  2), allocatable :: cymbol(:)
	!------------------------------------------------------------------------
	logical                     :: withElectrostatics
	logical                     :: if_shiftXY
	logical                     :: if_fitCell
	!------------------------------------------------------------------------
	!integer                     :: natom
	!integer       , allocatable :: nat(:)
	!real(DP)      , allocatable :: x(:,:)
	!real(DP)      , allocatable :: xout(:,:)
	!real(DP)      , allocatable :: acha(:)
	!character( 20), allocatable :: acolor(:)
	!character(  2), allocatable :: symbol(:)
	!------------------------------------------------------------------------
	integer                     :: ntips
	real(DP)      , allocatable :: zTips(:)
	real(DP)      , allocatable :: rTips(:,:)
	real(DP)      , allocatable :: rs(:)
	real(DP)      , allocatable :: fs(:)
	real(DP)      , allocatable :: rT(:)
	real(DP)      , allocatable :: rP(:)
	!------------------------------------------------------------------------
	real(DP)                    :: moleculeShift(3)
	real(DP)                    :: scanxyz(3)
	real(DP)                    :: scanxyz0(3)
	real(DP)                    :: scanxyz1(3)
	real(DP)                    :: scanxyz2(3)
	!------------------------------------------------------------------------
	logical                     :: Para
	logical                     :: PBC
	logical                     :: showpic
	integer                     :: debug_imin(3)
	integer                     :: debug_imax(3)
	logical                     :: debug_xy
	integer                     :: scanDim
	integer                     :: ForceCurve
	integer                     :: ForceAtom
	integer                     :: Tip_type
	real(DP)                    :: k_E(0:9,4)
	real(DP)                    :: k_F(1:9,4)
	real(DP)                    :: xrange(2,4)
	real(DP)                    :: ForceXYZ0(3)
	real(DP)                    :: ForceXYZ1(3)
	real(DP)                    :: ForceXYZ2(3)
	real(DP)                    :: f_eps
	character(200)              :: colorscale
	character(200)              :: imageInterpolation
	integer                     :: probeStart
	integer                     :: probeType
	integer                     :: maxIters
	real(DP)                    :: AtomRadius
	real(DP)                    :: charge
	real(DP)                    :: dstep
	real(DP)                    :: ColorAlpha
	real(DP)                    :: Amplitude
	real(DP)                    :: border
	integer                     :: gridN(3)
	real(DP)                    :: stiffness(3)
	real(DP)                    :: r0Probe(3)
	real(DP)                    :: gridA(3)
	real(DP)                    :: gridB(3)
	real(DP)                    :: gridC(3)
	real(DP)                    :: scanMin(3)
	real(DP)                    :: scanMax(3)
	real(DP)                    :: figsize(3)
	real(DP)                    :: scanStep(3)
	real(DP)                    :: fire_falpha
	real(DP)                    :: fire_dtmax
	real(DP)                    :: fire_damping
	real(DP)                    :: fire_finc
	real(DP)                    :: fire_fdec
	integer                     :: relaxAlg = 1
	integer      , parameter    :: nLJ=118
	real(DP)                    :: r0(nLJ)
	real(DP)                    :: ep(nLJ)
	type(OBJ_FF)                :: FF
	type(OBJ_TIP)               :: tip
	type(OBJ_LJp)               :: LJp
	type(OBJ_BAS)               :: bas
	type(OBJ_SCN)               :: scn
	type(OBJ_PRG)               :: prg
	!----------------------------------------------------------------------------
		data r0 / &
		  1.48700000D+00,  1.48150000D+00,  2.00000000D+00,  2.00000000D+00,  2.08000000D+00, &
		  1.90800000D+00,  1.78000000D+00,  1.66120000D+00,  1.75000000D+00,  1.54350000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  1.90000000D+00,  2.10000000D+00, &
		  2.00000000D+00,  1.94800000D+00,  1.88050000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  1.90800000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.22000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.35000000D+00,  2.18150000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00,  2.00000000D+00, &
		  0.20000000D+01,  2.00000000D+00,  2.00000000D+00 &
		/
		data ep / &
		  6.80805400D-04,  9.45322000D-04,  1.00000000D-02,  1.00000000D-02,  3.72925200D-03, &
		  3.72925240D-03,  7.37190000D-03,  9.10631400D-03,  2.64516700D-03,  3.64252600D-03, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  2.54899514D-02,  8.67268000D-03, &
		  1.08408500D-02,  1.14913010D-02,  1.23412240D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  2.50000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.38762880D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.73453600D-02,  2.43442128D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02,  1.00000000D-02, &
		  0.10000000D-01,  1.00000000D-02,  1.00000000D-02 &
		/
	contains
! 		!-------------------------------------------------------------------------
! 		subroutine makeConsistent(this)
! 		!subroutine makeConsist(this)
! 			! place rPP0 on the sphere to be consistent with radial spring
! 			type(TIP_ob) :: this
! 			if (abs(this%kRadial)>1.D-8) then
! 				this%rPP0(3) = -sqrt( this%lRadial*this%lRadial - this%rPP0(1)*this%rPP0(1) - this%rPP0(2)*this%rPP0(2) )
! 				write(iout, '(2x," rPP0 ", 3f15.7)') this%rPP0
! 			end if
! 		end subroutine
! 		!-------------------------------------------------------------------------
! 		subroutine s(this)
! 		!subroutine makeConsist(this)
! 			! place rPP0 on the sphere to be consistent with radial spring
! 			type(TIP_ob) :: this
! 			if (abs(this%kRadial)>1.D-8) then
! 				this%rPP0(3) = -sqrt( this%lRadial*this%lRadial - this%rPP0(1)*this%rPP0(1) - this%rPP0(2)*this%rPP0(2) )
! 				write(iout, '(2x," rPP0 ", 3f15.7)') this%rPP0
! 			end if
! 		end subroutine
		!-----------------------------------------------------------------
end module
!
!-----------------------------------------------------------------------
!
module force_grid
	!--------------------------------------------------------------------
	use kinds, only: DP
	implicit none
	!--------------------------------------------------------------------
	type, public :: OBJ_FGRID
		integer               :: n    (3)
		real(DP)              :: dx   (3)
		real(DP)              :: xmin (3)
		real(DP)              :: xmax (3)
		real(DP), allocatable :: f(:,:,:)
		contains
			procedure :: alloc
			procedure :: dealloc
			procedure :: setdx
			procedure :: reduce
			procedure :: deriv
	end type
	!--------------------------------------------------------------------
	contains
	!--------------------------------------------------------------------
	subroutine alloc(this, n)
		class(OBJ_FGRID) :: this
		integer          :: n(3)
		if (.not.allocated(this%f) .and. all(n>=1)) then
			this%n = n
			allocate(this%f(0:this%n(1), 0:this%n(2), 0:this%n(3)))
		end if
	end subroutine
	!--------------------------------------------------------------------
	subroutine dealloc(this)
		class(OBJ_FGRID) :: this
		if (allocated(this%f)) then
			deallocate(this%f)
		end if
	end subroutine
	!--------------------------------------------------------------------
	subroutine setdx(this, xmin, xmax)
		class(OBJ_FGRID) :: this
		real(DP)         :: xmin(3)
		real(DP)         :: xmax(3)
		if (all(xmax>=xmin) .and. all(this%n>0) ) then
			this%xmax = xmax
			this%xmin = xmin
			this%dx   = (this%xmax-this%xmin)/this%n
		else
			this%xmin = 0.0_DP
			this%xmax = 0.0_DP
			this%dx   = 0.0_DP
		end if
	end subroutine
	!--------------------------------------------------------------------
	subroutine reduce(this, that, m)
		class(OBJ_FGRID) :: this
		class(OBJ_FGRID) :: that
		integer          :: m
		integer          :: ix, iy, iz, i, jz
		integer          :: k(3)
		real(DP)         :: f
		real(DP)         :: xmin(3)
		real(DP)         :: xmax(3)
		!-----------------------------------------------------------------
		k    =  that%n
		k(3) = (that%n(3)+1) - m
		xmin = that%xmin
		xmax = that%xmax
		!-----------------------------------------------------------------
		call this%alloc(k)
		!-----------------------------------------------------------------
		do ix = 0, k(1)
			do iy = 0, k(2)
				do iz = 0, k(3)
					!--------------------------------------------------------
					f = 0.0_DP
					do i=0, m-1
						jz = iz + 1
						f = f + that%f(ix,iy,jz)
					end do
					!--------------------------------------------------------
					this%f(ix,iy,iz) = f
					!--------------------------------------------------------
				end do
			end do
		end do
		this%f =  this%f / m
		!-----------------------------------------------------------------
		xmin    = that%xmin
		xmax    = that%xmax
		xmin(3) = xmin(3) + (m - 1) * that%dx(3) / 2.0_DP
		xmax(3) = xmax(3) - (m - 1) * that%dx(3) / 2.0_DP
		call this%setdx(xmin, xmax)
		!-----------------------------------------------------------------
	end subroutine
	!--------------------------------------------------------------------
	subroutine deriv(this, that)
		class(OBJ_FGRID) :: this
		class(OBJ_FGRID) :: that
		integer          :: ix, iy, iz
		integer          :: k(3)
		real(DP)         :: xmin(3)
		real(DP)         :: xmax(3)
		!-----------------------------------------------------------------
		k    = that%n
		k(3) = that%n(3) - 2
		xmin = that%xmin
		xmax = that%xmax
		!-----------------------------------------------------------------
		call this%alloc(k)
		!-----------------------------------------------------------------
		do ix = 0, k(1)
			do iy = 0, k(2)
				do iz = 0, k(3)
					!--------------------------------------------------------
					this%f(ix,iy,iz) =  that%f(ix,iy,iz+2) - this%f(ix,iy,iz)
					!--------------------------------------------------------
				end do
			end do
		end do
		this%f =  this%f / that%dx(3) / 2.0_DP
		!-----------------------------------------------------------------
		xmin    = that%xmin
		xmax    = that%xmax
		xmin(3) = xmin(3) + that%dx(3)
		xmax(3) = xmax(3) - that%dx(3)
		call this%setdx(xmin, xmax)
		!-----------------------------------------------------------------
	end subroutine
end module
