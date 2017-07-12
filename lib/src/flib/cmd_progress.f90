module cmd_progress
	implicit none
	!--------------------------------------------------------------------
	private
	logical, parameter, public :: cmd_pROGRESS_ABSOLUTE = .true.
	!--------------------------------------------------------------------
	type, public :: cls_cmd_progress
		integer  , private  :: n , lens , i
		character           :: m = "*" , O = "."
		character(len=64)   :: prefix
		contains
			procedure :: set
			procedure :: put
	end type cls_cmd_progress
	!--------------------------------------------------------------------
	contains
	!--------------------------------------------------------------------
	subroutine set(this, n, L, prefix)
		class(cls_cmd_progress)               :: this
		integer,     intent(in)               :: n , L
		character(len(this%prefix)), optional :: prefix
		this%i    = 0
		this%n    = n
		this%lens = L
		if (present(prefix)) then
			this%prefix = trim(prefix)
		else
			this%prefix = "Progress"
		end if
	end subroutine set
	!--------------------------------------------------------------------
	subroutine put(this, K, bAbsol)
		class(cls_cmd_progress) :: this
		integer, intent(in)     :: K
		Logical, optional       :: bAbsol
		!-----------------------------------------------------------------
		character(len=1)        :: br
		integer                 :: jm
		!-----------------------------------------------------------------
		this%i = this%i + K
		!-----------------------------------------------------------------
		if (present(bAbsol)) then
		  if (bAbsol) this%i = K
		end if
		!-----------------------------------------------------------------
		if ( this%i > this%n ) this%i = this%n    
		!-----------------------------------------------------------------
		jm = nint( real( this%i * this%lens ) / real( this%n ) )
		!-----------------------------------------------------------------
		if ( this%i < this%n ) then
		  br = char(13)
		else
		  br = char(10)
		end if
		!-----------------------------------------------------------------
		!write(*, '(2x, a, ":" , x, "[", 2a, "]", f6.2, "%", a)', advance="no") &
		write(*, '(2x, a, ":", x, "[", 2a, "]", f6.2, "%", a, $)') &
			trim(this%prefix), repeat(this%m, jm), repeat(this%O, this%lens-jm), &
			this%i*100.0/this%n, br
	end subroutine put
	!--------------------------------------------------------------------
end module cmd_progress

!program www_fcode_cn
!  use cmd_progress
!  implicit none
!  type( cls_cmd_progress ) ::progress
!  integer :: i , j
!  call progress % set( n = 1700 , L = 25 )
!  progress % prefix = "Test progress:  "
!  do i = 0 , 1700 , 50
!    call progress % put( i , cmd_pROGRESS_ABSOLUTE ) !
!    !call progress % put( 50 ) 
!    call sleep(1)
!    !End do
!  End do
!  write(*,*) 'End'
!End program www_fcode_cn
