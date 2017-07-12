module array
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	!
	interface array_sum
		module procedure array_sum_i
		module procedure array_sum_d
	end interface
	!
	!----------------------------------------------------------------------------
	!
	interface array_group_number
		module procedure array_group_number_a
		module procedure array_group_number_i
		module procedure array_group_number_d
	end interface
	!
	!----------------------------------------------------------------------------
	!
	interface array_group
		module procedure array_group_a
		module procedure array_group_i
		module procedure array_group_d
	end interface
	!
	!----------------------------------------------------------------------------
	!
	interface in_array
		module procedure in_array_i
		module procedure in_array_d
		module procedure in_array_a
	end interface
	!
	!----------------------------------------------------------------------------
	!
	contains
	!
	!----------------------------------------------------------------------------
	!
	function in_array_i(b, n, a) result(m)
		implicit none
		integer, intent(in) :: n
		integer, intent(in) :: b, a(n)
		logical             :: m
		integer             :: i
		!-------------------------------------------------------------------------
		m=.false.
		do i=1, n
			if (b==a(i)) then
				m=.true.
				exit
			end if
		end do
		!-------------------------------------------------------------------------
		return
		!-------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function in_array_d(b, n, a) result(m)
		implicit none
		integer , intent(in) :: n
		real(DP), intent(in) :: b, a(n)
		logical              :: m
		integer              :: i
		!-------------------------------------------------------------------------
		m=.false.
		do i=1, n
			if (b==a(i)) then
				m=.true.
				exit
			end if
		end do
		!-------------------------------------------------------------------------
		return
		!-------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function in_array_a(b, n, a) result(m)
		implicit none
		integer     , intent(in) :: n
		character(*), intent(in) :: b, a(n)
		logical             :: m
		integer             :: i
		!-------------------------------------------------------------------------
		m=.false.
		do i=1, n
			if (trim(b)==trim(a(i))) then
				m=.true.
				exit
			end if
		end do
		!-------------------------------------------------------------------------
		return
		!-------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function array_sum_i(n,a) result(m)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer    , intent( in) :: n
		integer    , intent( in) :: a(n)
		integer                  :: m
		!-------------------------------------------------------------------------
		integer                  :: i
		!-------------------------------------------------------------------------
		m = 0
		do i=1, n
			m = m + a(i)
		end do
		!-------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function array_sum_d(n,a) result(m)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer    , intent( in) :: n
		real(DP)   , intent( in) :: a(n)
		real(DP)                 :: m
		!-------------------------------------------------------------------------
		integer                  :: i
		!-------------------------------------------------------------------------
		m = 0.0_DP
		do i=1, n
			m = m + a(i)
		end do
		!-------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function array_group_number_a(n,a) result(m)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		character(*), intent( in) :: a(n)
		integer                   :: m
		!-------------------------------------------------------------------------
		integer                   :: i, j
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m = 0
		do i=1, n
			if (.not.checked(i)) then
				m = m + 1
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	function array_group_number_d(n,a) result(m)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		real(DP)    , intent( in) :: a(n)
		integer                   :: m
		!-------------------------------------------------------------------------
		integer                   :: i, j
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m = 0
		do i=1, n
			if (.not.checked(i)) then
				m = m + 1
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	function array_group_number_i(n,a) result(m)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		integer     , intent( in) :: a(n)
		integer                   :: m
		!-------------------------------------------------------------------------
		integer                   :: i, j
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m = 0
		do i=1, n
			if (.not.checked(i)) then
				m = m + 1
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	subroutine array_group_a(n, a, ng, n1, a1)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		character(*), intent( in) :: a(n)
		integer     , intent( in) :: ng
		integer     , intent(out) :: n1(ng)
		character(*), intent(out) :: a1(ng)
		!-------------------------------------------------------------------------
		integer                   :: i, j
		integer                   :: m
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m  = 0
		n1 = 0
		do i=1, n
			if (.not.checked(i)) then
				m     = m + 1
				n1(m) = 1
				a1(m) = a(i)
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						n1(m) = n1(m) + 1
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end subroutine
	!
	!----------------------------------------------------------------------------
	!
	subroutine array_group_d(n, a, ng, n1, a1)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		real(DP)    , intent( in) :: a(n)
		integer     , intent( in) :: ng
		integer     , intent(out) :: n1(ng)
		real(DP)    , intent(out) :: a1(ng)
		!-------------------------------------------------------------------------
		integer                   :: i, j
		integer                   :: m
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m  = 0
		n1 = 0
		do i=1, n
			if (.not.checked(i)) then
				m     = m + 1
				n1(m) = 1
				a1(m) = a(i)
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						n1(m) = n1(m) + 1
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end subroutine
	!
	!----------------------------------------------------------------------------
	!
	subroutine array_group_i(n, a, ng, n1, a1)
		!-------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		integer     , intent( in) :: n
		integer     , intent( in) :: a(n)
		integer     , intent( in) :: ng
		integer     , intent(out) :: n1(ng)
		integer     , intent(out) :: a1(ng)
		!-------------------------------------------------------------------------
		integer                   :: i, j
		integer                   :: m
		logical     , allocatable :: checked(:)
		!-------------------------------------------------------------------------
		allocate(checked(n))
		!-------------------------------------------------------------------------
		checked = .false.
		m  = 0
		n1 = 0
		do i=1, n
			if (.not.checked(i)) then
				m     = m + 1
				n1(m) = 1
				a1(m) = a(i)
				do j=i+1, n
					if (.not.checked(j) .and. a(j)==a(i) ) then
						n1(m) = n1(m) + 1
						checked(j) = .true.
					end if
				end do
			end if
		end do
		!-------------------------------------------------------------------------
		deallocate(checked)
		!-------------------------------------------------------------------------
		return
	end subroutine
	!----------------------------------------------------------------------------
end module
