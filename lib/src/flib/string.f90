MODULE string
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: StrUpCase
   PUBLIC :: StrUpCaseFirst
   PUBLIC :: StrLowCase
   PUBLIC :: number_of_chars
   PUBLIC :: number_of_words
   public :: get_list_number
   public :: get_list_elements
   public :: last_word

   CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
   CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

CONTAINS

   FUNCTION last_word ( Input_String ) RESULT ( Output_String )
     ! -- Argument and result
     CHARACTER( * ), INTENT( IN ) :: Input_String
     CHARACTER( LEN( Input_String ) ) :: Output_String
     CHARACTER( LEN( Input_String ) ) :: tmp_String
     ! -- Local variables
     INTEGER :: n
     !--------------------------------------------------------------------------
     tmp_String=trim(adjustl(Input_String))
     !--------------------------------------------------------------------------
     if (len_trim(tmp_String)==0) then
       Output_String=""
       return
     end if
     !--------------------------------------------------------------------------
     n = INDEX(trim(adjustl(tmp_String))," ",.true.)
     if ( n /= 0 ) then
       Output_String = tmp_String(n+1:len_trim(tmp_String))
     else
       Output_String = tmp_String
     end if
   END FUNCTION last_word

   FUNCTION StrUpCase ( Input_String ) RESULT ( Output_String )
     ! -- Argument and result
     CHARACTER( * ), INTENT( IN ) :: Input_String
     CHARACTER( LEN( Input_String ) ) :: Output_String
     ! -- Local variables
     INTEGER :: i, n

     ! -- Copy input string
     Output_String = Input_String
     ! -- Loop over string elements
     DO i = 1, LEN( Output_String )
       ! -- Find location of letter in lower case constant string
       n = INDEX( LOWER_CASE, Output_String( i:i ) )
       ! -- If current substring is a lower case letter, make it upper case
       IF ( n /= 0 ) Output_String( i:i ) = UPPER_CASE( n:n )
     END DO
   END FUNCTION StrUpCase

   FUNCTION StrUpCaseFirst ( Input_String ) RESULT ( Output_String )
     ! -- Argument and result
     CHARACTER( * ), INTENT( IN ) :: Input_String
     CHARACTER( LEN( Input_String ) ) :: Output_String
     ! -- Local variables
     INTEGER :: i, n

     ! -- Copy input string
     Output_String = Input_String
     ! -- Loop over string elements
     DO i = 1, 1
       ! -- Find location of letter in lower case constant string
       n = INDEX( LOWER_CASE, Output_String( i:i ) )
       ! -- If current substring is a lower case letter, make it upper case
       IF ( n /= 0 ) Output_String( i:i ) = UPPER_CASE( n:n )
     END DO
   END FUNCTION StrUpCaseFirst

   FUNCTION StrLowCase ( Input_String ) RESULT ( Output_String )
     ! -- Argument and result
     CHARACTER( * ), INTENT( IN ) :: Input_String
     CHARACTER( LEN( Input_String ) ) :: Output_String
     ! -- Local variables
     INTEGER :: i, n

     ! -- Copy input string
     Output_String = Input_String
     ! -- Loop over string elements
     DO i = 1, LEN( Output_String )
       ! -- Find location of letter in upper case constant string
       n = INDEX( UPPER_CASE, Output_String( i:i ) )
       ! -- If current substring is an upper case letter, make it lower case
       IF ( n /= 0 ) Output_String( i:i ) = LOWER_CASE( n:n )
     END DO
   END FUNCTION StrLowCase

function number_of_words(words)
	implicit none
	!----------------------------------------------------------------------------
	character(*)           :: words
	integer                :: number_of_words
	!----------------------------------------------------------------------------
	integer                :: ln, i, j, n
	character(1)           :: c
	logical                :: find_cha, find_space
	!----------------------------------------------------------------------------
	find_cha   = .true.
	find_space = .false.
	!----------------------------------------------------------------------------
	n  = 0
	ln = len(trim(words))
	number_of_words = 0
	if (ln==0) return
	!----------------------------------------------------------------------------
	do i=1, ln
		c=words(i:i)
		j=iachar(c)
		!-------------------------------------------------------------------------
		if (find_cha .and. j/=32) then
			n=n+1
			find_cha   = .false.
			find_space = .true.
		end if
		!-------------------------------------------------------------------------
		if (find_space .and. j==32) then
			find_cha   = .true.
			find_space = .false.
		end if
		!-------------------------------------------------------------------------
	end do
	number_of_words=n
	!----------------------------------------------------------------------------
	return
end function
function number_of_chars(words,a)
	implicit none
	!----------------------------------------------------------------------------
	character(*)           :: words
	character(*), optional :: a
	integer                :: number_of_chars
	!----------------------------------------------------------------------------
	integer                :: ln, i, n, jn
	!----------------------------------------------------------------------------
	n     = 0
	ln    = len(trim(words))
	jn    = len(a)
	!----------------------------------------------------------------------------
	number_of_chars=0
	if (ln==0 .or. jn > ln) return
	!----------------------------------------------------------------------------
	i=1
	n=0
	do while (i<=ln)
		!-------------------------------------------------------------------------
		if ( words(i:i+jn-1) == a ) then
			n=n+1
			i=i+jn
		else
			i=i+1
		end if
		!-------------------------------------------------------------------------
	end do
	number_of_chars=n
	!----------------------------------------------------------------------------
	return
end function
!
!-------------------------------------------------------------------------------
!
subroutine get_list_number(c, n)
	implicit none
	!----------------------------------------------------------------------------
	character(*)     , intent( in) :: c
	integer          , intent(out) :: n
	!----------------------------------------------------------------------------
	integer                        :: i, i1, i2
	integer                        :: m
	integer                        :: info
	character(len(c))              :: a
	integer          , external    :: number_of_words
	character(len(c)), allocatable :: lines(:)
	!----------------------------------------------------------------------------
	call str_replace(",", " ", c, a, info)
	m = number_of_words(a)
	allocate(lines(m))
	!----------------------------------------------------------------------------
	read(a, *) lines
	n=0
	do i=1, m
		if (index(lines(i),"-")<=0) then
			n=n+1
		else
			call str_replace("-", " ", lines(i), lines(i), info)
			read(lines(i),*) i1, i2
			n = n + (i2-i1+1)
		end if
	end do 
	!----------------------------------------------------------------------------
	deallocate(lines)
	!----------------------------------------------------------------------------
end subroutine
!-------------------------------------------------------------------------------
subroutine get_list_elements(c, n, idx)
	implicit none
	!----------------------------------------------------------------------------
	character(*)     , intent( in) :: c
	integer          , intent( in) :: n
	integer          , intent(out) :: idx(n)
	!----------------------------------------------------------------------------
	integer                        :: i, i1, i2, j, k
	integer                        :: m
	integer                        :: info
	character(len(c))              :: a
	integer          , external    :: number_of_words
	character(len(c)), allocatable :: lines(:)
	!----------------------------------------------------------------------------
	call str_replace(",", " ", c, a, info)
	m = number_of_words(a)
	allocate(lines(m))
	!----------------------------------------------------------------------------
	read(a, *) lines
	j=0
	do i=1, m
		if (index(lines(i),"-")<=0) then
			j=j+1
			read(lines(i),*) idx(j)
		else
			call str_replace("-", " ", lines(i), lines(i), info)
			read(lines(i),*) i1, i2
			do k=i1, i2
				j=j+1
				idx(j) = k
			end do
		end if
	end do 
	!----------------------------------------------------------------------------
	deallocate(lines)
	!----------------------------------------------------------------------------
end subroutine
!-------------------------------------------------------------------------------
end module string
