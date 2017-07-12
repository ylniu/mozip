subroutine write_n_char(fid,string,n)
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	integer      ,   intent(in) :: fid, n
	character(*) ,   intent(in) :: string
	!----------------------------------------------------------------------------
	! local  variable
	integer                        :: i
	character(100)                 :: fmt
	!----------------------------------------------------------------------------
	! gfortran format
	write(fmt,'( "(",I0,"a",I0,")" )') n, len(string)
	write(fid,fmt) (string,i=1,n)
	! ifort format
	! write(fid,'(<n>a<len(string)>)') (string,i=1,n)
	return
end subroutine
