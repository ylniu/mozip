subroutine get_sym_num(n, symbol, sym_num)
	implicit none
	!----------------------------------------------------------------------------
	integer     , intent( in) :: n
	character(*), intent( in) :: symbol(n)
	character(*), intent(out) :: sym_num(n)
	!----------------------------------------------------------------------------
	integer                   :: i
	character(80)             :: fmt
	!----------------------------------------------------------------------------
	if (n<=0) return
	do i=1, n
		write(fmt,*) len(symbol(i))
		write(sym_num(i), '(a'//trim(fmt)//',"(",i0,")")') trim(symbol(i)),i
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
