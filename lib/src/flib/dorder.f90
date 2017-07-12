SUBROUTINE dorder(n, arr, indx, ntype)
	!----------------------------------------------------------------------------
	! Sort array arr(n). If ntype=-1, inverse
	! indx is the order of array arr(n)
	!
	! For example:
	!
	! Input:
	!
	! n=5
	!      = 1  ,  2  ,  3  ,  4  ,  5
	! arr  = 2.1,  3.4, -1.5,  0.4,  2.5
	!
	! Output:
	!
	! indx = 3  ,  4  ,  1  ,  5  ,  2
	!
	! If ntype = -1, then
	!
	! indx = 2  ,  5  ,  1  ,  4  ,  3
	!
	!----------------------------------------------------------------------------
	use kinds, only: DP
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent(in ) :: n
	integer , intent(in ) :: ntype
	real(DP), intent(in ) :: arr(n)
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer , intent(out) :: indx(n)
	!----------------------------------------------------------------------------
	! Parameters
	!
	integer , parameter   :: M=7
	integer , parameter   :: NSTACK=50
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer               :: i, j, k, l, ii
	integer               :: istack(NSTACK)
	integer               :: ir, indxt, itemp, jstack, swap
	real(DP)              :: a
	!----------------------------------------------------------------------------
	do j=1,n
		indx(j)=j
	end do
	!----------------------------------------------------------------------------
	jstack = 0
	l      = 1
	ir     = n
1  if(ir-l<M)then
		!-------------------------------------------------------------------------
		do j=l+1,ir
			indxt=indx(j)
			a=arr(indxt)
			do i=j-1,l,-1
				if(arr(indx(i)).le.a) goto 2
				indx(i+1)=indx(i)
			end do
			i=l-1
2        indx(i+1)=indxt
		end do
		!-------------------------------------------------------------------------
		if(jstack==0) then
			if (ntype==-1) then
				do ii=1,n/2
					swap=indx(ii)
					indx(ii)=indx(n-ii+1)
					indx(n-ii+1)=swap
				end do
			end if
			return
		end if
		!-------------------------------------------------------------------------
		ir     = istack(jstack)
		l      = istack(jstack-1)
		jstack = jstack-2
	else
		k         = (l+ir)/2
		itemp     = indx(k)
		indx(k)   = indx(l+1)
		indx(l+1) = itemp
		if(arr(indx(l))>arr(indx(ir))) then
			itemp    = indx(l)
			indx(l)  = indx(ir)
			indx(ir) = itemp
		end if
		if(arr(indx(l+1))>arr(indx(ir))) then
			itemp     = indx(l+1)
			indx(l+1) = indx(ir)
			indx(ir)  = itemp
		end if
		if(arr(indx(l))>arr(indx(l+1))) then
			itemp     = indx(l)
			indx(l)   = indx(l+1)
			indx(l+1) = itemp
		end if
		i     = l+1
		j     = ir
		indxt = indx(l+1)
		a     = arr(indxt)
3     continue
		i=i+1
		if(arr(indx(i))<a) goto 3
4     continue
		j=j-1
		if(arr(indx(j))>a) goto 4
		if(j<i)goto 5
		itemp   = indx(i)
		indx(i) = indx(j)
		indx(j) = itemp
		goto 3
5     indx(l+1) = indx(j)
		indx(j)   = indxt
		jstack    = jstack+2
		if(jstack>NSTACK) then
			write(*,*) 'NSTACK too small in indexx'
			write(*,*) "Stop"
		end if
		if(ir-i+1>=j-l)then
			istack(jstack)   = ir
			istack(jstack-1) = i
			ir               = j-1
		else
			istack(jstack)   = j-1
			istack(jstack-1) = l
			l                = i
		endif
	endif
	goto 1
		if (ntype==-1) then
			do ii=1,n/2
				swap         = indx(ii)
				indx(ii)     = indx(n-ii+1)
				indx(n-ii+1) = swap
			end do
		end if
	return
END
