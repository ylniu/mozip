subroutine sort_index(n,arr,indx,itype)
	!----------------------------------------------------------------------------
	! itype =  1:   1,   2,   3, .....
	! itype = -1:   n, n-1, n-2, .....
	!----------------------------------------------------------------------------
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer , parameter   :: M      = 7
	integer , parameter   :: NSTACK = 50
	!----------------------------------------------------------------------------
	integer , intent( in)  :: n
	integer , intent( in)  :: itype
	real(DP), intent( in)  :: arr(n)
	integer , intent(out)  :: indx(n)
	!----------------------------------------------------------------------------
	integer                :: i,j,k,l
	integer                :: indxt,ir,itemp,jstack
	integer                :: istack(NSTACK)
	real(DP)               :: a
	integer                :: i1
	integer                :: indx1(n)
	!----------------------------------------------------------------------------
	do j=1,n
		indx(j)=j
	end do
	jstack=0
	l=1
	ir=n
1	if(ir-l.lt.M)then
		do j=l+1,ir
			indxt=indx(j)
			a=arr(indxt)
			do i=j-1,l,-1
				if(arr(indx(i)).le.a) goto 2
				indx(i+1)=indx(i)
			end do
			i=l-1
2			indx(i+1)=indxt
		end do
		if(jstack.eq.0) then
			if (itype==-1) then
				do i1=1, n
					indx1(i1) = indx(n-i1+1)
				end do
				indx=indx1
			end if
			return
		end if
		ir=istack(jstack)
		l=istack(jstack-1)
		jstack=jstack-2
	else
		k=(l+ir)/2
		itemp=indx(k)
		indx(k)=indx(l+1)
		indx(l+1)=itemp
		if(arr(indx(l)).gt.arr(indx(ir)))then
			itemp=indx(l)
			indx(l)=indx(ir)
			indx(ir)=itemp
		end if
		if(arr(indx(l+1)).gt.arr(indx(ir)))then
			itemp=indx(l+1)
			indx(l+1)=indx(ir)
			indx(ir)=itemp
		end if
		if(arr(indx(l)).gt.arr(indx(l+1)))then
			itemp=indx(l)
			indx(l)=indx(l+1)
			indx(l+1)=itemp
		end if
		i=l+1
		j=ir
		indxt=indx(l+1)
		a=arr(indxt)
3		continue
		i=i+1
		if(arr(indx(i)).lt.a)goto 3
4		continue
		j=j-1
		if(arr(indx(j)).gt.a)goto 4
		if(j.lt.i)goto 5
		itemp=indx(i)
		indx(i)=indx(j)
		indx(j)=itemp
		goto 3
5		indx(l+1)=indx(j)
		indx(j)=indxt
		jstack=jstack+2
		if(jstack.gt.NSTACK) then
			write(*,*) "NSTACK too small in sort_array"
			write(*,*) "Stop"
			stop
		end if
		if(ir-i+1.ge.j-l)then
			istack(jstack)=ir
			istack(jstack-1)=i
			ir=j-1
		else
			istack(jstack)=j-1
			istack(jstack-1)=l
			l=i
		end if
	end if
	goto 1
	if (itype==-1) then
		do i1=1, n
			indx1(i1) = indx(n-i1+1)
		end do
		indx=indx1
	end if
	return
end subroutine
