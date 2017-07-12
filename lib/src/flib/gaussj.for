      SUBROUTINE gaussj(a,n,np,b,m,mp)
      use kinds, only: DP
      INTEGER m,mp,n,np,NMAX
      REAL(DP) a(np,np),b(np,mp)
		real(DP) c(np,np)
		real(DP) g(np,np)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
		integer ii, jj
      REAL(DP) big,dum,pivinv
		integer :: fid
      !-----------------------------------------------------------------
		call get_free_fid(fid)
		open(fid, file="debug_gaussj.dat")
      !-----------------------------------------------------------------
      do j=1,n
        ipiv(j)=0
      end do
      !-----------------------------------------------------------------
		c=0.0_DP
		do j=1,n
			c(j,j) = 1.0_DP
		end do
      !-----------------------------------------------------------------
			write(fid,*) "Matrix0"
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') a(ii,jj)
				end do
				write(fid,'("  ",$)')
				do jj=1, m
					write(fid,'(es14.6, $)') b(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') c(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
      !-----------------------------------------------------------------
      do i=1,n
        big=0.0_DP
        !---------------------------------------------------------------
        do j=1,n
          if(ipiv(j)/=1)then
            !-----------------------------------------------------------
            do k=1,n
              if (ipiv(k)==0) then
                !-------------------------------------------------------
                ! Find the largest element
                !
                if (abs(a(j,k))>=big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
                !-------------------------------------------------------
              endif
            end do
            !-----------------------------------------------------------
          endif
        end do
        !---------------------------------------------------------------
        ipiv(icol)=ipiv(icol)+1
			write(fid,'("irow, icol", 2i4)') irow, icol
        if (irow/=icol) then
          do l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
          end do
          do l=1,n
            dum=c(irow,l)
            c(irow,l)=c(icol,l)
            c(icol,l)=dum
          end do
          do l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
          end do
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol)==0.0_DP) then
          write(*,*) 'singular matrix in gaussj'
          call exit(1)
        end if
        pivinv=1.0D0/a(icol,icol)
        !a(icol,icol)=1.0D0
        do l=1,n
          a(icol,l)=a(icol,l)*pivinv
        end do
        do l=1,n
          c(icol,l)=c(icol,l)*pivinv
        end do
        do l=1,m
          b(icol,l)=b(icol,l)*pivinv
        end do

			a(icol,icol)=1.0D0

			write(fid,*) "Matrix1"
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') a(ii,jj)
				end do
				write(fid,'("  ",$)')
				do jj=1, m
					write(fid,'(es14.6, $)') b(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') c(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)

        do ll=1,n
          if(ll/=icol)then
            dum=a(ll,icol)
            !a(ll,icol)=0.0D0
            do l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
            end do
            do l=1,n
              c(ll,l)=c(ll,l)-c(icol,l)*dum
            end do
            do l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
            end do
				a(ll,icol)=0.0D0
          endif
        end do
			write(fid,*) "Matrix2"
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') a(ii,jj)
				end do
				write(fid,'("  ",$)')
				do jj=1, m
					write(fid,'(es14.6, $)') b(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') c(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
      end do
			write(fid,*) "Matrix3"
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') a(ii,jj)
				end do
				write(fid,'("  ",$)')
				do jj=1, m
					write(fid,'(es14.6, $)') b(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') c(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
			g=matmul(a,c)
			write(fid,*) "Matrix4"
			do ii=1, n
				do jj=1, n
					write(fid,'(es14.6, $)') g(ii,jj)
				end do
				write(fid,*)
			end do
			write(fid,*)
		close(fid)
      !-----------------------------------------------------------------
      do l=n,1,-1
        if(indxr(l)/=indxc(l))then
          do k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
          end do
        endif
      end do
      !-----------------------------------------------------------------
		a=c
      !-----------------------------------------------------------------
      return
      END
