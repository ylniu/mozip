      SUBROUTINE laguer(a,m,x,its)
      use kinds, only:DP
      INTEGER m,its,MAXIT,MR,MT
      REAL(DP) EPSS
      COMPLEX(DP) a(m+1),x
      PARAMETER (EPSS=2.e-10_DP,MR=8,MT=10,MAXIT=MT*MR)
      INTEGER iter,j
      REAL(DP) abx,abp,abm,err,frac(MR)
      COMPLEX(DP) dx,x1,b,d,f,g,h,sq,gp,gm,g2
      SAVE frac
      DATA frac /.5_DP,.25_DP,.75_DP,.13_DP,.38_DP,.62_DP,.88_DP,1.0_DP/
      do 12 iter=1,MAXIT
        its=iter
        b=a(m+1)
        err=abs(b)
        d=cmplx(0.0_DP,0.0_DP,DP)
        f=cmplx(0.0_DP,0.0_DP,DP)
        abx=abs(x)
        do 11 j=m,1,-1
          f=x*f+d
          d=x*d+b
          b=x*b+a(j)
          err=abs(b)+abx*err
11      continue
        err=EPSS*err
        if(abs(b).le.err) then
          return
        else
          g=d/b
          g2=g*g
          h=g2-2.*f/b
          sq=sqrt((m-1)*(m*h-g2))
          gp=g+sq
          gm=g-sq
          abp=abs(gp)
          abm=abs(gm)
          if(abp.lt.abm) gp=gm
          if (max(abp,abm).gt.0.0_DP) then
            dx=m/gp
          else
            dx=exp(cmplx(log(1.+abx),float(iter),DP))
          endif
        endif
        x1=x-dx
        if(x.eq.x1)return
        if (mod(iter,MT).ne.0) then
          x=x1
        else
          x=x-dx*frac(iter/MT)
        endif
12    continue
      write(*,*) 'too many iterations in laguer'
      write(*,*) 'Stop'
      call exit(1)
      return
      END
