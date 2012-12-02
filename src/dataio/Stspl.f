c----------------------------------------------------------------------

      SUBROUTINE SPLNFIT(nx,si,slpi,slpf,sigm,slt,ps,yz)
c
        integer nx,nm1,np1,i,ibak
        real si,slpi,slpf,sigm,xnm1,delx1,dx1,deln,slpp1,slppn
        real delx2,delx12,c1,c2,c3,delnm1,delnn,dels,exps
        real sinhs,sinhin,diag1,diagin,spdiag,dx2,diag2
        real slt(*),yz(*),ps(*),temp(1000)


      nm1=nx-1
      xnm1=nm1
      np1=nx+1
      delx1=slt( 2)-slt(1 )
      dx1=(ps(2)-ps(1))/delx1
      deln=slt(nx )-slt(nm1)
      sigm=abs(si)*xnm1/(slt(nx)-slt(1))
c determine slopes if necessary
        if(si.lt.0.) go to 50
      slpp1=slpi
      slppn=slpf
c10      sigm=abs(si)*xnm1/(slt(nx)-slt(1))
10      continue
c   perform forward elimination
      dels=sigm*delx1
      exps=exp(dels)
      sinhs=.5*(exps-1./exps)
      sinhin=1./(delx1*sinhs)
      diag1=sinhin*(dels*.5*(exps+1./exps)-sinhs)
      diagin=1./diag1
      yz(1)=diagin*(dx1-slpp1)
      spdiag=sinhin*(sinhs-dels)
      temp(1)=diagin*spdiag
      if(nx.eq.2) go to 30
      do 20 i=2,nm1
        delx2=slt(i+1)-slt(i)
        dx2=(ps(i+1)-ps(i))/delx2
        dels=sigm*delx2
        exps=exp(dels)
        sinhs=.5*(exps-1./exps)
        sinhin=1./(delx2*sinhs)
        diag2=sinhin*(dels*(.5*(exps+1./exps))-sinhs)
        diagin=1./(diag1+diag2-spdiag*temp(i-1))
        yz(i)=diagin*(dx2-dx1-spdiag*yz(i-1))
        spdiag=sinhin*(sinhs-dels)
        temp(i)=diagin*spdiag
        dx1=dx2
        diag1=diag2
20      continue
30      diagin=1./(diag1-spdiag*temp(nm1))
      yz(nx)=diagin*(slppn-dx2-spdiag*yz(nm1))
c    perform back substitution
      do 40 i=2,nx
        ibak=np1-i
        yz(ibak)=yz(ibak)-temp(ibak)*yz(ibak+1)
40      continue
      return
50      if(nx.eq.2) go to 60
c set initial and final slope, if desired
      delx2=slt(3)-slt(2)
      delx12=slt(3)-slt(1)
      c1=-(delx12+delx1)/delx12/delx1
      c2=delx12/delx1/delx2
      c3=-delx1/delx12/delx2
      slpp1=c1*ps(1)+c2*ps(2)+c3*ps(3)
      deln=slt(nx)-slt(nm1)
      delnm1=slt(nm1)-slt(nx-2)
      delnn=slt(nx)-slt(nx-2)
      c1=(delnn+deln)/delnn/deln
      c2=-delnn/deln/delnm1
      c3=deln/delnn/delnm1
      slppn=c3*ps(nx-2)+c2*ps(nm1)+c1*ps(nx)
      go to 10
c use straight line if two points and no derivatives given
60      yz(1) = 0.
      yz(2) = 0.
      return

      end   

c----------------------------------------------------------------------
