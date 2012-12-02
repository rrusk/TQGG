      SUBROUTINE SPLNDF(xg,sigm,slt,ps,yz,ysz,dyx)
c
        integer nj
        real xg,sigm,ysz,dyx,a,b,del1,del2,dels,exps1
        real sinhd1,coshd1,exps,sinhd2,coshd2,sinhs
      real slt(*), yz(*),ps(*)
c
      a=slt(1)
      b=slt(2)
      nj=2
145   if(xg.le.b)goto 147
        a=b
        nj=nj+1
        b=slt(nj)
        goto 145
147   continue
      del1=xg-a
      del2=b-xg
      dels=b-a
      exps1=exp(sigm*del1)
      sinhd1=.5*(exps1-1./exps1)
      coshd1=.5*(exps1+1./exps1)
      exps=exp(sigm*del2)
      sinhd2=.5*(exps-1/exps)
      coshd2=.5*(exps+1/exps)
      exps=exps*exps1
      sinhs=.5*(exps-1./exps)
      ysz=(yz(nj)*sinhd1+yz(nj-1)*sinhd2)/sinhs+
     &((ps(nj)-yz(nj))*del1+(ps(nj-1)-yz(nj-1))*del2)
     &/dels
      dyx=(yz(nj)*coshd1-yz(nj-1)*coshd2)*sigm/sinhs+
     &(ps(nj)-yz(nj)-ps(nj-1)+yz(nj-1))/dels

      return
      end
