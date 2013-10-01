!****************************************************************************

    SUBROUTINE InitEleInfo(index)

    IMPLICIT NONE

    INTEGER :: j,index,nv(4),ec
    real :: xc,yc,zc

  ! Initialize.

    call GetElementInfo( index,xc,yc,zc,ec,nv )
      
    write(*,*) ' index, code=', index, ec
    write(*,*) ' vertices=',(nv(j),j=1,4)
     
    END SUBROUTINE initeleinfo

!****************************************************************************

    SUBROUTINE InitNodeinfo(index)

    IMPLICIT NONE

    INTEGER :: j,index,nv(100),numngh,ec
    real :: xc,yc,zc

  ! Initialize.
      
    call GetNodeInfo( index,xc,yc,zc,ec,numngh,nv )

    write(*,*) ' index, code=', index, ec
    write(*,*) ' x,y,z=', xc, yc, zc
    write(*,*) ' nbrs=',(nv(j),j=1,numngh)
    
    END SUBROUTINE initnodeinfo

!****************************************************************************
