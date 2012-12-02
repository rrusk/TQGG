
    Module MainArrays

!     Variable names

!  maxnpts   - maximum no. of vertices (nodes) allowed in grid
!  maxNBTOT  - max. no. of neighbours allowed for each node

!    Associated variables which are used in routines:
!         NREC   - actual no. of vertices in grid
!         NBTOTR - actual max. no. of neighbours

!  (I        - vertex index number      I = 1,..,NREC)
!  (J        - neighbour number         J = 1,..,NBTOTR)
!  DXRAY(I) - x-coord. of vertex I 
!  DYRAY(I) - y-coord.
!  CODE(I)  - computational code
!  DEPTH(I) - water depth at vertex
!  NB(J,I) - index nos. of neighbours
!  ITOT = NREC 

      integer, parameter :: maxnpts=1000000, nbtot=20
      integer, parameter :: mrec=maxnpts, maxtri=2*maxnpts+1
      integer, parameter :: MAXSTRNODES = 60000, MAXMARKS = 50
      integer, parameter :: maxpts = mrec
      
      INTEGER ITOT  
      INTEGER NBTOTR

      INTEGER NL(NBTOT,maxnpts+1)
      REAL    DEPTH(maxnpts+1)
!      LOGICAL EXIST(maxnpts+1)
      INTEGER CODE(maxnpts+1)
      REAL DXRAY(maxnpts+4)
      REAL DYRAY(maxnpts+4)

!-----------------------------------------------------------------------*
! - DEFINITIONS
!       TotBndys = total # of boundaries in data.
!       PtsThisBnd(Maxnnb) = # of points on a given boundary in data.
!       Maxnnb = max # of boundaries allowed 
!       TotIntPts = total # of interior ( non-boundary ) points in data.
!       TotCoords = total # of coordinates ( boundary & interior )
!                   in data.
!-----------------------------------------------------------------------*

      integer, parameter :: maxnnb=10000
      integer PtsThisBnd(Maxnnb)
      integer TotBndys,TotIntBndys,TotIntPts,TotCoords

!-----------------------------------------------------------------------*

! - DEFINITIONS
!        x0off = offset in longitude for grid file
!        y0off = offset in latitude for grid file
!        xlong0 = longitude of display center for polar coordinates
!-----------------------------------------------------------------------*
      real*8 x0off, y0off, scaleX, scaleY
      real xlong0, xlongsum, xlongmin, xlongmax
      integer igridtype
!      common /polarxf/ x0off,y0off,xlong0,xlongsum,xlongmin,xlongmax,       &
!     &                  scaleX, scaleY,igridtype

! ***************************************************************
!          TotTr = total number of triangles
! was this...TrList( , ) = vertices of triangles in ccw order
!          ListTr( , ) = vertices of triangles in ccw order
!          TCode( )    = material type (code) for a triangle
!          CritLt( )   = storage for values of triangle criteria
! ***************************************************************

      INTEGER      TotTr, ListTr(4,maxtri), TCode(maxtri)
      REAL         CritLt(maxtri)

! ***************************************************************

    End Module
