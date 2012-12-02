! --------------------------------------------------------------------------*

      SUBROUTINE DrwFig( change )
!   
! PURPOSE : To draw grid figure onto screen.
! Given   : reset = TRUE if window is to be set to start-up default value,
!                 = FALSE if newly entered window value, from COMMON AREA 
!                   /WLIMIT/, is to be used.
!           change = TRUE if triangle list needs to be updated.
! Returns : None.

      use MainArrays

!     - PASSED PARAMETERS
      LOGICAL change
!      real    x1, x2, y1, y2

      include '../includes/graf.def'
      include '../includes/defaults.inc'

!--------------BEGIN----------------------

      call PigSetWindowNum( MAINWIN )
      call BlankMap
        
      if(DispNodes) then
        call DisplayNodes()
        call DrwVertMarkers()
      else
        call DRWTRI( change )
!     - Mark vertices if enabled. Must be drawn after the grid. S.P
        call DrwVertMarkers()
      endif

      call DisplayPermMarkers

      END   

! --------------------------------------------------------------------------*

      SUBROUTINE DrwTri( change )

! Purpose : To draw polygon for each grid record.
! Given   : nrec - nrec-1 is number of active nodes in the grid to be drawn
!           change - TRUE if triangle list needs to be updated.
! Returns : None.
! Modified: Steve Prestage and Daphne Connolly  May 1989
! Modified: RFH Jan 90 and Aug 92
! Comment : AGD Jun 94. Most of the time in this routine is spent inside
!           PigDrawPolyline
!           which is called with only 2 points very frequently...
!           It is probably possible to optimize this performance by
!           combining multiple edges for drawing.

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/edpolys.inc'
      INCLUDE '../includes/graf.def'

!     - PASSED PARAMETERS
!      INTEGER Nrec
      LOGICAL change

      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

      LOGICAL FULLCOLOUR
      COMMON /SHADE/ FULLCOLOUR

      REAL     CWXL,CWXH,CWYL,CWYH
      COMMON  /CURWIN/ CWXL,CWXH,CWYL,CWYH

      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

!   - Only outline (boundaries) of grid drawn if OUTLINEONLY is .true. 

!   - PolyDisplay indicates polygons to display on redraw.
!       PolyDisplay = 0 = display active polygon only.
!                   = 1 = display all polygons.
!                   = 2 = display NO polygons.
    integer PolyDisplay
    COMMON /POLYSTATUS/ PolyDisplay

!     - LOCAL VARIABLES
      INTEGER ncut
      REAL px(3), py(3)
      INTEGER j, jj, k
      INTEGER nbndx
!        - value in the neighborhood index
      real xj, yj, xnbndx, ynbndx
      real linep1x, linep1y, linep2x, linep2y
      integer nedges
      logical seccolour
      integer PrevColour

!------------------BEGIN--------------------------

      call PigGetLineColour(PrevColour)
      call PigGetWorldCoordinates(CWXL, CWXH, CWYL, CWYH)
      ncut = GridSIndex

      if(FlagC) then
!     Give contours the preference
      if(change) then
        call RemoveNotExist(itot,code,nbtot,nl)
        call Element_Lister(CHANGE, .FALSE. , &
               itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
               x0off,y0off,scaleX,scaleY,igridtype)
        change = .false.
      endif
        call DrwContours()
    else
!     Colour triangles before drawing grid, if full shading
        if ( FULLCOLOUR ) then
        call HiltTrs( change )
        endif
      endif

      if(FlagG) then
!     - draw the grid now
        call PigSetLineColour( GridPColour )
        seccolour = .false.
        IF(itot.gt.MREC) then
            call PigMessageOK('Grid.f itot.gt.MREC','drwtri')
            return
        endif

      DO j = 1, itot !Nrec-1
!      Following checks moved from inner loop mar 1995
!      boundaries when OUTLINEONLY is .TRUE.
          if(OUTLINEONLY.and.(CODE(J).eq.0.or.CODE(J).eq.90))then
!         - ignore this node
!          else if ( EXIST(j) ) then
          else if ( code(j).ge.0 ) then
!        - get neighbor indices
          xj = dxray(j)
          yj = dyray(j)

            if(FlagN) then
!     Draw nodes
          endif

          if( .not.FlagG) then
            cycle
          endif

          if ((.not.seccolour) .and. (j.ge.ncut)  ) then
            seccolour = .true.
            call PigSetLineColour( GridSColour )
          endif
          nedges = 0
          DO k = 1, NBTOTR
            if ( NL(k,j) .gt. j) then
              nbndx = NL(k,j) 

!           Following check introduced 20 Dec 94 to draw connections
!           only to points in defined grid arrays.
!            if(NBNDX.GT.MREC) then
!               - suppress display, because nbndx beyond array definitions
!           Following check introduced March 1995 to draw connections
!           only to points in actual grid
              if(NBNDX.GT.itot) then
!               - suppress display, because nbndx not in actual grid
!           Following check introduced 14 Aug 92 to draw only
!           boundaries when OUTLINEONLY is .TRUE.
              else if((OUTLINEONLY).and.((CODE(NBNDX).eq.0).or.(CODE(NBNDX).eq.90))) then
!               - suppress display, because not a boundary node on line
              else

!             Following 4 checks added 7 Jan 90 to check whether line is totally
!             above, below, to right or to left of screen window - i.e. whether
!             the lines bounding rectangle intersects the screen window
! comment: agd march 1995.
! this is equivalent to testing whether the bounding rectangles of the
! screen and the line intersect.
! there is a faster test for this, given the lower left and upper right
! points for each rectangle. See Cormen et al., Algorithms. p 889.

                xnbndx = DXRAY(nbndx)
                ynbndx = DYRAY(nbndx)
                linep1x = min(xj, xnbndx)
                linep2x = max(xj, xnbndx)
                linep1y = min(yj, ynbndx)
                linep2y = max(yj, ynbndx)
        
                if((linep2x.ge.cwxl).and.(cwxh.ge.linep1x).and.(linep2y.ge.cwyl).and.(cwyh.ge.linep1y) ) then
!                 the bounding rectangles do intersect

                  if(nedges.eq.0) then
                    px(2) = xj
                    py(2) = yj
                    px(1) = xnbndx
                    py(1) = ynbndx
                    nedges = 1
                  else
                    px(3) = xnbndx
                    py(3) = ynbndx
                    call PigDrawPolyline(3, px, py)
                    nedges = 0
                  endif
                endif
              endif
              if(nedges.eq.1) call PigDrawPolyline(2, px, py)
              nedges = 0
            endif
          enddo
          if(nedges.eq.1) call PigDrawPolyline(2, px, py)
          nedges = 0
          endif
        enddo
    endif

      if ( .NOT. FullColour ) then
      call HiltTrs( change )
      endif

!     - DISPLAY OTHER FEATURES AS REQUIRED...

!     - Contours and Boundaries are now drawn AFTER the grid has been drawn
      if(FlagD) then
      call DispContBound
    endif

!     - polygons
!      IF ( PolyDisplay .eq. 0 ) THEN
!       - display active polygon if any
         do j=1,numpolys
           jj = j
           call DisplayPoly ( jj )
         enddo
!      call DisplayPoly ( actvpoly )
!      ELSE IF ( PolyDisplay .eq. 1 ) THEN
!       - display all polygons
!      call AllPolys ( .TRUE. )
!      ELSE IF ( PolyDisplay .eq. 2 ) THEN
!       - remove all polygons from display
!      call AllPolys ( .FALSE. )
!      ENDIF
!       - ( PolyDisplay = 0 )

      call PigSetLineColour(PrevColour)

      END


! --------------------------------------------------------------------------*
! --------------------------------------------------------------------------*
