  !***********************************************************************
  !    Copyright (C) 1995-
  !        Roy A. Walters, R. Falconer Henry
  !
  !        rawalters@shaw.ca
  !
  !    This file is part of TQGG, Triangle-Quadrilateral Grid Generation,
  !    a grid generation and editing program.
  !
  !    TQGG is free software; you can redistribute it and/or
  !    modify it under the terms of the GNU General Public
  !    License as published by the Free Software Foundation; either
  !    version 3 of the License, or (at your option) any later version.
  !
  !    This program is distributed in the hope that it will be useful,
  !    but WITHOUT ANY WARRANTY; without even the implied warranty of
  !    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  !    General Public License for more details.
  !
  !    You should have received a copy of the GNU General Public
  !    License along with this program; if not, write to the Free Software
  !    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  !    USA, or see <http://www.gnu.org/licenses/>.
  !***********************************************************************

C-----------------------------------------------------------------------*
C                       NODERESL.FOR                                    *
C       This module contains routines for re-selection of boundary      *
C       nodes from a digitized (DIGIT format) boundary file.            *
C       ROUTINES: ReSelBndNodes, ReSelInfo, PickEndPoints, FindBndBlock,*
C                 ReSelectNodes, DigEndPts, DigPtsOrder, DigPtsNoOrder, *
C                 ReSelectC, ReSelectNC, AdvanceReSel, SaveReSel.       *
C       FUNCTIONS: Closer.                                              *
C-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*

      SUBROUTINE ReSelBndNodes(MouseX,MouseY,FirstPoint,NextPoint)

C PURPOSE: Control routine for boundary node reselection routines.
C   GIVEN: None.
C RETURNS: None.
C EFFECTS: Calls routines to perform reselection.
C-----------------------------------------------------------------------*

      use MainArrays

! *** passed variables
      real MouseX, MouseY
      logical FirstPoint, NextPoint

C - COMMON BLOCKS
C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C - LOCAL VARIABLES
      integer ierr, ndx, nodetype,option
      integer, save :: indxs(2), bnd, contighalf
      LOGICAL success, valid, saved

C-----------------START ROUTINE----------------------------------------

C       - get 2 delimiting nodes to mark section
      if(FirstPoint) then
        ierr = 0
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( ierr .eq. 1 ) THEN
C               - valid node chosen, save it & put marker
          indxs(1) = ndx
          call PigDrawModifySymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .true.
          return
        else
          return
        endif
      elseif(NextPoint) then
        ierr = 0
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( (ierr .eq. 1).and.(ndx .ne. indxs(1)) ) THEN
C               - valid node chosen, save it & put marker
          indxs(2) = ndx
          call PigDrawModifySymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .false.
          call Check2Nodes( bnd, indxs, valid )
          if(.not.valid) then
            return
          endif
        else
          return
        endif
      endif

      IF ( indxs(1) .gt. indxs(2) ) THEN
        ndx = indxs(1)
        indxs(1) = indxs(2)
        indxs(2) = ndx
      ENDIF

      dpth1 = depth ( indxs(1) )
      dpth2 = depth ( indxs(2) )
      call PickBndHalf ( bnd, indxs, contighalf )
!      call PickEndPoints ( bnd, indxs, contighalf, success )

! open input file
      option = 0
      call ReselectSetup (option)
      call PickEndPoints ( bnd, indxs, contighalf, success )

! reselect nodes
      do
        call ReSelectNodes(success)
!               - store reselection if user wants
        if(success) then 
          saved = .FALSE.
          call SaveReSel ( saved )
          IF ( .not.saved ) THEN
            option = 1
            call ReselectSetup (option)
          else
            exit
          ENDIF
        else
          exit
        endif
      enddo

      RETURN
      END

C-----------------------------------------------------------------------*

      SUBROUTINE ReselectSetup (option)

C PURPOSE: To control the information screen in the right hand panel for 
C          entering the ReSelBndNodes parameters, and a menu to
C          invoke routines to pick delimiting endpoints for nodes to
C          be replaced by reselection, and to perform the reselection.
C   GIVEN: In Common in file NODEDEF.INC;
C               BoundFName = default DIGIT file to use if not equal to 'NONE'.
C RETURNS: In Common RESEL;
C               digfile = name of DIGIT file to reselect from,
C               sdist = sample rate distance, if reselecting by distance,
C               srate = sample rate, if reselecting by every Nth point,
C               digX(2), digY(2) = coordinates of digit points delimiting
C                                  boundary stretch to sample from,
C               nodes() = indices of nodes in data arrays delimiting stretch
C                         of nodes to replace with reselected points.
C               Nth = TRUE if reselection by every nth point,
C                   = FALSE if reselection by distance between points.
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               order = TRUE if user indicates that NODE file boundary blocks
C                       are in the same order as DIGIT file boundary blocks,
C               contig = 1, if contiguous section of boundary block is to be
C                        reselected from,
C                      = 2, if non-contiguous section,
C                      = 0, if neither half, ie: no reselection to take place.
C EFFECTS: An entry screen is created in the right hand panel for the user
C          to enter boundary node reselection parameters. Default values 
C          for all parameters are given initially. Reselection may be 
C          performed.
C WRITTEN: July 1990 by JDM for NODER.
C MODIFIED: OCT91 - JDM - Segments removed, PANELMOD.FOR implementation.
C----------------------------------------------------------------------*

      use MainArrays

C - PARAMETERS (constants)
      REAL*4 seconds
      PARAMETER (seconds = 2.0)

C - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
      REAL sdist, digX(2), digY(2), blkdpth
      integer srate, nodes(2), bndsec, contig
      LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - PREVENDPTS stores coordinates of previous endpoints picked if any
      REAL prevendx, prevendy
      integer prevbnd
      COMMON /PREVENDPTS/ prevendx(2), prevendy(2), prevbnd


      REAL  xarr(maxstrnodes), yarr(maxstrnodes)
      common /maxlocal/ xarr, yarr, numinhalf
      integer numinhalf

C       - BNDCONN stores indication of when to show boundary connections
        LOGICAL showbndconn
!       COMMON /BNDCONN/ showbndconn

C - LOCAL VARIABLES
      CHARACTER*80 cstr
      CHARACTER*1 vartype
      REAL dumr, defsdist
      integer dumi, defsrate, hitnum, option
      LOGICAL filein, endptsin
      logical PigGetOpenFileName
C------------------START ROUTINE---------------------------------------

      if(option.eq.0) then
C       - initialize defaults
        showbndconn = .true.
        defsdist = 1.0
        defsrate = 10
        filein = .FALSE.
        DispBound = .FALSE.
        IF ( BoundFName(1:4) .ne. 'NONE' )  filein = .TRUE.
        endptsin = .FALSE.

C         - assign defaults
        digtype = .false.
        digfile = BoundFName(1:80)
        sdist = defsdist
        srate = defsrate
        Nth = .TRUE.
        order = .FALSE.

        IF ( digtype ) THEN
C           - get input digit file name
          if(.not.PigGetOpenFileName('Open DIGIT File', digfile,
     +          'Digit Files (*.dig),*.dig;All Files (*.*),*.*;')
     +          ) then
            filein = .FALSE.
            DispBound = .FALSE.
            BoundFName = 'NONE'
          ELSE
C             - digit file specified
            filein = .TRUE.
            BoundFName = digfile
          ENDIF
        ELSE
C           - get input node file name
          if(.not.PigGetOpenFileName('Open NODE File', digfile,
     +          'Node Files (*.nod),*.nod;All Files (*.*),*.*;')
     +          ) then
            filein = .FALSE.
            DispBound = .FALSE.
            BoundFName = 'NONE'
          ELSE
C             - digit file specified
            filein = .TRUE.
            BoundFName = digfile
          ENDIF
        ENDIF
      endif

       hitnum = 1
          IF ( hitnum .eq. 1 ) THEN
C             - get srate
            cstr = 'Enter N, For ReSelection Of Every ' //
     +                    'Nth Point (<RTN> for default):'
            vartype = 'I'
            call InputRealInt( srate, dumr, vartype, cstr )
            IF ( vartype .eq. 'D' ) THEN
              srate = defsrate
            ENDIF
            Nth = .TRUE.
C               - indicate with * that reselection will be by every Nth point
          ELSE IF ( hitnum .eq. 2 ) THEN
C             - get sdist
            cstr = 'Enter Distance Between ReSelected Points, ' //
     +                    '<RTN> for default:'
            vartype = 'R'
            call InputRealInt( dumi, sdist, vartype, cstr )
            IF ( vartype .eq. 'D' ) THEN
              sdist = defsdist
            ENDIF
            Nth = .FALSE.
          endif

C             - toggle same order flag
!          order = .NOT. order

      RETURN
      END

C-----------------------------------------------------------------------*
        SUBROUTINE PickEndPoints (  nodebnd, indxs, contighalf, picked )
C PURPOSE: Control routine to pick the node & digit end points defining a 
C          stretch of boundary that will be reselected.
C   GIVEN: In Common RESEL;
C               order = TRUE if user indicated that boundary blocks in
C                       DIGIT file & NODE file are in the same order,
C               digfile = name of DIGIT file to search for endpoints.
C RETURNS: picked = TRUE if NODE endpoints picked successfully and matched
C                   with a pair of DIGIT points, else = FALSE,
C          In Common RESEL;
C               nodes() = indices to data arrays of nodes defining endpoints,
C               contig = 1, if contiguous section,
C                      = 2, if non-contiguous section,
C                      = 0, if neither half was selected,
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays

C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

C - PASSED VARIABLES
      LOGICAL picked
      integer indxs(2), contighalf

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - PREVENDPTS stores coordinates of previous endpoints picked if any
      REAL prevendx, prevendy
      integer prevbnd
      COMMON /PREVENDPTS/ prevendx(2), prevendy(2), prevbnd

C - LOCAL VARIABLES
      integer nodebnd, i, offset, bpfirst, bplast
      CHARACTER*80 cstr
      LOGICAL valid, digok

C-----------------START ROUTINE----------------------------------------

C       - get 2 node endpoints
      picked = .FALSE.
!      call Pick2Nodes ( nodebnd, nodes, valid )
C       -nodes() are returned with nodes(1) < nodes(2)
      valid = .true.
      IF ( valid ) THEN
C         - valid nodes were selected
C         -- assign coordinates of nodes to Common PREVENDPTS
      nodes(1) = indxs(1)
      nodes(2) = indxs(2)
        prevendx(1) = dxray(nodes(1))
        prevendy(1) = dyray(nodes(1))
        prevendx(2) = dxray(nodes(2))
        prevendy(2) = dyray(nodes(2))
        prevbnd = nodebnd
C         -- determine boundary half to reselect
C         -- will set values for straddidx(1) & (2) if non-contig half
!        call PickBndHalf ( nodebnd, nodes, contig )
      contig = contighalf
        IF ( contig .ne. 0 ) THEN
C           - a boundary half was picked
          dpth1 = depth(nodes(1))
          dpth2 = depth(nodes(2))
          picked = .TRUE.
          bndsec = nodebnd
C           - assign coordinates of nodes to digit points to search for
          digX(1) = dxray(nodes(1))
          digY(1) = dyray(nodes(1))
          digX(2) = dxray(nodes(2))
          digY(2) = dyray(nodes(2))
C           - test for end points = 1st & last nodes in boundary & contig = 2
          IF ( contig .eq. 2 ) THEN
C             - determine indices of 1st & last point in boundary
            IF ( nodebnd .lt. 2 ) THEN
C               - outer boundary
                bpfirst = 1

              pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

                bplast = PtsThisBnd(nodebnd)
            ELSE
C               - an island boundary
                bpfirst = 0

              pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

                DO i = 1, nodebnd - 1
                  bpfirst = bpfirst + PtsThisBnd(i)
                END DO
                bpfirst = bpfirst + 1
                bplast = bpfirst + PtsThisBnd(nodebnd) - 1
            ENDIF
C               - ( nodebnd < 2 )
C             - now test 1st & last nodes against endpoints
            IF ( (nodes(1) .eq. bpfirst) .AND.
     +             (nodes(2) .eq. bplast) ) THEN
                    picked = .FALSE.
                    cstr='First & Last Boundary Nodes Selected, No '//
     +                 ' Room To ReSelect This Section.'
                    call PigMessageOK ( cstr,'resl' )
!                   call PigUwait ( seconds * 2.0 )
!                   call PigEraseMessage
C               - now restore display, remove endpoints
                    call PigSetWindowNum ( MAINWIN )
                    call PigSetSymbolNumber ( SQUARE )
                call PigSetSymbolColour ( backgr )
                call PigDrawSymbols ( 2, prevendx, prevendy )
C               - redraw boundary
                call PigSetSymbolNumber ( NodeMType )
                call PigSetSymbolColour ( NodeBColor )
                DO i = bpfirst, bplast
                  call PigDrawSymbols ( 1, dxray(i), dyray(i) )
                END DO
            ENDIF
C               - ( nodes(1) = bpfirst & nodes(2) = bplast )
          ENDIF
C             - ( contig = 2 )
        ELSE
C           - a boundary half was not picked
          picked = .FALSE.
C           - remove endpoints
          call PigSetWindowNum ( MAINWIN )
          call PigSetSymbolNumber ( SQUARE )
          call PigSetSymbolColour ( backgr )
          call PigDrawSymbols ( 2, prevendx, prevendy )
        ENDIF
C           - ( contig ne 0 )
      ENDIF
C         - ( valid )

      IF ( picked ) THEN
C         - node endpoints picked, determine digit endpoints
        cstr = 'Locating DIGIT End Points.'
        call PigPutMessage ( cstr )
        digok = .FALSE.
        call DigEndPts ( digok )
        picked = digok
        IF ( .NOT. digok ) THEN
C           - restore display, redraw whole boundary
          offset = 0
          IF ( nodebnd .gt. 1 ) THEN

              pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

            DO i = 1, nodebnd - 1
                offset = offset + PtsThisBnd(i)
            END DO
          ENDIF
C             - ( nodebnd > 1 )
          call PigSetWindowNum ( MAINWIN )
          call PigSetSymbolNumber ( NodeMType )
          call PigSetSymbolColour ( NodeBColor )

          pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

          DO i = 1, PtsThisBnd(nodebnd)
            call PigDrawSymbols ( 1, dxray(i+offset), dyray(i+offset) )
          END DO
        ENDIF
C           - ( NOT digok )
        call PigEraseMessage
      ENDIF
C         - ( picked )  

      return
      end

C-----------------------------------------------------------------------*
      SUBROUTINE DigEndPts ( ok )
C PURPOSE: To match 2 DIGIT file boundary points with a pair of nodes
C          previously selected.
C   GIVEN: In Common RESEL;
C               digX(1), digY(1) = coordinates of one of the nodes to be 
C                                  matched with a digit point,
C               digX(2), digY(2) = coordinates of the other node to be 
C                                  matched with a digit point,
C               bndsec = boundary section # in NODE file where digX(), digY()
C                        occur ( see also RETURNS: ),
C               contig = 1, if contiguous section,
C                      = 2, if non-contiguous section,
C                      = 0, if neither half was selected,
C               digfile = name of DIGIT file to search for endpoints.
C               order = TRUE if NODE & DIGIT file boundary blocks are in the
C                       same order, = FALSE if not same order.
C RETURNS: ok = TRUE if DIGIT points successfully matched with coordinates
C               in digX(), digY(), user will confirm this,
C          In Common RESEL;
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

C - PASSED VARIABLES
      LOGICAL ok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C - LOCAL VARIABLES
        REAL nearx(2), neary(2), showx(2), showy(2)
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo
      LOGICAL digfound

C-------------------START ROUTINE----------------------------------

      ok = .FALSE.

      IF ( order ) THEN
C         - NODE & DIGIT files are in same order
          if(digtype) then
          call DigPtsOrder ( nearx, neary, digfound )
          else
          call NodPtsOrder ( nearx, neary, digfound )
          endif
      ELSE
C         - NODE & DIGIT files not in same order
          if(digtype) then
          call DigPtsNoOrder ( nearx, neary, digfound )
          else
          call NodPtsNoOrder ( nearx, neary, digfound )
          endif
      ENDIF
C         - ( order )

      IF ( digfound ) THEN
C         - display the digit coords found
        call PigSetWindowNum ( MAINWIN )
        call PigSetSymbolColour ( white )
        call PigSetSymbolNumber ( NodeMType )
        showx(1) = nearx(1)
        showy(1) = neary(1)
        showx(2) = nearx(2)
        showy(2) = neary(2)
        call PigDrawSymbols ( 2, showx, showy )
C         - confirm digit endpoints appear OK
        cstr = 'Are White DIGIT endpoints close enough to node ' //
     +                                  'endpoints ?:'
c         cstr = 'Are White DIGIT EndPoints Close Enough To Node ' //
c     +                                 'EndPoints (Y/N)?'
          ans = PigCursYesNo (cstr)
        call PigEraseMessage
        IF ( ans(1:1) .eq. 'Y' ) THEN
          digfound = .TRUE.
        ELSE
C           - digit endpoints not close enough to node endpoints
          digfound = .FALSE.
C           - remove node endpoints which are still stored in digX(), digY()
          call PigSetWindowNum ( MAINWIN )
          call PigSetSymbolColour ( backgr )
          call PigSetSymbolNumber ( SQUARE )
          call PigDrawSymbols ( 1, digX(1), digY(1) )
          call PigDrawSymbols ( 1, digX(2), digY(2) )
        ENDIF
C           - ( ans = Y )
C         - remove digit endpoints
        call PigSetWindowNum ( MAINWIN )
        call PigSetSymbolColour ( backgr )
        call PigSetSymbolNumber ( NodeMType )
        call PigDrawSymbols ( 2, showx, showy )
        call PigSetSymbolNumber ( NodeMType )
      ENDIF
C         - ( digfound )

      IF ( digfound ) THEN
C         - assign digit coords found to Common variables
        digX(1) = nearx(1)
        digY(1) = nearY(1)
        digX(2) = nearx(2)
        digY(2) = nearY(2)
      ENDIF
C         - ( digfound )
      ok = digfound

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE DigPtsOrder ( nearx, neary, ok )
C PURPOSE: To search the specified block of a DIGIT file for the pair of 
C          points that most closely matches a given pair of points.
C   GIVEN: In Common RESEL;
C               order = TRUE, the user has specified that the ordering of
C                       boundary blocks in the DIGIT file matches the order
C                       of the NODE file,
C               digX(1), digY(1) = coordinates of one point to be 
C                                  matched with a digit point,
C               digX(2), digY(2) = coordinates of the other point to be 
C                                  matched with a digit point,
C               bndsec = boundary section # in NODE file where digX(), digY()
C                        occur ( see also RETURNS: ),
C               digfile = name of DIGIT file to search for endpoints.
C RETURNS: ok = TRUE if no I/O errors, or mismatch of blocks.
C          In Common RESEL;
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        closest matches occur,
C               blkdpth = depth from DIGIT file for block where digX() &
C                         digY() occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )
      integer funit
        PARAMETER ( funit = 19 )

C - "INCLUDES"
        include '../includes/graf.def'

C - PASSED VARIABLES
        REAL nearx(2), neary(2)
      LOGICAL ok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - CLSR stores variables used by Function Closer
        REAL idealx, idealy, currx, curry, testx, testy
      COMMON /CLSR/ idealx, idealy, currx, curry, testx, testy

C - LOCAL VARIABLES
      REAL xtemp, ytemp, tmpdpth
      integer digblk
      CHARACTER*80 cstr
      LOGICAL eoblk, blkfound, Closer

C-------------------START ROUTINE----------------------------------

      ok = .TRUE.
      OPEN ( unit = funit, file = digfile, status = 'OLD' )
C       -- boundary section where digX(), digY() occur should = block # 
C       -- where digit point matches will be found
      digblk = bndsec
C       - find block in DIGIT file 
      call FindBndBlock ( digblk, tmpdpth, blkfound, funit )
      IF ( blkfound ) THEN
C         - block has been found, READ positioned for reading 1st coords
C         -- in block, assign depth for block
        blkdpth = tmpdpth
C         - search for 2 digit points nearest to digX(), digY()
        nearx(1) = -1000.0
        neary(1) = -1000.0
        nearx(2) = -1000.0
        neary(2) = -1000.0
        eoblk = .FALSE.
        DO WHILE ( .NOT. eoblk )
C           - read digit coords
          READ ( funit, *, end = 224 ) xtemp, ytemp
          IF ( ytemp .eq. -9999 ) THEN
C             - end of section read, see if block continues
            READ ( funit, *, end = 222 ) xtemp, ytemp
            IF ( (xtemp .eq. 88) .AND. (ytemp .eq. 88) ) THEN
C               - continuation header read
            eoblk = .FALSE.
            ELSE
C               - not continuation header, must be end of block
            eoblk = .TRUE.
            ENDIF
C               - ( XTemp = 88 & YTemp = 88 )
            GOTO 223
222           eoblk = .TRUE.
223           CONTINUE
          ELSE
C             - x,y coordinates read
            testx = XTemp
            testy = YTemp
C             - check if a coord pair closer to digX(1), digY(1) found
            idealx = digX(1)
            idealy = digY(1)
            currx = nearx(1)
            curry = neary(1)
            IF ( Closer() ) THEN
C               - save the closer point
            nearx(1) = XTemp            
            neary(1) = YTemp
            ENDIF
C               - ( Closer() )
C             - check if a coord pair closer digX(2), digY(2) found
            idealx = digX(2)
            idealy = digY(2)
            currx = nearx(2)
            curry = neary(2)
            IF ( Closer() ) THEN
C               - save the closer point
            nearx(2) = XTemp
            neary(2) = YTemp
            ENDIF
C               - ( Closer() )
          ENDIF
C             - ( YTemp = -9999 )
C           - eof trap for read
          GOTO 225
224         eoblk = .TRUE.
225         CONTINUE
        END DO
C           - ( NOT eoblk )
      ELSE
C         - boundary block was not located
        ok = .FALSE.
          digblk = 0
          cstr = 'ERROR locating boundary block in DIGIT file,' //
     +              ' Try NO Order Option.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
        ENDIF
C         - ( blkfound )

C       - close the DIGIT file
      CLOSE ( funit )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE DigPtsNoOrder ( nearx, neary, ok )
C PURPOSE: To search an entire DIGIT file for the pair of points that 
C          most closely matches a given pair of points.
C   GIVEN: In Common RESEL;
C               order = TRUE or FALSE, should work in both cases,
C               digX(1), digY(1) = coordinates of one of the points to be 
C                                  matched with a digit point,
C               digX(2), digY(2) = coordinates of the other point to be
C                                  matched with a digit point,
C               digfile = name of DIGIT file to search for match.
C RETURNS: ok = TRUE if no I/O errors,
C          In Common RESEL;
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        occur,
C               blkdpth = depth from DIGIT file for block where digX() &
C                         digY() occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )
      integer funit
        PARAMETER ( funit = 19 )

C - "INCLUDES"
        include '../includes/graf.def'

C - PASSED VARIABLES
        REAL nearx(2), neary(2)
      LOGICAL ok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - CLSR stores variables used by Function Closer
        REAL idealx, idealy, currx, curry, testx, testy
      COMMON /CLSR/ idealx, idealy, currx, curry, testx, testy

C - LOCAL VARIABLES
      REAL xtemp, ytemp, tmpdpth
      integer blknum
      CHARACTER*80 cstr
      LOGICAL Closer

C-------------------START ROUTINE----------------------------------

      ok = .TRUE.

      OPEN ( unit = funit, file = digfile, status = 'OLD' )

C       - must search whole file for digit endpoints
C       - initialize nearest matches found
      nearx(1) = -1000.0
      neary(1) = -1000.0
      nearx(2) = -1000.0
      neary(2) = -1000.0
      bndsec = 0
C       - read initial header for block 1
      READ ( funit, *, end = 333 ) blknum, tmpdpth
      DO WHILE ( .TRUE. )
C         - loop will exit on end of file in read statement
        READ ( funit, *, end = 334 ) XTemp, YTemp
        IF ( YTemp .eq. -9999 ) THEN
C           - end of section encountered, see if block continues
          READ ( funit, *, end = 334 ) XTemp, YTemp
          IF ( (XTemp .ne. 88) .OR. (YTemp .ne. 88) ) THEN
C             - not continuation header, so...
C             - next block header, save values
            blknum = nint(XTemp)
            tmpdpth = YTemp
          ENDIF
C             - ( XTemp ne 88 OR YTemp ne 88 )
        ELSE
C           - x,y coordinates read
          testx = XTemp
          testY = YTemp
C           - check if a coord pair closer to digX(1), digY(1) found
          idealx = digX(1)
          idealy = digY(1)
          currx = nearx(1)
          curry = neary(1)
          IF ( Closer() ) THEN
C             - save closer point & block number
            nearx(1) = XTemp      
            neary(1) = YTemp
            bndsec = blknum
            blkdpth = tmpdpth
          ENDIF
C             - ( Closer() )
C           - check if a coord pair closer to digX(2), digY(2) found
          idealx = digX(2)
          idealy = digY(2)
          currx = nearx(2)
          curry = neary(2)
          IF ( Closer() ) THEN
C             - save closer point & block number
            nearx(2) = XTemp      
            neary(2) = YTemp
            bndsec = blknum
            blkdpth = tmpdpth
          ENDIF
C             - ( Closer() )
        ENDIF
C           - ( YTemp = -9999 )
      END DO
C         - ( TRUE )

C       - end of file traps for read statements
        GOTO 334
C       - eof on 1st read
333     cstr = 'End Of File On First Read In ' // digfile(1:20)
        call PigMessageOK ( cstr,'resl' )
!       call PigUwait ( seconds )
!       call PigEraseMessage
        ok = .FALSE.
C       - eof on subsequent read, ok
334     CONTINUE

C       - close the DIGIT file
      CLOSE ( funit )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE FindBndBlock ( blk, bdpth, found, funit )
C PURPOSE: To locate the read marker at the first set of coordinates
C          in block # blk of a DIGIT file that is already open
C          on file unit # funit.
C   GIVEN: A DIGIT file is open on file unit # funit, with the read
C          marker positioned at the first record of the file,
C          blk = block number to find,
C          funit = file unit # where DIGIT file to search is open,
C RETURNS: found = TRUE if block # blk was found and read marker is 
C                     positioned at first set of coordinates in block,
C                     else = FALSE,
C          bdpth = depth from DIGIT file for block where digX() & digY() occur.
C EFFECTS: DIGIT file open on funit is sequentially searched for a block 
C          header with a block number matching blk.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - PASSED VARIABLES
      LOGICAL found
      integer blk, funit
      REAL bdpth

C - LOCAL VARIABLES
      integer blknum
      REAL XTemp, YTemp, tmpdpth
      CHARACTER*80 cstr
      LOGICAL eoblk

C-------------------START ROUTINE----------------------------------

C       - 1st file read = block id # and depth
      READ ( funit, *, end = 111 ) blknum, tmpdpth

      found = .FALSE.
      DO WHILE ( .NOT. found )
        IF ( blknum .eq. blk ) THEN
          found = .TRUE.
          bdpth = tmpdpth
        ELSE
C           - keep looking, read up this block
          eoblk = .FALSE.
          DO WHILE ( .NOT. eoblk )
            READ ( funit, *, end = 112 ) XTemp, YTemp
            IF ( YTemp .eq. -9999 ) THEN
C               - end of a section encountered, check if block continues
            READ ( funit, *, end = 112 ) XTemp, YTemp
            IF ( (XTemp .eq. 88) .AND. (YTemp .eq. 88) ) THEN
C                 - block continues
              eoblk = .FALSE.
            ELSE
C                 - header for next block was read, assign values
              eoblk = .TRUE.
              blknum = nint(XTemp)
              tmpdpth = YTemp
            ENDIF
C                 - ( XTemp = 88 & YTemp = 88 )
            ENDIF
C               - ( YTemp = -9999 )
          END DO
C             - ( NOT eoblk )
        ENDIF
C           - ( blknum = blk )
      END DO
C         - ( NOT found )

C       - end of file traps for read statements
      GOTO 113
C       - eof on first read
111     found = .FALSE.
        cstr = 'End Of File Encountered On First Read In DIGIT File.'
        call PigMessageOK ( cstr,'resl' )
!       call PigUwait ( seconds )
!       call PigEraseMessage

        GO TO 113
C       - eof on subsequent read, if block wasnt found then found = FALSE
112     CONTINUE
113     CONTINUE

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE NodPtsOrder ( nearx, neary, ok )
C PURPOSE: To search the specified block of a NODE RESEL file for the pair of 
C          points that most closely matches a given pair of points.
C   GIVEN: In Common RESEL;
C               order = TRUE, the user has specified that the ordering of
C                       boundary blocks in the RESEL file matches the order
C                       of the NODE file,
C               digX(1), digY(1) = coordinates of one point to be 
C                                  matched with a digit point,
C               digX(2), digY(2) = coordinates of the other point to be 
C                                  matched with a digit point,
C               bndsec = boundary section # in NODE file where digX(), digY()
C                        occur ( see also RETURNS: ),
C               digfile = name of DIGIT file to search for endpoints.
C RETURNS: ok = TRUE if no I/O errors, or mismatch of blocks.
C          In Common RESEL;
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        closest matches occur,
C               blkdpth = depth from DIGIT file for block where digX() &
C                         digY() occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )
      integer funit
        PARAMETER ( funit = 19 )

C - "INCLUDES"
        include '../includes/graf.def'

C - PASSED VARIABLES
        REAL nearx(2), neary(2)
      LOGICAL ok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - CLSR stores variables used by Function Closer
        REAL idealx, idealy, currx, curry, testx, testy
      COMMON /CLSR/ idealx, idealy, currx, curry, testx, testy

C - LOCAL VARIABLES
        integer totrcoords, totrbndys, totrbndpts, j, k
      REAL xtemp, ytemp, tmpdpth
      integer digblk
      CHARACTER*80 cstr
      LOGICAL eoblk, blkfound, Closer
      character(80) Firstline 

C-------------------START ROUTINE----------------------------------

      ok = .TRUE.
      OPEN ( unit = funit, file = digfile, status = 'OLD' )

      READ(funit,'(a)') Firstline

      if(firstline(1:4).eq."#NOD") then  !new node format
        do
          READ(funit,'(a)') Firstline
          if(firstline(1:1).ne."#") then    !comment lines
         read(firstline,*) x0off2, y0off2, scaleX2, scaleY2, igridtype2
            exit
          endif
        enddo
      else
        rewind(funit)
      endif

        read(funit,*) TotRCoords
        read(funit,*) TotRBndys
        if(TotRBndys.lt.bndsec) then
          blkfound = .false.
        else
          blkfound = .true.
          if(bndsec.gt.1) then
            do j=1,bndsec-1
              read(funit,*) TotRBndPts
              do k=1,TotRBndPts
                read(funit,*)
              enddo
            enddo
          endif
        endif
c
C       -- boundary section where digX(), digY() occur should = block # 
C       -- where digit point matches will be found
      digblk = bndsec
C       - find block in DIGIT file 
c      call FindBndBlock ( digblk, tmpdpth, blkfound, funit )
      IF ( blkfound ) THEN
C         - block has been found, READ positioned for reading 1st coords
C         -- in block, assign depth for block
        blkdpth = tmpdpth
C         - search for 2 digit points nearest to digX(), digY()
        nearx(1) = -1000.0
        neary(1) = -1000.0
        nearx(2) = -1000.0
        neary(2) = -1000.0
        eoblk = .FALSE.
c        DO WHILE ( .NOT. eoblk )
C           - read digit coords
          read(funit,*) TotRBndPts
          do k=1,TotRBndPts
          READ ( funit, *, end = 224 ) xtemp, ytemp, tmpdpth
C             - x,y coordinates read
            testx = XTemp
            testy = YTemp
C             - check if a coord pair closer to digX(1), digY(1) found
            idealx = digX(1)
            idealy = digY(1)
            currx = nearx(1)
            curry = neary(1)
            IF ( Closer() ) THEN
C               - save the closer point
            nearx(1) = XTemp            
            neary(1) = YTemp
            ENDIF
C               - ( Closer() )
C             - check if a coord pair closer digX(2), digY(2) found
            idealx = digX(2)
            idealy = digY(2)
            currx = nearx(2)
            curry = neary(2)
            IF ( Closer() ) THEN
C               - save the closer point
            nearx(2) = XTemp
            neary(2) = YTemp
            ENDIF
C               - ( Closer() )
          enddo
224       eoblk = .TRUE.

c        END DO
C           - ( NOT eoblk )
      ELSE
C         - boundary block was not located
        ok = .FALSE.
          digblk = 0
          cstr = 'ERROR locating boundary block in RESELECT file,' //
     +              ' Try NO Order Option.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
        ENDIF
C         - ( blkfound )

C       - close the DIGIT file
      CLOSE ( funit )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE NodPtsNoOrder ( nearx, neary, ok )
C PURPOSE: To search an entire DIGIT file for the pair of points that 
C          most closely matches a given pair of points.
C   GIVEN: In Common RESEL;
C               order = TRUE or FALSE, should work in both cases,
C               digX(1), digY(1) = coordinates of one of the points to be 
C                                  matched with a digit point,
C               digX(2), digY(2) = coordinates of the other point to be
C                                  matched with a digit point,
C               digfile = name of DIGIT file to search for match.
C RETURNS: ok = TRUE if no I/O errors,
C          In Common RESEL;
C               digX(), digY() = nearest DIGIT coordinates found to nodes
C                                specified in nodes(),
C               bndsec = boundary block # in DIGIT file where digX() & digY()
C                        occur,
C               blkdpth = depth from DIGIT file for block where digX() &
C                         digY() occur.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )
      integer funit
        PARAMETER ( funit = 19 )

C - "INCLUDES"
        include '../includes/graf.def'

C - PASSED VARIABLES
        REAL nearx(2), neary(2)
      LOGICAL ok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - CLSR stores variables used by Function Closer
        REAL idealx, idealy, currx, curry, testx, testy
      COMMON /CLSR/ idealx, idealy, currx, curry, testx, testy

C - LOCAL VARIABLES
        integer totrcoords, totrbndys, totrbndpts, j, k
      REAL xtemp, ytemp, tmpdpth
c      integer blknum
      CHARACTER*80 cstr
      LOGICAL Closer
      character(80) firstline

C-------------------START ROUTINE----------------------------------

      ok = .TRUE.

      OPEN ( unit = funit, file = digfile, status = 'OLD' )

C       - must search whole file for digit endpoints
C       - initialize nearest matches found
      nearx(1) = -1000.0
      neary(1) = -1000.0
      nearx(2) = -1000.0
      neary(2) = -1000.0
      bndsec = 0

      READ(funit,'(a)') Firstline

      if(firstline(1:4).eq."#NOD") then  !new node format
        do
          READ(funit,'(a)') Firstline
          if(firstline(1:1).ne."#") then    !comment lines
         read(firstline,*) x0off2, y0off2, scaleX2, scaleY2, igridtype2
            exit
          endif
        enddo
      else
        rewind(funit)
      endif

        read(funit,*) TotRCoords
        read(funit,*) TotRBndys
        do j=1,TotRBndys
          read(funit,*) TotRBndPts
          do k=1,TotRBndPts
            READ ( funit, *, end = 334 ) XTemp, YTemp, tmpdpth
C           - x,y coordinates read
          testx = XTemp
          testY = YTemp
C           - check if a coord pair closer to digX(1), digY(1) found
          idealx = digX(1)
          idealy = digY(1)
          currx = nearx(1)
          curry = neary(1)
          IF ( Closer() ) THEN
C             - save closer point & block number
            nearx(1) = XTemp      
            neary(1) = YTemp
            bndsec = j
            blkdpth = tmpdpth
          ENDIF
C             - ( Closer() )
C           - check if a coord pair closer to digX(2), digY(2) found
          idealx = digX(2)
          idealy = digY(2)
          currx = nearx(2)
          curry = neary(2)
          IF ( Closer() ) THEN
C             - save closer point & block number
            nearx(2) = XTemp      
            neary(2) = YTemp
            bndsec = j
            blkdpth = tmpdpth
          ENDIF
C             - ( Closer() )
          enddo
      END DO
C         - ( TRUE )

C       - end of file traps for read statements
        GOTO 334
C       - eof on 1st read
        cstr = 'End Of File In ' // digfile(1:20)
        call PigMessageOK ( cstr,'resl' )
!       call PigUwait ( seconds )
!       call PigEraseMessage
        ok = .FALSE.
C       - eof on subsequent read, ok
334     CONTINUE

C       - close the DIGIT file
      CLOSE ( funit )

      RETURN
      END

C-----------------------------------------------------------------------*
      LOGICAL FUNCTION Closer()
C PURPOSE: To determine if a given pair of coordinates is closer to an
C          ideal pair of coordinates than another pair of given coordinates.
C   GIVEN: In Common CLSR;
C               idealx, idealy = pair of ideal coordinates that (currx,curry)
C                                and (testx,testy) will be tested against,
C               currx, curry = current pair of closest coordinates to test
C                              against (testx,testy),
C               testx, testy = new pair of coordinates to check if they are
C                              closer to (idealx,idealy) than (currx,curry) is.
C RETURNS: Closer = TRUE if (testx,testy) is closer to (idealx,idealy) than
C                   (currx,curry) is,
C                 = FALSE if the distance from (currx,curry) to (idealx,idealy)
C                   is less than or equal to the distance from (testx,testy)
C                   to (idealx,idealy).
C EFFECTS: Test is made as follows:
C       IF ABS ( idealx - currx ) +  ABS ( idealy - curry ) less than or equal
C          ABS ( idealx - testx ) +  ABS ( idealy - testy ) THEN
C         Closer = FALSE
C       ELSE Closer = TRUE.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - COMMON BLOCKS
C       - CLSR stores variables used by Function Closer()
      REAL idealx, idealy, currx, curry, testx, testy
      COMMON /CLSR/ idealx, idealy, currx, curry, testx, testy

C - LOCAL VARIABLES
      REAL distC, distT

C--------------START FUNCTION----------------------------------------

      distC = ABS ( idealx - currx ) +  ABS ( idealy - curry )
      distT = ABS ( idealx - testx ) +  ABS ( idealy - testy )

      IF ( distC .le. distT ) THEN
C         - curr is closer to ideal
        Closer = .FALSE.
      ELSE
C         - test is closer to ideal
        Closer = .TRUE.
      ENDIF
C         - ( distC <= distT )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE ReSelectNodes ( reselok )
C PURPOSE: Control routine to call condition specific routines that perform
C          boundary node reselection.
C   GIVEN: In Common RESEL;
C               digfile = name of DIGIT file to reselect from,
C               Nth = TRUE if reselection by every nth point,
C                   = FALSE if reselection by distance between points.
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               blkdpth = depth from DIGIT file for block where digX() &
C                         digY() occur,
C               contig = 1, if contiguous section of boundary block is to be
C                        reselected from,
C                      = 2, if non-contiguous section,
C                      = 0, if neither half, ie: no reselection to take place.
C RETURNS: 
C EFFECTS: Call routines to reselect nodes based on conditions determined
C          by RESEL variable values in GIVEN:.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays, only : maxstrnodes

C - PARAMETERS - (constants)
      integer funit
      PARAMETER ( funit = 12 )
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

C - PASSED VARIABLES
      LOGICAL reselok

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        integer totrcoords, totrbndys, totrbndpts, j, k
      integer i
      REAL tmpdpth
      CHARACTER*80 cstr, firstline
      LOGICAL bf

C--------------START ROUTINE----------------------------------------

      cstr = 'ReSelecting Nodes.'
      call PigPutMessage ( cstr )

C       - open the DIGIT file
      OPEN ( unit = funit, file = digfile, status = 'OLD' )

C       - locate the block to sample from
      if(digtype) then
        call FindBndBlock ( bndsec, tmpdpth, bf, funit )
      else

      READ(funit,'(a)') Firstline

      if(firstline(1:4).eq."#NOD") then  !new node format
        do
          READ(funit,'(a)') Firstline
          if(firstline(1:1).ne."#") then    !comment lines
         read(firstline,*) x0off2, y0off2, scaleX2, scaleY2, igridtype2
            exit
          endif
        enddo
      else
        rewind(funit)
      endif

          read(funit,*) TotRCoords
          read(funit,*) TotRBndys
c          write(*,*) 'blank'
c          write(*,*) 'blank'
c          write(*,*) 'blank'
c          write(*,*) 'blank'
c          write(*,*) 'TotRCoords =', TotRCoords
c          write(*,*) 'TotRBndys =', TotRBndys
          if(TotRBndys.lt.bndsec) then
            bf = .false.
          else
            bf = .true.
            if(bndsec.gt.1) then
              do j=1,bndsec-1
                read(funit,*) TotRBndPts
c                write(*,*) 'Skip TotRBndPts=',TotRBNdPts
                do k=1,TotRBndPts
                  read(funit,*)
                enddo
              enddo
            endif
          endif
        endif

        IF ( bf ) THEN
C         - block located, assign block depth
!         blkdpth = tmpdpth
C         - determine what part of this block to sample
          IF ( contig .eq. 1 ) THEN
C           - sample contiguous section of block
            if(digtype) then
            call ReSelectC ( reselok, funit )
            else
            call ReSelectNodC ( reselok, funit )
            endif
        ELSE
C           - sample non-contiguous section of block
            if(digtype) then
            call ReSelectNC ( reselok, funit )
            else
            call ReSelectNodNC ( reselok, funit )
            endif
        ENDIF
C           - ( contig = 1 )
        IF ( reselok ) THEN
C           - display reselected nodes
          call PigSetWindowNum ( MAINWIN )
          call PigSetSymbolColour ( yellow )
          call PigSetSymbolNumber ( NodeMType )
          DO i = 1, numnodes
            call PigDrawSymbols ( 1, strx(i), stry(i) )
          END DO
        ENDIF
C           - ( reselok )
        ELSE
C         - block not found
          cstr = 'ERROR Locating Block In ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          reselok = .FALSE.
        ENDIF
C         - ( bf )

      CLOSE ( funit )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE ReSelectC ( ok, funit )
C PURPOSE: To perform the actual reselection of boundary nodes from
C          DIGIT file digfile when contiguous half of boundary is to
C          be reselected.
C ASSUMES: Conditions for reselection are:
C               1) contig = 1, contiguous section of boundary is to be
C                            reselected,
C               2) FindBndBlk has been called immediately prior to ReSelectC
C                  to position read marker at 1st coordinate record in the
C                  DIGIT file boundary block to be reselected and to obtain
C                  depth for block to reselect.
C   GIVEN: In Common RESEL;
C               srate = sample rate, if reselecting by every Nth point,
C               sdist = sample rate, if reselecting by distance,
C               digX(), digY() = coordinates of digit points delimiting
C                                  boundary stretch to sample from,
C               nodes() = indices of nodes in data arrays delimiting stretch
C                         of nodes to replace with reselected points.
C               Nth = TRUE if reselection by every (srate)th point,
C                   = FALSE if reselection by distance between points
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               contig = 1, contiguous section of boundary block is to be
C                        reselected from,
C RETURNS: In Common /STRBND/;
C               numnodes = # nodes to insert
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C EFFECTS: 
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays, only : maxstrnodes

C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - PASSED VARIABLES
      LOGICAL ok
      integer funit

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        integer end, advstat
      REAL xtemp, ytemp, distlast, mindist
      CHARACTER*80 cstr
      LOGICAL startfnd, eoblk

C--------------START ROUTINE----------------------------------------

      ok = .TRUE.

C       - locate first point to start sampling from
      end = 0
      startfnd = .FALSE.
      eoblk = .FALSE.
      DO WHILE ( (.NOT. startfnd) .AND. (.NOT. eoblk) )
C         - read coordinates
        READ ( funit, *, end = 444 ) xtemp, ytemp
C         - eof trap for read
        GOTO 445
444     eoblk = .TRUE.
        startfnd = .FALSE.
445     CONTINUE
        IF ( ytemp .eq. -9999 ) THEN
C           - end of section read, check for continuation
          READ ( funit, *, end = 446 ) xtemp, ytemp
          IF ( (xtemp .eq. 88) .AND. (ytemp .eq. 88) ) THEN
C             - block continues
            eoblk = .FALSE.
          ELSE
C             - end of block
            eoblk = .TRUE.
          ENDIF
C             - ( xtemp = 88 & ytemp = 88 )
C           - eof trap for read
          GO TO 447
446       eoblk = .TRUE.
447       CONTINUE
        ELSE
C           - coordinates read, check for starting point
          IF ( (xtemp .eq. digX(1)) .AND.
     +           (ytemp .eq. digY(1)) ) THEN
C             - found the starting point
            end = 2
            startfnd = .TRUE.
          ELSE IF ( (xtemp .eq. digX(2)) .AND.
     +                (ytemp .eq. digY(2)) ) THEN
C             - found the starting point
            end = 1
            startfnd = .TRUE.
          ENDIF
C             - ( xtemp = digX(1) & ytemp = digY(1) )
        ENDIF
C           - ( ytemp = -9999 )
      END DO
C         - ( NOT startfnd & NOT eoblk )
      IF ( .NOT. startfnd ) THEN
C         - searched whole block and didnt find coordinate match
          cstr = 'Could Not Locate DIGIT EndPoints ' //
     +                  'During ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
        ELSE
C         - starting point found, start reselecting
        advstat = 0
        numnodes = 0
        eoblk = .FALSE.
        DO WHILE ( advstat .lt. 2 )
C           - continue till endpoint found, eoblock or eofile
C           - store a point
          numnodes = numnodes + 1
          strx(numnodes) = xtemp
          stry(numnodes) = ytemp
          strz(numnodes) = blkdpth
          call AdvanceReSel ( funit, xtemp, ytemp, advstat, end )
        END DO
C           - ( advstat < 2 )
        IF ( advstat .ge. 3 ) THEN
C           - end of block or file before endpoint found
          ok = .FALSE.
        ELSE
C           - advstat = 2, endpoint found
C           - determine how close last point is to endpoint
          IF ( Nth ) THEN
C             - numnodes must be > 2, so...
            IF ( numnodes .gt. 2 ) THEN
                mindist = SQRT ((strx(numnodes-1) - strx(numnodes-2))*
     |                        (strx(numnodes-1) - strx(numnodes-2)) +
     |                        (stry(numnodes-1) - stry(numnodes-2)) *
     |                        (stry(numnodes-1) - stry(numnodes-2)) )
                mindist = 0.5 * mindist
            ELSE
                mindist = 0
            ENDIF
C               - ( numnodes > 2 )
          ELSE
C             - NOT Nth
            mindist = 0.5 * sdist
          ENDIF
C             - ( Nth )
          distlast = SQRT ( (strx(numnodes) - digX(end)) *
     |                      (strx(numnodes) - digX(end)) +
     |                      (stry(numnodes) - digY(end)) *
     |                      (stry(numnodes) - digY(end)) )

C           - if too close scrap it
          IF ( distlast .lt. mindist )  numnodes = numnodes - 1
C           - store end as last point
          numnodes = numnodes + 1
          strx(numnodes) = digX(end)
          stry(numnodes) = digY(end)
        ENDIF
C           - ( advstat >= 3 )
      ENDIF
C         - ( NOT startfnd )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE ReSelectNC ( ok, funit )
C PURPOSE: To perform the actual reselection of boundary nodes from
C          DIGIT file digfile when non-contiguous half of boundary is to
C          be reselected.
C ASSUMES: Conditions for reselection are:
C               1) contig = 2, non-contiguous section of boundary is to
C                           be reselected,
C               2) FindBndBlk has been called immediately prior to ReSelectNC
C                  to position read marker at 1st coordinate record in the
C                  DIGIT file boundary block to be reselected and to obtain
C                  depth for block to reselect.
C   GIVEN: In Common RESEL;
C               srate = sample rate, if reselecting by every Nth point,
C               sdist = sample rate, if reselecting by distance,
C               digX(), digY() = coordinates of digit points delimiting
C                                boundary stretch to sample from,
C               nodes() = indices of nodes in data arrays delimiting stretch
C                         of nodes to replace with reselected points.
C               Nth = TRUE if reselection by every (srate)th point,
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               contig = 2, non-contiguous section of boundary block is to be
C                        reselected from,
C          In Common STRADDLE;
C               straddidx(1) = pairs with nodes(1) to give 2nd delimiting index
C                              to mark half of non-contig boundary stretch,
C               straddidx(2) = pairs with nodes(2) to give 2nd delimiting index
C                              to mark half of non-contig boundary stretch.
C RETURNS: In Common /STRBND/;
C               numnodes = # nodes to insert
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C EFFECTS: 
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays

C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - PASSED VARIABLES
      LOGICAL ok
      integer funit

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - BRK stores index to STRBND arrays of break in non-contig reselection
      integer ncbreak
      COMMON /BRK/ ncbreak

C - LOCAL VARIABLES
      integer start, end, advstat, scount
      REAL xtemp, ytemp, currdist, lastdist
      CHARACTER*80 cstr
      LOGICAL startfnd, endfnd, eoblk
      REAL minclose
C       - minclose is the range about sdist that currdist must be within
C       -- in order to consider currdist to be equal to sdist
      REAL prevx, prevy
C       - prevx, prevy are coordinates of previous digit point read in

C--------------START ROUTINE----------------------------------------

      ok = .TRUE.

C       - start of block is first point to start sampling from
      READ ( funit, *, end = 777 ) xtemp, ytemp
      eoblk = .FALSE.
C       - eof trap for read
      GO TO 778
777   eoblk = .TRUE.
778   CONTINUE

      numnodes = 0
      start = 0
      end = 0
      startfnd = .FALSE.
      endfnd = .FALSE.
      DO WHILE ( (.NOT. startfnd) .AND. (.NOT. eoblk) )
C         - start reselecting until one of the digit endpoints, eoblk, or
C         -- eofile is encountered
C         - store a point
        numnodes = numnodes + 1
        strx(numnodes) = xtemp
        stry(numnodes) = ytemp
        strz(numnodes) = blkdpth
C         - advance
        IF ( Nth ) THEN
C           - advance to (srate)th point
          advstat = 0
          scount = 0
          DO WHILE ( advstat .eq. 0 )
C             - read until (srate)th point
            scount = scount + 1
            READ ( funit, *, end = 800 ) xtemp, ytemp
C             - test for (srate)th point
            IF ( scount .eq. srate ) advstat = 1
C             - test for endpoint 1
            IF ( (xtemp .eq. digX(1)) .AND.
     +             (ytemp .eq. digY(1)) ) THEN
                advstat = 2
                start = 1
                end = 2
            ENDIF
C               - ( xtemp = digX(1) & ytemp = digY(1) )
C             - test for endpoint 2
            IF ( (xtemp .eq. digX(2)) .AND.
     +             (ytemp .eq. digY(2)) ) THEN
                advstat = 2
                start = 2
                end = 1
            ENDIF
C               - ( xtemp = digX(2) & ytemp = digY(2) )
C             - test for eoblk
            IF ( ytemp .eq. -9999 ) THEN
C               - end of a section, see if block continues
                READ ( funit, *, end = 888 ) xtemp, ytemp
                IF ( (xtemp .eq. 88) .AND. (ytemp .eq. 88) ) THEN
C                 - block continues, see how far back last point was
                  IF ( scount .lt. (srate / 2) ) THEN
C                   - scrap last point
                    numnodes = numnodes - 1
                  ENDIF
C                   - ( scount < (srate / 2) )
C                 - start of new section, want next point so set scount to
C                 -- accept 1 more point
                  scount = srate - 1
                ELSE
C                 - eoblock, set flag
                  advstat = 3
                ENDIF
C                 - ( xtemp = 88 & ytemp = 88 )
C               - eof trap for read
                GOTO 889
888           advstat = 4
                eoblk = .TRUE.
889           CONTINUE
            ENDIF
C               - ( yin = -9999 )
C             - eof trap for read
            GOTO 801
800         advstat = 4
            eoblk = .TRUE.
801         CONTINUE
          END DO
C             - ( advstat = 0 )
          IF ( advstat .eq. 2 ) THEN
C             - see how far back last point was
            IF ( scount .lt. (srate / 2) ) THEN
C               - too close so scrap it
                numnodes = numnodes - 1
            ENDIF
C             - store the endpoint found
            numnodes = numnodes + 1
            strx(numnodes) = xtemp
            stry(numnodes) = ytemp
            strz(numnodes) = blkdpth
C             - end outer loop
            startfnd = .TRUE.
          ENDIF
C             - ( advstat = 2 )
        ELSE
C           - NOT Nth, advance to distance = sdist from last point
          minclose = sdist / 1000.0
          currdist = 0.0
          advstat = 0
          prevx = strx(numnodes)
          prevy = stry(numnodes)
          DO WHILE ( advstat .eq. 0 )
C             - read until currdist = sdist, endpoint found, eoblk, or eofile
            READ ( funit, *, end = 655 ) xtemp, ytemp
C             - test for eoblk
            IF ( ytemp .eq. -9999 ) THEN
C               - end of a section, see if block continues
                READ ( funit, *, end = 657 ) xtemp, ytemp
                IF ( (xtemp .eq. 88) .AND. (ytemp .eq. 88) ) THEN
C                 - block continues, start of new section, want next point
                  READ ( funit, *, end = 659 ) xtemp, ytemp
C                 - see how far away last point was
                  lastdist = SQRT ( (strx(numnodes) - xtemp) *
     |                            (strx(numnodes) - xtemp) + 
     |                            (stry(numnodes) - ytemp) *
     |                            (stry(numnodes) - ytemp) )
                  IF ( lastdist .lt. (0.5 * sdist) ) THEN
C                   - last point less than 1/2 sdist away, scrap last point
                    numnodes = numnodes - 1
                  ENDIF
C                   - ( lastdist < 0.5 * sdist )
C                 - have next point, set flag
                  advstat = 1
C                 - eof trap for read
                  GOTO 660
659             advstat = 4
660             CONTINUE
                ELSE
C                 - eoblock, set flag
                  advstat = 3
                ENDIF
C                 - ( xtemp = 88 & ytemp = 88 )
C               - eof trap for read
                GOTO 658
657           advstat = 4
658           CONTINUE
            ELSE
C               - ytemp ne -9999, must be coords read in
C               -- test for currdist = sdist
                currdist = currdist + SQRT( (prevx - xtemp) *
     |                                      (prevx - xtemp) +
     |                                      (prevy - ytemp) *
     |                                      (prevy - ytemp) )
                IF ( (ABS(currdist - sdist) .le. minclose) .OR.
     +               (currdist .ge. sdist) ) THEN
C                 - sdist reached, flag next point reached
                  advstat = 1
                ELSE
C                 - save coordinates as previous digit point
                  prevx = xtemp
                  prevy = ytemp
                ENDIF
C                 - ( currdist - sdist <= minclose )
C               - test for endpoint 1
                IF ( (xtemp .eq. digX(1)) .AND.
     +               (ytemp .eq. digY(1)) ) THEN
                  advstat = 2
                  start = 1
                  end = 2
                ENDIF
C                 - ( xtemp = digX(1) & ytemp = digY(1) )
C               - test for endpoint 2
                IF ( (xtemp .eq. digX(2)) .AND.
     +               (ytemp .eq. digY(2)) ) THEN
                  advstat = 2
                  start = 2
                  end = 1
                ENDIF
C                 - ( xtemp = digX(2) & ytemp = digY(2) )
            ENDIF
C               - ( ytemp = -9999 )
C               - eof trap for read
            GOTO 656
655         advstat = 4
656         CONTINUE
          END DO
C             - ( advstat = 0 )
          IF ( advstat .eq. 2 ) THEN
C             - see how far away last point was
            lastdist = SQRT ( (strx(numnodes) - xtemp) *
     |                        (strx(numnodes) - xtemp) + 
     |                        (stry(numnodes) - ytemp) *
     |                        (stry(numnodes) - ytemp) )
            IF ( lastdist .lt. (0.5 * sdist) ) THEN
C               - last point less than 1/2 sdist away, scrap last point
                numnodes = numnodes - 1
            ENDIF
C               - ( lastdist < 0.5 * sdist )
C             - store the endpoint found
            numnodes = numnodes + 1
            strx(numnodes) = xtemp
            stry(numnodes) = ytemp
            strz(numnodes) = blkdpth
C             - end outer loop
            startfnd = .TRUE.
          ENDIF
C             - ( advstat = 2 )
        ENDIF
C           - ( Nth )
      END DO
C         - ( NOT startfnd & NOT eoblk )

      IF ( startfnd ) THEN
C         - 1st part of non-contig boundary has been reselected, ie: from
C         -- start of block to digX(start), digY(start)
C         -- now stop sampling until digX(end), digY(end) located
C         -- 1st mark break in STRBND arrays
        ncbreak = numnodes
        endfnd = .FALSE.
        eoblk = .FALSE.
        DO WHILE ( (.NOT. endfnd) .AND. (.NOT. eoblk) )
          READ ( funit, *, end = 805 ) xtemp, ytemp
C           - eof trap for read
          GOTO 806
805       eoblk = .TRUE.
          endfnd = .FALSE.
806       CONTINUE
          IF ( ytemp .eq. -9999 ) THEN
C             - end of section read, check for continuation
            READ ( funit, *, end = 807 ) xtemp, ytemp
            IF ( (xtemp .eq. 88) .AND. (ytemp .eq. 88) ) THEN
C               - block continues
                eoblk = .FALSE.
            ELSE
C               - end of block
                eoblk = .TRUE.
            ENDIF
C               - ( xtemp = 88 & ytemp = 88 )
C             - eof trap for read
            GO TO 808
807         eoblk = .TRUE.
808         CONTINUE
          ELSE
C             - coordinates read, check for ending point
            IF ( (xtemp .eq. digX(end)) .AND.
     +             (ytemp .eq. digY(end)) ) THEN
C               - found the starting point
                endfnd = .TRUE.
            ENDIF
C               - ( xtemp = digX(end) & ytemp = digY(end) )
          ENDIF
C             - ( ytemp = -9999 )
        END DO
C           - ( NOT endfnd & NOT eoblk )
      ELSE
C         - start was not found, so end will not be found
        endfnd = .FALSE.
      ENDIF
C         - ( startfnd )

C       - end has been found, or eoblk/eofile was reached before end was found 
      IF ( endfnd ) THEN
C         - ending point found, start reselecting till end of block
        advstat = 0
        eoblk = .FALSE.
C         - end will be next new point
        DO WHILE ( (advstat .ne. 3) .AND. (advstat .ne. 4) )
C           - continue till next point, eoblock or eofile
C           - for last argument to AdvanceReSel start is used because it was
C           -- located earlier and won't be again since this loop is to
C           -- continue until eob or eof
C           - store a point
          numnodes = numnodes + 1
          strx(numnodes) = xtemp
          stry(numnodes) = ytemp
          strz(numnodes) = blkdpth
          call AdvanceReSel ( funit, xtemp, ytemp, advstat, start )
        END DO
C           - ( advstat ne 3 & advstat ne 4 )
C         - end of block found or end of file, both ok
        ok = .TRUE.
      ELSE
C         - start and/or end was not found
          cstr = 'Could Not Locate DIGIT EndPoints ' //
     +                  'During ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
        ENDIF
C         - ( NOT endfnd )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE AdvanceReSel ( funit, xnext, ynext, advstat, end )
C PURPOSE: To advance the read marker to the next point to be reselected.
C ASSUMES: strx() & stry() have at least 1 point stored, this will be used
C          if advancing by distance to calculate distance to 1st point read.
C   GIVEN: funit = file unit # to read from,
C          end = index to digX() & digY() of digit endpoint,
C          In Common RESEL;
C               Nth = TRUE if advancing by counting points read,
C                   = FALSE if advancing by measuring distance
C               digX(end), digY(end) = coords of endpoint of reselection,
C               srate = # of points to advance if Nth = TRUE,
C               sdist = distance to advance if Nth = FALSE,
C RETURNS: advstat = 1 if next point found without error,
C                  = 2 if end point found,
C                  = 3 if end of block found without finding next point or 
C                       endpoint,
C                  = 4 if end of file encountered without finding next point 
C                       or endpoint.
C          xnext, ynext = coords of DIGIT point advanced to.
C EFFECTS: DIGIT file is sequentially read until a procedure ending situation
C          is encountered, indicated by advstat.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays, only : maxstrnodes

C - PASSED VARIABLES
      integer funit, advstat, end
      REAL xnext, ynext

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        integer scount
      REAL xin, yin, currdist, lastdist
      REAL minclose
C       - minclose is the range about sdist that currdist must be within
C       -- in order to consider currdist to be equal to sdist
      REAL prevx, prevy
C       - prevx, prevy are coordinates of previous digit point read in

C-----------------START ROUTINE-------------------------------------

      IF ( Nth ) THEN
C         - advance to (srate)th point
        advstat = 0
        scount = 0
        DO WHILE ( advstat .eq. 0 )
C           - read until (srate)th point
          scount = scount + 1
          READ ( funit, *, end = 555 ) xin, yin
C           - test for (srate)th point
          IF ( scount .eq. srate ) advstat = 1
C           - test for endpoint
          IF ( (xin .eq. digX(end)) .AND.
     +           (yin .eq. digY(end)) )  advstat = 2
C           - test for eoblk
          IF ( yin .eq. -9999 ) THEN
C             - end of a section, see if block continues
            READ ( funit, *, end = 555 ) xin, yin
            IF ( (xin .eq. 88) .AND. (yin .eq. 88) ) THEN
C               - block continues, see how far back last point was
            IF ( scount .lt. (srate / 2) ) THEN
C                 - scrap last point
              numnodes = numnodes - 1
            ENDIF
C                 - ( scount < (srate / 2) )
C               - start of new section, want next point so set scount to
C               -- accept 1 more point
            scount = srate - 1
            ELSE
C               - eoblock, set flag
            advstat = 3
            ENDIF
C               - ( xin = 88 & yin = 88 )
          ENDIF
C             - ( yin = -9999 )
C           - eof trap for read
          GOTO 556
555         advstat = 4
556         CONTINUE
        END DO
C           - ( advstat = 0 )
      ELSE
C         - NOT Nth, advance to distance = sdist from last point
        minclose = sdist / 1000.0
        currdist = 0.0
        advstat = 0
        prevx = strx(numnodes)
        prevy = stry(numnodes)
        DO WHILE ( advstat .eq. 0 )
C           - read until currdist = sdist, endpoint found, eoblk, or eofile
          READ ( funit, *, end = 655 ) xin, yin
C           - test for eoblk
          IF ( yin .eq. -9999 ) THEN
C             - end of a section, see if block continues
            READ ( funit, *, end = 657 ) xin, yin
            IF ( (xin .eq. 88) .AND. (yin .eq. 88) ) THEN
C               - block continues, start of new section, want next point
            READ ( funit, *, end = 659 ) xin, yin
C               - see how far away last point was
            lastdist = SQRT ( (strx(numnodes) - xin) *
     |                 (strx(numnodes) - xin) + (stry(numnodes) - yin) *
     |                 (stry(numnodes) - yin) )
            IF ( lastdist .lt. (0.5 * sdist) ) THEN
C                 - last point less than 1/2 sdist away, scrap last point
              numnodes = numnodes - 1
            ENDIF
C                 - ( lastdist < 0.5 * sdist )
C               - have next point, set flag
            advstat = 1
C               - eof trap for read
            GOTO 660
659             advstat = 4
660             CONTINUE
            ELSE
C               - eoblock, set flag
            advstat = 3
            ENDIF
C               - ( xin = 88 & yin = 88 )
C             - eof trap for read
            GOTO 658
657           advstat = 4
658           CONTINUE
          ELSE
C             - yin ne -9999, must be coords read in
C             -- test for currdist = sdist
            currdist = currdist + SQRT ( (prevx - xin) *
     |                 (prevx - xin) + (prevy - yin) * (prevy - yin) )
            IF ( (ABS(currdist - sdist) .le. minclose) .OR.
     +             (currdist .ge. sdist) ) THEN
C               - sdist reached, flag next point reached
            advstat = 1
            ELSE
C               - save the coordinates as previous digit point
            prevx = xin
            prevy = yin
            ENDIF
C               - ( currdist - sdist <= minclose )
C             - test for endpoint
            IF ( (xin .eq. digX(end)) .AND.
     +           (yin .eq. digY(end)) )  advstat = 2
          ENDIF
C             - ( yin = -9999 )
C             - eof trap for read
          GOTO 656
655         advstat = 4
656         CONTINUE
        END DO
C           - ( advstat = 0 )
      ENDIF
C         - ( Nth )
      xnext = xin
      ynext = yin

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE ReSelectNodC ( ok, funit )
C PURPOSE: To perform the actual reselection of boundary nodes from
C          NODE file digfile when contiguous half of boundary is to
C          be reselected.
C ASSUMES: Conditions for reselection are:
C               1) contig = 1, contiguous section of boundary is to be
C                            reselected,
C               2) Prior to callingReSelectC
C                  read marker positioned at 1st coordinate record in the
C                  NODE file boundary block to be reselected and to obtain
C                  depth for block to reselect.
C   GIVEN: In Common RESEL;
C               srate = sample rate, if reselecting by every Nth point,
C               sdist = sample rate, if reselecting by distance,
C               digX(), digY() = coordinates of digit points delimiting
C                                  boundary stretch to sample from,
C               nodes() = indices of nodes in data arrays delimiting stretch
C                         of nodes to replace with reselected points.
C               Nth = TRUE if reselection by every (srate)th point,
C                   = FALSE if reselection by distance between points
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               contig = 1, contiguous section of boundary block is to be
C                        reselected from,
C RETURNS: In Common /STRBND/;
C               numnodes = # nodes to insert
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C EFFECTS: 
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays, only : maxstrnodes

C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - PASSED VARIABLES
      LOGICAL ok
      integer funit

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        integer j, end, scount, TotRBndPts
        real currdist, minclose, prevx, prevy
      REAL xtemp, ytemp, ztemp, distlast, mindist
      CHARACTER*80 cstr
      LOGICAL startfnd, eoblk

C--------------START ROUTINE----------------------------------------

      ok = .TRUE.

C       - locate first point to start sampling from
      end = 0
      startfnd = .FALSE.
      eoblk = .FALSE.
      minclose = sdist / 1000.0
      currdist = 0.0

        read(funit,*) TotRBndPts
c          write(*,*) 'blank'
c          write(*,*) 'blank'
cc          write(*,*) 'blank'
c          write(*,*) 'Reselc, TotRBndPts=', TotRBndPts
        if(TotRBndPts.eq.0) then
          cstr = 'No nodes in block for ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
          return
        endif
c
      DO j = 1,TotRBndPts
C         - read coordinates
        READ ( funit, *, end = 9444 ) xtemp, ytemp, ztemp
c
C           - coordinates read, check for starting point
          if(.NOT. startfnd) then
          IF ( (xtemp .eq. digX(1)) .AND.
     +           (ytemp .eq. digY(1)) ) THEN
C             - found the starting point
            end = 2
            startfnd = .TRUE.
          ELSEIF ( (xtemp .eq. digX(2)) .AND.
     +                (ytemp .eq. digY(2)) ) THEN
C             - found the starting point
            end = 1
            startfnd = .TRUE.
          ENDIF
C             - ( xtemp = digX(1) & ytemp = digY(1) )
            if(startfnd) then
              numnodes = 1
            strx(numnodes) = xtemp
            stry(numnodes) = ytemp
            strz(numnodes) = ztemp
              scount = 0
            prevx = strx(numnodes)
            prevy = stry(numnodes)
            endif
        ELSE
            IF(Nth) then
C             - select by index
              scount = scount + 1
C             - test for (srate)th point
            IF ( scount .eq. srate ) then
              IF ( (xtemp .eq. digX(end)) .AND.
     +             (ytemp .eq. digY(end)) )  then
                  go to 995
                endif
                scount = 0
              numnodes = numnodes + 1
              strx(numnodes) = xtemp
              stry(numnodes) = ytemp
              strz(numnodes) = ztemp
              endif
C             - test for endpoint
            IF ( (xtemp .eq. digX(end)) .AND.
     +           (ytemp .eq. digY(end)) )  then
                go to 995
              endif
          ELSE
C             - NOT Nth, advance to distance = sdist from last point
C             -- test for currdist = sdist
            currdist = currdist + SQRT ( (prevx - xtemp)**2
     |                 + (prevy - ytemp)**2 )
            IF ( (ABS(currdist - sdist) .le. minclose) .OR.
     +             (currdist .ge. sdist) ) THEN
C               - sdist reached, flag next point reached
              IF ( (xtemp .eq. digX(end)) .AND.
     +           (ytemp .eq. digY(end)) )  then
                  go to 995
                endif
                currdist = 0.
              numnodes = numnodes + 1
              strx(numnodes) = xtemp
              stry(numnodes) = ytemp
              strz(numnodes) = ztemp
                prevx = xtemp
                prevy = ytemp
            ELSE
C               - save the coordinates as previous digit point
                prevx = xtemp
                prevy = ytemp
            ENDIF
C               - ( currdist - sdist <= minclose )
C             - test for endpoint
            IF ( (xtemp .eq. digX(end)) .AND.
     +           (ytemp .eq. digY(end)) )  then
                go to 995
              endif
          ENDIF
        ENDIF
      ENDDO

      IF ( .NOT. startfnd .OR. eoblk) THEN
C         - searched whole block and didnt find coordinate match
          cstr = 'Could Not Locate NODE EndPoints ' //
     +                  'During ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
        ENDIF

995     continue
          IF ( Nth ) THEN
C             - numnodes must be > 2, so...
            IF ( numnodes .gt. 2 ) THEN
            mindist = SQRT ( (strx(numnodes-1) - strx(numnodes-2)) *
     |                           (strx(numnodes-1) - strx(numnodes-2)) +
     |                           (stry(numnodes-1) - stry(numnodes-2)) *
     |                           (stry(numnodes-1) - stry(numnodes-2)) )
            mindist = 0.5 * mindist
            ELSE
            mindist = 0
            ENDIF
C               - ( numnodes > 2 )
          ELSE
C             - NOT Nth
            mindist = 0.5 * sdist
          ENDIF
C             - ( Nth )
          distlast = SQRT ( (strx(numnodes) - digX(end)) *
     |                        (strx(numnodes) - digX(end)) +
     |                        (stry(numnodes) - digY(end)) *
     |                        (stry(numnodes) - digY(end)) )

C           - if too close scrap it
          IF ( distlast .lt. mindist )  numnodes = numnodes - 1
C           - store end as last point

          numnodes = numnodes + 1
          strx(numnodes) = digX(end)
          stry(numnodes) = digY(end)
          strz(numnodes) = ztemp

      RETURN

9444    continue
          cstr = 'Premature end of block ' //
     +                  'During ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
          return

      END

C-----------------------------------------------------------------------*
      SUBROUTINE ReSelectNodNC ( ok, funit )
C PURPOSE: To perform the actual reselection of boundary nodes from
C          NODE file digfile when non-contiguous half of boundary is to
C          be reselected.
C ASSUMES: Conditions for reselection are:
C               1) contig = 2, non-contiguous section of boundary is to

C                           be reselected,
C               2) FindBndBlk has been called immediately prior to ReSelectNC
C                  to position read marker at 1st coordinate record in the
C                  NODE file boundary block to be reselected and to obtain
C                  depth for block to reselect.
C   GIVEN: In Common RESEL;
C               srate = sample rate, if reselecting by every Nth point,
C               sdist = sample rate, if reselecting by distance,
C               digX(), digY() = coordinates of digit points delimiting
C                                boundary stretch to sample from,
C               nodes() = indices of nodes in data arrays delimiting stretch
C                         of nodes to replace with reselected points.
C               Nth = TRUE if reselection by every (srate)th point,
C               bndsec = boundary section # in NODE file where reselection
C                        is to take place,
C               contig = 2, non-contiguous section of boundary block is to be
C                        reselected from,
C          In Common STRADDLE;
C               straddidx(1) = pairs with nodes(1) to give 2nd delimiting index
C                              to mark half of non-contig boundary stretch,
C               straddidx(2) = pairs with nodes(2) to give 2nd delimiting index
C                              to mark half of non-contig boundary stretch.
C RETURNS: In Common /STRBND/;
C               numnodes = # nodes to insert
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C EFFECTS: 
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays

C - PARAMETERS - (constants)
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - PASSED VARIABLES
      LOGICAL ok
      integer funit

C - COMMON BLOCKS
C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - BRK stores index to STRBND arrays of break in non-contig reselection
      integer ncbreak
      COMMON /BRK/ ncbreak

C - LOCAL VARIABLES
      integer j, end, scount
      integer TotRBndPts
      REAL xtemp, ytemp, ztemp, currdist, lastdist
      CHARACTER*80 cstr
      LOGICAL startfnd, endfnd, eoblk
      REAL minclose
C       - minclose is the range about sdist that currdist must be within
C       -- in order to consider currdist to be equal to sdist
      REAL prevx, prevy
C       - prevx, prevy are coordinates of previous digit point read in

C--------------START ROUTINE----------------------------------------

      ok = .TRUE.

C       - locate first point to start sampling from
      end = 0
      startfnd = .FALSE.
      endfnd = .false.
      eoblk = .FALSE.
      minclose = sdist / 1000.0
      currdist = 2*sdist
      numnodes = 0
      scount = srate -1
      prevx = 0.
      prevy = 0.

      read(funit,*) TotRBndPts
c          write(*,*) 'blank'
c          write(*,*) 'blank'
c          write(*,*) 'blank'
c        write(*,*) ' Resl, TotRBndPts=', TotRBndPts
      if(TotRBndPts.eq.0) then
          cstr = 'No nodes in block for ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
        return
      endif
c
      DO j = 1,TotRBndPts
C         - read coordinates
        READ ( funit, *, end = 9444 ) xtemp, ytemp, ztemp
c
        if(.not. startfnd ) then
          IF ( (xtemp .eq. digX(1)) .AND.
     +           (ytemp .eq. digY(1)) ) THEN
C             - found the starting point
            end = 2
            startfnd = .TRUE.
          ELSEIF ( (xtemp .eq. digX(2)) .AND.
     +                (ytemp .eq. digY(2)) ) THEN
C             - found the starting point
            end = 1
            startfnd = .TRUE.
          endif
          if(startfnd) then
            scount = 0
            currdist = 0.
            numnodes = numnodes + 1
            strx(numnodes) = xtemp
            stry(numnodes) = ytemp
            strz(numnodes) = ztemp
            prevx = xtemp
            prevy = ytemp
!            ncbreak = numnodes
          endif
        elseif(.not. endfnd ) then
C             - test for endpoint
          IF ( (xtemp .eq. digX(end)) .AND.
     +           (ytemp .eq. digY(end)) )  then
            endfnd = .true.
C             - see how far back last point was
            IF ( Nth ) then
              if( scount .lt. (srate / 2) ) THEN
C               - too close so scrap it
                  numnodes = numnodes - 1
              endif
            else
C             - see how far away last point was
              lastdist = currdist + SQRT ( (prevx - xtemp)**2
     |                 + (prevy - ytemp)**2 )
              IF ( lastdist .lt. (0.5 * sdist) ) THEN
C               - last point less than 1/2 sdist away, scrap last point
                    numnodes = numnodes - 1
              ENDIF
C               - ( lastdist < 0.5 * sdist )
            ENDIF
            scount = 0
            currdist = 0.
            numnodes = numnodes + 1
            strx(numnodes) = xtemp
            stry(numnodes) = ytemp
            strz(numnodes) = ztemp
             prevx = strx(numnodes)
            prevy = stry(numnodes)
            ncbreak = numnodes
            go to 97
!          endif
!        endif
c
!        if((.not. startfnd) .or. endfnd ) then
          else   !if((startfnd) .and. .not. endfnd ) then
          IF(Nth) then
C             - select by index
              scount = scount + 1
C             - test for (srate)th point
            IF ( scount .eq. srate ) then
              scount = 0
              numnodes = numnodes + 1
              strx(numnodes) = xtemp
              stry(numnodes) = ytemp
              strz(numnodes) = ztemp
            endif
          ELSE
C             - NOT Nth, advance to distance = sdist from last point
C             -- test for currdist = sdist
            currdist = currdist + SQRT ( (prevx - xtemp)**2
     |                 + (prevy - ytemp)**2 )
            prevx = xtemp
            prevy = ytemp
            IF ( (ABS(currdist - sdist) .le. minclose) .OR.
     +             (currdist .ge. sdist) ) THEN
C               - sdist reached, flag next point reached
              currdist = 0.
              numnodes = numnodes + 1
              strx(numnodes) = xtemp
              stry(numnodes) = ytemp
              strz(numnodes) = ztemp
            ENDIF
C               - ( currdist - sdist <= minclose )
          ENDIF
          endif
        ENDIF
!97      continue
      ENDDO
97      continue

      if(.not. endfnd ) then
C         - start and/or end was not found
          cstr = 'Could Not Locate NODE EndPoints ' //
     +                  'During ReSelection.'
          call PigMessageOK ( cstr,'resl' )
!         call PigUwait ( seconds )
!         call PigEraseMessage
          ok = .FALSE.
        ENDIF
C         - ( NOT endfnd )

      RETURN

9444  continue
        cstr = 'Premature end of block ' //
     +                  'During ReSelection.'
        call PigMessageOK ( cstr,'resl' )
!       call PigUwait ( seconds )
!       call PigEraseMessage
        ok = .FALSE.
      
      return

      END

C-----------------------------------------------------------------------*
      SUBROUTINE SaveReSel ( rsaved )
C PURPOSE: To save the latest reselection of boundary nodes.
C   GIVEN: In Common RESEL;
C          blkdpth = depth along boundary block reselected from DIGIT file.
C          nodes(2) = array containing the indices of the node endpoints
C                     delimiting the section of boundary that is to be updated,
C          contig = 1, if contiguous section of boundary block is to be
C                      reselected from,
C                 = 2, if non-contiguous section,
C                 = 0, if neither half, ie: no reselection to take place.
C          In Common STRBND;
C               strx() = array of X coords of reselected nodes that will
C                        replace existing nodes between delimiting endpoints,
C               stry() = array of Y coords of reselected nodes that will
C                        replace existing nodes between delimiting endpoints,
C               numnodes = # of nodes in strx() & stry().
C RETURNS: rsaved = TRUE if user answers Yes to "Save Reselection", else FALSE,
C          In Common in file NODESTOR.INC;
C               dxray() = updated with reselected nodes,
C               dyray() = updated with reselected nodes,
C               depth() = updated with reselected nodes.
C EFFECTS: Data arrays dxray(), dyray(), depth() are updated to rflect 
C          boundary reselection.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays

C - PARAMETERS - ( constants )
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )

C - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

C - PASSED VARIABLES
      LOGICAL rsaved

C - COMMON BLOCKS
C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - BRK stores index to STRBND arrays of break in non-contig reselection
        integer ncbreak
      COMMON /BRK/ ncbreak

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - FDEP stores depth from DIGIT file for SaveReSel
      REAL filedepth
        logical deptype
      COMMON /FDEP/ filedepth, deptype

C       - RESEL stores boundary reselection parameters set in ReSelInfo
      CHARACTER*80 digfile
        REAL sdist, digX(2), digY(2), blkdpth
        integer srate, nodes(2), bndsec, contig
        LOGICAL Nth, order, digtype
      COMMON /RESEL/  sdist, digX, digY,  blkdpth,
     +       nodes, srate, contig,bndsec,Nth, order,  digtype, digfile

C       - UPDATE3 stores indication of whether StrBndUpdate or StrSpecUpdate
C       -- was called by StraightBnd or SaveReSel
      LOGICAL reselcall
      COMMON /UPDATE3/ reselcall

C - LOCAL VARIABLES
      integer indexs(2), i, j, up, dwn, midbreak
      REAL tmpx, tmpy
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo

C-----------------START ROUTINE-------------------------------------

      cstr = 'SAVE this reselection ?:'
      ans = PigCursYesNo (cstr)
      call PigEraseMessage
      deptype = digtype
      IF ( ans(1:1) .eq. 'Y' ) THEN
C       - save reselection
        rsaved = .TRUE.
        reselcall = .TRUE.
        indexs(1) = nodes(1)
        indexs(2) = nodes(2)
        IF ( contig .eq. 1 ) THEN
C         - contiguous section of boundary is to be updated
          filedepth = blkdpth
C         - set straddidx(1) & (2) to flag contig half update in DepthUpdate
          straddidx(1) = 0
          straddidx(2) = 0
C         - check if island boundary to update

c         Don't reverse order in selecting from contours (17/3/96)
          IF (digtype.and.(bndsec.gt.1).and.(bndsec.lt.TotBndys+1)) THEN
ccc        IF ( bndsec .gt. 1 ) THEN     as prior to 17 Mar 96

C           - island, adjust STRBND arrays, reverse order
            midbreak = numnodes / 2
            dwn = numnodes
            DO up = 1, midbreak
C             - midbreak should be ok if odd (doesnt need to swap with itself)
C             - save values that will be overwritten
              tmpx = strx(up)
              tmpy = stry(up)
C             - swap values
              strx(up) = strx(dwn)
              stry(up) = stry(dwn)
              strx(dwn) = tmpx
              stry(dwn) = tmpy
              dwn = dwn - 1
            END DO
C             - ( up = 1, midbreak )
            ncbreak = numnodes - ncbreak
          ENDIF
C           - ( bndsec > 1 )
          call StrBndUpdate ( indexs )
        ELSE IF ( contig .eq. 2 ) THEN
C         - non-contiguous section of boundary is to be updated
          IF ( (digtype .and. bndsec .lt. 2) .or.
     *          (.not. digtype ) ) THEN
C           - outer boundary, reverse the order of the array halves, ie:
C           -- 1 -> ncbreak = ncbreak -> 1
C           -- ncbreak+1 -> numnodes = numnodes -> ncbreak+1
C           -- do 1 -> ncbreak 1st
            midbreak = ncbreak / 2
            dwn = ncbreak
            DO up = 1, midbreak
C             - midbreak should be ok if odd (doesnt need to swap with itself)
C             - save values that will be overwritten
              tmpx = strx(up)
              tmpy = stry(up)
C             - swap values
              strx(up) = strx(dwn)
              stry(up) = stry(dwn)
              strx(dwn) = tmpx
              stry(dwn) = tmpy
              dwn = dwn - 1
            END DO
C             - ( up = 1, midbreak )
C           - do ncbreak+1 -> numnodes 2nd
            midbreak = ((numnodes - ncbreak) / 2) + ncbreak
            dwn = numnodes
            DO up = ncbreak+1, midbreak
C             - midbreak should be ok if odd (doesnt need to swap with itself)
C             - save values that will be overwritten
              tmpx = strx(up)
              tmpy = stry(up)
C             - swap values
              strx(up) = strx(dwn)
              stry(up) = stry(dwn)
              strx(dwn) = tmpx
              stry(dwn) = tmpy
              dwn = dwn - 1
            END DO
C             - ( up = 1, midbreak )
          ELSE
C           - an island boundary to update, NODE islands are clockwise, DIGIT
C           -- points sampled were stored counter-clockwise
C           - swap the 2 non-contig boundary halves
            j = numnodes
            DO i = 1, ncbreak
C             - move nodes before break to end of list
              j = j + 1
              strx(j) = strx(i)
              stry(j) = stry(i)
            END DO
C             - ( i = 1, ncbreak )
C           - now shuffle down list to start at index 1 again
            DO i = 1, numnodes
              strx(i) = strx(i + ncbreak)
              stry(i) = stry(i + ncbreak)
            END DO
C             - ( i = 1, numnodes )
            ncbreak = numnodes - ncbreak
          ENDIF
C           - ( bndsec < 2 )
C         - now call update procedure
          filedepth = blkdpth
          call StrSpecUpdate ( indexs )
          ELSE
C         - error, invalid value for contig
            cstr = 'Invalid Value For contig In SaveReSel.'
            call PigMessageOK ( cstr,'resl' )
!           call PigUwait ( seconds )
!           call PigEraseMessage
          ENDIF
C         - ( contig = 1 )
C       - update screen after save
        call DisplayNodes ()
      ELSE
C       - dont save reselection
        rsaved = .FALSE.
C       - restore original display
        call PigSetWindowNum ( MAINWIN )
C       - remove reselected nodes
        call PigSetSymbolColour ( backgr )
        call PigSetSymbolNumber ( NodeMType )
        DO i = 1, numnodes
          call PigDrawSymbols ( 1, strx(i), stry(i) )
        END DO
C         - ( i = 1, numnodes )
C       - color original nodes in boundary color, color whole boundary
        call PigSetSymbolColour ( NodeBColor )
C       - determine boundary section, borrow midbreak to use as temp counter
        midbreak = 0
        i = 0

        pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

        DO WHILE ( i .lt. TotBndys + 1 )
          i = i + 1
          midbreak = midbreak + PtsThisBnd(i)
          IF ( nodes(1) .le. midbreak ) THEN
C           - borrow j to use as boundary #
            j = i
C           - now end the loop
            i = 9999
          ENDIF
        END DO
C           - ( i > TotBndys + 1 )
C       - borrow up to mark 1st node & dwn to mark last node in bndry j
        up = 0
        dwn = 0
        IF ( j .eq. 1 ) THEN
C         - 1st boundary
          up = 1
          dwn = PtsThisBnd(1)
        ELSE IF ( j .eq. 2 ) THEN
C         - 2nd boundary
          up = PtsThisBnd(1) + 1
          dwn = up - 1 + PtsThisBnd(2)
        ELSE
C         - 3rd or higher boundary
          
          pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

          DO i = 1, j - 1
            up = PtsThisBnd(i)
          END DO
          up = up + 1
          dwn = up - 1 + PtsThisBnd(j)
        ENDIF
C         - ( j = 1 )
C       - now color the nodes
        DO i = up, dwn
          call PigDrawSymbols ( 1, dxray(i), dyray(i) )
        END DO
C         - ( i = up, dwn )
C       - endpoints will still be valid, refresh endpoint markers
        call PigSetSymbolColour ( yellow )
        call PigSetSymbolNumber ( SQUARE )
        call PigDrawSymbols ( 1, dxray(nodes(1)), dyray(nodes(1)) )
        call PigDrawSymbols ( 1, dxray(nodes(2)), dyray(nodes(2)) )
      ENDIF
C       - ( ans = Y )

      return
      end
C-----------------------------------------------------------------------*
      SUBROUTINE InputRealInt( inpnumi, inpnumr, numtype, prmpt )
C PURPOSE: Prompts for and accepts REAL or integer values.
C   GIVEN: prmpt = message to prompt with, must end with ':'
C          numtype = 'R' if real value input desired,
C                  = 'I' if integer value input desired.
C RETURNS: inpnumr = returned value if numtype is 'R' (real)
C          inpnumi = returned value if numtype is 'I' (integer)
C          numtype = 'D' (default) if <RTN> only pressed
C EFFECTS:
C WRITTEN: OCT91 by JDM for NODER.
C-----------------------------------------------------------------------*


C - PARAMETERS - (constants)
      REAL*4 seconds
        PARAMETER ( seconds = 2.0 )

C - "INCLUDES"
        INCLUDE '../includes/graf.def'

C - PASSED VARIABLES
        integer inpnumi
      REAL inpnumr
      CHARACTER numtype*1, prmpt*80

C - LOCAL VARIABLES
      LOGICAL valin
      CHARACTER*80 cstr, ans
      integer varlngth
      REAL newval

C------------------START ROUTINE------------------------------

      IF ( numtype .eq. 'I' ) THEN
        varlngth = 6
      ELSE
        varlngth = 9
      ENDIF
C         - ( numtype = 'I' )

C       - prompt for new value
      valin = .FALSE.
      DO WHILE ( .NOT. valin )
        call PigPrompt ( prmpt, ans )
        call PigReadReal ( ans, newval, valin )
        IF ( .NOT. valin ) THEN
C           - a valid number was not entered
          IF ( numtype .eq. 'I' ) THEN
            cstr = 'Not a Valid Number, Enter an integer.'
          ELSE
              cstr = 'Not a Valid Number, Enter a Real Number.'
            ENDIF
C             - ( numtype = 'I' )
            call PigMessageOK ( cstr,'resl' )
!           call PigUwait ( seconds )
          ELSE
C           - success, a valid number was entered, check it
            IF ( newval .eq. 0.0 ) THEN
C             - <RTN> was pressed for default
            numtype = 'D'
          ELSE
C             - user entered value
            IF ( numtype .eq. 'I' ) THEN
            inpnumi = INT( newval )
            ELSE
            inpnumr = newval
            ENDIF
C               - ( numtype = 'I' )
          ENDIF
C             - ( newval = 0.0 )
        ENDIF
C           - ( NOT valin )
      END DO
C         - ( NOT valin )
      END
C-----------------------------------------------------------------------*

C--------------------END NODERESL.FOR-----------------------------------*
C-----------------------------------------------------------------------*
