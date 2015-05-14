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

!*--------------------------------------------------------------------------*
!                         CONTBND.FOR                                       *
!    The subroutines in this module are the routines for reading            *
!    and displaying contour and boundary lines.                             *
!*--------------------------------------------------------------------------*
!*--------------------------------------------------------------------------*
      SUBROUTINE DispContBound

! Given   : None
! Returns : None
! Effects : The contour file is read if DispCont flag is set true.
!           Boundary file is read if DispBond flag is set true.
! Modified: Steve Prestage and Daphne Connolly May 1989

      INCLUDE '../includes/defaults.inc'
      include '../includes/graf.def'

      call PigSetWindowNum( MAINWIN )

      IF ( DispBound ) THEN
        call READB1
!       CLOSE(33)
      ENDIF

      IF ( DispCont ) THEN
        call READC1
!       CLOSE(34)
      ENDIF
      END


!*--------------------------------------------------------------------------*
      SUBROUTINE READB1

! Given   :  None
! Returns :  None
! Effects :  Boundary file specified is opened and points are read
!            and drawn on the screen.
! Modified:  Daphne Connolly and Steve Prestage May 1989

      use mainarrays, only: x0off, y0off

      integer maxpoint
      parameter (maxpoint=500)

      INCLUDE '../includes/defaults.inc'
      include '../includes/graf.def'

! Following common block used for reading in boundary and contour points.
      REAL XOUT, YOUT
      COMMON /tempstore/ XOUT(MAXPOINT), YOUT(MAXPOINT)

!***local variables
      REAL XTemp, YTemp
      integer PTCOUNT
      integer itemp, jtemp, PrevColour
      integer :: NodBnds, NodThisBnd
      character(80) Firstline 

!     BEGIN

      call PigGetLineColour(PrevColour)
      call PigSetLineColour( BoundColour )

      OPEN(UNIT=33, FILE=BoundFName, ERR=890, STATUS='OLD')

      READ(33,'(a)', err=990, end=990) Firstline

      if(firstline(1:4).eq."#NOD") then  !node boundary file
        do
          READ(33,'(a)', err=990, end=990) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
            read(firstline,*,err=990, end=990) x0off2, y0off2, scaleX2, scaleY2, igridtype2
            exit
          endif
        enddo

        READ (33, *, ERR=990, END=990 )    !skip line
        READ (33, *, ERR=990, END=990 ) NodBnds
        do j=1,NodBnds
          PTCOUNT = 0
          READ (33, *, ERR=990, END=990 ) NodThisBnd
          do k=1,NodThisBnd
            READ (33, *, ERR=999, END=999 ) XTemp, YTemp
            PTCOUNT = PTCOUNT + 1
            XOut(PTCOUNT) = (XTemp+x0off2)*scaleX2 - x0off
            YOut(PTCOUNT) = (YTemp+y0off2)*scaleX2 - y0off
            IF ((PTCOUNT .GE. MAXPOINT)) THEN
              CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
              PTCOUNT = 0
            endif
          enddo
          IF ((PTCOUNT .GT. 1)) THEN
            CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
            PTCOUNT = 0
          endif
        enddo
        go to 999

      else                 !dig boundary file

        rewind (33)
        READ (33, *, ERR=990, END=990 ) XTemp, YTemp

        ITemp=nint(XTemp)
        JTemp=nint(YTemp)
        if(ITemp.ne.77.or.JTemp.ne.77) go to 75
 
50      CONTINUE
        READ (33, *, ERR=990, END=990 ) XTemp, YTemp
        ITemp = nint(YTemp)
        IF (ITemp .EQ. -9999) GOTO 70
        go to 50

70      CONTINUE

        READ (33, *, ERR=990, END=990 ) XTemp, YTemp

75      continue

        PTCOUNT = 0
! DO WHILE PTCOUNT .LE. MAXPOINT
115     IF ( PTCOUNT .NE. MAXPOINT) THEN
          READ (33, *, ERR=999, END=999 ) XTemp, YTemp
          ITemp = nint(YTemp)
          IF ((ITemp .EQ. -9999) .AND. (PTCOUNT .LE. 1)) THEN
            PTCOUNT = 0
            GOTO 140
          ELSEIF ((ITemp .EQ. -9999).AND.(PTCOUNT .GT. 1)) THEN
            CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
            PTCOUNT = 0
            GOTO 140
          ELSE
            PTCOUNT = PTCOUNT + 1
            XOut(PTCOUNT) = XTemp
            YOut(PTCOUNT) = YTemp
          ENDIF
          GOTO 115
        ENDIF
! ENDWHILE
        CALL PigDrawPolyline( PTCOUNT, XOUT, YOUT )
        XOUT(1) = XOUT(PTCOUNT)
        YOUT(1) = YOUT(PTCOUNT)
        PTCOUNT = 1
        go to 115

140     CONTINUE
        READ (33, *, ERR=999, END=999 ) XTemp, YTemp
        go to 115

      endif  !file type

!    Error trap : could not open file..
890   continue
      call PigPutMessage( 'ERROR - Can not open boundary file!!  Pl'//&
     &         'ease check configuration.             ')
      DispBound = .FALSE.
      goto 999

!    Error trap : end of file while reading reference points
990   continue
      call PigPutMessage( 'ERROR - Premature end-of-file.. Please c'//&
     &         'heck configuration and/or data file.')
      DispBound = .FALSE.

999   CONTINUE

      close(33)
      call PigSetLineColour(PrevColour)

      END


!*--------------------------------------------------------------------------*
      SUBROUTINE READC1

! Given    : None
! Returns  : None
! Effect   : Opens the contour file and reads and draws contours.
! Modified : Daphne Connolly and Steve Prestage May 1989

      use mainarrays, only: x0off, y0off

      integer maxpoint
      parameter (maxpoint=500)

      INCLUDE '../includes/defaults.inc'

! Following common block used for reading in boundary and contour points.
      REAL XOUT, YOUT
      COMMON /tempstore/ XOUT(MAXPOINT), YOUT(MAXPOINT)

!**   Local variables
      REAL XTemp, YTemp
      integer PTCOUNT
      integer itemp, jtemp, PrevColour
        integer :: NodBnds, NodThisBnd
      character(80) Firstline 

!     BEGIN

      call PigGetLineColour(PrevColour)
      call PigSetLineColour(ContColour)

      OPEN( UNIT=34, FILE=ContFName, ERR=890, STATUS='OLD')

      READ(34,'(a)', err=990, end=990) Firstline

      if(firstline(1:4).eq."#NOD") then  !node boundary file
        do
          READ(34,'(a)', err=990, end=990) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
            read(firstline,*,err=990, end=990) x0off2, y0off2, scaleX2, scaleY2, igridtype2
            exit
          endif
        enddo

        READ (34, *, ERR=990, END=990 )    !skip line
        READ (34, *, ERR=990, END=990 ) NodBnds
        do j=1,NodBnds
          PTCOUNT = 0
          READ (34, *, ERR=990, END=990 ) NodThisBnd
            do k=1,NodThisBnd
              READ (34, *, ERR=999, END=999 ) XTemp, YTemp
              PTCOUNT = PTCOUNT + 1
              XOut(PTCOUNT) = (XTemp+x0off2)*scaleX2 - x0off
              YOut(PTCOUNT) = (YTemp+y0off2)*scaleX2 - y0off
              IF ((PTCOUNT .GE. MAXPOINT)) THEN
                CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
                PTCOUNT = 0
              endif
            enddo
            IF ((PTCOUNT .GT. 1)) THEN
              CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
              PTCOUNT = 0
            endif
        enddo
        go to 999

      else                 !dig contour file

        rewind (34)
        READ (34, *, ERR=990, END=990 ) XTemp, YTemp

        ITemp=nint(XTemp)
        JTemp=nint(YTemp)
        if(ITemp.ne.77.or.JTemp.ne.77) go to 75
 
50      continue
          READ (34, *, ERR=990, END=990 ) XTemp, YTemp
          ITemp = nint(YTemp)
          IF (ITemp .EQ. -9999) GOTO 70
        go to 50

70      CONTINUE

        READ (34, *, ERR=990, END=990 ) XTemp, YTemp

75      continue

        PTCOUNT = 0

! DO WHILE (PTCOUNT .LE. MAXPOINT)
115     IF(PTCOUNT .NE. MAXPOINT) THEN
          READ (34, *, ERR=999, END=999 ) XTemp, YTemp
          ITemp = nint(YTemp)
          IF ((ITemp .EQ. -9999).AND.(PTCOUNT .LE. 1)) THEN
            PTCOUNT = 0
            GOTO 140
          ELSEIF ((ITemp .EQ. -9999).AND.(PTCOUNT .GT. 1)) THEN
            CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
            PTCOUNT = 0
            GOTO 140
          ELSE
            PTCOUNT = PTCOUNT + 1
            XOut(PTCOUNT) = XTemp
            YOut(PTCOUNT) = YTemp
          ENDIF
            GOTO 115
        ENDIF
! ENDWHILE
        CALL PigDrawPolyline(PTCOUNT, XOUT, YOUT)
        XOUT(1) = XOUT(PTCOUNT)
        YOUT(1) = YOUT(PTCOUNT)
        PTCOUNT = 1
        go to 115

140     CONTINUE
        READ (34, *, ERR=999, END=999 ) XTemp, YTemp
        go to 115

      endif

!    Error trap : could not open file..
890   continue
      call PigPutMessage( 'ERROR - Can not open contour file!!   Pl'//&
     &         'ease check configuration.             ')
      DispCont = .FALSE.
      goto 999

!    Error trap : end of file while reading reference points
990   continue
      call PigPutMessage( 'ERROR - Premature end-of-file.. Please c'//&
     &         'heck configuration and/or data file.  ')
      DispCont = .FALSE.

999   CONTINUE
      close(34)
!      call PigSetWindowNum( MAINWIN )
      call PigSetLineColour(PrevColour)

      END

!*--------------------------------------------------------------------------*
!                       END CONTBND.FOR
!*--------------------------------------------------------------------------*
