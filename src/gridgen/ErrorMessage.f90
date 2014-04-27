  !***********************************************************************
  !    Copyright (C) 1995-
  !        Roy A. Walters, R. Falconer Henry
  !
  !        TQGridGen@gmail.com
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

      subroutine TestErrorMessages
      
      implicit none

!      local variables
      integer :: ierr,v1=1,v2=2,v3=3,v4=4
      
      do ierr=1,32
        call ErrorMessage(ierr,v1,v2,v3,v4)
      enddo
      
      end

      subroutine ErrorMessage(ierr,v1,v2,v3,v4)
      
      implicit none
      
      INCLUDE '../includes/defaults.inc'
      
!      passed variables
      integer, intent(in) :: ierr,v1,v2,v3,v4
      
!      local variables
      integer :: lstr,lstr1,lstr2
      character*80 str1,str2
      character*256 cmess
      
      if(ierr.eq.1) then
       ! Error 1
        cmess = '***ERROR IN BCLIP***'//newline//&
                'CONSTRAINED BOUNDARY EDGE NOT FOUND'//newline//&
                'CHECK THAT THIS EDGE IS NOT CROSSED'//newline//&
                'BY ANOTHER BOUNDARY EDGE'//newline
        write(str1,'(2(a,i7))') 'v1 = ',v1,' v2 = ',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.2) then
        ! Error 2
        cmess = '***ERROR IN CONTRI***'//newline//&
                'LESS THAN 3 POINTS IN DATASET'//newline//&
                'CHECK VALUE OF NPTS'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.3) then
       ! Error 3
        cmess = '***ERROR IN CONTRI***'//newline//&
                'LESS THAN 3 POINTS TO BE TRIANGULATED'//newline//&
                'CHECK VALUE OF N'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.4) then
        ! Error 4
       cmess = '***ERROR IN CONTRI***'//newline//&
                'TOO MANY POINTS TO BE TRIANGULATED'//newline//&
                'N MUST BE LESS THAN OR EQUAL TO NPTS'//newline//&
                'CHECK VALUES OF N AND NPTS'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

       elseif(ierr.eq.5) then
       ! Error 5
       cmess = '***ERROR IN CONTRI***'//newline//&
                'NCB GREATER THAN NCE'//newline//&
                'CHECK BOTH VALUES'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.6) then
       ! Error 6
        cmess = '***ERROR IN CONTRI***'//newline//&
                'POINT SHOULD APPEAR ONLY ONCE'//newline
        write(str1,'(2(a,i7)a)') 'POINT',v1,' OCCURS',v2,' TIMES IN LIST'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.7) then
      ! Error 7
        cmess = '***ERROR IN CONTRI***'//newline//&
                'BOUNDARY SEGMENTS DO NOT FORM A CLOSED LOOP'//newline//&
                'CHECK ENTRIES IN COLS 1...NCB OF ELIST'//newline
        write(str1,'(2(a,i7)a)') 'FOR NODE',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.8) then
       !error 8
        cmess = '***ERROR IN CONTRI***'//newline//&
                'ALL POINTS HAVE SAME X-COORD'//newline//&
                'ALL POINTS ARE COLLINEAR'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.9) then
      !error 9
        cmess = '***ERROR IN CONTRI***'//newline//&
                'ALL POINTS HAVE SAME Y-COORD'//newline//&
                'ALL POINTS ARE COLLINEAR'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.10) then
       ! Error 10
        cmess = '***ERROR IN CONTRI***'//newline//&
                'ILLEGAL VALUE IN LIST'//newline
        write(str1,'(2(a,i7))') 'LIST(',v1,' )=',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.11) then
       ! Error 11
        cmess = '***ERROR IN CONTRI***'//newline//&
                'ILLEGAL VALUE IN ELIST'//newline
        write(str1,'(3(a,i7))') 'ELIST(',v1,',',v2,' )=',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.12) then
       ! Error 12
        cmess = '***ERROR IN CONTRI***'//newline//&
                'ILLEGAL VALUE IN ELIST'//newline//&
                'THIS POINT IS NOT IN LIST'//newline
        write(str1,'(3(a,i7))') 'ELIST(',v1,',',v2,' )=',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.13) then
      ! Error 13
        cmess = '***ERROR IN DELAUN***'//newline//&
                'INCORRECT NUMBER OF TRIANGLES FORMED'//newline
        write(str1,'(3(a,i7))') 'NTRI=',v1,' N=',v2,' ndel=',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
    
      elseif(ierr.eq.14) then
        ! Error 14
        cmess = '***ERROR IN INSERTP***'//newline//&
                'STACK OVERFLOW'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.15) then
       ! Error 15
        cmess = '***WARNING IN INSERTP***'//newline
        write(str1,'(3(a,i7))') 'POINTS',v1,' AND',v2,' ARE COINCIDENT'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//&
             'DELETE EITHER POINT FROM LIST VECTOR'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.16) then
      ! Error 16
        cmess = '***ERROR IN EDGE***'//newline//&
                'ILLEGAL VALUE FOR VI'//newline
        write(str1,'(3(a,i7))') 'VI=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.17) then
       ! Error 17
        cmess = '***ERROR IN EDGE***'//newline//&
                'ILLEGAL VALUE FOR VJ'//newline
        write(str1,'(3(a,i7))') 'VJ=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.18) then
       ! Error 18
        cmess = '***ERROR IN EDGE***'//newline
        write(str1,'(3(a,i7))') 'VERTEX ADJACENT TO',v1,' IS ON ARC'
        write(str2,'(3(a,i7))') 'BETWEEN VERTICES',v2,' AND',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//str2(:lstr2)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.19) then
        ! Error 19
        cmess = '***ERROR IN EDGE***'//newline//&
                'NOT ENOUGH WORKSPACE, INCREASE JW'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.20) then
        ! Error 20
        cmess = '***ERROR IN EDGE***'//newline
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS ON ARC'
        write(str2,'(3(a,i7))') 'BETWEEN VERTICES',v2,' AND',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//str2(:lstr2)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.21) then
        ! Error 21
        cmess = '***ERROR IN EDGE***'//newline
        write(str1,'(3(a,i7))') 'ARC BETWEEN VERTICES',v1,' AND',v2
        write(str2,'(3(a,i7))') 'BY VERTICES',v3,'AND' ,v4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//&
           'CROSSES SUPERTRIANGLE BOUNDARY DEFINED '//newline&
            //str2(:lstr2)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.22) then
       ! Error 22
        cmess = '***ERROR IN EDGE***'//newline
        write(str1,'(3(a,i7))') 'ARC BETWEEN VERTICES',v1,' AND',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//&
           'CANNOT BE OPTIMISED SINCE IT IS A '//newline//&
            'SUPERTRIANGLE BOUNDARY'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
       elseif(ierr.eq.23) then
       ! Error 23
        cmess = '***ERROR IN EDGE***'//newline
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS NOT IN ANY TRIANGLE'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.24) then
        ! Error 24
        cmess = '***ERROR IN TCHECK***'//newline//&
                'NUMBER OF BOUNDARY VERTICES LT 3'//newline
        write(str1,'(3(a,i7))') 'NBOV=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.25) then
        ! Error 25
        cmess = '***ERROR IN TCHECK***'//newline//&
                'INVALID TRIANGULATION'//newline//&
                'NTRI IS NOT EQUAL TO 2*(N+NB)-NBOV-4'//newline
        write(str1,'(a,4i7)') ' ntri,n,nb,nbov=',v1,v2,v3,v4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.26) then
        ! Error 26
        cmess = '***ERROR IN TCHECK***'//newline//&
                'INVALID TRIANGULATION'//newline//&
                'NEDG IS NOT EQUAL TO N+NTRI+NB-2'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.27) then
        ! Error 27
        cmess = '***ERROR IN TCHECK***'//newline//&
                'INVALID TRIANGULATION'//newline//&
                'TOO MANY NON-OPTIMAL EDGES'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.28) then
        ! Error 28
        cmess = '***ERROR IN TCHECK***'//newline//&
                'INVALID TRIANGULATION'//newline//&
                'NO. BOUNDARY VERTICES NOT EQUAL TO NBC'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!          WRITE(76,1000)
!          WRITE(76,2000)
! 1000 FORMAT(//,'CHECK THE FOLLOWING CONDITIONS:',
!     +        /,'-EDGES WHICH DEFINE BOUNDARIES MUST COME FIRST IN',
!     +        /,' ELIST AND THUS OCCUPY THE FIRST NCB COLUMNS',
!     +        /,'-EDGES WHICH DEFINE AN EXTERNAL BOUNDARY MUST BE',
!     +        /,' LISTED ANTICLOCKWISE BUT MAY BE PRESENTED IN ANY', 
!     +        /,' ORDER',
!     +        /,'-EDGES WHICH DEFINE AN INTERNAL BOUNDARY (HOLE) MUST',
!     +        /,' BE LISTED CLOCKWISE BUT MAY BE PRESENTED IN ANY', 
!     +        /,' ORDER',
!     +        /,'-AN INTERNAL BOUNDARY (HOLE) CANNOT BE SPECIFIED',
!     +        /,' UNLESS AN EXTERNAL BOUNDARY IS ALSO SPECIFIED')
! 2000 FORMAT(   '-ALL BOUNDARIES MUST FORM CLOSED LOOPS',
!     +        /,'-AN EDGE MAY NOT APPEAR MORE THAN ONCE IN ELIST',
!     +        /,'-AN EXTERNAL OR INTERNAL BOUNDARY MAY NOT CROSS',
!     +        /,' ITSELF AND MAY NOT SHARE A COMMON EDGE WITH ANY', 
!     +        /,' OTHER BOUNDARY',
!     +        /,'-INTERNAL EDGES, WHICH ARE NOT MEANT TO DEFINE',
!     +        /,' BOUNDARIES BUT MUST BE PRESENT IN THE FINAL', 
!     +        /,' TRIANGULATION, OCCUPY COLUMNS NCB+1,... ,NCE OF', 
!     +        /,' ELIST',
!     +        /,'-NO POINT IN THE LIST VECTOR MAY LIE OUTSIDE ANY',
!     +        /,' EXTERNAL BOUNDARY OR INSIDE ANY INTERNAL BOUNDARY')
       
      elseif(ierr.eq.29) then
        ! Error 29
        cmess = '***ERROR IN TCHECK***'//newline//&
                'INVALID TRIANGULATION'//newline
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS NOT IN ANY TRIANGLE'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.30) then
        ! error 30
!              WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                        /,''ZERO OR -VE AREA FOR TRIANGLE'',I5)')L
        cmess = '***WARNING IN TCHECK***'//newline
        write(str1,'(3(a,i7))') 'ZERO OR -VE AREA FOR TRIANGLE',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
    
      elseif(ierr.eq.31) then
        ! error 31
!          WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                    /,''CONSTRAINED EDGE WITH VERTICES'',I5,&
!                      '' AND'',I5,'' IS NOT IN TRIANGULATION'')')V1,V2
        cmess = '***WARNING IN TCHECK***'//newline
        write(str1,'(3(a,i7))') 'CONSTRAINED EDGE WITH VERTICES',v1,' and',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//&
            'IS NOT IN TRIANGULATION'//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.32) then
        ! error 32
        cmess = '***WARNING IN TCHECK***'//newline
        write(str1,'(3(a,i7))') 'CONSTRAINED EDGE WITH VERTICES',v1,' and',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//&
            'IS NOT IN TRIANGULATION'//newline//newline// &
            'IT INTERSECTS ANOTHER CONSTRAINED EDGE'//newline
        write(str1,'(3(a,i7))') 'WITH VERTICES',v3,' AND',V4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//newline//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!                WRITE(76,'(''IT INTERSECTS ANOTHER CONSTRAINED EDGE '',&
!                         ''WITH VERTICES'',I5,'' AND'',I5)')V3,V4
      
      endif
      
      return
      end
      
