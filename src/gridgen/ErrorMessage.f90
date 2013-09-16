
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
      
!      passed variables
      integer, intent(in) :: ierr,v1,v2,v3,v4
      
!      local variables
      integer :: lstr,lstr1,lstr2
      character*80 str1,str2
      character*256 cmess
      
      if(ierr.eq.1) then
       ! Error 1
        cmess = '***ERROR IN BCLIP***'//char(10)//&
                'CONSTRAINED BOUNDARY EDGE NOT FOUND'//char(10)//&
                'CHECK THAT THIS EDGE IS NOT CROSSED'//char(10)//&
                'BY ANOTHER BOUNDARY EDGE'//char(10)
        write(str1,'(2(a,i7))') 'v1 = ',v1,' v2 = ',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.2) then
        ! Error 2
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'LESS THAN 3 POINTS IN DATASET'//char(10)//&
                'CHECK VALUE OF NPTS'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.3) then
       ! Error 3
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'LESS THAN 3 POINTS TO BE TRIANGULATED'//char(10)//&
                'CHECK VALUE OF N'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.4) then
        ! Error 4
       cmess = '***ERROR IN CONTRI***'//char(10)//&
                'TOO MANY POINTS TO BE TRIANGULATED'//char(10)//&
                'N MUST BE LESS THAN OR EQUAL TO NPTS'//char(10)//&
                'CHECK VALUES OF N AND NPTS'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

       elseif(ierr.eq.5) then
       ! Error 5
       cmess = '***ERROR IN CONTRI***'//char(10)//&
                'NCB GREATER THAN NCE'//char(10)//&
                'CHECK BOTH VALUES'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.6) then
       ! Error 6
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'POINT SHOULD APPEAR ONLY ONCE'//char(10)
        write(str1,'(2(a,i7)a)') 'POINT',v1,' OCCURS',v2,' TIMES IN LIST'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.7) then
      ! Error 7
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'BOUNDARY SEGMENTS DO NOT FORM A CLOSED LOOP'//char(10)//&
                'CHECK ENTRIES IN COLS 1...NCB OF ELIST'//char(10)
        write(str1,'(2(a,i7)a)') 'FOR NODE',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.8) then
       !error 8
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'ALL POINTS HAVE SAME X-COORD'//char(10)//&
                'ALL POINTS ARE COLLINEAR'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.9) then
      !error 9
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'ALL POINTS HAVE SAME Y-COORD'//char(10)//&
                'ALL POINTS ARE COLLINEAR'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.10) then
       ! Error 10
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'ILLEGAL VALUE IN LIST'//char(10)
        write(str1,'(2(a,i7))') 'LIST(',v1,' )=',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.11) then
       ! Error 11
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'ILLEGAL VALUE IN ELIST'//char(10)
        write(str1,'(3(a,i7))') 'ELIST(',v1,',',v2,' )=',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
     
      elseif(ierr.eq.12) then
       ! Error 12
        cmess = '***ERROR IN CONTRI***'//char(10)//&
                'ILLEGAL VALUE IN ELIST'//char(10)//&
                'THIS POINT IS NOT IN LIST'//char(10)
        write(str1,'(3(a,i7))') 'ELIST(',v1,',',v2,' )=',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.13) then
      ! Error 13
        cmess = '***ERROR IN DELAUN***'//char(10)//&
                'INCORRECT NUMBER OF TRIANGLES FORMED'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
    
      elseif(ierr.eq.14) then
        ! Error 14
        cmess = '***ERROR IN INSERTP***'//char(10)//&
                'STACK OVERFLOW'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.15) then
       ! Error 15
        cmess = '***WARNING IN INSERTP***'//char(10)
        write(str1,'(3(a,i7))') 'POINTS',v1,' AND',v2,' ARE COINCIDENT'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//&
             'DELETE EITHER POINT FROM LIST VECTOR'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.16) then
      ! Error 16
        cmess = '***ERROR IN EDGE***'//char(10)//&
                'ILLEGAL VALUE FOR VI'//char(10)
        write(str1,'(3(a,i7))') 'VI=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.17) then
       ! Error 17
        cmess = '***ERROR IN EDGE***'//char(10)//&
                'ILLEGAL VALUE FOR VJ'//char(10)
        write(str1,'(3(a,i7))') 'VJ=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.18) then
       ! Error 18
        cmess = '***ERROR IN EDGE***'//char(10)
        write(str1,'(3(a,i7))') 'VERTEX ADJACENT TO',v1,' IS ON ARC'
        write(str2,'(3(a,i7))') 'BETWEEN VERTICES',v2,' AND',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//str2(:lstr2)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.19) then
        ! Error 19
        cmess = '***ERROR IN EDGE***'//char(10)//&
                'NOT ENOUGH WORKSPACE, INCREASE JW'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.20) then
        ! Error 20
        cmess = '***ERROR IN EDGE***'//char(10)
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS ON ARC'
        write(str2,'(3(a,i7))') 'BETWEEN VERTICES',v2,' AND',v3
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//str2(:lstr2)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.21) then
        ! Error 21
        cmess = '***ERROR IN EDGE***'//char(10)
        write(str1,'(3(a,i7))') 'ARC BETWEEN VERTICES',v1,' AND',v2
        write(str2,'(3(a,i7))') 'BY VERTICES',v3,'AND' ,v4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        lstr2 = len_trim(str2)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//&
           'CROSSES SUPERTRIANGLE BOUNDARY DEFINED '//char(10)&
            //str2(:lstr2)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
      
      elseif(ierr.eq.22) then
       ! Error 22
        cmess = '***ERROR IN EDGE***'//char(10)
        write(str1,'(3(a,i7))') 'ARC BETWEEN VERTICES',v1,' AND',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//&
           'CANNOT BE OPTIMISED SINCE IT IS A '//char(10)//&
            'SUPERTRIANGLE BOUNDARY'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
       elseif(ierr.eq.23) then
       ! Error 23
        cmess = '***ERROR IN EDGE***'//char(10)
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS NOT IN ANY TRIANGLE'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.24) then
        ! Error 24
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'NUMBER OF BOUNDARY VERTICES LT 3'//char(10)
        write(str1,'(3(a,i7))') 'NBOV=',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
       
      elseif(ierr.eq.25) then
        ! Error 25
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'INVALID TRIANGULATION'//char(10)//&
                'NTRI IS NOT EQUAL TO 2*(N+NB)-NBOV-4'//char(10)
        write(str1,'(a,4i7)') ' ntri,n,nb,nbov=',v1,v2,v3,v4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.26) then
        ! Error 26
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'INVALID TRIANGULATION'//char(10)//&
                'NEDG IS NOT EQUAL TO N+NTRI+NB-2'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.27) then
        ! Error 27
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'INVALID TRIANGULATION'//char(10)//&
                'TOO MANY NON-OPTIMAL EDGES'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!        WRITE(76,1000)
!        WRITE(76,2000)
       
      elseif(ierr.eq.28) then
        ! Error 28
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'INVALID TRIANGULATION'//char(10)//&
                'NO. BOUNDARY VERTICES NOT EQUAL TO NBC'//char(10)//char(0)
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
        cmess = '***ERROR IN TCHECK***'//char(10)//&
                'INVALID TRIANGULATION'//char(10)
        write(str1,'(3(a,i7))') 'VERTEX',v1,' IS NOT IN ANY TRIANGLE'
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.30) then
        ! error 30
!              WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                        /,''ZERO OR -VE AREA FOR TRIANGLE'',I5)')L
        cmess = '***WARNING IN TCHECK***'//char(10)
        write(str1,'(3(a,i7))') 'ZERO OR -VE AREA FOR TRIANGLE',v1
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
    
      elseif(ierr.eq.31) then
        ! error 31
!          WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                    /,''CONSTRAINED EDGE WITH VERTICES'',I5,&
!                      '' AND'',I5,'' IS NOT IN TRIANGULATION'')')V1,V2
        cmess = '***WARNING IN TCHECK***'//char(10)
        write(str1,'(3(a,i7))') 'CONSTRAINED EDGE WITH VERTICES',v1,' and',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//&
            'IS NOT IN TRIANGULATION'//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))

      elseif(ierr.eq.32) then
        ! error 32
        cmess = '***WARNING IN TCHECK***'//char(10)
        write(str1,'(3(a,i7))') 'CONSTRAINED EDGE WITH VERTICES',v1,' and',v2
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//&
            'IS NOT IN TRIANGULATION'//char(10)//char(10)// &
            'IT INTERSECTS ANOTHER CONSTRAINED EDGE'//char(10)
        write(str1,'(3(a,i7))') 'WITH VERTICES',v3,' AND',V4
        lstr = len_trim(cmess)
        lstr1 = len_trim(str1)
        cmess = cmess(:lstr)//str1(:lstr1)//char(10)//char(0)
        call PigMessageOK(cmess, 'triangulate'//char(0))
!                WRITE(76,'(''IT INTERSECTS ANOTHER CONSTRAINED EDGE '',&
!                         ''WITH VERTICES'',I5,'' AND'',I5)')V3,V4
      
      endif
      
      return
      end
      
