!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: bclip.f
!X-Sun-Content-Lines: 277

      SUBROUTINE BCLIP(NPTS,NCB,BLIST,TN,V,T,NTRI,NB)
!**********************************************************************
!
!     PURPOSE:
!     --------
!
!     Clip constrained Delaunay triangulation to boundaries in BLIST
!     If BLIST is empty, then the triangulation is clipped to a convex
!     hull by removing all triangles that are formed with the 
!     supertriangle vertices
!     The triangulation MUST initially include the supertriangle 
!     vertices
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set (NPTS ge N)
!     NCB    - Total number of constrained edges which define one
!              external boundary and any internal boundaries (holes)
!            - NCB=0 indicates there are no boundary edge constraints
!              and the code will produce a triangulation of the convex
!              hull
!     BLIST  - List of edges which must be present in triangulation
!              and define boundaries
!            - Edge I defined by vertices in BLIST(1,I) and BLIST(2,I)
!              where I ranges from 1,...,NCB
!            - Edges which define an external boundary must be listed
!              anticlockwise but may be presented in any order
!            - Edges which define an internal boundary (hole) must be 
!              listed clockwise but may be presented in any order
!     TN     - List of triangle numbers such that vertex I can be 
!              found in triangle TN(I)
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Number of triangles, including those formed with
!              vertices of supertriangle
!     NB     - Not defined
!
!     OUTPUT:
!     -------
!
!     NPTS   - Unchanged
!     NCB    - Unchanged
!     BLIST  - Unchanged
!     TN     - Not defined 
!     V      - Updated vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Updated adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Updated number of triangles in final triangulation
!     NB     - Number of boundaries defining the triangulation
!            - NB=1 for a simply connected domain with no holes 
!            - NB=H+1 for a domain with H holes
!
!     PROGRAMMER:           Scott Sloan
!     -----------
!
!     LAST MODIFIED:        3 march 1991        Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:       Scott Sloan
!     ---------------       Department of Civil Engineering
!                           University of Newcastle
!                           NSW 2308
!
!**********************************************************************
      INTEGER A,C,E,I,J,L,R,S
      INTEGER NB,TS,V1,V2
      INTEGER NCB,NEV
      INTEGER NPTS,NTRI
      INTEGER NNTRI,NPTS1,NTRI1
      INTEGER T(3,NTRI),V(4,NTRI)
      INTEGER TN(NPTS+3)
      INTEGER BLIST(2,*)
!      integer ncode(npts+3)
!---------------------------------------------------------------------
!     Skip to triangle deletion step if triangulation has no
!     boundary constraints
!
      IF(NCB.EQ.0)THEN
        NB=1
        GOTO 55
      ENDIF
!---------------------------------------------------------------------
!     Mark all edges which define the boundaries
!     S=triangle in which search starts
!     R=triangle to right of edge V1-V2
!     
      DO 20 I=1,NCB
        V1=BLIST(1,I)
        V2=BLIST(2,I)
        S=TN(V1)
        R=S

!       Circle anticlockwise round V1 until edge V1-V2 is found

   10   IF(V(1,R).EQ.V1)THEN
          IF(V(3,R).EQ.V2)THEN
            T(3,R)=-T(3,R)
            GOTO 20
          ELSE
            R=ABS(T(3,R))
          ENDIF
        ELSEIF(V(2,R).EQ.V1)THEN
          IF(V(1,R).EQ.V2)THEN
            T(1,R)=-T(1,R)
            GOTO 20
          ELSE
            R=ABS(T(1,R))
          ENDIF
        ELSEIF(V(2,R).EQ.V2)THEN
            T(2,R)=-T(2,R)
            GOTO 20
        ELSE
            R=ABS(T(2,R))
        ENDIF
        IF(R.EQ.S)THEN
       ! Error 1
!          WRITE(76,'(//,''***ERROR IN BCLIP***'',
!     +               /,''CONSTRAINED BOUNDARY EDGE NOT FOUND'',
!     +               /,''V1='',I5,'' V2='',I5,
!     +               /,''CHECK THAT THIS EDGE IS NOT CROSSED'',
!     +               /,''BY ANOTHER BOUNDARY EDGE'')')V1,V2
!          STOP
          NB = -1
          call ErrorMessage(1,v1,v2,v2,v2)
          return
        ENDIF
        GOTO 10
   20 CONTINUE
!--------------------------------------------------------------------
!     Mark all triangles which are to right of boundaries by
!     circling anticlockwise around the outside of each boundary
!     Loop while some boundary edges have not been visited
!     S = triangle from which search starts
!     NEV = number of edges visited

      S  =0
      NB =0
      NEV=0
      NTRI1=NTRI+1
      NPTS1=NPTS+1
   30 IF(NEV.LT.NCB)THEN

!       Find an edge on a boundary

   40   S=S+1
        IF(T(1,S).LT.0)THEN
          E=1
        ELSEIF(T(2,S).LT.0)THEN
          E=2
        ELSEIF(T(3,S).LT.0)THEN
          E=3
        ELSE
          GOTO 40
        ENDIF

!       Store and mark starting edge
!       Mark starting triangle for deletion 
!       Increment count of edges visited
!       C = current triangle

        TS =T(E,S)
        T(E,S)=NTRI1
        V(1,S)=NPTS1
        NEV=NEV+1
        C=S

!       Increment to next edge

        E=MOD(E+1,3)+1

!       Loop until trace of current boundary is complete

   50   IF(T(E,C).NE.NTRI1)THEN
          IF(T(E,C).LT.0)THEN

!           Have found next boundary edge
!           Increment count of boundary edges visited, unmark the edge
!           and move to next edge 

            NEV=NEV+1
            T(E,C)=-T(E,C)
            E=MOD(E+1,3)+1
          ELSE

!           Non-boundary edge, circle anticlockwise around boundary 
!           vertex to move to next triangle, and mark next
!           triangle for deletion

            L=C
            C=T(E,L)
            IF(T(1,C).EQ.L)THEN
              E=3
            ELSEIF(T(2,C).EQ.L)THEN
              E=1
            ELSE
              E=2
            ENDIF
            V(1,C)=NPTS1
          ENDIF
          GOTO 50
        ENDIF

!       Trace of current boundary is complete
!       Reset marked edge to original value and check for any more
!       boundaries

        T(E,C)=-TS
        NB=NB+1
        GOTO 30
      ENDIF
!---------------------------------------------------------------------
!     Remove all triangles which have been marked for deletion
!     Any triangle with a vertex number greater than NPTS is deleted
!     
   55 CONTINUE
      NNTRI=NTRI
      NTRI =0
      DO 80 J=1,NNTRI
        IF(MAX(V(1,J),V(2,J),V(3,J)).GT.NPTS)THEN

!         call SetOutsideCodes( V(1,J),V(2,J),V(3,J) )
!          ncode(V(1,J)) = 1
!          ncode(V(2,J)) = 1
!          ncode(V(3,J)) = 1

!         Triangle J is to be deleted
!         Update adjacency lists for triangles adjacent to J
! 
          DO 60 I=1,3
            A=T(I,J)
            IF(A.NE.0)THEN
              IF(T(1,A).EQ.J)THEN
                T(1,A)=0
              ELSEIF(T(2,A).EQ.J)THEN
                T(2,A)=0
              ELSE
                T(3,A)=0
              ENDIF
            ENDIF
   60     CONTINUE
        ELSE

!         Triangle J is not to be deleted
!         Update count of triangles

          NTRI=NTRI+1
          IF(NTRI.LT.J)THEN

!           At least one triangle has already been deleted
!           Relabel triangle J as triangle NTRI

            DO 70 I=1,3
              A=T(I,J)
              T(I,NTRI)=A
              V(I,NTRI)=V(I,J)
              IF(A.NE.0)THEN
                IF(T(1,A).EQ.J)THEN
                  T(1,A)=NTRI
                ELSEIF(T(2,A).EQ.J)THEN
                  T(2,A)=NTRI
                ELSE
                  T(3,A)=NTRI
                ENDIF
              ENDIF
  70        CONTINUE
          ENDIF
        ENDIF
  80  CONTINUE 
      END
!X----------
!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: bsort.f
!X-Sun-Content-Lines: 137

      SUBROUTINE BSORT(N,NPTS,X,Y,XMIN,XMAX,YMIN,YMAX,DMAX,BIN,LIST,W)
!***********************************************************************
!
!     PURPOSE:
!     --------
!
!     Sort points such that consecutive points are close to one another 
!     in the x-y plane using a bin sort
!
!     INPUT:
!     ------
!
!     N      - Total number of points to be triangulated (N le NPTS)
!     NPTS   - Total number of points in data set
!     X      - X-coords of all points in data set
!            - If point is in list,the coordinate must be normalised
!              according to X=(X-XMIN)/DMAX
!            - X-coord of point I given by X(I)
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - If point is in list,the coordinate must be normalised
!              according to Y=(Y-YMIN)/DMAX
!            - Y-coord of point I given by Y(I)
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     XMIN   - Min x-coord of points in LIST
!     XMAX   - Max x-coord of points in LIST
!     YMIN   - Min y-coord of points in LIST
!     YMAX   - Max y-coord of points in LIST
!     DMAX   - DMAX=MAX(XMAX-XMIN,YMAX-YMIN)
!     BIN    - Not defined
!     LIST   - List of points to be triangulated
!     W      - Undefined workspace
!
!     OUTPUT:
!     -------
!
!     N      - Unchanged
!     NPTS   - Unchanged
!     X      - Unchanged
!     Y      - Unchanged
!     XMIN   - Unchanged
!     XMAX   - Unchanged
!     YMIN   - Unchanged
!     YMAX   - Unchanged
!     DMAX   - Unchanged
!     BIN    - Not used
!     LIST   - List of points to be triangulated
!            - Points ordered such that consecutive points are close
!              to one another in the x-y plane
!     W      - Not used
!
!     PROGRAMMER:             Scott Sloan
!     -----------
!
!     LAST MODIFIED:          3 march 1991        Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:         Scott Sloan
!     ---------------         Department of Civil Engineering
!                             University of Newcastle
!                             NSW 2308
!
!***********************************************************************
      INTEGER B,I,J,K,N,P
      INTEGER LI,LT,NB
      INTEGER NDIV,NPTS
      INTEGER W(N)
      INTEGER BIN(NPTS)
      INTEGER LIST(N)

      REAL DMAX,XMAX,XMIN,YMAX,YMIN
      REAL FACTX,FACTY
      REAL X(NPTS+3),Y(NPTS+3)
!---------------------------------------------------------------------
!     Compute number of bins in x-y directions
!     Compute inverse of bin size in x-y directions

      NDIV=NINT(REAL(N)**0.25)
      FACTX=REAL(NDIV)/((XMAX-XMIN)*1.01/DMAX)
      FACTY=REAL(NDIV)/((YMAX-YMIN)*1.01/DMAX)
!---------------------------------------------------------------------
!     Zero count of points in each bin

      NB=NDIV*NDIV
      DO 5 I=1,NB
        W(I)=0
    5 CONTINUE
!---------------------------------------------------------------------
!     Assign bin numbers to each point
!     Count entries in each bin and store in W

      DO 10 K=1,N
        P=LIST(K)
        I=INT(Y(P)*FACTY)
        J=INT(X(P)*FACTX)
        IF(MOD(I,2).EQ.0)THEN
          B=I*NDIV+J+1
        ELSE
          B=(I+1)*NDIV-J
        ENDIF
        BIN(P)=B
        W(B)=W(B)+1
   10 CONTINUE
!---------------------------------------------------------------------
!     Form pointers to end of each bin in final sorted list
!     Note that some bins may contain no entries

      DO 20 I=2,NB
        W(I)=W(I-1)+W(I)
   20 CONTINUE
!---------------------------------------------------------------------
!     Now perform linear sort

      DO 40 I=1,N
        IF(LIST(I).GT.0)THEN
          LI=LIST(I)
          B=BIN(LI)
          P=W(B)
   30     IF(P.NE.I)THEN
            W(B)=W(B)-1
            LT =LIST(P)
            LIST(P)=LI
            LI=LT
            B=BIN(LI)
            LIST(P)=-LIST(P)
            P=W(B)
            GOTO 30
          ENDIF
          W(B)=W(B)-1
          LIST(I)=LI
        ELSE
          LIST(I)=-LIST(I)
        ENDIF
   40 CONTINUE
      END
!X----------
!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: contri.f
!X-Sun-Content-Lines: 323

      SUBROUTINE CONTRI(NPTS,N,NCE,NCB,ELIST,X,Y,ncode,LIST,W,V,T,NTRI,AutoGenFlag)
!**********************************************************************
!
!     PURPOSE:
!     --------
!
!     Assemble constrained Delaunay triangulation for collection of 
!     points in the plane. This code has a total memory requirement
!     equal to 4*NPTS + 13*N + 2*NCE + 18
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set (NPTS ge N)
!     N      - Total number of points to be triangulated (N ge 3)
!     NCE    - Total number of constrained edges which must be 
!              present, including those which define boundaries
!            - NCE=0 indicates triangulation is unconstrained so that
!              ELIST is empty and the code will produce a 
!              triangulation of the convex hull
!     NCB    - Total number of constrained edges which define one
!              external boundary and any internal boundaries (holes)
!            - NCB=0 indicates there are no boundary edge constraints
!              and the code will produce a triangulation of the convex
!              hull
!            - NCB must not be greater than NCE
!            - If NCB gt 0, then at least one of the boundaries
!              specified must be external
!     ELIST  - List of edges which must be present in triangulation
!            - These may be internal edges or edges which define 
!              boundaries
!            - Edge I defined by vertices in ELIST(1,I) and ELIST(2,I)
!            - Edges which define boundaries must come first in ELIST
!              and thus occupy the first NCB columns
!            - Edges which define an external boundary must be listed
!              anticlockwise but may be presented in any order
!            - Edges which define an internal boundary (hole) must be 
!              listed clockwise but may be presented in any order
!            - An internal boundary (hole) cannot be specified unless
!              an external boundary is also specified
!            - All boundaries must form closed loops
!            - An edge may not appear more than once in ELIST
!            - An external or internal boundary may not cross itself
!              and may not share a common edge with any other boundary
!            - Internal edges, which are not meant to define boundaries 
!              but must be present in the final triangulation, occupy
!              columns NCB+1,... ,NCE of ELIST
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     LIST   - List of points to be triangulated
!            - If N eq NPTS, set LIST(I)=I for I=1,2,...,NPTS
!              prior to calling this routine
!            - No point in LIST may lie outside any external boundary
!              or inside any internal boundary 
!     W      - Not defined, workspace of length ge 2*(NPTS+3)
!     V      - Not defined
!     T      - Not defined
!     NTRI   - Not defined
!
!     OUTPUT:
!     -------
!
!     NPTS   - Unchanged
!     N      - Unchanged
!     NCE    - Unchanged
!     NCB    - Unchanged
!     ELIST  - Unchanged
!     X      - Unchanged, except that last three locations contain
!              normalised x-coords of supertriangle vertices
!     Y      - Unchanged, except that last three locations contain
!              normalised y-coords of supertriangle vertices
!     LIST   - List of points to be triangulated
!            - Points ordered such that consecutive points are close
!              to one another in the x-y plane
!     W      - Not defined
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Total number of triangles in final triangulation
!
!     SUBROUTINES  CALLED:  BSORT, DELAUN, EDGE, TCHECK, BCLIP
!     --------------------
!
!     PROGRAMMER:             Scott Sloan
!     -----------
!
!     LAST MODIFIED:          3 march 1991        Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:         Scott Sloan
!     ---------------         Department of Civil Engineering
!                             University of Newcastle
!                             NSW 2308
!
!**********************************************************************
      integer AutoGenFlag
      INTEGER I,J,N,P
      INTEGER JW,NB,VI,VJ
      INTEGER NCB,NCE,NEF
      INTEGER NPTS,NTRI
      INTEGER T(3,2*NPTS+1),V(4,2*NPTS+1),W(NPTS+3,2)
      INTEGER LIST(NPTS)
      INTEGER ELIST(2,npts)
      integer ncode(npts+3)


      REAL DMAX,XMIN,XMAX,YMIN,YMAX
      REAL C00001
      REAL FACT
      REAL X(NPTS+3),Y(NPTS+3)

      PARAMETER(C00001=1.0)
!---------------------------------------------------------------------

!     Test error messages
!      call TestErrorMessages


!     Check input for obvious errors

      IF(NPTS.LT.3)THEN
       ! Error 2
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''LESS THAN 3 POINTS IN DATASET'',
!     +             /,''CHECK VALUE OF NPTS'')')
!        STOP

        AutoGenFlag = -2
        call ErrorMessage(2,N,N,N,N)
        return
      ENDIF
      IF(N.LT.3)THEN
       ! Error 3
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''LESS THAN 3 POINTS TO BE TRIANGULATED'',
!     +             /,''CHECK VALUE OF N'')')
!        STOP
        AutoGenFlag = -3
        call ErrorMessage(3,N,N,N,N)
        return

      ELSEIF(N.GT.NPTS)THEN
       ! Error 4
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''TOO MANY POINTS TO BE TRIANGULATED'',
!     +             /,''N MUST BE LESS THAN OR EQUAL TO NPTS'',
!     +             /,''CHECK VALUES OF N AND NPTS'')')
!        STOP
        AutoGenFlag = -4
        call ErrorMessage(4,N,N,N,N)
        return

      ENDIF
      IF(NCB.GT.NCE)THEN
       ! Error 5
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''NCB GREATER THAN NCE'',
!     +             /,''CHECK BOTH VALUES'')')
!        STOP
        AutoGenFlag = -5
        call ErrorMessage(5,N,N,N,N)
        return

      ENDIF
!---------------------------------------------------------------------
!     Check for invalid entries in LIST

      DO 10 I=1,N
        P=LIST(I)
        IF(P.LT.1.OR.P.GT.NPTS)THEN
       ! Error 10
!          WRITE(76,1000)I,P
!          STOP
          AutoGenFlag = -10
          call ErrorMessage(10,I,P,P,P)
          return

        ENDIF
        W(P,1)=0
   10 CONTINUE  
      DO 20 I=1,N
        P=LIST(I)
        W(P,1)=W(P,1)+1
   20 CONTINUE
      DO 30 I=1,N
        P=LIST(I)
        IF(W(P,1).GT.1)THEN
       ! Error 6
!          WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +               /,''POINT'',I5,'' OCCURS'',I5,'' TIMES IN LIST'',
!     +               /,''POINT SHOULD APPEAR ONLY ONCE'')')P,W(P,1)
!          STOP
          AutoGenFlag = -6
          call ErrorMessage(6,P,W(P,1),P,P)
          return

        ENDIF
   30 CONTINUE
!---------------------------------------------------------------------
!     Check for invalid entries in ELIST

      IF(NCE.GT.0)THEN
        DO 40 I=1,NPTS
          W(I,1)=0
   40   CONTINUE
        DO 50 I=1,N
          W(LIST(I),1)=1
   50   CONTINUE
        DO 60 I=1,NCE
          VI=ELIST(1,I)
          IF(VI.LT.1.OR.VI.GT.NPTS)THEN
       ! Error 11
!            WRITE(76,2000)1,I,VI
!            STOP
            AutoGenFlag = -11
            call ErrorMessage(11,1,I,VI,VI)
            return

          ELSEIF(W(VI,1).NE.1)THEN
       ! Error 12
!            WRITE(76,3000)1,I,VI
!            STOP
            AutoGenFlag = -12
            call ErrorMessage(12,1,I,VI,VI)
            return

          ENDIF
          VJ=ELIST(2,I)
          IF(VJ.LT.1.OR.VJ.GT.NPTS)THEN
       ! Error 11
!            WRITE(76,2000)2,I,VJ
!            STOP
          AutoGenFlag = -11
          call ErrorMessage(11,2,I,VJ,VJ)
          return

          ELSEIF(W(VJ,1).NE.1)THEN
       ! Error 12
!            WRITE(76,3000)2,I,VJ
!            STOP
            AutoGenFlag = -12
            call ErrorMessage(12,2,I,VJ,VJ)
            return

          ENDIF
   60   CONTINUE
      ENDIF
!---------------------------------------------------------------------
!     Check that all boundaries (if there are any) form closed loops
!     Count appearances in ELIST(1,.) and ELIST(2,.) of each node
!     These must match if each boundary forms a closed loop

      IF(NCB.GT.0)THEN
        DO 70 I=1,NPTS
          W(I,1)=0
          W(I,2)=0
   70   CONTINUE
        DO 80 I=1,NCB
          VI=ELIST(1,I)
          VJ=ELIST(2,I)
          W(VI,1)=W(VI,1)+1
          W(VJ,2)=W(VJ,2)+1
   80   CONTINUE
        DO 90 I=1,NCB
          VI=ELIST(1,I)
          IF(W(VI,1).NE.W(VI,2))THEN
       ! Error 7
!            WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +                 /,''BOUNDARY SEGMENTS DO NOT FORM A '',
!     +                   ''CLOSED LOOP'',
!     +                 /,''CHECK ENTRIES IN COLS 1...NCB OF ELIST '',
!     +                   ''FOR NODE'',I5)')VI
!            STOP
            AutoGenFlag = -7
            call ErrorMessage(7,vi,vi,VI,VI)
            return

          ENDIF
   90   CONTINUE        
      ENDIF
!---------------------------------------------------------------------
!     Compute min and max coords for x and y
!     Compute max overall dimension

      P=LIST(1)
      XMIN=X(P)
      XMAX=XMIN
      YMIN=Y(P)
      YMAX=YMIN
      DO 100 I=2,N
        P=LIST(I)
        XMIN=MIN(XMIN,X(P))
        XMAX=MAX(XMAX,X(P))
        YMIN=MIN(YMIN,Y(P))
        YMAX=MAX(YMAX,Y(P))
  100 CONTINUE
      IF(XMIN.EQ.XMAX)THEN
      !error 8
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''ALL POINTS HAVE SAME X-COORD'',
!     +             /,''ALL POINTS ARE COLLINEAR'')')
!        STOP
        AutoGenFlag = -8
        call ErrorMessage(8,P,P,P,P)
        return

      ENDIF
      IF(YMIN.EQ.YMAX)THEN
      !error 9
!        WRITE(76,'(//,''***ERROR IN CONTRI***'',
!     +             /,''ALL POINTS HAVE SAME Y-COORD'',
!     +             /,''ALL POINTS ARE COLLINEAR'')')
!        STOP
        AutoGenFlag = -9
        call ErrorMessage(9,P,P,P,P)
        return

      ENDIF
      DMAX=MAX(XMAX-XMIN,YMAX-YMIN)

!---------------------------------------------------------------------
!     Normalise x-y coords of points

      FACT=C00001/DMAX
      DO 110 I=1,N
        P=LIST(I)
        X(P)=(X(P)-XMIN)*FACT
        Y(P)=(Y(P)-YMIN)*FACT
  110 CONTINUE



!---------------------------------------------------------------------
!     Sort points into bins using a linear bin sort
!     This call is optional

      CALL BSORT(N,NPTS,X,Y,XMIN,XMAX,YMIN,YMAX,DMAX,W,LIST,W(1,2))
!---------------------------------------------------------------------
!     Compute Delaunay triangulation

!      CALL DELAUN(NPTS,N,X,Y,LIST,W,V,T,NTRI) !,AutoGenFlag)
      CALL DELAUN(NPTS,N,X,Y,LIST,V,T,NTRI,AutoGenFlag)
      
      if(AutoGenFlag.lt.0) then
        go to 145
      endif
      
      DO 141 I=1,NCE
        VI=ELIST(1,I)
        VJ=ELIST(2,I)
  141 CONTINUE


! insert new front-related and/or cluster-based nodes if creating grid
      if(AutoGenFlag.gt.0)    then

      call GenPoints(NPTS,N,X,Y,ncode,LIST,V,T,NTRI,ELIST,NCB,NCE, &
                                   DMAX,XMIN,YMIN,AutoGenFlag)

      end if

    
!---------------------------------------------------------------------
!     For each node, store any triangle in which it is a vertex
!     Include supertriangle vertices

      DO 130 J=1,NTRI
        DO 120 I=1,3
          VI=V(I,J)
          W(VI,1)=J
  120   CONTINUE
  130 CONTINUE
!---------------------------------------------------------------------
!     Constrain triangulation by forcing edges to be present

      NEF=0
      JW=(NPTS+3)/2
      DO 140 I=1,NCE
        VI=ELIST(1,I)
        VJ=ELIST(2,I)
        CALL EDGE(VI,VJ,NPTS,NTRI,NEF,JW,X,Y,V,T,W,W(1,2),AutoGenFlag)
        if(AutoGenFlag.lt.0) go to 145
  140 CONTINUE



!---------------------------------------------------------------------
!     Clip triangulation and check it

      CALL BCLIP(NPTS,NCB,ELIST,W,V,T,NTRI,NB)
      if(nb.lt.0) then !error
        AutoGenFlag = nb
        go to 145
        endif

      CALL TCHECK(NPTS,N,X,Y,LIST,V,T,NTRI,NEF,NB,NCE,NCB,ELIST,W,AutoGenFlag)

!---------------------------------------------------------------------
!     Reset x-y coords to original values

145   continue
      DO 150 I=1,N
        P=LIST(I)
        X(P)=X(P)*DMAX+XMIN
        Y(P)=Y(P)*DMAX+YMIN
  150 CONTINUE
!---------------------------------------------------------------------
       ! Error 10
! 1000 FORMAT(//,'***ERROR IN CONTRI***',
!     +        /,'ILLEGAL VALUE IN LIST',
!     +        /,'LIST(',I5,' )=',I5)
!       ! Error 11
! 2000 FORMAT(//,'***ERROR IN CONTRI***',
!     +        /,'ILLEGAL VALUE IN ELIST',
!     +        /,'ELIST(',I5,',',I5,' )=',I5)
!       ! Error 12
! 3000 FORMAT(//,'***ERROR IN CONTRI***',
!     +        /,'ILLEGAL VALUE IN ELIST',
!     +        /,'ELIST(',I5,',',I5,' )=',I5,
!     +        /,'THIS POINT IS NOT IN LIST')
      END 
!X----------
!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: delaun.f
!X-Sun-Content-Lines: 363

      SUBROUTINE DELAUN(NPTS,N,X,Y,LIST,V,T,NTRI,AutoGenFlag)
!      SUBROUTINE DELAUN(NPTS,N,X,Y,LIST,STACK,V,T,NTRI) !,AutoGenFlag)
!***********************************************************************
!
!     PURPOSE:
!     --------
!
!     Assemble Delaunay triangulation
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set
!     N      - Total number of points to be triangulated (N le NPTS)
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - If point is in LIST, coordinate must be normalised
!              such that X=(X-XMIN)/DMAX
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - If point is in LIST, coordinate must be normalised
!              such that Y=(Y-YMIN)/DMAX
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     LIST   - List of points to be triangulated
!            - Coincident points are flagged by an error message
!            - Points are ordered such that consecutive points are 
!              close to one another in the x-y plane
!     STACK  - Not defined
!     V      - Not defined
!     T      - Not defined
!     NTRI   - Not defined
!
!     OUTPUT:
!     ------- 
!
!     NPTS   - Unchanged
!     N      - Unchanged
!     X      - Unchanged
!     Y      - Unchanged
!     LIST   - Unchanged
!     STACK  - Not defined
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Number of triangles in triangulation, including those
!              formed with the supertriangle vertices
!            - NTRI = 2*N+1
!
!     NOTES:
!     ------
!
!     - This is a faster version of the code appearing in AES 1987 vol 9
!       (small subroutines and functions have been coded in-line)
!     - Also some changes in code to improve efficiency
!     - Saving in cpu-time about 60 percent for larger problems
!     - A test has been implemented to detect coincident points
!     - Triangles formed with supertriangle vertices have not been 
!       deleted
!
!     PROGRAMMER:             Scott Sloan
!     -----------
!
!     LAST MODIFIED:          3 march 1991          Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:         Scott Sloan
!     ---------------         Department of Civil Engineering
!                             University of Newcastle
!                             NSW 2308
!
!***********************************************************************
      integer AutoGenFlag
!      INTEGER A,B,C,I,J,L,N,P,R
      INTEGER I,J,N,P
      INTEGER V1,V2,V3
      INTEGER NPTS,NTRI
      INTEGER TOPSTK
      INTEGER T(3,2*NPTS+1),V(4,2*NPTS+1)
      INTEGER LIST(NPTS)
!      INTEGER STACK(NPTS)

!      REAL D
      REAL XP,YP
      REAL TOL
!      REAL TOL,X13,X23,X1P,X2P,Y13,Y23,Y1P,Y2P
!      REAL COSA,COSB
      REAL C00000 !,C00100
      REAL X(NPTS+3),Y(NPTS+3)

!     TOL is the tolerance used to detect coincident points
!     The square of the distance between any two points must be
!     greater then TOL to avoid an error message
!     This value of TOL is suitable for single precision on most
!     32-bit machines (which typically have a precision of 0.000001)

      PARAMETER (TOL=1.E-10)
      PARAMETER (C00000=0.0, C00100=100.0)

!---------------------------------------------------------------------
!     Define vertices for supertriangle

      V1=NPTS+1
      V2=NPTS+2
      V3=NPTS+3
!---------------------------------------------------------------------
!     Set coords of supertriangle

      X(V1)=-C00100
      X(V2)= C00100
      X(V3)= C00000
      Y(V1)=-C00100
      Y(V2)=-C00100
      Y(V3)= C00100
!---------------------------------------------------------------------
!     Introduce first point
!     Define vertex and adjacency lists for first 3 triangles
!
      P=LIST(1)
      V(1,1)=P
      V(2,1)=V1
      V(3,1)=V2
      T(1,1)=3
      T(2,1)=0
      T(3,1)=2
      V(1,2)=P
      V(2,2)=V2
      V(3,2)=V3
      T(1,2)=1
      T(2,2)=0
      T(3,2)=3
      V(1,3)=P
      V(2,3)=V3
      V(3,3)=V1
      T(1,3)=2
      T(2,3)=0
      T(3,3)=1
!---------------------------------------------------------------------
!     Loop over each point (except first) and construct triangles
!   
      NTRI=3
      TOPSTK=0
      DO 140 I=2,N
        P=LIST(I)
        XP=X(P)
        YP=Y(P)
!
!       Locate triangle J in which point lies
!
        J=NTRI
   10   CONTINUE
        V1=V(1,J)
        V2=V(2,J)
        IF((Y(V1)-YP)*(X(V2)-XP).GT.(X(V1)-XP)*(Y(V2)-YP))THEN
          J=T(1,J)
          GOTO 10
        ENDIF
        V3=V(3,J)
        IF((Y(V2)-YP)*(X(V3)-XP).GT.(X(V2)-XP)*(Y(V3)-YP))THEN
          J=T(2,J)
          GOTO 10
        ENDIF
        IF((Y(V3)-YP)*(X(V1)-XP).GT.(X(V3)-XP)*(Y(V1)-YP))THEN
          J=T(3,J)
          GOTO 10
        ENDIF
!    Code replaced by AKS
!        CALL INSERTP(NPTS,N,X,Y,V,T,NTRI,V1,V2,V3,XP,YP,P,J)
        CALL INSERTP(NPTS,X,Y,V,T,NTRI,V1,V2,V3,XP,YP,P,J)

  140 CONTINUE

!---------------------------------------------------------------------
!     Check consistency of triangulation

      IF(NTRI.NE.2*N+1)THEN
       ! Error 13
!        WRITE(76,'(//,''***ERROR IN DELAUN***'',
!     +             /,''INCORRECT NUMBER OF TRIANGLES FORMED'')')
!        STOP
        AutoGenFlag = -13
        call ErrorMessage(13,P,P,P,P)
        return
      ENDIF
!---------------------------------------------------------------------

      END
!X----------
!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: edge.f
!X-Sun-Content-Lines: 569

      SUBROUTINE EDGE(VI,VJ,NPTS,NTRI,NEF,JW,X,Y,V,T,TN,W,AutoGenFlag)
!***********************************************************************
!
!     PURPOSE:
!     --------
!
!     Force edge VI-VJ to be present in Delaunay triangulation
!
!     INPUT:
!     ------
!
!     VI,VJ  - Vertices defining edge to be present in triangulation
!     NPTS   - Total number of points in data set
!     NTRI   - Number of triangles in triangulation
!     NEF    - Running total of edges that have been forced
!            - Set to zero before first call to EDGE
!     JW     - Number of cols in workspace vector W 
!            - JW must not be less than the number of edges in the
!              existing triangulation that intersect VI-VJ
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     TN     - List of triangle numbers such that vertex I can be 
!              found in triangle TN(I)
!     W      - Not defined, used as workspace
!
!     OUTPUT:
!     ------- 
!
!     VI,VJ  - Unchanged
!     NPTS   - Unchanged
!     NTRI   - Unchanged
!     NEF    - If VI-VJ needs to be forced, NEF is incremented by unity
!            - Else NEF is unchanged
!     JW     - Unchanged
!     X      - Unchanged
!     Y      - Unchanged
!     V      - Vertex array for triangulation updated so that edge 
!              V1-V2 is present
!     T      - Adjacency array for triangulation updated so that edge 
!              V1-V2 is present
!     TN     - List of triangle numbers updated so that edge 
!              V1-V2 is present
!     W      - List of new edges that replace old edges in
!              triangulation
!            - Vertices in W(1,I) and W(2,I) define each new edge I
!
!     NOTES:
!     ------
!
!     - This routine assumes that the edge defined by VI-VJ does not
!       lie on an outer boundary of the triangulation and, thus, the
!       triangulation must include the triangles that are formed with
!       the supertriangle vertices
!
!     PROGRAMMER:             Scott Sloan
!     -----------
!
!     LAST MODIFIED:          3 march 1991          Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:         Scott Sloan
!     ---------------         Department of Civil Engineering
!                             University of Newcastle
!                             NSW 2308
!
!***********************************************************************
      integer AutoGenFlag
      INTEGER A,C,E,I,L,R,S
      INTEGER JW,NC,V1,V2,V3,V4,VI,VJ
      INTEGER ELR,ERL,NEF
      INTEGER LAST,NTRI,NPTS
      INTEGER FIRST
      INTEGER T(3,NTRI),V(4,NTRI),W(2,JW)
      INTEGER TN(NPTS+3)

      REAL X1,X2,X3,X4,XI,XJ,Y1,Y2,Y3,Y4,YI,YJ
      REAL TOL,X13,X14,X23,X24,Y13,Y14,Y24,Y23
      REAL COSA,COSB
      REAL C00000,DETIJ3
      REAL X(NPTS+3),Y(NPTS+3)

      LOGICAL SWAP

      PARAMETER (TOL=1.E-6)
      PARAMETER (C00000=0.0)
!---------------------------------------------------------------------
!     Check data

      IF(VI.LE.0.OR.VI.GT.NPTS)THEN
       ! Error 16
!        WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +             /,''ILLEGAL VALUE FOR VI'',
!     +             /,''VI='',I5)')VI
!        STOP
        AutoGenFlag = -16
        call ErrorMessage(16,VI,VI,VI,VI)
        return
      ENDIF
      IF(VJ.LE.0.OR.VJ.GT.NPTS)THEN
       ! Error 17
!        WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +             /,''ILLEGAL VALUE FOR VJ'',
!     +             /,''VJ='',I5)')VJ
!        STOP
        AutoGenFlag = -17
        call ErrorMessage(17,VJ,VJ,VJ,VJ)
        return
      ENDIF
!---------------------------------------------------------------------- 
!     Find any triangle which has VI as a vertex

      S=TN(VI)
      IF(S.LE.0)THEN
!        WRITE(76,1000)VI
!        STOP
        AutoGenFlag = -23
        call ErrorMessage(23,VI,VI,VI,VI)
        return
      ENDIF           
      IF(TN(VJ).LE.0)THEN
!        WRITE(76,1000)VJ
!        STOP
        AutoGenFlag = -23
        call ErrorMessage(23,VJ,VJ,VJ,VJ)
        return
      ENDIF
      XI=X(VI)
      YI=Y(VI)
      XJ=X(VJ)
      YJ=Y(VJ)
!----------------------------------------------------------------------
!     Find an arc that crosses VI-VJ
!     C=current triangle
!     S=triangle in which search is started

      C=S

!     Vertices V1 and V2 are such that V1-V2 is opposite VI
!     Circle anticlockwise round VI until V1-V2 crosses VI-VJ

   10 CONTINUE
      IF(V(1,C).EQ.VI)THEN
        V2=V(3,C)
        E =2
      ELSEIF(V(2,C).EQ.VI)THEN
        V2=V(1,C)
        E =3
      ELSE   
        V2=V(2,C)
        E =1
      ENDIF

!     Test if arc VI-VJ already exists

      IF(V2.EQ.VJ)RETURN

!     Test if V1-V2 crosses VI-VJ

      X2=X(V2)
      Y2=Y(V2)
      IF((XI-X2)*(YJ-Y2).GT.(XJ-X2)*(YI-Y2))THEN

!       V2 is left of VI-VJ
!       Check if V1 is right of VI-VJ

        V1=V(E,C)
        X1=X(V1)
        Y1=Y(V1)
        IF((XI-X1)*(YJ-Y1).LT.(XJ-X1)*(YI-Y1))THEN

!         V1-V2 crosses VI-VJ , so edge needs to be forced

          NEF=NEF+1
          GOTO 15
        ENDIF
      ENDIF

!     No crossing, move anticlockwise around VI to the next triangle

      C=T(MOD(E,3)+1,C)
      IF(C.NE.S)GOTO 10
       ! Error 18
!      WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +           /,''VERTEX ADJACENT TO'',I5,'' IS ON ARC'',
!     +           /,''BETWEEN VERTICES'',I5,'' AND'',I5)')VI,VI,VJ
!      STOP
        AutoGenFlag = -18
        call ErrorMessage(18,VI,VI,VJ,VJ)
        return
   15 CONTINUE
!-------------------------------------------------------------------
!     Loop to store all arcs which cross VI-VJ
!     Vertices V1/V2 are right/left of   VI-VJ 

      NC=0
   20 CONTINUE
      NC=NC+1
      IF(NC.GT.JW)THEN
       ! Error 19
!        WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +             /,''NOT ENOUGH WORKSPACE'',
!     +             /,''INCREASE JW'')')
!        STOP
        AutoGenFlag = -19
        call ErrorMessage(19,VJ,VJ,VJ,VJ)
        return
      ENDIF
      W(1,NC)=V1
      W(2,NC)=V2
      C=T(E,C)
      IF(V(1,C).EQ.V2)THEN
        V3=V(3,C)
        E =2
      ELSEIF(V(2,C).EQ.V2)THEN
        V3=V(1,C)
        E =3
      ELSE
        V3=V(2,C)
        E =1
      ENDIF

!     Termination test, all arcs crossing VI-VJ have been stored

      IF(V3.EQ.VJ)GOTO 30
      X3=X(V3)
      Y3=Y(V3)
      DETIJ3=(XI-X3)*(YJ-Y3)-(XJ-X3)*(YI-Y3)
      IF(DETIJ3.LT.C00000)THEN
         E =MOD(E,3)+1
         V1=V3
      ELSEIF(DETIJ3.GT.C00000)THEN
         V2=V3
      ELSE 
       ! Error 20
!        WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +             /,''VERTEX'',I5,'' IS ON ARC'',
!     +             /,''BETWEEN VERTICES'',I5,'' AND'',I5)')V3,VI,VJ
!        STOP
        AutoGenFlag = -20
        call ErrorMessage(20,V3,VI,VJ,VJ)
        return
      ENDIF
      GOTO 20
   30 CONTINUE
!-------------------------------------------------------------------
!     Swap each arc that crosses VI-VJ if it is a diagonal of a 
!     convex quadrilateral
!     Execute all possible swaps, even if newly formed arc also 
!     crosses VI-VJ, and iterate until no arcs cross

      LAST=NC
   35 IF(LAST.GT.0)THEN
        FIRST=1
   40   IF(FIRST.LE.LAST)THEN

!         Find triangle L which is left of V1-V2 
!         Find triangle R which is right of V1-V2

          V1=W(1,FIRST)
          V2=W(2,FIRST)

!         Exchange V1 and V2 if V1 is a supertriangle vertex

          IF(V1.GT.NPTS)THEN
            IF(V2.GT.NPTS)THEN
       ! Error 21
!              WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +                   /,''ARC BETWEEN VERTICES'',I5,'' AND'',I5,
!     +                   /,''CROSSES SUPERTRIANGLE BOUNDARY DEFINED '',
!     +                     ''BY VERTICES'',I5,''AND'',I5)')VI,VJ,V1,V2
!               STOP
               AutoGenFlag = -21
               call ErrorMessage(21,VI,VJ,V1,V2)
               return
            ENDIF
            W(1,FIRST)=V2
            W(2,FIRST)=V1
            V2=V1
            V1=W(1,FIRST)
          ENDIF
          L=TN(V1)
   45     IF(V(1,L).EQ.V1)THEN
            IF(V(2,L).EQ.V2)THEN
              V3 =V(3,L)
              ELR=1
              R  =T(1,L)
            ELSE
              L=T(3,L)
              GOTO 45
            ENDIF
          ELSEIF(V(2,L).EQ.V1)THEN
            IF(V(3,L).EQ.V2)THEN
              V3 =V(1,L)
              ELR=2
              R  =T(2,L)
            ELSE
              L=T(1,L)
              GOTO 45
            ENDIF
          ELSEIF(V(1,L).EQ.V2)THEN
              V3 =V(2,L)
              ELR=3
              R  =T(3,L)
          ELSE
              L=T(2,L)
              GOTO 45
          ENDIF

!         Find vertices V3 and V4 where:
!         triangle L is defined by V3-V1-V2 
!         triangle R is defined by V4-V2-V1

          IF(T(1,R).EQ.L)THEN
            V4=V(3,R)
            A =T(2,R)
            ERL=1
          ELSEIF(T(2,R).EQ.L)THEN
            V4=V(1,R)
            A =T(3,R)
            ERL=2
          ELSE
            V4=V(2,R)
            A =T(1,R)
            ERL=3
          ENDIF

!         Test if quad formed by V3-V1-V4-V2 is convex

          X3=X(V3)
          Y3=Y(V3)
          X4=X(V4)
          Y4=Y(V4)
          X1=X(V1)
          Y1=Y(V1)
          X2=X(V2)
          Y2=Y(V2)
          IF((X3-X1)*(Y4-Y1).LT.(X4-X1)*(Y3-Y1))THEN
            IF((X3-X2)*(Y4-Y2).GT.(X4-X2)*(Y3-Y2))THEN

!             Quad is convex so swap diagonal arcs
!             Update vertex and adjacency lists for triangle L

              IF(ELR.EQ.1)THEN
                V(2,L)=V4
                C     =T(2,L)
                T(1,L)=A
                T(2,L)=R
              ELSEIF(ELR.EQ.2)THEN
                V(3,L)=V4
                C     =T(3,L)
                T(2,L)=A
                T(3,L)=R             
              ELSE
                V(1,L)=V4
                C     =T(1,L)
                T(3,L)=A
                T(1,L)=R             
              ENDIF                 

!             Update vertex and adjacency lists for triangle R

              IF(ERL.EQ.1)THEN
                V(2,R)=V3
                T(1,R)=C
                T(2,R)=L
              ELSEIF(ERL.EQ.2)THEN
                V(3,R)=V3
                T(2,R)=C
                T(3,R)=L             
              ELSE
                V(1,R)=V3
                T(3,R)=C
                T(1,R)=L             
              ENDIF

!             Update adjacency lists for triangles A and C

              IF(T(1,C).EQ.L)THEN
                T(1,C)=R
              ELSEIF(T(2,C).EQ.L)THEN
                T(2,C)=R
              ELSE
                T(3,C)=R
              ENDIF
              IF(T(1,A).EQ.R)THEN
                T(1,A)=L
              ELSEIF(T(2,A).EQ.R)THEN
                T(2,A)=L
              ELSE
                T(3,A)=L
              ENDIF

!             Update vertex-triangle list

              TN(V1)=L
              TN(V2)=R

!             Test if new diagonal arc crosses VI-VJ and store it if it
!             does

              IF(((XI-X3)*(YJ-Y3)-(XJ-X3)*(YI-Y3))*&
                  ((XI-X4)*(YJ-Y4)-(XJ-X4)*(YI-Y4)).LT.C00000)THEN
                W(1,FIRST)=V4
                W(2,FIRST)=V3
                FIRST=FIRST+1
              ELSE
                W(1,FIRST)=W(1,LAST)
                W(1,LAST) =V3
                W(2,FIRST)=W(2,LAST)
                W(2,LAST) =V4        
                LAST=LAST-1                    
              ENDIF
              GOTO 40
            ENDIF
          ENDIF

!         Arc cannot be swapped, so move to next intersecting arc

          FIRST=FIRST+1
          GOTO 40
        ENDIF
        GOTO 35
      ENDIF
!----------------------------------------------------------------------
!     Optimise all new arcs (except VI-VJ)
!        
      SWAP=.TRUE.
   50 IF(SWAP)THEN
        SWAP=.FALSE.
        DO 70 I=2,NC

!         Find triangle L which is left of V1-V2 
!         Find triangle R which is right of V1-V2

          V1=W(1,I)
          V2=W(2,I)

!         Exchange V1 and V2 if V1 is a supertriangle vertex

          IF(V1.GT.NPTS)THEN
            IF(V2.GT.NPTS)THEN
       ! Error 22
!              WRITE(76,'(//,''***ERROR IN EDGE***'',
!     +                   /,''ARC BETWEEN VERTICES'',I5,'' AND'',I5,
!     +                   /,''CANNOT BE OPTIMISED SINCE IT IS A '',
!     +                     ''SUPERTRIANGLE BOUNDARY'')')V1,V2
!               STOP
               AutoGenFlag = -22
               call ErrorMessage(22,V1,V2,V2,V2)
               return
            ENDIF
            W(1,I)=V2
            W(2,I)=V1
            V2=V1
            V1=W(1,I)
          ENDIF
          L=TN(V1)
   60     IF(V(1,L).EQ.V1)THEN
            IF(V(2,L).EQ.V2)THEN
              V3 =V(3,L)
              ELR=1
              R  =T(1,L)
            ELSE
              L=T(3,L)
              GOTO 60
            ENDIF
          ELSEIF(V(2,L).EQ.V1)THEN
            IF(V(3,L).EQ.V2)THEN
              V3 =V(1,L)
              ELR=2
              R  =T(2,L)
            ELSE
              L=T(1,L)
              GOTO 60
            ENDIF
          ELSEIF(V(1,L).EQ.V2)THEN
              V3 =V(2,L)
              ELR=3
              R  =T(3,L)
          ELSE
              L=T(2,L)
              GOTO 60
          ENDIF

!         Find vertices V3 and V4 where:
!         triangle L is defined by V3-V1-V2 
!         triangle R is defined by V4-V2-V1

          IF(T(1,R).EQ.L)THEN
            V4=V(3,R)
            A =T(2,R)
            ERL=1
          ELSEIF(T(2,R).EQ.L)THEN
            V4=V(1,R)
            A =T(3,R)
            ERL=2
          ELSE
            V4=V(2,R)
            A =T(1,R)
            ERL=3
          ENDIF
          X13=X(V1)-X(V3)
          Y13=Y(V1)-Y(V3)
          X14=X(V1)-X(V4)
          Y14=Y(V1)-Y(V4)
          X23=X(V2)-X(V3)
          Y23=Y(V2)-Y(V3)
          X24=X(V2)-X(V4)
          Y24=Y(V2)-Y(V4)
          COSA=X13*X23+Y23*Y13
          COSB=X24*X14+Y24*Y14
          IF(COSA.LT.C00000.OR.COSB.LT.C00000)THEN
            IF((COSA.LT.C00000.AND.COSB.LT.C00000).OR. &
               ((X13*Y23-X23*Y13)*COSB-(X14*Y24-X24*Y14)*COSA.LT. &
               -TOL*SQRT((X13*X13+Y13*Y13)*(X23*X23+Y23*Y23)* &
                         (X24*X24+Y24*Y24)*(X14*X14+Y14*Y14))))THEN

!             V4 is inside circumcircle for triangle L
!             Swap diagonal for convex quad formed by V3-V1-V4-V2
!             Update vertex and adjacency lists for triangle L

              SWAP=.TRUE.
              IF(ELR.EQ.1)THEN
                V(2,L)=V4
                C     =T(2,L)
                T(1,L)=A
                T(2,L)=R
              ELSEIF(ELR.EQ.2)THEN
                V(3,L)=V4
                C     =T(3,L)
                T(2,L)=A
                T(3,L)=R             
              ELSE
                V(1,L)=V4
                C     =T(1,L)
                T(3,L)=A
                T(1,L)=R             
              ENDIF                 

!             Update vertex and adjacency lists for triangle R

              IF(ERL.EQ.1)THEN
                V(2,R)=V3
                T(1,R)=C
                T(2,R)=L
              ELSEIF(ERL.EQ.2)THEN
                V(3,R)=V3
                T(2,R)=C
                T(3,R)=L             
              ELSE
                V(1,R)=V3
                T(3,R)=C
                T(1,R)=L             
              ENDIF

!             Update adjacency lists for triangles A and C

              IF(T(1,C).EQ.L)THEN
                T(1,C)=R
              ELSEIF(T(2,C).EQ.L)THEN
                T(2,C)=R
              ELSE
                T(3,C)=R
              ENDIF
              IF(T(1,A).EQ.R)THEN
                T(1,A)=L
              ELSEIF(T(2,A).EQ.R)THEN
                T(2,A)=L
              ELSE
                T(3,A)=L
              ENDIF

!             Update vertex-triangle list and arc list

              TN(V1)=L
              TN(V2)=R
              W(1,I)=V3
              W(2,I)=V4
            ENDIF
          ENDIF
   70   CONTINUE
        GOTO 50
      ENDIF
!---------------------------------------------------------------------
        ! Error 23
! 1000 FORMAT(//,'***ERROR IN EDGE***',
!     +        /,'   VERTEX',I5,' NOT IN ANY TRIANGLE')
      END
!X----------
!X-Sun-Data-Type: fortran-file
!X-Sun-Data-Name: tcheck.f
!X-Sun-Content-Lines: 404

      SUBROUTINE TCHECK(NPTS,N,X,Y,LIST,V,T,NTRI,NEF,NB,NCE,NCB,ELIST,W,ierror)
!***********************************************************************
!
!     PURPOSE:
!     --------
!
!     Check Delaunay triangulation which may be constrained
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set
!     N      - Total number of points to be triangulated (N le NPTS)
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun          
!     LIST   - List of points to be triangulated
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Number of triangles in triangulation
!     NEF    - Number of forced edges in triangulation
!     NB     - Number of boundaries defining the triangulation
!            - NB=1 for a simply connected domain with no holes 
!            - NB=H+1 for a domain with H holes
!     NCE    - Total number of constrained edges which must be 
!              present, including those which define boundaries
!            - NCE=0 indicates triangulation is unconstrained so that
!              ELIST is empty and the code will produce a 
!              triangulation of the convex hull
!     NCB    - Total number of constrained edges which define one
!              external boundary and any internal boundaries (holes)
!            - NCB=0 indicates there are no boundary edge constraints
!              and the code will produce a triangulation of the convex
!              hull
!     ELIST  - List of edges which must be present in triangulation
!            - These may be internal edges or edges which define 
!              boundaries
!     W      - Undefined, vector used as workspace
!
!     OUTPUT:
!     ------- 
!
!     NPTS   - Unchanged
!     N      - Unchanged
!     X      - Unchanged
!     Y      - Unchanged
!     LIST   - Unchanged
!     V      - Unchanged
!     T      - Unchanged
!     NTRI   - Unchanged
!     NEF    - Unchanged
!     NB     - Unchanged
!     NCE    - Unchanged
!     NCB    - Unchanged
!     ELIST  - Unchanged
!     W      - Not used
!
!     NOTES:
!     ------
!     
!     - This routine performs a number of elementary checks to test
!       the integrity of the triangulation
!     - NTRI=2*(N+NB)-NBOV-4 for a valid triangulation, where NBOV is
!       the number of boundary vertices
!     - NEDG=N+NTRI+NB-2 for a valid triangulation, where NEDG is the
!       number of edges in the triangulation
!     - NOPT le NEF for a valid triangulation, where NOPT is the number
!       of non-optimal edges in the triangulation
!     - The triangulation is tested to ensure that each non-boundary
!       constrained edge (if there are any) is present
!
!     PROGRAMMER:             Scott Sloan
!     -----------
!
!     LAST MODIFIED:          3 march 1991          Scott Sloan
!     --------------
!
!     COPYRIGHT 1990:         Scott Sloan
!     ---------------         Department of Civil Engineering
!                             University of Newcastle
!                             NSW 2308
!
!***********************************************************************
      INTEGER I,J,L,N,R,S
      INTEGER NB,V1,V2,V3,V4
      INTEGER NBOV,NEDG,NOPT,NPTS,NTRI
      INTEGER NCB,NCE,NEF
      INTEGER T(3,NTRI),V(4,NTRI),W(NPTS)
      INTEGER LIST(N)
      INTEGER ELIST(2,*)

      REAL X1,X2,X3,X4,Y1,Y2,Y3,Y4
      REAL DET,DIS,R21,R31,RAD,TOL,X21,X31,XCC,Y21,Y31,YCC
      REAL C00000,CP5000
      REAL X(NPTS+3),Y(NPTS+3)

      integer ierror

      character(80) cstr



      PARAMETER (TOL=1.E-5)
      PARAMETER (C00000=0.0, CP5000=0.5)
!-----------------------------------------------------------------------
!     Loop over each triangle and count the following
!     NOPT=number of edges which are not optimal
!     NEDG=number of edges in triangulation
!     NBOV=number of boundary vertices

      NOPT=0
      NEDG=0
      NBOV=0
      DO 20 L=1,NTRI

!       Loop over each side of triangle

        DO 10 I=1,3
          R=T(I,L)
          IF(R.EQ.0)THEN
            NEDG=NEDG+1
            NBOV=NBOV+1
          ELSEIF(R.LT.L)THEN
            NEDG=NEDG+1                                   
            IF(I.EQ.1)THEN
              V1=V(1,L)
              V2=V(2,L)
              V3=V(3,L)
            ELSEIF(I.EQ.2)THEN
              V1=V(2,L)
              V2=V(3,L)
              V3=V(1,L)
            ELSE
              V1=V(3,L)
              V2=V(1,L)
              V3=V(2,L)
            ENDIF

!           Triangle L is left of V1-V2 and is defined by V3-V1-V2 
!           Triangle R is right of V1-V2 and is defined by V4-V2-V1

            IF(T(1,R).EQ.L)THEN
              V4=V(3,R)
            ELSEIF(T(2,R).EQ.L)THEN
              V4=V(1,R)
            ELSE
              V4=V(2,R)
            ENDIF

!           Find circumcentre of triangle L and its circumcircle radius

            X1 =X(V1)
            Y1 =Y(V1)
            X21=X(V2)-X1
            Y21=Y(V2)-Y1
            X31=X(V3)-X1
            Y31=Y(V3)-Y1
            DET=X21*Y31-X31*Y21
            IF(DET.LE.C00000)THEN
            ! error 30
!              WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                        /,''ZERO OR -VE AREA FOR TRIANGLE'',I5)')L
              ierror = -30
              call ErrorMessage(30,L,L,L,L)
              return
            ELSE
              DET=CP5000/DET
              R21=X21*X21+Y21*Y21
              R31=X31*X31+Y31*Y31
              XCC=DET*(R21*Y31-R31*Y21)
              YCC=DET*(X21*R31-X31*R21)
              RAD=SQRT(XCC*XCC+YCC*YCC)
              XCC=XCC+X1
              YCC=YCC+Y1

!             Check if V4 is inside circumcircle for triangle L

              DIS=SQRT((XCC-X(V4))**2+(YCC-Y(V4))**2)
              IF(RAD-DIS.GT.TOL*RAD)THEN
                NOPT=NOPT+1
              ENDIF
            ENDIF
          ENDIF
   10   CONTINUE
   20 CONTINUE
!-----------------------------------------------------------------------
!     Check triangulation is valid


      ierror = 0


      IF(NBOV.LT.3)THEN
        ! Error 24
!        ierror = 24

!        WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +             /,''NUMBER BOUNDARY VERTICES LT 3'',
!     +             /,''NBOV='',I5)')NBOV
!        STOP
        ierror = -24
        call ErrorMessage(24,NBOV,NBOV,NBOV,NBOV)
        return
      ENDIF
      IF(NTRI.NE.2*(N+NB)-NBOV-4)THEN
        ! Error 25
!        ierror = 25

!        WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +             /,''INVALID TRIANGULATION'',
!     +             /,''NTRI IS NOT EQUAL TO 2*(N+NB)-NBOV-4'')')
!        write(76,*) ' ntri,n,nb,nbov=',ntri,n,nb,nbov

        WRITE(76,1000)
        WRITE(76,2000)
!        STOP
        ierror = -25
        call ErrorMessage(25,ntri,n,nb,nbov)
        return
      ENDIF
      IF(NEDG.NE.N+NTRI+NB-2)THEN
        ! Error 26
!        ierror = 26
!        WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +             /,''INVALID TRIANGULATION'',
!     +             /,''NEDG IS NOT EQUAL TO N+NTRI+NB-2'')')
        WRITE(76,1000)
        WRITE(76,2000)
!        STOP
        ierror = -26
        call ErrorMessage(26,nedg,n,ntri,nb)
        return
      ENDIF
      IF(NOPT.GT.NEF)THEN
        ! Error 27
!        ierror = 27
!        WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +             /,''INVALID TRIANGULATION'',
!     +             /,''TOO MANY NON-OPTIMAL EDGES'')')
        WRITE(76,1000)
        WRITE(76,2000)
!        STOP    
        ierror = -27
        call ErrorMessage(27,nedg,n,ntri,nb)
        return
      ENDIF
      IF(NCB.GT.0)THEN
        IF(NCB.NE.NBOV)THEN
        ! Error 28
!        ierror = 28
!          WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +               /,''INVALID TRIANGULATION'',
!     +               /,''NO. BOUNDARY VERTICES NOT EQUAL TO NBC'')')
          WRITE(76,1000)
          WRITE(76,2000)
!          STOP    
        ierror = -28
        call ErrorMessage(28,ncb,nbov,ntri,nb)
        return
        ENDIF
      ENDIF

      if(ierror.ne.0) then

        cstr =' WARNING: grid errors- see file TriErrors.txt'

          call PigMessageOK(cstr, 'Error')

      ENDIF
!----------------------------------------------------------------------
!     Check that each node appears in at least one triangle

      DO 25 I=1,NPTS
        W(I)=0
   25 CONTINUE
      DO 35 J=1,NTRI
        DO 30 I=1,3
          W(V(I,J))=J
   30   CONTINUE
   35 CONTINUE
      DO 40 I=1,N
        V1=LIST(I)
        IF(W(V1).EQ.0)THEN
        ! Error 29
!          WRITE(76,'(//,''***ERROR IN TCHECK***'',
!     +               /,''INVALID TRIANGULATION'',
!     +               /,''VERTEX'',I5,'' IS NOT IN ANY TRIANGLE'')')V1
!          STOP
          ierror = -29
          call ErrorMessage(29,ncb,nbov,ntri,nb)
        ENDIF
   40 CONTINUE   
      IF(NCE.EQ.NCB)RETURN
!----------------------------------------------------------------------
!     Check that all non-boundary constrained edges are present

      DO 70 I=NCB+1,NCE
        V1=ELIST(1,I)
        V2=ELIST(2,I)
        S=W(V1)
        R =S

!       Circle anticlockwise round V1 until
!       - an edge V1-V2 is found, or,
!       - we are back at the starting triangle, this indicates an error
!         since V1 is interior and all neighbours have been checked, or,
!       - a boundary edge is found, this indicates that V1 is a boundary
!         vertex and the search must be repeated by circling clockwise 
!           round V1

   50   IF(V(1,R).EQ.V1)THEN
          IF(V(3,R).EQ.V2)THEN
            GOTO 70
          ELSE
            R=T(3,R)
          ENDIF
        ELSEIF(V(2,R).EQ.V1)THEN
          IF(V(1,R).EQ.V2)THEN
            GOTO 70
          ELSE
            R=T(1,R)
          ENDIF
        ELSEIF(V(2,R).EQ.V2)THEN
            GOTO 70
        ELSE
            R=T(2,R)
        ENDIF
        IF(R.EQ.S)THEN
        ! error 31
!          WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                    /,''CONSTRAINED EDGE WITH VERTICES'',I5,&
!                      '' AND'',I5,'' IS NOT IN TRIANGULATION'')')V1,V2
          ierror = -31
!          call ErrorMessage(31,V1,V2,V2,V2)

!         Edge V1-V2 is not in triangulation
!         Check if it crosses any other constrained edges

          X1=X(V1)
          Y1=Y(V1)
          X2=X(V2)
          Y2=Y(V2)
          DO 55 J=NCB+1,NCE
            IF(J.EQ.I)GOTO 55
            V3=ELIST(1,J)
            V4=ELIST(2,J)
            X3=X(V3)
            Y3=Y(V3)
            X4=X(V4)
            Y4=Y(V4)
            IF(((X1-X3)*(Y2-Y3)-(X2-X3)*(Y1-Y3))*&
                ((X1-X4)*(Y2-Y4)-(X2-X4)*(Y1-Y4)).LT.C00000)THEN
              IF(((X3-X1)*(Y4-Y1)-(X4-X1)*(Y3-Y1))*&
                ((X3-X2)*(Y4-Y2)-(X4-X2)*(Y3-Y2)).LT.C00000)THEN
               
!               Edge V1-V2 crosses edge V3-V4  

        ! error 32
!                WRITE(76,'(''IT INTERSECTS ANOTHER CONSTRAINED EDGE '',&
!                         ''WITH VERTICES'',I5,'' AND'',I5)')V3,V4
                ierror = -32
                call ErrorMessage(32,V1,V2,V3,V4)
                GOTO 70
              ENDIF
            ENDIF
   55     CONTINUE
          if(ierror.eq.-31) then
            call ErrorMessage(31,V1,V2,V2,V2)
          endif
          GOTO 70
        ENDIF
        IF(R.GT.0)GOTO 50

!       V1 must be a boundary vertex
!       Circle clockwise round V1 until 
!       - an edge V1-V2 is found, or,
!       - a boundary edge is found which indicates an error

        L=S
   60   IF(V(1,L).EQ.V1)THEN
          IF(V(2,L).EQ.V2)THEN
            GOTO 70
          ELSE
            L=T(1,L)
          ENDIF
        ELSEIF(V(2,L).EQ.V1)THEN
          IF(V(3,L).EQ.V2)THEN
            GOTO 70
          ELSE
            L=T(2,L)
          ENDIF
        ELSEIF(V(1,L).EQ.V2)THEN
            GOTO 70
        ELSE
            L=T(3,L)
        ENDIF
        IF(L.GT.0)GOTO 60
        ! error 31
!        WRITE(76,'(//,''***WARNING IN TCHECK***'',&
!                  /,''CONSTRAINED EDGE WITH VERTICES'',I5,&
!                    '' AND'',I5,'' IS NOT IN TRIANGULATION'')')V1,V2
          ierror = -31
!          call ErrorMessage(31,V1,V2,V2,V2)

!       Edge V1-V2 is not in triangulation
!       Check if it crosses any other constrained edges

        X1=X(V1)
        Y1=Y(V1)
        X2=X(V2)
        Y2=Y(V2)
        DO 65 J=NCB+1,NCE
          IF(J.EQ.I)GOTO 65
          V3=ELIST(1,J)
          V4=ELIST(2,J)
          X3=X(V3)
          Y3=Y(V3)
          X4=X(V4)
          Y4=Y(V4)
          IF(((X1-X3)*(Y2-Y3)-(X2-X3)*(Y1-Y3))*&
              ((X1-X4)*(Y2-Y4)-(X2-X4)*(Y1-Y4)).LT.C00000)THEN
            IF(((X3-X1)*(Y4-Y1)-(X4-X1)*(Y3-Y1))*&
              ((X3-X2)*(Y4-Y2)-(X4-X2)*(Y3-Y2)).LT.C00000)THEN
              
!             Edge V1-V2 crosses edge V3-V4  

        ! error 32
!              WRITE(76,'(''IT INTERSECTS ANOTHER CONSTRAINED EDGE '',&
!                       ''WITH VERTICES'',I5,'' AND'',I5)')V3,V4
                ierror = -32
                call ErrorMessage(32,V1,V2,V3,V4)
              GOTO 70
            ENDIF
          ENDIF
   65   CONTINUE
        if(ierror.eq.-31) then
          call ErrorMessage(31,V1,V2,V2,V2)
        endif
   70 CONTINUE
 1000 FORMAT(//,'CHECK THE FOLLOWING CONDITIONS:',&
             /,'-EDGES WHICH DEFINE BOUNDARIES MUST COME FIRST IN',&
             /,' ELIST AND THUS OCCUPY THE FIRST NCB COLUMNS',&
             /,'-EDGES WHICH DEFINE AN EXTERNAL BOUNDARY MUST BE',&
             /,' LISTED ANTICLOCKWISE BUT MAY BE PRESENTED IN ANY', &
             /,' ORDER',&
             /,'-EDGES WHICH DEFINE AN INTERNAL BOUNDARY (HOLE) MUST',&
             /,' BE LISTED CLOCKWISE BUT MAY BE PRESENTED IN ANY',& 
             /,' ORDER',&
             /,'-AN INTERNAL BOUNDARY (HOLE) CANNOT BE SPECIFIED',&
             /,' UNLESS AN EXTERNAL BOUNDARY IS ALSO SPECIFIED')
 2000 FORMAT(   '-ALL BOUNDARIES MUST FORM CLOSED LOOPS',&
             /,'-AN EDGE MAY NOT APPEAR MORE THAN ONCE IN ELIST',&
             /,'-AN EXTERNAL OR INTERNAL BOUNDARY MAY NOT CROSS',&
             /,' ITSELF AND MAY NOT SHARE A COMMON EDGE WITH ANY', &
             /,' OTHER BOUNDARY',&
             /,'-INTERNAL EDGES, WHICH ARE NOT MEANT TO DEFINE',&
             /,' BOUNDARIES BUT MUST BE PRESENT IN THE FINAL',& 
             /,' TRIANGULATION, OCCUPY COLUMNS NCB+1,... ,NCE OF',& 
             /,' ELIST',&
             /,'-NO POINT IN THE LIST VECTOR MAY LIE OUTSIDE ANY',&
             /,' EXTERNAL BOUNDARY OR INSIDE ANY INTERNAL BOUNDARY')
      END

! **********************************************************

      SUBROUTINE INSERTP(NPTS,X,Y,V,T,NTRI,V1,V2,V3,XP,YP,P,J)

!
!     PURPOSE:
!     --------
!
!     Insert a new point P and generate the new Delaunay triangulation
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set
!     P      - point to be inserted
!     XP, YP - normalised coords of P
!     J      - triangle encompassing P
!     V1,V2,V3 - vertices of J
!     N      - Total number of points to be triangulated (N le NPTS)
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - If point is in LIST, coordinate must be normalised
!              such that X=(X-XMIN)/DMAX
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - If point is in LIST, coordinate must be normalised
!              such that Y=(Y-YMIN)/DMAX
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Number of triangles in triangulation, including those
!              formed with the supertriangle vertices
!            - NTRI = 2*N+1
!
!
!     NOTES:
!     ------

!     - code originally from 140 loop in Sub Delaun (we can now call this sub 
!       from Delaun to clean up)
!     Notes from orginal sub Delaun:
!     - This is a faster version of the code appearing in AES 1987 vol 9
!       (small subroutines and functions have been coded in-line)
!     - Also some changes in code to improve efficiency
!     - Saving in cpu-time about 60 percent for larger problems
!     - A test has been implemented to detect coincident points
!     - Triangles formed with supertriangle vertices have not been 
!       deleted

!     PROGRAMMER:             Scott Sloan
!     -----------

!     LAST MODIFIED:          11 February 2002          Alastair Senior
!     ---------------   



!     COPYRIGHT 1990:         Scott Sloan

!     ---------------         Department of Civil Engineering

!                             University of Newcastle

!                             NSW 2308



!***********************************************************************

!      INTEGER A,B,C,I,J,L,N,P,R
      INTEGER A,B,C,J,L,P,R
      INTEGER V1,V2,V3
      INTEGER NPTS,NTRI
      INTEGER,save :: TOPSTK=0
      INTEGER T(3,2*NPTS+1),V(4,2*NPTS+1)
      INTEGER STACK(NPTS)

      REAL D
      REAL XP,YP
      REAL TOL,X13,X23,X1P,X2P,Y13,Y23,Y1P,Y2P
      REAL COSA,COSB
      REAL C00000  !,C00100
      REAL X(NPTS+3),Y(NPTS+3)
    
      PARAMETER (TOL=1.E-10)
      PARAMETER (C00000=0.0)

!       Check that new point is not coincident with any previous point
        D=(X(V1)-XP)**2
        IF(D.LT.TOL)THEN
           D=D+(Y(V1)-YP)**2
           IF(D.LT.TOL)THEN
       ! Error 15
!             WRITE(76,2000)V1,P
             AutoGenFlag = -15
             call ErrorMessage(15,V1,P,P,P)
             return
           ENDIF
        ENDIF
        D=(X(V2)-XP)**2
        IF(D.LT.TOL)THEN
           D=D+(Y(V2)-YP)**2
           IF(D.LT.TOL)THEN
       ! Error 15
!             WRITE(76,2000)V2,P
             AutoGenFlag = -15
             call ErrorMessage(15,V2,P,P,P)
             return
           ENDIF
        ENDIF
        D=(X(V3)-XP)**2
        IF(D.LT.TOL)THEN
           D=D+(Y(V3)-YP)**2
           IF(D.LT.TOL)THEN
       ! Error 15
!             WRITE(76,2000)V3,P
             AutoGenFlag = -15
             call ErrorMessage(15,V3,P,P,P)
             return
           ENDIF
        ENDIF


!       Create new vertex and adjacency lists for triangle J

        A=T(1,J)
        B=T(2,J)
        C=T(3,J)
        V(1,J)=P
        V(2,J)=V1
        V(3,J)=V2
        T(1,J)=NTRI+2    
        T(2,J)=A
        T(3,J)=NTRI+1

!       Create new triangles

        NTRI=NTRI+1
        V(1,NTRI)=P
        V(2,NTRI)=V2
        V(3,NTRI)=V3
        T(1,NTRI)=J
        T(2,NTRI)=B
        T(3,NTRI)=NTRI+1
        NTRI=NTRI+1
        V(1,NTRI)=P
        V(2,NTRI)=V3
        V(3,NTRI)=V1
        T(1,NTRI)=NTRI-1
        T(2,NTRI)=C
        T(3,NTRI)=J
        
!       Put each edge of triangle J on STACK
!       Store triangles on left side of each edge
!       Update adjacency lists for triangles B and C

        TOPSTK=TOPSTK+2
        STACK(TOPSTK-1)=J
        IF(T(1,C).EQ.J)THEN
          T(1,C)=NTRI
        ELSE
          T(2,C)=NTRI
        ENDIF
        STACK(TOPSTK)=NTRI
        IF(B.NE.0)THEN
          IF(T(1,B).EQ.J)THEN
            T(1,B)=NTRI-1
          ELSEIF(T(2,B).EQ.J)THEN
            T(2,B)=NTRI-1
          ELSE
            T(3,B)=NTRI-1
          ENDIF
          TOPSTK=TOPSTK+1
          STACK(TOPSTK)=NTRI-1
        ENDIF

!       Loop while STACK is not empty

  609   IF(TOPSTK.GT.0)THEN

!         Find triangles L and R which are either side of stacked edge
!         triangle L is defined by V3-V1-V2 and is left of V1-V2
!         triangle R is defined by V4-V2-V1 and is right of V1-V2

          R=STACK(TOPSTK)
          TOPSTK=TOPSTK-1
          L=T(2,R)

!         Check if new point P is in circumcircle for triangle L

          IF(T(1,L).EQ.R)THEN
            V1=V(1,L)
            V2=V(2,L)
            V3=V(3,L)
            A=T(2,L)
            B=T(3,L)
          ELSEIF(T(2,L).EQ.R)THEN
            V1=V(2,L)
            V2=V(3,L)
            V3=V(1,L)
            A=T(3,L)
            B=T(1,L)
          ELSE
            V1=V(3,L)
            V2=V(1,L)
            V3=V(2,L)
            A=T(1,L)
            B=T(2,L)
          ENDIF
          X13=X(V1)-X(V3)
          Y13=Y(V1)-Y(V3)
          X23=X(V2)-X(V3)
          Y23=Y(V2)-Y(V3)
          X1P=X(V1)-XP
          Y1P=Y(V1)-YP
          X2P=X(V2)-XP
          Y2P=Y(V2)-YP
          COSA=X13*X23+Y13*Y23
          COSB=X2P*X1P+Y1P*Y2P
          IF(COSA.LT.C00000.OR.COSB.LT.C00000)THEN
            IF((COSA.LT.C00000.AND.COSB.LT.C00000).OR. &
             ((X13*Y23-X23*Y13)*COSB.LT.(X1P*Y2P-X2P*Y1P)*COSA))THEN

!             New point is inside circumcircle for triangle L
!             Swap diagonal for convex quad formed by P-V2-V3-V1

              C=T(3,R)

!             Update vertex and adjacency list for triangle R

              V(3,R)=V3
              T(2,R)=A
              T(3,R)=L
! 
!             Update vertex and adjacency list for triangle L

              V(1,L)=P
              V(2,L)=V3
              V(3,L)=V1
              T(1,L)=R
              T(2,L)=B
              T(3,L)=C

!             Put edges R-A and L-B on STACK
!             Update adjacency lists for triangles A and C

              IF(A.NE.0)THEN
                IF(T(1,A).EQ.L)THEN
                  T(1,A)=R
                ELSEIF(T(2,A).EQ.L)THEN
                  T(2,A)=R
                ELSE
                  T(3,A)=R
                ENDIF
                TOPSTK=TOPSTK+1
                IF(TOPSTK.GT.NPTS)THEN
       ! Error 14
!                  WRITE(76,1000)
!                  STOP
                  AutoGenFlag = -14
                  call ErrorMessage(14,V1,V1,V1,V1)
                  return
                ENDIF
                STACK(TOPSTK)=R
              ENDIF
              IF(B.NE.0)THEN
                TOPSTK=TOPSTK+1
                IF(TOPSTK.GT.NPTS)THEN
       ! Error 14
!                  WRITE(76,1000)
!                  STOP
                  AutoGenFlag = -14
                  call ErrorMessage(14,V1,V1,V1,V1)
                  return
                ENDIF
                STACK(TOPSTK)=L
              ENDIF
              T(1,C)=L
            ENDIF
          ENDIF
          GOTO 609
        ENDIF
! end code from inside 140 loop

!---------------------------------------------------------------------
       ! Error 14
! 1000 FORMAT(//,'***ERROR IN SUBROUTINE INSERTP***',
!     +        /,'STACK OVERFLOW')
       ! Error 15
! 2000 FORMAT(//,'***WARNING IN INSERTP***',
!     +        /,'POINTS',I5,' AND',I5,' ARE COINCIDENT',
!     +        /,'DELETE EITHER POINT FROM LIST VECTOR' )

      END


!**********************************************************
