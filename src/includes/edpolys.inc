!*----------------------------------------------------------------------*
!                       POLYSTOR.INC                                    *
!       Common Blocks for POLYGON data storage in memory.               *
!-----------------------------------------------------------------------*

! - DEFINITIONS
!       vnum = vertex number.
!       actvpoly = primary index to vertx(,), verty(,) of active polygon,
!                = 0 if no polygon is active.
!       curpoly = next available index to store a polygon in the arrays.
!       scrnpoly = polygon currently on display screen
!       vertx(,) = 2dim. array of polygons and their X coordinates.
!       verty(,) = 2dim. array of polygons and their Y coordinates.
!       vertcnt() = # vertices in a given polygon.
!       numpolys = # of polygons currently defined.

!   PARAMETERS set for polygon storage
!       maxvert = max # vertices a polygon may have.
!       maxpolys = max # polygons that may be defined.
!       maxpolydels = array dimension for pd(), the pending deletion array.
!-----------------------------------------------------------------------*

! - Storage for polygon definitions
      INTEGER, parameter ::  MAXVERT=500, MAXPOLYS=20
      INTEGER vnum, actvpoly, numpolys
      REAL vertx(MAXPOLYS,MAXVERT+1), verty(MAXPOLYS,MAXVERT+1)
      INTEGER vertcnt(MAXPOLYS), PolyCode(MAXPOLYS), curpoly, scrnpoly
      COMMON /POLYDEFS/ vertx, verty, vertcnt, PolyCode,                             &
     &         vnum, actvpoly, curpoly, numpolys, scrnpoly

!---------------------END POLYSTOR.INC----------------------------------*
