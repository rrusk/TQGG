!******************************************************************************
!******************************************************************************
!  This module provides an interface between grid generation programs
!  for unstructured grids and model preprocessors. The netCDF file is
!  UGRID 0.9.0 compliant. The file contains the following:
!    n_nodes:  dimension for number of nodes (vertices)
!    n_elements:  dimension for the number of elements (2d faces) 
!    n_element_nodes: dimension for the number of nodes per element
!    node_x: variable containing x-coordinate data (longitude or Cartesian x)
!    node_y: variable containing y-coordinate data (latitude or Cartesian y)
!    node_z: variable containing z-coordinate data (depth or height)
!    node_code: variable containing integer node attribute
!    element_code: variable containing integer element attribute
!    element_node_connectivity: variable containing integer element vertices in CCW order
!  Initial version written by Roy Walters
!******************************************************************************
!******************************************************************************

  MODULE UGrid_netCDFio

    USE netcdf        ! enables the fortran 90 interface to netcdf

    implicit none

!  netCDF parameters

    integer     ::  ncid         ! Id of file
    integer     ::  NodeDimId    ! Id of the node dimension
    integer     ::  EleDimId     ! Id of the element dimension
    integer     ::  NcnDimId     ! Id of the element nodes dimension
    integer     ::  BNodeDimId   ! Id of the boundary node dimension
    integer     ::  BndDimId     ! Id of the boundary dimension
    integer     ::  BndIndxDimId     ! Id of the boundary index dimension

! *** Grid variables
    integer     ::  XVarId       ! Id of the X variable
    integer     ::  YVarId       ! Id of the Y variable
    integer     ::  ZVarId       ! Id of the Z variable
    integer     ::  LongVarId    ! Id of the longitude variable
    integer     ::  LatVarId     ! Id of the latitude variable
    integer     ::  DepthVarId   ! Id of the depth variable
    integer     ::  NcodeVarId   ! Id of the node code variable
    integer     ::  nenVarId     ! Id of the element variable
    integer     ::  EcodeVarId   ! Id of the element code variable
    integer     ::  BNodeVarId   ! Id of the element code variable
    integer     ::  BndIndexVarId! Id of the boundary index variable
    integer     ::  BndIdVarId   ! Id of the boundary id (type) variable

    private
    public Create_Grid_netCDF,Write_Grid_netCDF,Read_GridSize_netCDF,Read_Grid_netCDF
    public Write_Boundary_netCDF,Close_netCDF,Read_Boundary_netCDF
    public Read_BoundarySize_netCDF

!******************************************************************************
  CONTAINS
!******************************************************************************

    subroutine Create_Grid_netCDF(np,ne,ncn,iXYcoord,iZcoord,outresfile,err)

!    Create netCDF file to contain output.

    implicit none

! *** passed variables
    integer ne,np,ncn,iXYcoord,iZcoord
    character(len=80) :: outresfile

! Local variables
    integer status,ivar
    character(len=80)  ::  title
    logical err

! Local variables for creation time
    CHARACTER*30 datestr, tmpstr
    CHARACTER*10 timestr
    CHARACTER*5 zonestr
    INTEGER timval(8)

! *** Now create netCDF file

    err = .false.
    
!  ncid is returned
    status=nf90_create(trim(OutResFile),nf90_clobber,ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_close(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! *** Open file for writing

    status=nf90_open(trim(OutResFile),nf90_write,ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! *** Put in define mode

    status=nf90_redef(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! Global Attributes

    title=trim(outresfile)
    status=nf90_put_att(ncid,nf90_global,"title",'grid_description')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,"source","myProgram")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,'cf_role','mesh_topology')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,'long_name',"Topology data of 2D unstructured mesh")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,"Conventions","UGRID 0.9.0")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 2
    status=nf90_put_att(ncid,nf90_global,'topology_dimension',ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,'node_coordinates','node_x node_y')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,nf90_global,"face_node_connectivity",'element_node_connectivity')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

!    status=nf90_put_att(ncid,nf90_global,"comment","any comments")
!    if(status /= nf90_noerr)call handle_nf_err(status)

! Add file creation time to global attributes
    CALL date_and_time(datestr, timestr, zonestr, timval)
! *** Adjust to UTC
    IF(timval(4) .ne. 0) THEN
      timval(5)=timval(5)-timval(4)/60
      IF(mod(timval(4),60) .eq. 0) THEN
        IF (timval(5) .lt. 0) THEN
          timval(5)=timval(5)+24
          timval(3)=timval(3)-1
        ELSE IF (timval(5) .ge. 24) THEN
          timval(3)=timval(3)+1
          timval(5)=timval(5)-24
        END IF
      ELSE
        timval(6)=timval(6)-mod(timval(4),60)
        IF(timval(6) .lt. 0) THEN
          timval(6)=timval(6)+60
          timval(5)=timval(5)-1
        END IF
      END IF
      IF (timval(3) .lt. 10) then
        datestr(7:7)="0"
        write(datestr(8:8),'(I1)'),timval(3)
      ELSE
        write(datestr(7:8),'(I2)'),timval(3)
      END IF
      IF (timval(5) .lt. 10) then
        timestr(1:1)="0"
        write(timestr(2:2),'(I1)'),timval(5)
      ELSE
        write(timestr(1:2),'(I2)'),timval(5)
      END IF
      IF (timval(6) .lt. 10) then
        timestr(3:3)="0"
        write(timestr(4:4),'(I1)'),timval(6)
      ELSE
        write(timestr(3:4),'(I2)'),timval(6)
      END IF
      tmpstr = datestr(1:4)//'-'//datestr(5:6)//'-'// &
      datestr(7:8)//':'//timestr(1:4)
    END IF
    
    status = nf90_put_att(ncid, NF90_GLOBAL, 'file_creation_time', tmpstr)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif
 
! *** dimensions

    status=nf90_def_dim(ncid,"n_nodes",np,NodeDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_dim(ncid,"n_elements",ne,EleDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_dim(ncid,"n_element_nodes",ncn,NcnDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! *** Grid variables

    status=nf90_def_var(ncid,'node_x',nf90_double,NodeDimId,XVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_var(ncid,'node_y',nf90_double,NodeDimId,YVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    if(iXYcoord.eq.0) then  !spherical coordinates

! Latitude

      status=nf90_put_att(ncid,YVarId,"standard_name","latitude")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"long_name","latitude_in_spherical_coordinates")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"units","degrees_north")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,'positive','north')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"axis","y")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

! Longitude

      status=nf90_put_att(ncid,XVarId,"standard_name","longitude")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"long_name","longitude_in_spherical_coordinates")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"units","degrees_east")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,'positive','east')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"axis","x")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

    else  ! Cartesian coordinates

! X

      status=nf90_put_att(ncid,XVarId,"standard_name",'x_coordinate')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"long_name",'Cartesian_coordinate_x')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"units","m")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,'positive','right')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,XVarId,"axis","x")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

! Y

      status=nf90_put_att(ncid,YVarId,"standard_name",'y_coordinate')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"long_name",'Cartesian_coordinate_y')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"units",'m')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,'positive','90_degrees_counterclockwise_from_x')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,YVarId,"axis","y")
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif


    endif

! Depth

    status=nf90_def_var(ncid,'node_z',nf90_double,NodeDimId,ZVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif
    
    status=nf90_put_att(ncid,ZVarId,"standard_name",'depth')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,ZVarId,"units",'m')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,ZVarId,"axis",'z')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif
      
    if(iZcoord.eq.0) then !depth, plus down

      status=nf90_put_att(ncid,ZVarId,"long_name",'depth_below_geoid')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,ZVarId,"positive",'down')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

    
    else !height, plus up

      status=nf90_put_att(ncid,ZVarId,"long_name",'height_above_geoid')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif

      status=nf90_put_att(ncid,ZVarId,"positive",'up')
      if(status /= nf90_noerr) then
        call handle_nf_err(status)
        err = .true.
        return
      endif
      
    endif

! Node codes

    status=nf90_def_var(ncid,"node_code",nf90_int,NodeDimId,NcodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NcodeVarId,"standard_name","node_code")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NcodeVarId,"long_name","node_codes_for_boundary_conditions")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 0
    status=nf90_put_att(ncid,NcodeVarId,"no_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 1
    status=nf90_put_att(ncid,NcodeVarId,"land_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 2
    status=nf90_put_att(ncid,NcodeVarId,"island_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 3
    status=nf90_put_att(ncid,NcodeVarId,"open_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 4
    status=nf90_put_att(ncid,NcodeVarId,"radiation_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 5
    status=nf90_put_att(ncid,NcodeVarId,"discharge_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 6
    status=nf90_put_att(ncid,NcodeVarId,"weir_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! Element list
    status=nf90_def_var(ncid,"element_node_connectivity",nf90_int,(/NcnDimId,EleDimId/),NenVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NenVarId,"standard_name","element_node_connectivity")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NenVarId,"long_name","list_of nodes_in_each_element_in_CCW_order")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NenVarId,'cf_role','face_node_connectivity')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,NenVarId,'units','nondimensional')
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! Element codes

    status=nf90_def_var(ncid,"element_code",nf90_int,EleDimId,EcodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,EcodeVarId,"standard_name","element_code")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_att(ncid,EcodeVarId,"long_name","element_codes_for_element_attributes")
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 0
    status=nf90_put_att(ncid,EcodeVarId,"inactive",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 1
    status=nf90_put_att(ncid,EcodeVarId,"active",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    ivar = 9
    status=nf90_put_att(ncid,EcodeVarId,"discharge_bc",ivar)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

!    Finish defs

    status=nf90_enddef(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Create_Grid_netCDF

!********************************************************************************

    subroutine Write_Grid_netCDF ( np,ne,ncn,x,y,depth,ncode,ecode,nen,err)

!  Output data for a particular constituent

    implicit none

! *** passed variables
    integer np,ne,ncn
    integer nen(ncn,ne),ncode(np),ecode(ne)
    real*8 x(np), y(np), depth(np)

! *** Local variables
    integer status
    logical err

! *** Put the grid data into the files, 

    err = .false.

    status=nf90_put_var(ncid,XVarId,x)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,YVarId,y)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,ZVarId,depth)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,NcodeVarId,ncode)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,nenVarId,nen)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,EcodeVarId,ecode)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Write_Grid_netCDF

!********************************************************************************

    subroutine Write_Boundary_netCDF (nbp,nbnd,bnode_index,bnode_id,bnodes,err)

!  Output data for a particular constituent

    implicit none

! *** passed variables
    integer, intent(in) :: nbp,nbnd
    integer, intent(in) :: bnode_index(nbnd+1),bnode_id(nbnd),bnodes(nbp)
    logical, intent(out) :: err

! *** Local variables
    integer status

! *** Put the grid data into the files
    err = .false.
    
! *** Put in define mode

    status=nf90_redef(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! *** dimensions

    status=nf90_def_dim(ncid,"n_bnodes",nbp,BNodeDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_dim(ncid,"n_boundaries",nbnd,BndDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_dim(ncid,"n_bound_index",nbnd+1,BndIndxDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

! *** Boundary variables

    status=nf90_def_var(ncid,'boundary_nodes',nf90_int,BNodeDimId,BNodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_var(ncid,'boundary_node_index',nf90_int,BndIndxDimId,BndIndexVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_def_var(ncid,'boundary_node_id',nf90_int,BndDimId,BndIdVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

!    Finish defs

    status=nf90_enddef(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,BNodeVarId,bnodes)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,BndIndexVarId,bnode_index)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_put_var(ncid,BndIdVarId,bnode_id)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif


    end subroutine Write_Boundary_netCDF


!********************************************************************************

    subroutine Read_GridSize_netCDF ( fname,np,ne,ncn,err )

!   Retrieve the dimensions from PreMod netCDF file

    implicit none

! *** passed variables
    character(len=*),intent(in)         :: fname
    integer,intent(out)                 :: ne
    integer,intent(out)                 :: np
    integer,intent(out)                 :: ncn
    logical, intent(out)                :: err

! *** local variables
    integer         :: status
    character*80 :: name

!   Open the netCDF file

    err = .false.

    status=nf90_open(trim(fname),nf90_nowrite,ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

!   Retrieve dimension lengths

    status=nf90_inq_dimid(ncid,'n_nodes',NodeDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,NodeDimId,name,np)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_dimid(ncid,'n_elements',EleDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,EleDimId,name,ne)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_dimid(ncid,'n_element_nodes',NcnDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,NcnDimId,name,ncn)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Read_GridSize_netCDF

!********************************************************************************

    subroutine Read_Grid_netCDF(np,ne,ncn,x,y,depth,ncode,ecode,nen,ixy,iz,err )

!   Get the grid stuff from the already open netCDF file

    implicit none

! *** passed variables
    integer,intent(in)      :: ne
    integer,intent(in)      :: np
    integer,intent(in)      :: ncn
    integer,intent(out)     :: ncode(np)
    integer,intent(out)     :: ecode(ne)
    integer,intent(out)     :: nen(ncn,ne)
    integer,intent(out)     :: ixy
    integer,intent(out)     :: iz
    real*8,intent(out)      :: x(np)
    real*8,intent(out)      :: y(np)
    real*8,intent(out)      :: depth(np)
    logical, intent(out)    :: err

! Local variables
    integer         :: status
    character(len=80) value

    err = .false.

    status=nf90_inq_varid(ncid,'element_node_connectivity',nenVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,nenVarId,nen)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_varid(ncid,'node_x',XVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,XVarId,x)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status= nf90_get_att(ncid,XVarId,'standard_name',value)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    if(value(1:4).eq.'long') then
      ixy = 0
    elseif(value(1:4).eq.'x_coo') then
      ixy = 1
    endif

    status=nf90_inq_varid(ncid,'node_y',YVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,YVarId,y)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_varid(ncid,'node_z',ZVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,ZVarId,depth)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status= nf90_get_att(ncid,ZVarId,'positive',value)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    if(value(1:2).eq.'up') then
      iz = 1
    elseif(value(1:4).eq.'down') then
      iz = 0
    endif

    status=nf90_inq_varid(ncid,'node_code',NcodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,NcodeVarId,ncode)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_varid(ncid,'element_code',EcodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,EcodeVarId,ecode)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Read_Grid_netCDF

!********************************************************************************

    subroutine Read_BoundarySize_netCDF (nbp,nbnd,nbnd1,err)

!  Output data for a particular constituent

    implicit none

! *** passed variables
    integer, intent(out) :: nbp,nbnd,nbnd1
    logical, intent(out) :: err

! *** Local variables
    integer status
    character*80 :: name

! *** Put the grid data into the files
    err = .false.

!   Retrieve dimension lengths

    status=nf90_inq_dimid(ncid,"n_bnodes",BNodeDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,BNodeDimId,name,nbp)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_dimid(ncid,"n_boundaries",BndDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,BndDimId,name,nbnd)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_dimid(ncid,"n_bound_index",BndIndxDimId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inquire_dimension(ncid,BndIndxDimId,name,nbnd1)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Read_BoundarySize_netCDF

!********************************************************************************

    subroutine Read_Boundary_netCDF (nbp,nbnd,bnode_index,bnode_id,bnodes,err)

!  Output data for a particular constituent

    implicit none

! *** passed variables
    integer, intent(in) :: nbp,nbnd
    integer, intent(out) :: bnode_index(nbnd+1),bnode_id(nbnd),bnodes(nbp)
    logical, intent(out) :: err

! *** Local variables
    integer status

! *** Put the grid data into the files
    err = .false.

    status=nf90_inq_varid(ncid,'boundary_nodes',BNodeVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,BNodeVarId,bnodes)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_varid(ncid,'boundary_node_index',BndIndexVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,BndIndexVarId,bnode_index)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_inq_varid(ncid,'boundary_node_id',BndIdVarId)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    status=nf90_get_var(ncid,BndIdVarId,bnode_id)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif


    end subroutine Read_Boundary_netCDF

!********************************************************************************
    

    subroutine Close_netCDF (err)

    implicit none

! *** passed variables
    logical, intent(out)    :: err

! Local variables
    integer         :: status

! *** Close file

    status=nf90_close(ncid)
    if(status /= nf90_noerr) then
      call handle_nf_err(status)
      err = .true.
      return
    endif

    end subroutine Close_netCDF

!********************************************************************************

    subroutine handle_nf_err(status)
!
!  Deal with a netCDF error message
!

    implicit none

    integer,intent(in)  ::  status

    if(status /= nf90_noerr)then
      write(*,*)'netCDF status error: ',trim(nf90_strerror(status))
!      stop 'Execution Stopped'
    endif

    end subroutine handle_nf_err

!********************************************************************************

  END MODULE UGrid_netCDFio

!********************************************************************************
