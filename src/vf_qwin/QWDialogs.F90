
!****************************************************************************

SUBROUTINE WPigElementInfo(index)
    
    USE IFLOGM
    IMPLICIT NONE
    INCLUDE 'RESOURCE.FD'

    INTEGER :: retint,ilen,index,nv(4),ec
    real :: xc,yc,zc
    LOGICAL retlog
    character(10) CString
    TYPE (dialog) dlg
    external UpdateElementInfo

  ! Initialize.
    IF ( .not. DlgInit( idd_eleinfo, dlg ) ) THEN
      WRITE (*,*) "Error: dialog not found"
    ELSE
      write(CString,'(I8)') index   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EINDEX, CString(1:ilen) )

      call UpdateElementInfo(dlg,IDC_EDIT_EINDEX,ilen)
      call GetElementInfo( index,xc,yc,zc,ec,nv )
      
      write(CString,'(F8.2)') xc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EX, CString(1:ilen) )

      write(CString,'(F8.2)') yc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EY, CString(1:ilen) )

      write(CString,'(F8.2)') zc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EZ, CString(1:ilen) )

      write(CString,'(I7)') ec   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_ECODE, CString(1:ilen) )

      write(CString,'(I8)') nv(1)   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EN1, CString(1:ilen) )

      write(CString,'(I8)') nv(2)   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EN2, CString(1:ilen) )

      write(CString,'(I8)') nv(3)   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EN3, CString(1:ilen) )

      write(CString,'(I7)') nv(4)   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_EN4, CString(1:ilen) )
    
      retlog = DlgSetSub(dlg,IDC_BUTTON_ELEUPDATE, UpdateElementInfo)

      retint = DlgModal( dlg )
        
    ! Release dialog resources.
        CALL DlgUninit( dlg )

    END IF
END SUBROUTINE wpigelementinfo

!****************************************************************************

SUBROUTINE WPigNodeinfo(index)
    
    USE IFLOGM
    IMPLICIT NONE
    INCLUDE 'RESOURCE.FD'

    INTEGER :: j,retint,ilen,index,nv(100),numngh,ec
    real :: xc,yc,zc
    LOGICAL retlog
    character(10) CString
    TYPE (dialog) dlg
    external UpdateNodeInfo

  ! Initialize.
    IF ( .not. DlgInit( idd_nodeinfo, dlg ) ) THEN
        WRITE (*,*) "Error: dialog not found"
    ELSE
      write(CString,'(I7)') index   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_NINDEX, CString(1:ilen) )

      call UpdateNodeInfo(dlg,IDC_EDIT_NINDEX,ilen)
      call GetNodeInfo( index,xc,yc,zc,ec,numngh,nv )

      write(CString,'(F8.2)') xc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_NX, CString(1:ilen) )

      write(CString,'(F8.2)') yc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_NY, CString(1:ilen) )

      write(CString,'(F8.2)') zc   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_NZ, CString(1:ilen) )

      write(CString,'(I7)') ec   
      ilen = len_trim(CString)
      retlog = DlgSet( dlg, IDC_EDIT_NCODE, CString(1:ilen) )

      retlog = DlgSet(dlg, IDC_LIST_NADJ, numngh, DLG_NUMITEMS)
      do j=1,numngh
        write(CString,'(I7)') nv(j)   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_LIST_NADJ, CString(1:ilen),j )
      enddo
    
      retlog = DlgSetSub(dlg,IDC_BUTTON_UPDATE, UpdateNodeInfo)

      retint = DlgModal( dlg )
        
    ! Release dialog resources.
        CALL DlgUninit( dlg )

    END IF
END SUBROUTINE wpignodeinfo

!****************************************************************************

SUBROUTINE WPigNodeCheck(ans, TheCriteria, MaxCrit)
    
    USE IFLOGM
    IMPLICIT NONE
    INCLUDE 'RESOURCE.FD'

!     - PASSED PARAMETERS
    integer MaxCrit
    logical ans, TheCriteria(MaxCrit)
    
    INTEGER :: retint
    LOGICAL retlog
    TYPE (dialog) dlg
    external UpdateNodeCheck

  ! Initialize.
    IF ( .not. DlgInit( idd_nodecheck, dlg ) ) THEN
      WRITE (*,*) "Error: dialog not found"
    ELSE
    
      retlog = DlgSet( dlg, IDC_CHECK_NC0, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC0, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC1, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC1, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC2, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC2, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC3, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC3, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC4, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC4, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC5, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC5, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC6, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC6, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC7, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC7, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC8, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC8, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NC9, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NC9, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NNOTC0, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NNOTC0, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NCINP, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NCINP, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NDLT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NDLT, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NDGT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NDGT, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NDBT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NDBT, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NNLT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NNLT, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NNGT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NNGT, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NNEQ, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NNEQ, UpdateNodeCheck)
      retlog = DlgSet( dlg, IDC_CHECK_NEXT, .false. )
      retlog = DlgSetSub(dlg,IDC_CHECK_NEXT, UpdateNodeCheck)
      retlog = DlgSetSub(dlg,IDC_BUTTON_RUNNODECHECK, UpdateNodeCheck)

      retint = DlgModal( dlg )
        
    ! Release dialog resources.
      CALL DlgUninit( dlg )

    END IF
END SUBROUTINE wpignodecheck

!****************************************************************************

SUBROUTINE WPigElementCheck()
    
    USE IFLOGM
    IMPLICIT NONE
    INCLUDE 'RESOURCE.FD'

    INTEGER :: retint
    LOGICAL retlog
    TYPE (dialog) dlg
    external UpdateEleCheck

  ! Initialize.
    IF ( .not. DlgInit( idd_elementcheck, dlg ) ) THEN
      WRITE (*,*) "Error: dialog not found"
    ELSE

      call UpdateEleCheck(dlg,IDC_RADIO_FULLCOLOR,DLG_CHANGE)
      call UpdateEleCheck(dlg,IDC_RADIO_EEQL,DLG_CHANGE)
      retlog = DlgSetSub(dlg,IDC_RADIO_FULLCOLOR, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_MARKCOLOR, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_EEQL, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_EDEP, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_EA2D, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_ECCW, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_EG90, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_ECODE, UpdateEleCheck)
      retlog = DlgSetSub(dlg,IDC_RADIO_EEXT, UpdateEleCheck)
          
      retlog = DlgSetSub(dlg,IDC_BUTTON_RUNELECHECK, UpdateEleCheck)

      retint = DlgModal( dlg )
        
    ! Release dialog resources.
      CALL DlgUninit( dlg )

    END IF

END SUBROUTINE WPigElementCheck

!****************************************************************************

SUBROUTINE UpdateNodeInfo(dlg,id, callbacktype)
  
  USE IFLOGM
  IMPLICIT NONE
  INCLUDE 'RESOURCE.FD'
  type (dialog) dlg
  integer id
  integer callbacktype

    INTEGER :: j,ilen,index,numngh,ec
    integer :: nv(100)
    integer, save :: index0, yellow = 14
    real :: xc,yc,zc
    LOGICAL retlog
    character(10) CString

    if(id.eq.IDC_EDIT_NINDEX) then
      retlog = DlgGet( dlg, IDC_EDIT_NINDEX, CString )
      read(CString,*) index0   
    
    else
      retlog = DlgGet( dlg, IDC_EDIT_NINDEX, CString )
      read(CString,*) index   

      if(index.ne.index0) then
        index0 = index
        
        write(CString,'(I8)') index   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NINDEX, CString(1:ilen) )

        call GetNodeInfo( index,xc,yc,zc,ec,numngh,nv )
	    call PutMarker( xc, yc, 4, yellow)
        
        write(CString,'(F8.2)') xc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NX, CString(1:ilen) )

        write(CString,'(F8.2)') yc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NY, CString(1:ilen) )

        write(CString,'(F8.2)') zc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NZ, CString(1:ilen) )

        write(CString,'(I7)') ec   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NCODE, CString(1:ilen) )

        retlog = DlgSet(dlg, IDC_LIST_NADJ, numngh, DLG_NUMITEMS)
        do j=1,numngh
          write(CString,'(I7)') nv(j)   
          ilen = len_trim(CString)
          retlog = DlgSet( dlg, IDC_LIST_NADJ, CString(1:ilen),j )
        enddo
      
      else
        
        retlog = DlgGet( dlg, IDC_EDIT_NZ, CString )
        read(CString,*) zc   
        retlog = DlgGet( dlg, IDC_EDIT_NCODE, CString )
        read(CString,*) ec   
        
        call  SetNodeInfo( index,ec,zc )

        write(CString,'(F8.2)') zc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_NZ, CString(1:ilen) )
        
        write(CString,'(I7)') ec   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_ECODE, CString(1:ilen) )
      endif
      
    endif

END SUBROUTINE updatenodeinfo

!****************************************************************************

SUBROUTINE UpdateElementInfo(dlg,id, callbacktype)
  
  USE IFLOGM
  IMPLICIT NONE
  INCLUDE 'RESOURCE.FD'
  type (dialog) dlg
  integer id
  integer callbacktype

    INTEGER :: ilen,index,nv(4),ec
    integer, save :: index0,yellow=14
    real :: xc,yc,zc
    LOGICAL retlog
    character(10) CString

    if(id.eq.IDC_EDIT_EINDEX) then
      retlog = DlgGet( dlg, IDC_EDIT_EINDEX, CString )
      read(CString,*) index0   
    
    else
      retlog = DlgGet( dlg, IDC_EDIT_EINDEX, CString )
      read(CString,*) index   

      if(index.ne.index0) then
        index0 = index
        
        write(CString,'(I8)') index   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EINDEX, CString(1:ilen) )

        call GetElementInfo( index,xc,yc,zc,ec,nv )
	    call PutMarker( xc, yc, 4, yellow)
        
        write(CString,'(F8.2)') xc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EX, CString(1:ilen) )

        write(CString,'(F8.2)') yc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EY, CString(1:ilen) )

        write(CString,'(F8.2)') zc   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EZ, CString(1:ilen) )

        write(CString,'(I7)') ec   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_ECODE, CString(1:ilen) )

        write(CString,'(I8)') nv(1)   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EN1, CString(1:ilen) )

        write(CString,'(I8)') nv(2)   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EN2, CString(1:ilen) )

        write(CString,'(I8)') nv(3)   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EN3, CString(1:ilen) )

        write(CString,'(I7)') nv(4)   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_EN4, CString(1:ilen) )
      
      else
        retlog = DlgGet( dlg, IDC_EDIT_ECODE, CString )
        read(CString,*) ec   
        call  SetElementInfo( index,ec )
        write(CString,'(I7)') ec   
        ilen = len_trim(CString)
        retlog = DlgSet( dlg, IDC_EDIT_ECODE, CString(1:ilen) )
      endif
      
    endif
    
END SUBROUTINE updateelementinfo

!****************************************************************************

SUBROUTINE UpdateNodeCheck(dlg,id, callbacktype)
  
  USE IFLOGM
  IMPLICIT NONE
  INCLUDE 'RESOURCE.FD'

  type (dialog) dlg
  integer id,ntest
  integer callbacktype
  logical check,retlog

    select case (id)
      case (IDC_CHECK_NC0)
        ntest = 1
        retlog = DlgGet( dlg, IDC_CHECK_NC0, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC1)
        ntest = 2
        retlog = DlgGet( dlg, IDC_CHECK_NC1, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC2)
        ntest = 3
        retlog = DlgGet( dlg, IDC_CHECK_NC2, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC3)
        ntest = 4
        retlog = DlgGet( dlg, IDC_CHECK_NC3, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC4)
        ntest = 5
        retlog = DlgGet( dlg, IDC_CHECK_NC4, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC5)
        ntest = 6
        retlog = DlgGet( dlg, IDC_CHECK_NC5, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC6)
        ntest = 7
        retlog = DlgGet( dlg, IDC_CHECK_NC6, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC7)
        ntest = 8
        retlog = DlgGet( dlg, IDC_CHECK_NC7, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC8)
        ntest = 9
        retlog = DlgGet( dlg, IDC_CHECK_NC8, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NC9)
        ntest = 10
        retlog = DlgGet( dlg, IDC_CHECK_NC9, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NNOTC0)
        ntest = 11
        retlog = DlgGet( dlg, IDC_CHECK_NNOTC0, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NCINP)
        ntest = 12
        retlog = DlgGet( dlg, IDC_CHECK_NCINP, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NDLT)
        ntest = 13
        retlog = DlgGet( dlg, IDC_CHECK_NDLT, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NDGT)
        ntest = 14
        retlog = DlgGet( dlg, IDC_CHECK_NDGT, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NDBT)
        ntest = 15
        retlog = DlgGet( dlg, IDC_CHECK_NDBT, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NNLT)
        ntest = 16
        retlog = DlgGet( dlg, IDC_CHECK_NNLT, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NNGT)
        ntest = 17
        retlog = DlgGet( dlg, IDC_CHECK_NNGT, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NNEQ)
        ntest = 18
        retlog = DlgGet( dlg, IDC_CHECK_NNEQ, check )
        call SetUserValue(ntest,check)
      case (IDC_CHECK_NEXT)
        ntest = 19
        retlog = DlgGet( dlg, IDC_CHECK_NEXT, check )
        call SetUserValue(ntest,check)
      case (IDC_BUTTON_RUNNODECHECK)
        call ReDrawOnly()
  end select

END SUBROUTINE updatenodecheck

!****************************************************************************

SUBROUTINE UpdateEleCheck(dlg,id, callbacktype)
  
  USE IFLOGM
  IMPLICIT NONE
  INCLUDE 'RESOURCE.FD'

  type (dialog) dlg
  integer :: id,ntest=1
  logical :: mode= .true.
  integer callbacktype

	select case (id)
	  case (IDC_RADIO_FULLCOLOR)
!	    write(*,*) 'Full color'
	    mode = .true.
	  case (IDC_RADIO_MARKCOLOR)
!	    write(*,*) 'Color markers'
        mode = .false.
	  case (IDC_RADIO_EEQL)
!	    write(*,*) 'Equilateral test'
	    ntest = 1
	  case (IDC_RADIO_EDEP)
!	    write(*,*) 'Depth test'
        ntest = 2
	  case (IDC_RADIO_EA2D)
!	    write(*,*) 'Area/depth test'
        ntest = 3
	  case (IDC_RADIO_ECCW)
!	    write(*,*) 'Counterclockwise test'
        ntest = 4
	  case (IDC_RADIO_EG90)
!	    write(*,*) 'Angle > 90 test'
        ntest = 5
	  case (IDC_RADIO_ECODE)
!	    write(*,*) 'Code test'
        ntest = 6
	  case (IDC_RADIO_EEXT)
!	    write(*,*) 'External test'
        ntest = 7
      case (IDC_BUTTON_RUNELECHECK)
        call ElementCheck( ntest, mode )
    end select

END SUBROUTINE updateelecheck

!****************************************************************************