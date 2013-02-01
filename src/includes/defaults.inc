!-----------------------------------------------------------------------*
!                                                                       *
!                       DEFAULTS.INC                                    *
!       The following common block declarations keep track of all       *
!       default variables used in the new version of the TRIGRID        *
!       Editor.                                                         *
!                                                                       *
!-----------------------------------------------------------------------*

      CHARACTER*256 GridRName, GridIName, LastInterim, Fblank
      CHARACTER*256 ContFName, BoundFName, SoundFName
      CHARACTER*256 VCritName, TCritName
      PARAMETER( Fblank=' '  )

      COMMON /FNames/ GridRName, GridIName, LastInterim,VCritName
      COMMON /FNames2/ ContFName, BoundFName, SoundFName,TCritName

      INTEGER GridSIndex
      INTEGER GridPColour, GridSColour, ContColour, BoundColour
      integer SoundColour, ModificationColour
      LOGICAL DispCont, DispBound, DispSound, GMerge, DispNodes

      COMMON /DrawDefaults/ GridPColour, GridSColour, ContColour
      COMMON /DrawDefaults2/ BoundColour, GridSIndex, DispCont 
      COMMON /DrawDefaults3/ DispBound, GMerge,ModificationColour
      COMMON /DrawDefaults4/ SoundColour, DispSound, DispNodes

      INTEGER TitleColour
      PARAMETER( TitleColour = 3 )

      INTEGER MarkType1
      COMMON /MoreDefaults/ MarkType1

!*----------------------------------------------------------------------*
!                       NODEDEF.INC                                     *
!       The following common block declarations keep track of all       *
!       default variables used in the TRIGRID NODER.                    *
!
!*----------------------------------------------------------------------*

! - FILE NAMES
      CHARACTER*256 NodeRName, NodeIName, LastNInterim

      COMMON /FILENAMES/ NodeRName, NodeIName, LastNInterim

! - COLORS & MARKERS
      integer NodeSIndex
      integer NodeIColor, NodeBColor, NodeSColor
      integer ContNColor, BoundNColor
      integer ModifColor, PendColor, InfoColor
      integer NodeMType, PendMType, MarkMType, MarkColor

      COMMON /NODEDEFAULTS/ NodeIColor, NodeBColor, NodeSColor
      COMMON /NODEDEFAULTS2/ ContNColor,BoundNColor, NodeSIndex
      COMMON /NODEDEFAULTS3/ ModifColor, PendColor, NodeMType 
      COMMON /NODEDEFAULTS4/ PendMType, MarkMType,MarkColor, InfoColor

      integer MaxMarkType,MinMarkType
      PARAMETER( MaxMarkType = 5 )
      PARAMETER( MinMarkType = -1 )

!*------------------------ END NODEDEF.INC ------------------------------*

!--------------------- END DEFAULTS.INC --------------------------------*

