!-----------------------------------------------------------------------*
!                                                                       *
!                       DEFAULTS.INC                                    *
!       The following common block declarations keep track of all       *
!       default variables.                                              *
!                                                                       *
!-----------------------------------------------------------------------*
!        Grid related variables                                         *
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
      LOGICAL DispCont, DispBound, DispSound, GMerge 
      logical DispNodes, BoundCCW, CheckBN1

      COMMON /DrawDefaults/ GridPColour, GridSColour, ContColour
      COMMON /DrawDefaults2/ BoundColour, GridSIndex 
      COMMON /DrawDefaults3/ SoundColour,ModificationColour
      COMMON /DrawDefaults4/ DispBound, DispCont, DispSound, GMerge
      COMMON /DrawDefaults5/ DispNodes, BoundCCW, CheckBN1

      INTEGER TitleColour
      PARAMETER( TitleColour = 3 )

      character*1 :: newline
      INTEGER MarkType1
      COMMON /MoreDefaults/ MarkType1,newline

!-----------------------------------------------------------------------*
!        Node related variables                                         *
!-----------------------------------------------------------------------*

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


!--------------------- END DEFAULTS.INC --------------------------------*

