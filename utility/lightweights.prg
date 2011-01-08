*=====================================================
DEFINE CLASS LightWeight AS RELATION OLEPUBLIC
*=====================================================
  * Our base lightweight class.

  * These intrinsic properties are semantics of Relation
  * classes, which we are not interested in.
  Name= "Lightweight"
  PROTECTED ADDPROPERTY
  PROTECTED CHILDALIAS
  PROTECTED CHILDORDER
  PROTECTED ERROR
  PROTECTED ONETOMANY
  PROTECTED PARENTALIAS
  PROTECTED RELATIONALEXPR
  PROTECTED PARENT
  PROTECTED PARENTCLASS
  PROTECTED CLASSLIBRARY
  PROTECTED CLASS
  PROTECTED COMMENT
  PROTECTED TAG
  PROTECTED BASECLASS

  PROTECTED FUNCTION INIT
  PROTECTED FUNCTION DESTROY
  PROTECTED FUNCTION READMETHOD
  PROTECTED FUNCTION RESETTODEFAULT
  PROTECTED FUNCTION WRITEMETHOD
  PROTECTED FUNCTION READEXPRESSION
  PROTECTED FUNCTION WRITEEXPRESSION

  FUNCTION Release()
    This.GarbageCollect()
    RELEASE THIS

  PROTECTED FUNCTION GarbageCollect()
  RETURN

ENDDEFINE


*======================================================================
DEFINE CLASS cusLightWeight AS custom
*======================================================================
* Lightweight based on the Custom base class
*
  HIDDEN addobject
  HIDDEN cloneobject
  HIDDEN controlcount
  HIDDEN controls
  HIDDEN helpcontextid
  HIDDEN newobject
  HIDDEN objects
  HIDDEN parentclass
  HIDDEN picture
  HIDDEN readexpression
  HIDDEN readmethod
  HIDDEN removeobject
  HIDDEN resettodefault
  HIDDEN saveasclass
  HIDDEN showwhatsthis
  HIDDEN whatsthishelpid
  HIDDEN writeexpression
  HIDDEN writemethod

  FUNCTION RELEASE()
    RELEASE This
    RETURN
ENDDEFINE


