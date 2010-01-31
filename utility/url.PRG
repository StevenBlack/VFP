*
* url.prg 1.0
*
* Copyright (c) 2010 (http://stevenblack/com)
* Dual licensed under the MIT (MIT-LICENSE.txt)
* and GPL (GPL-LICENSE.txt) licenses.
*
* URL manager class for Visual FoxPro 
*


*=======================================================================
*  Test code for this class
*  To test, in the VFP command window, "DO url"
*=======================================================================
CLEAR
LOCAL x
x=CREATEOBJECT( "url", "http://foo.com/virtual/bar.xyz?x=1&y=2&z=3" )
?x.getURL()
x.AddElement( "test", "1" )
?x.getURL()
x.AddElement( "y", "4" )
?x.getURL()
?"nelements=", x.nElements
?"First element=", x.getValue( 1 )
?"y=", x.getvalue( "y" )
? "===================="
LOCAL lni
FOR lni= 1 TO x.nElements
  ?x.getAttribute( lni ), "=", x.getValue( lni )
ENDFOR
? "===================="
x.Coalesce( "http://zzz.org?x=Override&z=55&new=great" )
?x.getURL()
? "===================="
x.RemoveElement( 1 )
x.RemoveElement( "z" )
?x.getURL()
*=======================================================================

*****************************************************
DEFINE CLASS URL AS Custom
*****************************************************
DIMENSION aElements[ 1, 2 ]
cBase = ""
cElements = ""
cElementDelimiter = CHR( 38 )
cBaseDelimiter = "?"
nElements = 0

*=====================================
* URL::
FUNCTION INIT( tcURL )
*=====================================
tcURL= EVL( NVL(tcURL,""), "")
aElements = ""
This.LoadURL( tcURL )
RETURN

*=====================================
* URL::
FUNCTION LoadURL( tcURL )
*=====================================
IF !EMPTY( tcURL )
  LOCAL lcBaseDelimiter
  lcBaseDelimiter = This.cBaseDelimiter
  This.cBase = GETWORDNUM( tcURL, 1, lcBaseDelimiter )
  This.cElements = GETWORDNUM( tcURL, 2, lcBaseDelimiter )
ENDIF
RETURN

*=====================================
* URL::
FUNCTION GetURL()
*=====================================
LOCAL lcRetVal, lni
lcRetVal = This.cBase+ This.cBaseDelimiter
FOR lni = 1 TO ALEN( This.aElements, 1 )
  IF !EMPTY( This.aElements[ lni, 2 ] )
    IF lni>1
      lcRetVal = lcRetVal+ This.cElementDelimiter
    ENDIF
    lcRetVal = lcRetVal+ TRANSFORM( This.aElements[ lni, 1 ] )+ "="+ TRANSFORM( This.aElements[ lni, 2 ] )
  ELSE
    lcRetVal = lcRetVal+ TRANSFORM( EVL( This.aElements[ lni, 1 ], "" ) )
  ENDIF
ENDFOR
IF lcRetVal = This.cBaseDelimiter
  lcRetVal = ""
ENDIF
IF RIGHT( lcRetVal, LEN( This.cBaseDelimiter )) = This.cBaseDelimiter
  lcRetVal= SUBSTR(lcRetVal, 1, LEN( lcRetVal ) - LEN( This.cBaseDelimiter ))
ENDIF
RETURN lcRetVal

*=====================================
* URL::
FUNCTION AddElement( tcAttrib, tcValue )
*=====================================
LOCAL lnIndex
lnIndex = ASCAN( This.aElements, tcAttrib, 1, ALEN( This.aElements, 1 ), 1, 15 )
IF lnIndex = 0
  lnIndex = IIF( EMPTY( This.aElements[ 1,1 ] ), 1, ALEN( This.aElements,1 )+1 )
  DIMENSION This.aElements[ lnIndex, 2 ]
ENDIF
This.aElements[ lnIndex, 1 ] = tcAttrib
This.aElements[ lnIndex, 2 ] = tcValue
RETURN

*=====================================
* URL::
FUNCTION GetAttribute( tnPassed )
*=====================================
LOCAL lcRetVal, lnIndex
lcRetVal = ""
DO CASE
CASE VARTYPE( tnPassed ) $ "NI"
  IF tnPassed <= ALEN( This.aElements, 1 )
    lcRetVal = This.aElements[ tnPassed, 1 ]
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN lcRetVal

*=====================================
* URL::
FUNCTION GetValue( tuPassed )
*=====================================
LOCAL lcRetVal, lnIndex
lcRetVal = ""
DO CASE
CASE VARTYPE( tuPassed ) $ "NI"
  IF tuPassed <= ALEN( This.aElements, 1 )
    lcRetVal = This.aElements[ tuPassed, 2 ]
  ENDIF
CASE VARTYPE( tuPassed ) = "C"
  lnIndex = ASCAN( This.aElements, tuPassed, 1, ALEN( This.aElements, 1 ), 1, 15 )
  IF lnIndex>0
    lcRetVal = This.aElements[ lnIndex, 2 ]
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN lcRetVal

*=====================================
* URL::
FUNCTION RemoveElement( tuPassed )
*=====================================
LOCAL lcRetVal, lnIndex
lcRetVal = ""
DO CASE
CASE VARTYPE( tuPassed ) $ "NI"
  IF tuPassed <= ALEN( This.aElements, 1 )
    This.aElements[ tuPassed, 2 ] = ""
  ENDIF
CASE VARTYPE( tuPassed ) = "C"
  lnIndex = ASCAN( This.aElements, tuPassed, 1, ALEN( This.aElements, 1 ), 1, 15 )
  IF lnIndex>1
    * This.aElements[ lnIndex, 2 ] = ""
    ADEL( This.aElements, lnIndex )
    DIMENSION This.aElements[ MAX( ALEN( This.aElements, 1 )-1, 1 ), 2 ]
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN

*=====================================
* URL::
FUNCTION Coalesce( tcURL )
*=====================================
LOCAL loURL, lni
loURL = CREATEOBJECT( This.Class, tcURL )
FOR lni = 1 TO loURL.nElements
  This.AddElement( loURL.GetAttribute( lni ), loURL.GetValue( lni ) )
ENDFOR
loURL = .NULL.
RETURN

*=====================================
* URL::
FUNCTION cElements_Assign( tcElements )
*=====================================
LOCAL lcBaseDelimiter, lcElementDelimiter
lcBaseDelimiter = This.cBaseDelimiter
lcElementDelimiter = This.cElementDelimiter

IF This.cBaseDelimiter $ tcElements
  This.cElements = LOWER( GETWORDNUM( tcElements, 2, lcBaseDelimiter ) )
ELSE
  This.cElements = LOWER( tcElements )
ENDIF
LOCAL lni, lc2PartElement
FOR lni = 1 TO GETWORDCOUNT( tcElements, lcElementDelimiter ) )
  lc2PartElement = GETWORDNUM( tcElements, lni, lcElementDelimiter )
  THIS.AddElement( GETWORDNUM( lc2PartElement,1,"=" ), GETWORDNUM( lc2PartElement,2,"=" ) )
ENDFOR
RETURN

*=====================================
* URL::
FUNCTION cbase_Assign( tcbase )
*=====================================
IF This.cBaseDelimiter $ tcBase
  This.cBase = LOWER( GETWORDNUM( tcBase, 1, cBaseDelimiter ) )
ELSE
  This.cBase = LOWER( tcBase )
ENDIF
RETURN

*=====================================
* URL::
FUNCTION nElements_Access
*=====================================
RETURN IIF( EMPTY( This.aElements[ 1,1 ] ), 0, ALEN( This.aElements,1 ) )


ENDDEFINE

