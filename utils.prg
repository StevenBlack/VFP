************************************************************************
FUNCTION Shellexec(tcFile)
************************************************************************
oShellExecute = NEWOBJECT("_shellexecute", HOME()+"FFC\_environ.vcx")
oShellExecute.ShellExecute(FULLPATH(tcFile))
RETURN

************************************************************************
FUNCTION GetProperty( tcProperty, tcPropertyText)
************************************************************************
* Returns a property form a properties memo
LOCAL lcRetVal, lnAtPos, lcLine
lcRetVal = ""
DO WHILE " ="$ tcPropertyText
  tcPropertyText = STRTRAN( tcPropertyText, " =", "=")
ENDDO
DO CASE
CASE EMPTY(tcProperty) OR EMPTY(tcPropertyText)
  *-- Do nothing

OTHERWISE
  lnAtPos = ATC(tcProperty + "=", tcPropertyText)
  IF lnAtPos > 0
    lcLine = MLINE(tcPropertyText, 1, lnAtPos)
    lcRetVal = ALLTRIM(SUBS( lcLine, AT("=", lcLine)+1))
  ENDIF
ENDCASE
RETURN lcRetVal

************************************************************************
FUNCTION Cell( tcContents, tcAttributes)
************************************************************************
*  Format a cell for display
LOCAL lcRetVal, lcCell, lcAttributes
IF EMPTY(tcAttributes)
  lcAttributes = ""
ELSE
  lcAttributes = " " + ALLTRIM(tcAttributes)
ENDIF
lcRetVal =[<td] + lcAttributes + [>]
lcCell = ALLTRIM(TEXTMERGE(tcContents, .t., "{{","}}"))
IF EMPTY(lcCell)
  lcCell = CHR(38) + "nbsp" + CHR(59)
ENDIF
lcRetVal = lcRetVal + lcCell + [</td>]
RETURN lcRetVal

* --------------------------------------------------------- *
FUNCTION GIFSize( pcFileName, pnWidth, pnHeight )
*
* Returns the size WxH of a GIF file.
*
* by Thomas Gehrke
*
* EXAMPLE 1:
*
* ? GIFSize( 'C:\IMAGES\Test.GIF')
*
* Returns: '320x200'
*
* Pass optional numeric 2nd and 3rd parameters BY REFERENCE
* to receive separate values into variables.
*
* EXAMPLE 2:
*
* LOCAL lnWidth, lnHeight
* STORE 0 TO lnWidth, lnHeight
* = GIFSize( 'C:\IMAGES\Test.GIF', @lnWidth, @lnHeight)
*
IF EMPTY( m.pcFileName)
  pcFileName = GETFILE( 'GIF', 'Select GIF File:', 'Get Size')
ENDIF

IF EMPTY( m.pcFileName)
  RETURN ''
ENDIF

PRIVATE lnHandle, lcRetStr
lnHandle = FOPEN( m.pcFileName)
lcRetStr = ''

IF m.lnHandle > -1

  PRIVATE lnFileSize, lnCounter, lcBytes
  lnFileSize = FSEEK( m.lnHandle, 0, 2)
  lnCounter  = 0

  IF m.lnFileSize >= 10
    = FSEEK( m.lnHandle, 0, 0)

    * Read the 1st 10 bytes:
    lcBytes = FREAD( m.lnHandle, 10)

    = FCLOSE( m.lnHandle)

    pnWidth  = ASC( SUBSTR( m.lcBytes, 8, 1)) * 256 + ;
      ASC( SUBSTR( m.lcBytes, 7, 1))

    pnHeight = ASC( SUBSTR( m.lcBytes, 10, 1)) * 256 + ;
      ASC( SUBSTR( m.lcBytes, 9, 1))

    lcRetStr = LTRIM( STR( m.pnWidth)) + 'x' + LTRIM( STR( m.pnHeight))
  ENDIF
ELSE
  pnWidth = 0
  pnHeight = 0
  lcRetStr = ''
ENDIF

RETURN m.lcRetStr
ENDFUNC  && GIFSize

* --------------------------------------------------------- *
FUNCTION JPGSize( pcFileName, pnWidth, pnHeight )
*
* Returns the size WxH of a JPG file.
*
* by Thomas Gehrke
*
* See Comments to function GIFSize() for usage notes.
*
IF EMPTY( m.pcFileName)
  pcFileName = GETFILE( 'JPG', 'Select JPG File:', 'Get Size')
ENDIF

IF EMPTY( m.pcFileName)
  RETURN ''
ENDIF

PRIVATE lnHandle, lcRetStr
lnHandle = FOPEN( m.pcFileName)
lcRetStr = ''

IF m.lnHandle > -1

  PRIVATE lnFileSize, lnCounter, lcBytes
  lnFileSize = FSEEK( m.lnHandle, 0, 2)
  lnCounter  = 0

  = FSEEK( m.lnHandle, 0, 0)

  FOR lnCounter = 1 TO m.lnFileSize - 2
    lcBytes = FREAD( m.lnHandle, 3)

    IF m.lcBytes = CHR(0) + CHR(17) + CHR(8) OR ;
        m.lcBytes = CHR(0) + CHR(11) + CHR(8)
      *
      * Found block marker for dimensions!
      lcBytes = FREAD( m.lnHandle, 4)
      EXIT
    ELSE
      = FSEEK( m.lnHandle, -2, 1)
    ENDIF
  ENDFOR

  = FCLOSE( m.lnHandle)

  pnWidth  = ASC( SUBSTR( m.lcBytes, 3, 1)) * 256 + ;
    ASC( SUBSTR( m.lcBytes, 4, 1))

  pnHeight = ASC( SUBSTR( m.lcBytes, 1, 1)) * 256 + ;
    ASC( SUBSTR( m.lcBytes, 2, 1))

  lcRetStr = LTRIM( STR( m.pnWidth)) + 'x' + LTRIM( STR( m.pnHeight))

ELSE
  pnWidth = 0
  pnHeight = 0
  lcRetStr = ''
ENDIF

RETURN m.lcRetStr
ENDFUNC  && JPGSize

************************************************************
*  FUNCTION Within()
************************************************************
FUNCTION within( tcExpression, tcLeft, tcRight, tnFirstOne, tnFollowing)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:06:58
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION Within()
*)                   : Returns string contained within two
*)                     others.  Case sensitive
*  Calling Samples...: within( <expC>, <expC>, <expC> [,<expN> [,<expN>]])
*  Parameter List....: tcExpression
*                      tcLeft
*                      tcRight
*                      tnFirstOne
*                      tnFollowing
*  Major change list.:
*  ERs...............:

LOCAL lcReturnVal, tnLeftpos
lcReturnVal = []
tnLeftpos = AT( tcLeft, tcExpression, IIF( EMPTY( tnFirstOne), 1, tnFirstOne))
IF tnLeftpos > 0
  tnLeftpos = tnLeftpos + LEN( tcLeft)
  IF tnLeftpos< LEN( tcExpression)
    lcReturnVal = SUBSTR( tcExpression, ;
      tnLeftpos, ;
      AT( tcRight, ;
      SUBSTR( tcExpression, tnLeftpos), ;
      IIF( EMPTY( tnFollowing), 1, tnFollowing))-1)
  ENDIF
ENDIF
RETURN lcReturnVal

************************************************************
*  FUNCTION WithinC()
************************************************************
FUNCTION withinC( tcExpression, tcLeft, tcRight, tnFirstOne, tnFollowing)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:06:58
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION WithinC()
*)                   : Returns string contained within two
*)                     others.  Case INsensitive
*  Calling Samples...: withinC( <expC>, <expC>, <expC> [,<expN> [,<expN>]])
*  Parameter List....: tcExpression
*                      tcLeft
*                      tcRight
*                      tnFirstOne
*                      tnFollowing
*  Major change list.:
*  ERs...............:
*
PRIVATE lcRetVal, lnlftpos
lcRetVal = ''
lnlftpos = ATC(tcLeft, tcExpression, IIF(EMPTY( tnFirstOne), 1, tnFirstOne))
IF lnlftpos > 0
  lnlftpos = lnlftpos + LEN( tcLeft)
  IF lnlftpos < LEN( tcExpression)
    lcRetVal = SUBSTR( tcExpression, lnlftpos, ATC( tcRight, SUBSTR( tcExpression, lnlftpos), IIF(EMPTY(tnFollowing), 1, tnFollowing))-1)
  ENDIF
ENDIF
RETURN lcRetVal
*

*****************************************************
DEFINE CLASS URL AS Lightweight
*****************************************************
*!* Test code for this class
*!* clear
*!* x=CREATEOBJECT("url","http://qatest.emedicine.com/gps/userhome.gps?x=1&y=2&z=3")
*!* ?x.getURL()
*!* x.AddElement("test","1")
*!* ?x.getURL()
*!* x.AddElement("y","4")
*!* ?x.getURL()
*!* ?"nelements=", x.nElements
*!* ?"First element=", x.getValue(1)
*!* ?"y=", x.getvalue("y")
*!* ? "===================="
*!* LOCAL lni
*!* FOR lni= 1 TO x.nElements
*!*   ?x.getAttribute(lni), "=", x.getValue(lni)
*!* ENDFOR
*!* ? "===================="
*!* x.Coalesce("http://zzz.org?x=Override&z=55&new=great")
*!* ?x.getURL()
*!* ? "===================="
*!* x.RemoveElement(1)
*!* x.RemoveElement("z")
*!* ?x.getURL()
* [2002.09.11 dragan] - initial dimension now 1,2
* - GetUrl() now doesn't exit on first blank, and tests for blank values, not names.
* It now skips the blank values and traverses all the elements.
* - RemoveElement doesn't remove it, just blanks the value. Reason for all this was
* situation we had when all elements are removed.
DIMENSION aElements[1, 2]
cBase = ""
cElements = ""
cElementDelimiter = CHR(38)
cBaseDelimiter = "?"
nElements = 0

*=====================================
FUNCTION INIT(tcURL)
*=====================================
aElements = ""
this.LoadURL( tcURL)
RETURN

*=====================================
FUNCTION LoadURL( tcURL)
*=====================================
IF !EMPTY( tcURL)
  LOCAL lcBaseDelimiter
  lcBaseDelimiter = this.cBaseDelimiter
  this.cBase    = GETWORDNUM(tcURL, 1, lcBaseDelimiter)
  this.cElements = GETWORDNUM(tcURL, 2, lcBaseDelimiter)
ENDIF
RETURN

*=====================================
FUNCTION GetURL( )
*=====================================
LOCAL lcRetVal, lni
lcRetVal = this.cBase + this.cBaseDelimiter
FOR lni = 1 TO ALEN(this.aElements, 1)
  IF !EMPTY(this.aElements[lni, 2])
    IF lni > 1
      lcRetVal = lcRetVal + this.cElementDelimiter
    ENDIF
    lcRetVal = lcRetVal + TRANSFORM(this.aElements[lni, 1])+ "="+ TRANSFORM(this.aElements[lni, 2])
  ENDIF
ENDFOR
RETURN lcRetVal

*=====================================
FUNCTION AddElement( tcAttrib, tcValue)
*=====================================
LOCAL lnIndex
lnIndex = ASCAN(this.aElements, tcAttrib, 1, ALEN(this.aElements, 1), 1, 15)
IF lnIndex = 0
  lnIndex = IIF( EMPTY( this.aElements[1, 1]), 1, ALEN(this.aElements, 1)+1)
  DIMENSION this.aElements[lnIndex, 2]
ENDIF
this.aElements[lnIndex, 1] = tcAttrib
this.aElements[lnIndex, 2] = tcValue
RETURN

*=====================================
FUNCTION GetAttribute( tnPassed)
*=====================================
LOCAL lcRetVal, lnIndex
lcRetVal = ""
DO CASE
CASE VARTYPE(tnPassed) $ "NI"
  IF tnPassed <= ALEN(this.aElements, 1)
    lcRetVal = this.aElements[tnPassed, 1]
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN lcRetVal

*=====================================
FUNCTION GetValue( tuPassed)
*=====================================
LOCAL lcRetVal, lnIndex
lcRetVal = ""
DO CASE
CASE VARTYPE(tuPassed) $ "NI"
  IF tuPassed<= ALEN(this.aElements, 1)
    lcRetVal = this.aElements[tuPassed, 2]
  ENDIF
CASE VARTYPE(tuPassed) = "C"
  lnIndex = ASCAN(this.aElements, tuPassed, 1, ALEN(this.aElements, 1), 1, 15)
  IF lnIndex > 0
    lcRetVal = this.aElements[lnIndex, 2]
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN lcRetVal


*=====================================
FUNCTION RemoveElement( tuPassed)
*=====================================
LOCAL lcRetVal
lcRetVal = ""
DO CASE
CASE VARTYPE(tuPassed) $ "NI"
  IF tuPassed<= ALEN(this.aElements, 1)
    this.aElements[tuPassed, 2]= ""
  ENDIF
CASE VARTYPE(tuPassed) = "C"
  lnIndex = ASCAN(this.aElements, tuPassed, 1, ALEN(this.aElements, 1), 1, 15)
  IF lnIndex > 1
    this.aElements[lnIndex, 2]= ""
  ENDIF
OTHERWISE
  * Bogus
ENDCASE
RETURN


*=====================================
FUNCTION Coalesce( tcURL)
*=====================================
LOCAL loURL, lni
loURL = CREATEOBJECT(this.Class, tcURL)
FOR lni = 1 TO loURL.nElements
  this.AddElement(loURL.GetAttribute(lni), loURL.GetValue(lni))
ENDFOR
loURL = .NULL.
RETURN

*=====================================
FUNCTION cElements_Assign(tcElements)
*=====================================
LOCAL lcBaseDelimiter, lcElementDelimiter
lcBaseDelimiter   = this.cBaseDelimiter
lcElementDelimiter = this.cElementDelimiter

IF "?" $ tcElements
  this.cElements = LOWER(GETWORDNUM(tcElements, 2, lcBaseDelimiter))
ELSE
  this.cElements = LOWER(tcElements)
ENDIF
LOCAL lni, lc2PartElement
FOR lni = 1 TO GETWORDCOUNT(tcElements, lcElementDelimiter))
  lc2PartElement = GETWORDNUM(tcElements, lni, lcElementDelimiter)
  this.AddElement( GETWORDNUM(lc2PartElement, 1, "="), GETWORDNUM(lc2PartElement, 2, "=") )
ENDFOR
RETURN

*=====================================
FUNCTION cbase_Assign( tcbase)
*=====================================
IF "?" $ tcBase
  this.cBase = LOWER(GETWORDNUM(tcBase, 1, "?"))
ELSE
  this.cBase = LOWER(tcBase)
ENDIF
RETURN


*=====================================
FUNCTION nElements_Access
*=====================================
RETURN IIF( EMPTY( this.aElements[1, 1]), 0, ALEN(this.aElements, 1))

ENDDEFINE

*************************************************************
* StringDateTime(t)
*************************************************************
FUNCTION StringDateTime( tdDateTime)
* assumes you've passed in a Date or DateTime type
RETURN CDOW(tdDateTime)+ " " + MDY(tdDateTime)+ " "+ SUBSTR(TTOC(tdDatetime), 10)

*************************************************************
* IsEmail(c)
*************************************************************
* email address validation -
* is there one and only one @ symbol
* is there a dot (.) somewhere post @ symbol
* is there atleast 2 chars following dot

FUNCTION IsEmail( tcEmailAddress )

  * validate parameter
  IF VARTYPE(tcEmailAddress) != 'C' OR EMPTY(tcEmailAddress)
    RETURN .F.
  ENDIF

  * ok you gave me a string, but does it look like a valid address?
  LOCAL lnCheckAtSymbol, lnCheckLength, lnCheckDot, lnCheckme, lcEmail
  lnCheckme = 0
  lnCheckAtSymbol = 0
  lcEmail = ALLTR(tcEmailAddress)

  * make sure only one AT symbol
  lnCheckMe = AT('@', lcEmail, 2)

  IF NOT lnCheckMe = 0
    RETURN .F.
  ENDIF

  * make sure @ symbol seemingly in right place
  lnCheckLength = LEN(ALLTR(lcEmail))

  lnCheckAtSymbol = AT('@', lcEmail)

  IF lnCheckAtSymbol > 1 AND ;
    lnCheckAtSymbol <= (lnCheckLength - 4) && min chars after at symbol is 4; i.e. tom@x.uk

    * we be cool, keep going
  ELSE
    RETURN .F.
  ENDIF

  * do we have a dot after @ symbol followed by atleast 2 chars (somebody@group.uk)
  * RoX fix 6.6.01 - use RAT here instead of AT cuz sometimes and address has periods
  *    before @ symbol
  lnCheckDot = RAT('.', lcEmail)

  IF lnCheckDot > lnCheckAtSymbol AND ;
    lnCheckDot <= (lnCheckLength - 2)

    * we be cool, keep going
  ELSE
    RETURN .F.
  ENDIF

  * made it this far, looks good
  RETURN .T.

ENDFUNC

*************************************************************
* MakeLink(c)
*************************************************************
* Link determination -
* @ symbol means email
* no @ and a period means regular hyperlink
* else we just got a string.

FUNCTION MakeLink( tcPassed )

  * validate parameter
  IF VARTYPE( tcPassed ) != 'C' OR EMPTY( tcPassed )
    RETURN tcPassed
  ENDIF

  LOCAL lcRetStr

  DO CASE
    CASE "@" $ tcPassed
      RETURN [<a href="mailto:] + ALLTR( tcPassed ) + [">] + ALLTR( tcPassed ) + [</a>]

    CASE "." $ tcPassed
      RETURN [<a href="http://] + ALLTR( tcPassed ) + [">] + ALLTR( tcPassed ) + [</a>]

    OTHERWISE
      RETURN tcPassed
  ENDCASE

ENDFUNC


*=====================================
DEFINE CLASS Decorator AS CUSTOM
* The universal decorator.
*=====================================
oDecorated = .NULL.

FUNCTION INIT( toDecorated)
this.oDecorated = toDecorated
RETURN

FUNCTION THIS_Access( tcMember)
IF PEMSTATUS(THIS, UPPER(tcMember), 5)
  RETURN THIS
ELSE
  RETURN this.oDecorated
ENDIF
RETURN

FUNCTION DESTROY()
  this.oDecorated = .null.
RETURN

ENDDEFINE


*************************************************************
DEFINE CLASS cParameter AS Custom
* The universal decorator.
*************************************************************
FUNCTION This_Access(tcMember)
IF TYPE('this.' + tcMember) = 'U'
  this.ADDPROPERTY(tcMember)
ENDIF
RETURN THIS

ENDDEFINE


********************************
DEFINE CLASS TempFile AS Relation
********************************
cFilename = ""
cStem = ""
cExt = "dbf"

FUNCTION INIT( tcPassed)
  LOCAL lcTemp
  IF EMPTY( tcPassed)
    tcPassed = ""
  ENDIF

  DO CASE
  CASE "." $ tcPassed
    this.cStem = JUSTSTEM( tcPassed)
    this.cExt = JUSTEXT( tcPassed)
    this.cFileName = tcPassed

  CASE ! EMPTY( tcPassed) && Extension
    this.cExt = tcPassed
    this.cStem = SUBSTR( SYS(2015), 3, 10)
    this.cFilename = FORCEEXT( this.cStem, this.cExt)

  OTHERWISE
    this.cStem = SUBSTR( SYS(2015), 3, 10)
    this.cFilename = FORCEEXT( this.cStem, this.cExt)
  ENDCASE


FUNCTION Destroy

  IF USED( this.cStem)
    USE IN ( this.cStem)
  ENDIF
  IF FILE( this.cFileName)
    ERASE ( this.cFileName)
  ENDIF

  LOCAL lcMemo
  lcMemo = FORCEEXT( this.cFileName, "FPT")
  IF FILE( lcMemo)
    ERASE ( lcMemo)
  ENDIF

ENDDEFINE


*****************************************
FUNCTION STOD( tcString )
*****************************************
* String To Date - RoX 5.5.2001
* inverse of DTOS() for converting backend date values
* stored as char in form of yyyymmdd.

* validate we got soemthing to work with
IF VARTYP( tcString ) != "C" OR EMPTY( tcString ) OR LEN( tcString ) != 8
  RETURN tcString
ENDIF

* ok disect it and gimme a string in mmddyyyy format that I can work with
LOCAL lcNewStr, ldRetVal

lcNewStr = SUBSTR( tcString, 5, 2) + SUBSTR( tcString, 7, 2) + SUBSTR( tcString, 1, 4)

* make it look like a real date
lcNewStr = STUFF( lcNewStr, 3, 0, "/" )
lcNewStr = STUFF( lcNewStr, 6, 0, "/" )

* and convert it
ldRetVal = CTOD( lcNewStr )

RETURN ldRetVal

ENDFUNC


************************************************************
*  FUNCTION ToLeft()
************************************************************
FUNCTION ToLeft(tcsearch, tcexp, tnocc)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:37:10
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION ToLeft()
*)                   : Returns characters from a character expression
*)                   : to the left of a specified string.\
*  Remarks...........: If <findC> is found in <expC>, the character string
*)                   : to the left of <findC> is returned.  Otherwise the
*)                   : null string is returned.  The search performed by
*)                   : TOLEFT() is case-sensitive.  To perform a search
*)                   : that isn't case-sensitive, use TOLEFTC().
*  Calling Samples...: TOLEFT(<findC>, <expC> [, <occurN>])
*  Parameter List....: <findC>, <expC> ToLeft() searches <expC> for <findC>.
*)                   : <occurN> Specify which occurrence of <findC> in <expC>
*)                   :          to search for (searches for the <occurN>th
*)                   :          occurrence). Default value: 1
RETURN LEFT(tcexp, AT(tcsearch, tcexp, IIF(EMPTY(tnocc), 1, tnocc))-1)


*
************************************************************
*  FUNCTION ToLeftC()
************************************************************
FUNCTION ToLeftC(tcsearch, tcexp, tnocc)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:37:40
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION ToLeftC()
*)                   : Returns characters from a character expression
*)                   : to the left of a specified string. CASE UNSENSITIVE.
*  Remarks...........: Remarks: If <findC> is found in <expC>, the character
*)                   : string to the left of <findC> is returned.  Otherwise the
*)                   : null string is returned.  The search is not case-sensitive.
*)                   : To perform a case-sensitive search, use TOLEFT().
*  Calling Samples...: TOLEFT(<findC>, <expC> [, <occurN>])
*  Parameter List....: <findC>, <expC> ToLeft() searches <expC> for <findC>.
*)                   : <occurN> Specify which occurrence of <findC> in <expC>
*)                   :          to search for (searches for the <occurN>th
*)                   :          occurrence). Default value: 1

*  Major change list.:
*  ERs...............:
RETURN LEFT(tcexp, ATC(tcsearch, tcexp, IIF(EMPTY(tnocc), 1, tnocc))-1)

*
************************************************************
*  FUNCTION ToRight()
************************************************************
FUNCTION ToRight(tcsearch, tcexp, tnocc)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:38:05
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION ToRight()
*)                   : Returns characters from a character expression to the right
*)                   : of a specified string.
*) Remarks...........: If <findC> is found in <expC>, the character string to the right
*)                   : of <findC> is returned.  Otherwise the null string is returned.
*)                   : The search performed by TORIGHT() is case-sensitive.  To perform a search that
*)                   : isn't case-sensitive, use TORIGHTC().
*  Calling Samples...: TORIGHT(<findC>, <expC> [, <occurN>])
*  Parameter List....: <findC>, <expC> TORIGHT() searches <expC> for <findC>.
*)                   : <occurN> Specify which occurrence of <findC> in <expC> to
*)                   :          search for (searches for the <occurN>th occurrence). Default value: 1

*  Major change list.:
*  ERs...............:
LOCAL lnsplitpos
lnsplitpos = AT(tcsearch, tcexp, IIF(EMPTY(tnocc), 1, tnocc))
RETURN IIF(lnsplitpos == 0, '', RIGHT(tcexp, LEN(tcexp)-lnsplitpos-LEN(tcsearch)+1))
*


************************************************************
*  FUNCTION ToRightC()
************************************************************
FUNCTION ToRightC(tcsearch, tcexp, tnocc)
*  Author............: Steven M. Black
*  Project...........: SMS
*  Created...........: 27/08/96  11:38:14
*  Copyright.........: (c) Steven Black Consulting / UP!, 1996
*) Description.......: FUNCTION ToRightC()
*)                   : Returns characters from a character expression to the
*)                   : right of a specified string.  Searches without regard for case.
*) Remarks...........: If <findC> is found in <expC>, the character string to the right
*)                   : of <findC> is returned.  Otherwise the null string is returned.  The search
*)                   : is not case-sensitive.  To perform a case-sensitive search, use TORIGHT().
*  Calling Samples...: TORIGHTC(<findC>, <expC> [, <occurN>])
*  Parameter List....:
*  Major change list.:
*  ERs...............:
LOCAL lnsplitpos
lnsplitpos = ATC(tcsearch, tcexp, IIF(EMPTY(tnocc), 1, tnocc))
RETURN IIF(lnsplitpos = 0, '', RIGHT(tcexp, LEN(tcexp)-lnsplitpos-LEN(tcsearch)+1))

************************************************************
*  FUNCTION CDATA(C)
************************************************************
FUNCTION CDATA(tcPassed)
RETURN "<![CDATA["+ ALLTRIM( tcPassed)+ "]]>"



*=====================================================
DEFINE CLASS LightWeight AS RELATION OLEPUBLIC
*=====================================================
  * Our base lightweight class.

  * These intrinsic properties are semantics of Relation
  * classes, which we are not interested in.
  PROTECTED CHILDALIAS
  PROTECTED CHILDORDER
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
  PROTECTED FUNCTION WRITEMETHOD
  PROTECTED FUNCTION READEXPRESSION
  PROTECTED FUNCTION WRITEEXPRESSION

  FUNCTION RELEASE()
    this.GarbageCollect()
    RELEASE THIS

  FUNCTION GarbageCollect()

ENDDEFINE

*=====================================================
DEFINE CLASS Q AS Lightweight
*=====================================================
Name = "Q"
H = 0
C = "Temp"
Q = ""
XStat = 0
Halt =.f.&& halt on error
E =.F. && auto error-check
LastId = 0
I = .F.  && Instrumentation
InstrumentTable = "Q_Instrument"

nStart = 0
nEnd = 0
nInstrumentTime = 0.025 && Ignore queries below this time thresshold
Cache = .F.
CacheDie = 3600 && 1 hour
CacheTable = "Q_Cache"

DSN = "ConNLRollup"
User = "configure.me"
PW  = "configure.me"
DSN = "configure.me"
User = "configure.me"
PW  = ""

********************************
FUNCTION LastId_Access
********************************
LOCAL lnOldSelect
lnOldSelect = SELECT()
SQLExec(this.H, "SELECT @@IDENTITY AS nLast", "TempLastId")
lnRetVal = nLast
USE IN TempLastId
SELECT (lnOldSelect)
RETURN lnRetVal

********************************
FUNCTION INIT
********************************
this.Connect()
RETURN

********************************
FUNCTION Connect()
********************************
IF this.H > 0
  SQLDISCONNECT(this.H)
ENDIF
this.H = SQLCONNECT(this.DSN, this.User, this.PW)
RETURN this.H

********************************
FUNCTION Destroy
********************************
SQLDISCONNECT(this.H)
RETURN

********************************
FUNCTION X
* execute
********************************
LOCAL lnOldSelect
IF this.Cache
  lnOldSelect = SELECT()
  IF ! FILE(this.Cachetable + ".dbf")
    CREATE TABLE (this.Cachetable) (tTime T, Query M, Results M)
    USE  && releasing the EXCLUSIVE that CREATE TABLE yields
  ENDIF

  LOCAL llOut, llOpenedHere
  llOut = .F.
  llOpenedHere = .F.
  IF ! USED(this.Cachetable)
    SELECT 0
    USE (this.Cachetable)
    llOpenedHere = .T.
  ELSE
    SELECT (this.Cachetable)
  ENDIF
  DELETE FOR tTime < DATETIME()
  LOCATE FOR Query == this.q
  IF ! EOF()
    llOut = .T.
    XMLTOCURSOR(Results, this.C)
  ENDIF
  SELECT (lnOldSelect)
  IF llOUT
    SELECT (this.C)
    IF llOpenedHere
      USE IN (this.Cachetable)
    ENDIF
    RETURN
  ENDIF
ENDIF

this.nStart = SECONDS()
this.XStat = SQLExec(this.H, this.Q, this.C)
this.nEnd = SECONDS()

IF (this.I AND USED(this.C) AND (this.nEnd-this.nStart > this.nInstrumentTime)) OR ;
   (this.I AND this.XStat =-1)

  LOCAL lnOldSelect
  lnOldSelect = SELECT()
  IF ! FILE(this.InstrumentTable + ".dbf")
    CREATE TABLE (this.InstrumentTable) (tTime T, cSQL M, nTime N (7, 3), nRecs I)
    USE
  ENDIF
  INSERT INTO (this.InstrumentTable) (tTime, cSQL, nTime, nRecs) ;
         VALUES (DATETIME(), this.Q, this.nEnd-this.nStart, IIF(USED(this.c), RECCOUNT(this.C), 0))
  USE IN (this.InstrumentTable)
  SELECT (lnOldSelect)
ENDIF
IF this.E
  this.S()
ENDIF

IF this.Cache AND this.xStat > 0
  LOCAL lnOldSelect
  lnOldSelect = SELECT()
  LOCAL lcResults
  CURSORTOXML(this.C, "lcResults", 1, 1, 0, "1")
  INSERT INTO (this.Cachetable) (tTime, Query, Results) ;
         VALUES (DATETIME()+ this.CacheDie, this.Q, lcResults)
  USE IN (this.Cachetable)
  RELEASE lcResults
  SELECT (lnOldSelect)
ENDIF

********************************
FUNCTION S
********************************
IF this.XStat  = -1
  = AERROR(aErrorArray)  && Data from most recent error
  ? 'The error provided the following information'  && Display message
  FOR n = 1 TO 7  && Display all elements of the array
    ? aErrorArray(n)
  ENDFOR
  IF this.Halt
     SET STEP ON
  ENDIF
ENDIF

********************************
FUNCTION ClearCache
********************************
IF FILE(this.Cachetable + ".dbf")
  LOCAL llOpenedHere
  llOpenedHere = .F.
  IF ! USED(this.Cachetable)
     TRY
       USE (this.Cachetable) IN 0 EXCLUSIVE
       llOpenedHere = .T.
       ZAP IN (this.Cachetable)
     CATCH
       USE (this.Cachetable) IN 0
       llOpenedHere = .T.
       DELETE ALL IN (this.Cachetable)
     ENDTRY
  ELSE
    DELETE ALL IN (this.Cachetable)
  ENDIF
  IF llOpenedHere
    USE IN (this.Cachetable)
  ENDIF
ENDIF
RETURN
ENDDEFINE


