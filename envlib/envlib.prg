* Program: EnvLib.prg
* Classes: Many definitions, see below.
*   Bases: All abstract classes are based on Custom.
*  Notice: The author releases all rights to the public domain
*        : subject to the Warranty Disclaimer below.
*  Author: Tom Rettig
*        : Rettig Micro Corporation
*        : 2532 Lincoln Boulevard, Suite 110
*        : Marina del Rey, CA  90291-5978
*        :  Telephone: 310-301-0911
*        :        Fax: 310-821-1162
*        : CompuServe: 75066,352
*        :           : in FOXUSER forum's 3rd-Party section or MAIL
* Version: ENV Version 1.0 July 15, 1995 (#defined in True.h)
*  Action: Save, set, and restore SET, ON, open table, system varaible,
*        :    and object property environments.
*   Usage: See Env.doc for examples.
*Requires: Visual FoxPro for Windows version 3.0 or later
*        : True.h named constant file (#included below)
*   Notes: - May be freely used, modified, and distributed in
*        : compiled and/or source code form.
*        : - The author appreciates acknowledgment in commercial
*        : products and publications that use or learn from this class.
*        : - Technical support is not officially provided.  The
*        : author is very interested in hearing about problems
*        : or enhancement requests you have, and will try to be
*        : helpful within reasonable limits.  Email or fax preferred.
*        : - Warranty Disclaimer: NO WARRANTY!!!
*        : THE AUTHOR RELEASES TO THE PUBLIC DOMAIN ALL CLAIMS TO ANY
*        : RIGHTS IN THIS PROGRAM AND FREELY PROVIDES IT “AS IS” WITHOUT
*        : WARRANTY OF ANY KIND, EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
*        : LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
*        : FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE AUTHOR, OR ANY
*        : OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THIS PROGRAM, BE
*        : LIABLE FOR ANY COMMERCIAL, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
*        : DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM
*        : INCLUDING, BUT NOT LIMITED TO, LOSS OF DATA OR DATA BEING
*        : RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR LOSSES
*        : SUSTAINED BY THIRD PARTIES OR A FAILURE OF THE PROGRAM TO
*        : OPERATE WITH ANY OTHER PROGRAMS, EVEN IF YOU OR OTHER PARTIES
*        : HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

* Multiple set parameters are defined in True.h.
#INCLUDE True.h


*************************************************************
* SET Parent Classes
*************************************************************

DEFINE CLASS Set AS Custom  && abstract class
   PROTECTED uDefault,;
             uOldSet,;
             uNewSet,;
             lNoReset

   FUNCTION GetOld
      RETURN THIS.uOldSet
   ENDFUNC  && GetOld

   FUNCTION GetNew
      RETURN THIS.uNewSet
   ENDFUNC  && GetNew

   FUNCTION GetDefault
      RETURN THIS.uDefault
   ENDFUNC  && GetDefault

   PROTECTED PROCEDURE Init(tcSet, tuValue)
      THIS.uOldSet = SET(tcSet)
      THIS.uNewSet = NVL(tuValue, THIS.uDefault)
   ENDPROC  && Init
ENDDEFINE  && CLASS Set AS Custom  && abstract class


DEFINE CLASS SetTwo AS Set   && abstract class
   PROTECTED uDefaultTwo,;
             uOldSetTwo,;
             uNewSetTwo,;
             cSet

   FUNCTION GetOldTwo
      RETURN THIS.uOldSetTwo
   ENDFUNC  && GetOldTwo

   FUNCTION GetNewTwo
      RETURN THIS.uNewSetTwo
   ENDFUNC  && GetNewTwo

   FUNCTION GetDefaultTwo
      RETURN THIS.uDefaultTwo
   ENDFUNC  && GetDefaultTwo

   PROTECTED PROCEDURE Init(tcSet, tuValueOne,;
                            tuValueTwo, tnParams)
      DO CASE  && of which to set
         CASE EMPTY(tnParams)
            ERROR cnVF_ERR_PARAM_INVALID
            RETURN .F.  && early exit
         CASE tnParams == 1
            THIS.cSet = ccSET_ONE
         CASE EMPTY(tuValueOne)  && never a valid value
            THIS.cSet = ccSET_TWO
         OTHERWISE
            THIS.cSet = ccSET_BOTH
      ENDCASE  && of which to set

      * Primary value as returned by SET("whatever").
      IF INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
         =DODEFAULT(tcSet, tuValueOne)
      ENDIF

      * Secondary value as returned by SET("whatever", 1).
      IF INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
         THIS.uOldSetTwo = SET(tcSet, 1)
         THIS.uNewSetTwo = NVL(tuValueTwo, THIS.uDefaultTwo)
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SetTwo AS Set   && abstract class


DEFINE CLASS SetOnOff AS Set   && abstract class
   PROTECTED PROCEDURE Init(tcSet, tcValue)
      DO CASE
         CASE ISNULL(tcValue)
            =DODEFAULT(tcSet, tcValue)
         CASE NOT INLIST(UPPER(ALLTRIM(tcValue)), "ON", "OFF")
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            =DODEFAULT(tcSet, UPPER(ALLTRIM(tcValue)))
      ENDCASE
   ENDPROC  && Init
ENDDEFINE  && CLASS SetOnOff AS Set  && abstract class


DEFINE CLASS SetOnOffTwo AS SetTwo   && abstract class
   PROTECTED PROCEDURE Init(tcSet,;
                            tcValueOne,;
                            tuValueTwo,;
                            tnParams)
      DO CASE
         CASE ISNULL(tcValueOne)
            =DODEFAULT(tcSet,;
                          tcValueOne,;
                          tuValueTwo,;
                          tnParams)
         CASE NOT INLIST(UPPER(ALLTRIM(tcValueOne)), "ON", "OFF")
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            =DODEFAULT(tcSet,;
                          UPPER(ALLTRIM(tcValueOne)),;
                          tuValueTwo,;
                          tnParams)
      ENDCASE
   ENDPROC  && Init
ENDDEFINE  && CLASS SetOnOffTwo AS SetTwo  && abstract class


*************************************************************
* SET Classes
*************************************************************

DEFINE CLASS SetAlternate AS SetOnOffTwo
   uDefault    = "OFF"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init(tcOnOff, tcTo, tcOption, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("ALTERNATE",;
                                    tcOnOff, tcTo,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET ALTERNATE ON
         OTHERWISE
            SET ALTERNATE OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            * Do nothing.
         CASE EMPTY(THIS.uNewSetTwo)
            SET ALTERNATE TO
         CASE (NOT EMPTY(tcOption)) AND;
              UPPER(ALLTRIM(tcOption)) == "ADDITIVE"
            SET ALTERNATE TO (THIS.uNewSetTwo) ADDITIVE
            IF THIS.uNewSet == "ON"
               SET ALTERNATE ON
            ENDIF
         OTHERWISE
            SET ALTERNATE TO (THIS.uNewSetTwo)
            IF THIS.uNewSet == "ON"
               SET ALTERNATE ON
            ENDIF
      ENDCASE  && of secondary set
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET ALTERNATE ON
            OTHERWISE
               SET ALTERNATE OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE EMPTY(THIS.uOldSetTwo)
               SET ALTERNATE TO
            OTHERWISE
               SET ALTERNATE TO (THIS.uOldSetTwo)
         ENDCASE  && of secondary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetAlternate AS SetOnOffTwo


DEFINE CLASS SetAnsi AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("ANSI", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET ANSI ON
         OTHERWISE
            SET ANSI OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET ANSI ON
         OTHERWISE
            SET ANSI OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetAnsi AS SetOnOff


DEFINE CLASS SetAutosave AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("AUTOSAVE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET AUTOSAVE ON
         OTHERWISE
            SET AUTOSAVE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET AUTOSAVE ON
         OTHERWISE
            SET AUTOSAVE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetAutosave AS SetOnOff


DEFINE CLASS SetBell AS SetOnOff
   * Limit - no way to get SET BELL TO <freq|.wav>, <sec>
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("BELL", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET BELL ON
         OTHERWISE
            SET BELL OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET BELL ON
         OTHERWISE
            SET BELL OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetBell AS SetOnOff


DEFINE CLASS SetBlocksize AS Set
   uDefault = 64

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("BLOCKSIZE", tnValue)
         SET BLOCKSIZE TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET BLOCKSIZE TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetBlocksize AS Set


DEFINE CLASS SetBrstatus AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("BRSTATUS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET BRSTATUS ON
         OTHERWISE
            SET BRSTATUS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET BRSTATUS ON
         OTHERWISE
            SET BRSTATUS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetBrstatus AS SetOnOff


DEFINE CLASS SetCarry AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CARRY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CARRY ON
         OTHERWISE
            SET CARRY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CARRY ON
         OTHERWISE
            SET CARRY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCarry AS SetOnOff


DEFINE CLASS SetCentury AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CENTURY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CENTURY ON
         OTHERWISE
            SET CENTURY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CENTURY ON
         OTHERWISE
            SET CENTURY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCentury AS SetOnOff


DEFINE CLASS SetClassLib AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tcOption, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("CLASSLIB", tcValue)
         LOCAL lcTemp
         lcTemp = THIS.uNewSet
         IF (NOT EMPTY(tcOption)) AND;
            (UPPER(ALLTRIM(tcOption))="ADDITIVE")
            SET CLASSLIB TO &lcTemp ADDITIVE  && macro alert
         ELSE
            SET CLASSLIB TO &lcTemp  && macro alert
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         LOCAL lcTemp
         lcTemp = THIS.uOldSet
         SET CLASSLIB TO &lcTemp  && macro alert
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetClassLib AS Set


DEFINE CLASS SetClear AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CLEAR", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CLEAR ON
         OTHERWISE
            SET CLEAR OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CLEAR ON
         OTHERWISE
            SET CLEAR OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetClear AS SetOnOff


DEFINE CLASS SetClock AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CLOCK", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CLOCK ON
         CASE THIS.uNewSet == "STATUS"
            SET CLOCK STATUS
         OTHERWISE
            SET CLOCK OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CLOCK ON
         CASE THIS.uOldSet == "STATUS"
            SET CLOCK STATUS
         OTHERWISE
            SET CLOCK OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetClock AS SetOnOff


DEFINE CLASS SetCollate AS Set
   uDefault = "MACHINE"

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("COLLATE", tnValue)
         SET COLLATE TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET COLLATE TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCollate AS Set


DEFINE CLASS SetColor AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("COLOR", tcValue)
         SET COLOR TO (THIS.uNewSet)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET COLOR TO (THIS.uOldSet)
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetColor AS Set


DEFINE CLASS SetCompatible AS SetOnOffTwo
   uDefault    = "OFF"
   uDefaultTwo = "PROMPT"

   PROTECTED PROCEDURE Init(tcOnOff, tcPrompt, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      LOCAL lcOnOff, lcPrompt
      lcOnOff  = IIF(ISNULL(tcOnOff), tcOnOff, UPPER(ALLTRIM(tcOnOff)))
      lcPrompt = IIF(ISNULL(tcPrompt), tcPrompt, UPPER(ALLTRIM(tcPrompt)))
      DO CASE
         CASE PARAMETERS() > 1 AND EMPTY(tcOnOff)
            lcOnOff = SET("COMPATIBLE")
         CASE lcOnOff == "FOXPLUS"
            lcOnOff = "OFF"
         CASE lcOnOff == "DB4"
            lcOnOff = "ON"
      ENDCASE

      DO CASE  && of primary set
         CASE NOT DODEFAULT("COMPATIBLE",;
                                    lcOnOff, lcPrompt,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT THIS.cSet == ccSET_ONE
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET COMPATIBLE ON
         OTHERWISE
            SET COMPATIBLE OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            * Do nothing.
         CASE THIS.uNewSetTwo == "PROMPT"
            IF THIS.uNewSet == "ON"
               SET COMPATIBLE ON PROMPT
            ELSE
               SET COMPATIBLE OFF PROMPT
            ENDIF
         CASE THIS.uNewSetTwo == "NOPROMPT"
            IF THIS.uNewSet == "ON"
               SET COMPATIBLE ON NOPROMPT
            ELSE
               SET COMPATIBLE OFF NOPROMPT
            ENDIF
         OTHERWISE
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
      ENDCASE  && of secondary set
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT THIS.cSet == ccSET_ONE
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET COMPATIBLE ON
            OTHERWISE
               SET COMPATIBLE OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSetTwo == "NOPROMPT"
               IF THIS.uOldSet == "ON"
                  SET COMPATIBLE ON NOPROMPT
               ELSE
                  SET COMPATIBLE OFF NOPROMPT
               ENDIF
            OTHERWISE
               IF THIS.uOldSet == "ON"
                  SET COMPATIBLE ON PROMPT
               ELSE
                  SET COMPATIBLE OFF PROMPT
               ENDIF
         ENDCASE  && of secondary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCompatible AS SetOnOffTwo


DEFINE CLASS SetConfirm AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CONFIRM", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CONFIRM ON
         OTHERWISE
            SET CONFIRM OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CONFIRM ON
         OTHERWISE
            SET CONFIRM OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetConfirm AS SetOnOff


DEFINE CLASS SetConsole AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CONSOLE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CONSOLE ON
         OTHERWISE
            SET CONSOLE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CONSOLE ON
         OTHERWISE
            SET CONSOLE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetConsole AS SetOnOff


DEFINE CLASS SetCpcompile AS Set
   uDefault = CPCURRENT()

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("CPCOMPILE", tnValue)
         SET CPCOMPILE TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET CPCOMPILE TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCpcompile AS Set


DEFINE CLASS SetCpdialog AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CPDIALOG", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CPDIALOG ON
         OTHERWISE
            SET CPDIALOG OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CPDIALOG ON
         OTHERWISE
            SET CPDIALOG OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCpdialog AS SetOnOff


DEFINE CLASS SetCurrency AS SetTwo
   uDefault    = "LEFT"
   uDefaultTwo = "$"

   PROTECTED PROCEDURE Init(tcLeftRight, tcTo, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("CURRENCY",;
                               IIF(ISNULL(tcLeftRight),;
                                   tcLeftRight,;
                                   UPPER(ALLTRIM(tcLeftRight))),;
                               tcTo,;
                               PARAMETERS())
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE NOT INLIST(THIS.uNewSet, "LEFT", "RIGHT")
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "LEFT"
            SET CURRENCY LEFT
         OTHERWISE
            SET CURRENCY RIGHT
      ENDCASE  && of primary set

      * Secondary set.
      IF INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
         SET CURRENCY TO (THIS.uNewSetTwo)
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "LEFT"
               SET CURRENCY LEFT
            OTHERWISE
               SET CURRENCY RIGHT
         ENDCASE  && of primary set

         * Secondary set.
         IF INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            SET CURRENCY TO (THIS.uOldSetTwo)
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCurrency AS SetTwo


DEFINE CLASS SetCursor AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("CURSOR", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET CURSOR ON
         OTHERWISE
            SET CURSOR OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET CURSOR ON
         OTHERWISE
            SET CURSOR OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetCursor AS SetOnOff


DEFINE CLASS SetDatabase AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("DATABASE", tcValue)
            RETURN .F.  && early exit
         CASE EMPTY(THIS.uNewSet)
            SET DATABASE TO
         OTHERWISE
            SET DATABASE TO (THIS.uNewSet)
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE EMPTY(THIS.uOldSet)
            SET DATABASE TO
         OTHERWISE
            SET DATABASE TO (THIS.uOldSet)
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDatabase AS Set


DEFINE CLASS SetDataSession AS Set
   uDefault = 1

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("DATASESSION", tnValue)
         IF NOT EMPTY(tnValue)
            SET DATASESSION TO THIS.uNewSet
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET DATASESSION TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDataSession AS Set


DEFINE CLASS SetDate AS Set
   uDefault = "AMERICAN"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("DATE", tcValue)
         SET DATE TO (THIS.uNewSet)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET DATE TO (THIS.uOldSet)
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDate AS Set


DEFINE CLASS SetDebug AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("DEBUG", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET DEBUG ON
         OTHERWISE
            SET DEBUG OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET DEBUG ON
         OTHERWISE
            SET DEBUG OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDebug AS SetOnOff


DEFINE CLASS SetDecimals AS Set
   uDefault = 2

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("DECIMALS", tnValue)
         SET DECIMALS TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET DECIMALS TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDecimals AS Set


DEFINE CLASS SetDefault AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("DEFAULT", tcValue)
         SET DEFAULT TO (THIS.uNewSet)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET DEFAULT TO (THIS.uOldSet)
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDefault AS Set


DEFINE CLASS SetDeleted AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("DELETED", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET DELETED ON
         OTHERWISE
            SET DELETED OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET DELETED ON
         OTHERWISE
            SET DELETED OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDeleted AS SetOnOff


DEFINE CLASS SetDelimiters AS SetOnOffTwo
   uDefault    = "OFF"
   uDefaultTwo = ":"

   PROTECTED PROCEDURE Init(tcOnOff, tcDelimiter, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      LOCAL lcOnOff, lcDelimiter
      lcOnOff  = IIF(ISNULL(tcOnOff), tcOnOff, UPPER(ALLTRIM(tcOnOff)))
      lcDelimiter = NVL(tcDelimiter, "")
      IF PARAMETERS() > 1 AND EMPTY(tcOnOff)
         lcOnOff = SET("DELIMITERS")
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("DELIMITERS",;
                                    lcOnOff, lcDelimiter,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT THIS.cSet == ccSET_ONE
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET DELIMITERS ON
         OTHERWISE
            SET DELIMITERS OFF
      ENDCASE  && of primary set

      IF NOT EMPTY(lcDelimiter)
         SET DELIMITERS TO lcDelimiter
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset

         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET DELIMITERS ON
            OTHERWISE
               SET DELIMITERS OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE NOT EMPTY(THIS.uOldSetTwo)
               SET DELIMITERS TO (THIS.uOldSetTwo)
            OTHERWISE
               IF THIS.uOldSet == "ON"
                  SET DELIMITERS ON
               ELSE
                  SET DELIMITERS OFF
               ENDIF
         ENDCASE  && of secondary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDelimiters AS SetOnOffTwo


DEFINE CLASS SetDevelopment AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("DEVELOPMENT", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET DEVELOPMENT ON
         OTHERWISE
            SET DEVELOPMENT OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET DEVELOPMENT ON
         OTHERWISE
            SET DEVELOPMENT OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDevelopment AS SetOnOff


DEFINE CLASS SetDisplay AS Set
   uDefault = "VGA25"

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("DISPLAY", tnValue)
         lcTemp = THIS.uNewSet
         SET DISPLAY TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         lcTemp = THIS.uOldSet
         SET DISPLAY TO &lcTemp
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDisplay AS Set


DEFINE CLASS SetDohistory AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("DOHISTORY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET DOHISTORY ON
         OTHERWISE
            SET DOHISTORY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET DOHISTORY ON
         OTHERWISE
            SET DOHISTORY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetDohistory AS SetOnOff


DEFINE CLASS SetEcho AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("ECHO", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET ECHO ON
         OTHERWISE
            * Must RELEASE WINDOW TRACE to set ECHO OFF
            RELEASE WINDOW TRACE
            SET ECHO OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET ECHO ON
         OTHERWISE
            * Must RELEASE WINDOW TRACE to set ECHO OFF
            RELEASE WINDOW TRACE
            SET ECHO OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetEcho AS SetOnOff


DEFINE CLASS SetEscape AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("ESCAPE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET ESCAPE ON
         OTHERWISE
            SET ESCAPE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET ESCAPE ON
         OTHERWISE
            SET ESCAPE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetEscape AS SetOnOff


DEFINE CLASS SetExact AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("EXACT", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET EXACT ON
         OTHERWISE
            SET EXACT OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET EXACT ON
         OTHERWISE
            SET EXACT OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetExact AS SetOnOff


DEFINE CLASS SetExclusive AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("EXCLUSIVE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET EXCLUSIVE ON
         OTHERWISE
            SET EXCLUSIVE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET EXCLUSIVE ON
         OTHERWISE
            SET EXCLUSIVE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetExclusive AS SetOnOff


DEFINE CLASS SetFdow AS Set
   uDefault = 1

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("FDOW", tnValue)
         SET FDOW TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET FDOW TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetFdow AS Set


DEFINE CLASS SetFixed AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("FIXED", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET FIXED ON
         OTHERWISE
            SET FIXED OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET FIXED ON
         OTHERWISE
            SET FIXED OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetFixed AS SetOnOff


DEFINE CLASS SetFullPath AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("FULLPATH", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET FULLPATH ON
         OTHERWISE
            SET FULLPATH OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET FULLPATH ON
         OTHERWISE
            SET FULLPATH OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetFullPath AS SetOnOff


DEFINE CLASS SetFweek AS Set
   uDefault = 1

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("FWEEK", tnValue)
         SET FWEEK TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET FWEEK TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetFweek AS Set


DEFINE CLASS SetHeadings AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("HEADINGS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET HEADINGS ON
         OTHERWISE
            SET HEADINGS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET HEADINGS ON
         OTHERWISE
            SET HEADINGS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetHeadings AS SetOnOff


DEFINE CLASS SetHelp AS SetOnOffTwo
   uDefault    = "ON"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init(tcOnOff, tcTo, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("HELP",;
                                    tcOnOff, tcTo,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET HELP ON
         OTHERWISE
            SET HELP OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            * Do nothing.
         CASE EMPTY(THIS.uNewSetTwo)
            SET HELP TO
         OTHERWISE
            SET HELP TO (THIS.uNewSetTwo)
      ENDCASE  && of secondary set
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET HELP ON
            OTHERWISE
               SET HELP OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE EMPTY(THIS.uOldSetTwo)
               SET HELP TO
            OTHERWISE
               SET HELP TO (THIS.uOldSetTwo)
         ENDCASE  && of secondary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetHelp AS SetOnOffTwo


DEFINE CLASS SetHelpfilter AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("HELPFILTER", tcValue)
         LOCAL lcTemp
         lcTemp = THIS.uNewSet
         SET HELPFILTER TO &lcTemp  && macro alert
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         LOCAL lcTemp
         lcTemp = THIS.uOldSet
         SET HELPFILTER TO &lcTemp  && macro alert
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetHelpfilter AS Set


DEFINE CLASS SetHours AS Set
   uDefault = 12

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("HOURS", tnValue)
            * No op?
         CASE ISNULL(THIS.uNewSet) OR EMPTY(THIS.uNewSet)
            SET HOURS TO  && will default to 12
         * SET HOURS ignores decimals, i.e. 12.5 is legal
         CASE NOT TYPE("THIS.uNewSet")="N" OR ;
            NOT INLIST(INT(THIS.uNewSet), 12, 24)
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            SET HOURS TO THIS.uNewSet
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         * SET("hours") can only return 12 or 24 - never EMPTY()
         SET HOURS TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetHours AS Set


DEFINE CLASS SetIntensity AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("INTENSITY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET INTENSITY ON
         OTHERWISE
            SET INTENSITY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET INTENSITY ON
         OTHERWISE
            SET INTENSITY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetIntensity AS SetOnOff


DEFINE CLASS SetKeycomp AS Set
   * Cannot initialize uDefault in the class body because DO CASE
   * logic is invalid here.  Done at start of Init instead.

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      DO CASE
         CASE _WINDOWS
            THIS.uDefault = "WINDOWS"
         CASE _MAC
            THIS.uDefault = "MAC"
         CASE _DOS
            THIS.uDefault = "DOS"
         OTHERWISE  && should never happen
            ERROR "Unknown operating system"
      ENDCASE
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("KEYCOMP", tcValue)
            RETURN .F.
         CASE NOT INLIST(UPPER(THIS.uNewSet), "DOS", "WIND",;
               "WINDO", "WINDOW", "WINDOWS")
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE "DOS" $ THIS.uNewSet
            SET KEYCOMP TO DOS
         OTHERWISE
            SET KEYCOMP TO WINDOWS
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         IF "DOS" $ THIS.uOldSet
            SET KEYCOMP TO DOS
         ELSE
            SET KEYCOMP TO WINDOWS
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetKeycomp AS Set


DEFINE CLASS SetLibrary AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tcOption, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("LIBRARY", tcValue)
         lcTemp = THIS.uNewSet
         IF (NOT EMPTY(tcOption)) AND;
            (UPPER(ALLTRIM(tcOption))="ADDITIVE")
            SET LIBRARY TO &lcTemp ADDITIVE
         ELSE
            SET LIBRARY TO &lcTemp
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         lcTemp = THIS.uOldSet
         SET LIBRARY TO &lcTemp
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetLibrary AS Set


DEFINE CLASS SetLock AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("LOCK", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET LOCK ON
         OTHERWISE
            SET LOCK OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET LOCK ON
         OTHERWISE
            SET LOCK OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetLock AS SetOnOff


DEFINE CLASS SetLogErrors AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("LOGERRORS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET LOGERRORS ON
         OTHERWISE
            SET LOGERRORS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET LOGERRORS ON
         OTHERWISE
            SET LOGERRORS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetLogErrors AS SetOnOff


DEFINE CLASS SetMargin AS Set
   uDefault = 0

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      * VFP sets a maximum of 256 when given a higher number.
      IF DODEFAULT("MARGIN", MIN(256, NVL(tnValue, THIS.uDefault)))
         SET MARGIN TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET MARGIN TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMargin AS Set


DEFINE CLASS SetMackey AS Set
   uDefault = "SHIFT+F10"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("MACKEY", tcValue)
         lcTemp = THIS.uNewSet
         SET MACKEY TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         lcTemp = THIS.uOldSet
         SET MACKEY TO &lcTemp
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMackey AS Set


DEFINE CLASS SetMark AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("MARK", tcValue)
         SET MARK TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET MARK TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMark AS Set


DEFINE CLASS SetMemoWidth AS Set
   uDefault = 50

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      * VFP sets a maximum of 256 when given a higher number.
      IF DODEFAULT("MEMOWIDTH", MIN(256, NVL(tnValue, THIS.uDefault)))
         SET MEMOWIDTH TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET MEMOWIDTH TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMemoWidth AS Set


DEFINE CLASS SetMessage AS SetTwo
   uDefaultTwo = ""  && using #2 for SET(... ,1) to save

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("MESSAGE", , tcValue, 2)
         IF EMPTY(THIS.uNewSetTwo)
            SET MESSAGE TO
         ELSE
            SET MESSAGE TO THIS.uNewSetTwo
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         IF EMPTY(THIS.uOldSetTwo)
            SET MESSAGE TO
         ELSE
            SET MESSAGE TO THIS.uOldSetTwo
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMessage AS SetTwo


DEFINE CLASS SetMultiLocks AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("MULTILOCKS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET MULTILOCKS ON
         OTHERWISE
            SET MULTILOCKS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET MULTILOCKS ON
         OTHERWISE
            SET MULTILOCKS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetMultiLocks AS SetOnOff


DEFINE CLASS SetNear AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("NEAR", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET NEAR ON
         OTHERWISE
            SET NEAR OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET NEAR ON
         OTHERWISE
            SET NEAR OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetNear AS SetOnOff


DEFINE CLASS SetNotify AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("NOTIFY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET NOTIFY ON
         OTHERWISE
            SET NOTIFY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET NOTIFY ON
         OTHERWISE
            SET NOTIFY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetNotify AS SetOnOff


DEFINE CLASS SetNull AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("NULL", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET NULL ON
         OTHERWISE
            SET NULL OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET NULL ON
         OTHERWISE
            SET NULL OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetNull AS SetOnOff


DEFINE CLASS SetOdometer AS Set
   uDefault = 100

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("ODOMETER", tnValue)
         SET ODOMETER TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET ODOMETER TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetOdometer AS Set


DEFINE CLASS SetOLEObject AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("OLEOBJECT", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET OLEOBJECT ON
         OTHERWISE
            SET OLEOBJECT OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET OLEOBJECT ON
         OTHERWISE
            SET OLEOBJECT OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetOLEObject AS SetOnOff


DEFINE CLASS SetOptimize AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("OPTIMIZE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET OPTIMIZE ON
         OTHERWISE
            SET OPTIMIZE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET OPTIMIZE ON
         OTHERWISE
            SET OPTIMIZE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetOptimize AS SetOnOff


DEFINE CLASS SetPalette AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("PALETTE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET PALETTE ON
         OTHERWISE
            SET PALETTE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET PALETTE ON
         OTHERWISE
            SET PALETTE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetPalette AS SetOnOff


DEFINE CLASS SetPath AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("PATH", tcValue)
         SET PATH TO (THIS.uNewSet)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET PATH TO (THIS.uOldSet)
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetPath AS Set


DEFINE CLASS SetPrinter AS SetOnOffTwo
   * Limit: No way to get SET PRINTER FONT|STYLE settings
   * or COM port settings.
   uDefault    = "OFF"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init(tcOnOff, tcTo, tcOption, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("PRINTER",;
                                    tcOnOff, tcTo,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET PRINTER ON
         OTHERWISE
            SET PRINTER OFF
      ENDCASE  && of primary set

      DO CASE  && of secondary set
         CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            * Do nothing.
         CASE EMPTY(THIS.uNewSetTwo) OR THIS.uOldSetTwo == "PRN"
            SET PRINTER TO
         CASE (NOT EMPTY(tcOption)) AND;
              UPPER(ALLTRIM(tcOption)) == "ADDITIVE"
            SET PRINTER TO &tcTo ADDITIVE  && macro alert
         OTHERWISE  && macros used to enable setting COM port specs
            SET PRINTER TO &tcTo  && macro alert
      ENDCASE  && of secondary set
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET PRINTER ON
            OTHERWISE
               SET PRINTER OFF
         ENDCASE  && of primary set

         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE EMPTY(THIS.uOldSetTwo) OR THIS.uOldSetTwo == "PRN"
               SET PRINTER TO
            OTHERWISE  && macro won't help here
               SET PRINTER TO (THIS.uOldSetTwo)
         ENDCASE  && of secondary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetPrinter AS SetOnOffTwo


DEFINE CLASS SetPoint AS Set
   uDefault = "."

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("POINT", tcValue)
         lcTemp = THIS.uNewSet
         SET POINT TO &lcTemp
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         lcTemp = THIS.uOldSet
         SET POINT TO &lcTemp
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetPoint AS Set


DEFINE CLASS SetProcedure AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tcOption, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("PROCEDURE", tcValue)
         IF EMPTY(THIS.uNewSet)
            SET PROCEDURE TO
         ELSE
            LOCAL lcTemp
            lcTemp = THIS.uNewSet
            IF (NOT EMPTY(tcOption)) AND;
               (UPPER(ALLTRIM(tcOption))="ADDITIVE")
               SET PROCEDURE TO &lcTemp ADDITIVE  && macro alert
            ELSE
               SET PROCEDURE TO &lcTemp  && macro alert
            ENDIF
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         LOCAL lcTemp
         lcTemp = THIS.uOldSet
         SET PROCEDURE TO &lcTemp  && macro alert
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetProcedure AS Set


DEFINE CLASS SetReadBorder AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("READBORDER", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET READBORDER ON
         OTHERWISE
            SET READBORDER OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET READBORDER ON
         OTHERWISE
            SET READBORDER OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetReadBorder AS SetOnOff


DEFINE CLASS SetRefresh AS SetTwo
   uDefault    = 0
   uDefaultTwo = 5

   PROTECTED PROCEDURE Init(tnEditSeconds, tnBufferSeconds, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE
         CASE NOT DODEFAULT("REFRESH", tnEditSeconds,;
                               tnBufferSeconds, PARAMETERS())
            * Do nothing.
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE ISNULL(tnEditSeconds) AND ISNULL(tnBufferSeconds)
            * Do nothing.
         CASE tnEditSeconds < 0 OR tnBufferSeconds < 0
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE tnEditSeconds >= 0 AND tnBufferSeconds >= 0
            * Set both
            SET REFRESH TO tnEditSeconds, tnBufferSeconds
         CASE tnEditSeconds >= 0
            * Set first only
            SET REFRESH TO tnEditSeconds
         CASE tnBufferSeconds >= 0
            * Must set both to set the second
            lnTemp = SET("REFRESH")
            SET REFRESH TO lnTemp, tnBufferSeconds
         OTHERWISE
            ERROR "CASE...OTHERWISE: Unexpected."
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE
            CASE THIS.uOldSet >= 0 AND THIS.uOldSetTwo >= 0
               * Set both
               SET REFRESH TO THIS.uOldSet, THIS.uOldSetTwo
            CASE THIS.uOldSet >= 0
               * Set first only
               SET REFRESH TO THIS.uOldSet
            OTHERWISE
               ERROR "CASE...OTHERWISE: Unexpected."
         ENDCASE
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetRefresh AS SetTwo


DEFINE CLASS SetReprocess AS Set
* If the old set is to <n> SECONDS, it will be reset as just <n>
* because DISPLAY STATUS is the only way in VFP to detect when set
* to SECONDS.

   uDefault = 0

   PROTECTED cType

   PROTECTED PROCEDURE Init(tuValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      THIS.cType = TYPE("tuValue")
      DO CASE
         CASE ISNULL(tuValue)
            =DODEFAULT("REPROCESS", tuValue)
         CASE (NOT THIS.cType $ "CN") OR;
              (NOT DODEFAULT("REPROCESS",;
                             IIF(THIS.cType=="C",;
                                 UPPER(ALLTRIM(tuValue)),;
                                 tuValue)))
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         CASE THIS.cType == "C"
            DO CASE
               CASE THIS.uNewSet == "AUTOMATIC"
                  SET REPROCESS TO AUTOMATIC
               CASE RIGHT(THIS.uNewSet,7) == "SECONDS"
                  SET REPROCESS TO VAL(THIS.uNewSet) SECONDS
               OTHERWISE
                  ERROR cnVF_ERR_SETARGINVALID
                  RETURN .F.  && early exit
            ENDCASE
         OTHERWISE
            SET REPROCESS TO THIS.uNewSet
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET REPROCESS TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetReprocess AS Set


DEFINE CLASS SetResource AS SetOnOffTwo
   uDefault    = "ON"
   uDefaultTwo = ""

   PROTECTED PROCEDURE Init(tcOnOff, tcTo, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      * SET RESOURCE TO is first because it also sets it ON.
      DO CASE  && of secondary set
         CASE NOT DODEFAULT("RESOURCE",;
                                    tcOnOff, tcTo,;
                                    PARAMETERS())
            RETURN .F.  && early exit
         CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            * Do nothing.
         CASE EMPTY(THIS.uNewSetTwo)
            SET RESOURCE TO
         OTHERWISE
            SET RESOURCE TO (THIS.uNewSetTwo)
      ENDCASE  && of secondary set

      DO CASE  && of primary set
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         CASE THIS.uNewSet == "ON"
            SET RESOURCE ON
         OTHERWISE
            SET RESOURCE OFF
      ENDCASE  && of primary set
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         * SET RESOURCE TO is first because it also sets it ON.
         DO CASE  && of secondary set
            CASE NOT INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
               * Do nothing.
            CASE EMPTY(THIS.uOldSetTwo)
               SET RESOURCE TO
            OTHERWISE
               SET RESOURCE TO (THIS.uOldSetTwo)
         ENDCASE  && of secondary set

         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            CASE THIS.uOldSet == "ON"
               SET RESOURCE ON
            OTHERWISE
               SET RESOURCE OFF
         ENDCASE  && of primary set
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetResource AS SetOnOffTwo


DEFINE CLASS SetResourceCreate AS SetResource
   PROTECTED PROCEDURE Init(tcOnOff, tcTo, tlNoReset)
      LOCAL lcTo
      lcTo = IIF(EMPTY(tcTo),;
                 HOME() + "FoxUser.dbf",;
                 TRIM(tcTo) + IIF("." $ tcTo, "", ".dbf"))
      IF NOT (FILE(lcTo) OR THIS.CreateResource(lcTo))
         RETURN .F.
      ELSE
         RETURN DODEFAULT(tcOnOff, tcTo, tlNoReset)
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF (NOT THIS.lNoReset) AND INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
         LOCAL lcTo
         lcTo = IIF(EMPTY(THIS.uOldSetTwo),;
                    HOME() + "FoxUser.dbf",;
                    TRIM(THIS.uOldSetTwo) + IIF("." $ THIS.uOldSetTwo,;
                                                "", ".dbf"))
         IF NOT (FILE(lcTo) OR THIS.CreateResource(lcTo))
            RETURN
         ELSE
            DODEFAULT()
         ENDIF
      ENDIF
   ENDPROC  && Destroy

   PROTECTED FUNCTION CreateResource(tcTable)
      LOCAL llReturn,;
            loSaveSelect
      loSaveSelect = CREATEOBJECT("SaveSelect")
      CREATE TABLE (tcTable) FREE;
         (Type     C(12),;
          Id       C(12),;
          Name     C(24),;
          ReadOnly L,;
          CkVal    N(6),;
          Data     M,;
          Updated  D)
      llReturn = UPPER(tcTable) $ FULLPATH(DBF())
      USE
      RETURN llReturn
   ENDFUNC   && CreateResource
ENDDEFINE  && CLASS SetResourceCreate AS SetResource


DEFINE CLASS SetSafety AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("SAFETY", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET SAFETY ON
         OTHERWISE
            SET SAFETY OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET SAFETY ON
         OTHERWISE
            SET SAFETY OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSafety AS SetOnOff


DEFINE CLASS SetSeconds AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("SECONDS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET SECONDS ON
         OTHERWISE
            SET SECONDS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET SECONDS ON
         OTHERWISE
            SET SECONDS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSeconds AS SetOnOff


DEFINE CLASS SetSeparator AS Set
   uDefault = ","   && "" will not work!

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("SEPARATOR", tcValue)
         SET SEPARATOR TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET SEPARATOR TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSeparator AS Set


DEFINE CLASS SetSpace AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("SPACE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET SPACE ON
         OTHERWISE
            SET SPACE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET SPACE ON
         OTHERWISE
            SET SPACE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSpace AS SetOnOff


DEFINE CLASS SetStatus AS SetOnOff
   * Limit:  no way to get SET STATUS TIMEOUT TO <n> value
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("STATUS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET STATUS ON
         OTHERWISE
            SET STATUS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET STATUS ON
         OTHERWISE
            SET STATUS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetStatus AS SetOnOff


DEFINE CLASS SetStatusBar AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("STATUS BAR", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET STATUS BAR ON
         OTHERWISE
            SET STATUS BAR OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET STATUS BAR ON
         OTHERWISE
            SET STATUS BAR OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetStatusBar AS SetOnOff


DEFINE CLASS SetStep AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("STEP", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET STEP ON
         OTHERWISE
            SET STEP OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET STEP ON
         OTHERWISE
            SET STEP OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetStep AS SetOnOff


DEFINE CLASS SetSysFormats AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("SYSFORMATS", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET SYSFORMATS ON
         OTHERWISE
            SET SYSFORMATS OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET SYSFORMATS ON
         OTHERWISE
            SET SYSFORMATS OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSysFormats AS SetOnOff


DEFINE CLASS SetSysMenu AS Set
* Handles only ON, OFF, and AUTOMATIC.  Does not handle SET TO.

   uDefault = "AUTOMATIC"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE ISNULL(tcValue)
            =DODEFAULT("SYSMENU", tcValue)
         CASE NOT INLIST(UPPER(ALLTRIM(tcValue)),;
                         "ON", "OFF", "AUTOMATIC")
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
         OTHERWISE
            LOCAL lcValue
            lcValue = UPPER(ALLTRIM(tcValue))
            =DODEFAULT("SYSMENU", lcValue)
            DO CASE
               CASE lcValue == "AUTOMATIC"
                  SET SYSMENU AUTOMATIC
               CASE lcValue == "ON"
                  SET SYSMENU ON
               CASE lcValue == "OFF"
                  SET SYSMENU OFF
            ENDCASE
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "AUTOMATIC"
            SET SYSMENU AUTOMATIC
         CASE THIS.uOldSet == "ON"
            SET SYSMENU ON
         CASE THIS.uOldSet == "OFF"
            SET SYSMENU OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSysMenu AS Set


DEFINE CLASS SetTalk AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("TALK", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET TALK ON
         OTHERWISE
            SET TALK OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET TALK ON
         OTHERWISE
            SET TALK OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetTalk AS SetOnOff


DEFINE CLASS SetTopic AS SetTwo
   uDefault    = ""
   uDefaultTwo = 0

   PROTECTED PROCEDURE Init(tcTopic, tcID, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF

      DO CASE  && of primary set
         CASE NOT DODEFAULT("TOPIC",;
                               tcTopic,;
                               tcID,;
                               PARAMETERS())
         CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
            * Do nothing.
         OTHERWISE
            LOCAL lcTopic
            lcTopic = THIS.uNewSet
            SET TOPIC TO &lcTopic  && macro alert
      ENDCASE  && of primary set

      * Secondary set.
      IF INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
         SET TOPIC ID TO THIS.uNewSetTwo
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         DO CASE  && of primary set
            CASE NOT INLIST(THIS.cSet, ccSET_ONE, ccSET_BOTH)
               * Do nothing.
            OTHERWISE
               LOCAL lcTopic
               lcTopic = THIS.uOldSet
               SET TOPIC TO &lcTopic  && macro alert
         ENDCASE  && of primary set

         * Secondary set.
         IF INLIST(THIS.cSet, ccSET_TWO, ccSET_BOTH)
            SET TOPIC ID TO THIS.uOldSetTwo
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetTopic AS SetTwo


DEFINE CLASS SetTrBetween AS SetOnOff
   uDefault = "ON"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("TRBETWEEN", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET TRBETWEEN ON
         OTHERWISE
            SET TRBETWEEN OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET TRBETWEEN ON
         OTHERWISE
            SET TRBETWEEN OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetTrBetween AS SetOnOff


DEFINE CLASS SetTypeahead AS Set
   uDefault = 20

   PROTECTED PROCEDURE Init(tnValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("TYPEAHEAD", tnValue)
         SET TYPEAHEAD TO THIS.uNewSet
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         SET TYPEAHEAD TO THIS.uOldSet
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetTypeahead AS Set


DEFINE CLASS SetUdfParms AS Set
   uDefault = "VALUE"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("UDFPARMS",;
                            IIF(ISNULL(tcValue),;
                                tcValue, UPPER(ALLTRIM(tcValue))))
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "VALUE"
            SET UDFPARMS VALUE
         CASE THIS.uNewSet == "REFERENCE"
            SET UDFPARMS REFERENCE
         OTHERWISE
            ERROR cnVF_ERR_SETARGINVALID
            RETURN .F.  && early exit
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "VALUE"
            SET UDFPARMS VALUE
         OTHERWISE
            SET UDFPARMS REFERENCE
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetUdfParms AS Set


DEFINE CLASS SetUnique AS SetOnOff
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("UNIQUE", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET UNIQUE ON
         OTHERWISE
            SET UNIQUE OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET UNIQUE ON
         OTHERWISE
            SET UNIQUE OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetUnique AS SetOnOff


DEFINE CLASS SetView AS SetOnOff
* Does not handle SET VIEW TO.
   uDefault = "OFF"

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      DO CASE
         CASE NOT DODEFAULT("VIEW", tcValue)
            RETURN .F.  && early exit
         CASE THIS.uNewSet == "ON"
            SET VIEW ON
         OTHERWISE
            SET VIEW OFF
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE THIS.uOldSet == "ON"
            SET VIEW ON
         OTHERWISE
            SET VIEW OFF
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetView AS SetOnOff


DEFINE CLASS SetWindowOfMemo AS Set
   uDefault = ""

   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF DODEFAULT("WINDOW", tcValue)
         lcTemp = THIS.uNewSet
         SET WINDOW OF MEMO TO &lcTemp  && macro alert
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF NOT THIS.lNoReset
         lcTemp = THIS.uOldSet
         SET WINDOW OF MEMO TO &lcTemp  && macro alert
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetWindowOfMemo AS Set


*************************************************************
* SET Default Classes
*************************************************************

DEFINE CLASS SetVfpDefaults AS Custom
* Visual FoxPro Defaults.
   PROTECTED PROCEDURE Init(tlNoReset)
      =THIS.AddObject("SetAlternate", "SetAlternate", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetAnsi", "SetAnsi", .NULL., tlNoReset)
      =THIS.AddObject("SetAutosave", "SetAutosave", .NULL., tlNoReset)
      =THIS.AddObject("SetBell", "SetBell", .NULL., tlNoReset)
      =THIS.AddObject("SetBlocksize", "SetBlocksize", .NULL., tlNoReset)
      =THIS.AddObject("SetBrstatus", "SetBrstatus", .NULL., tlNoReset)
      =THIS.AddObject("SetCarry", "SetCarry", .NULL., tlNoReset)
      =THIS.AddObject("SetCentury", "SetCentury", .NULL., tlNoReset)
      =THIS.AddObject("SetClear", "SetClear", .NULL., tlNoReset)
      =THIS.AddObject("SetClock", "SetClock", .NULL., tlNoReset)
      =THIS.AddObject("SetCollate", "SetCollate", .NULL., tlNoReset)
      =THIS.AddObject("SetColor", "SetColor", .NULL., tlNoReset)
      =THIS.AddObject("SetCompatible", "SetCompatible", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetConfirm", "SetConfirm", .NULL., tlNoReset)
      =THIS.AddObject("SetConsole", "SetConsole", .NULL., tlNoReset)
      =THIS.AddObject("SetCpcompile", "SetCpcompile", .NULL., tlNoReset)
      =THIS.AddObject("SetCpdialog", "SetCpdialog", .NULL., tlNoReset)
      =THIS.AddObject("SetCurrency", "SetCurrency", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetCursor", "SetCursor", .NULL., tlNoReset)
      =THIS.AddObject("SetDatabase", "SetDatabase", .NULL., tlNoReset)
      =THIS.AddObject("SetDataSession", "SetDataSession", .NULL., tlNoReset)
      =THIS.AddObject("SetDate", "SetDate", .NULL., tlNoReset)
      =THIS.AddObject("SetDebug", "SetDebug", .NULL., tlNoReset)
      =THIS.AddObject("SetDecimals", "SetDecimals", .NULL., tlNoReset)
      =THIS.AddObject("SetDefault", "SetDefault", .NULL., tlNoReset)
      =THIS.AddObject("SetDeleted", "SetDeleted", .NULL., tlNoReset)
      =THIS.AddObject("SetDelimiters", "SetDelimiters", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetDevelopment", "SetDevelopment", .NULL., tlNoReset)
      =THIS.AddObject("SetDisplay", "SetDisplay", .NULL., tlNoReset)
      =THIS.AddObject("SetDohistory", "SetDohistory", .NULL., tlNoReset)
      =THIS.AddObject("SetEcho", "SetEcho", .NULL., tlNoReset)
      =THIS.AddObject("SetEscape", "SetEscape", .NULL., tlNoReset)
      =THIS.AddObject("SetExact", "SetExact", .NULL., tlNoReset)
      =THIS.AddObject("SetExclusive", "SetExclusive", .NULL., tlNoReset)
      =THIS.AddObject("SetFdow", "SetFdow", .NULL., tlNoReset)
      =THIS.AddObject("SetFixed", "SetFixed", .NULL., tlNoReset)
      =THIS.AddObject("SetFullPath", "SetFullPath", .NULL., tlNoReset)
      =THIS.AddObject("SetFweek", "SetFweek", .NULL., tlNoReset)
      =THIS.AddObject("SetHeadings", "SetHeadings", .NULL., tlNoReset)
      =THIS.AddObject("SetHelp", "SetHelp", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetHelpFilter", "SetHelpFilter", .NULL., tlNoReset)
      =THIS.AddObject("SetHours", "SetHours", .NULL., tlNoReset)
      =THIS.AddObject("SetIntensity", "SetIntensity", .NULL., tlNoReset)
      =THIS.AddObject("SetKeycomp", "SetKeycomp", .NULL., tlNoReset)
      =THIS.AddObject("SetLibrary", "SetLibrary", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetLock", "SetLock", .NULL., tlNoReset)
      =THIS.AddObject("SetLogErrors", "SetLogErrors", .NULL., tlNoReset)
      =THIS.AddObject("SetMargin", "SetMargin", .NULL., tlNoReset)
      =THIS.AddObject("SetMackey", "SetMackey", .NULL., tlNoReset)
      =THIS.AddObject("SetMark", "SetMark", .NULL., tlNoReset)
      =THIS.AddObject("SetMemoWidth", "SetMemoWidth", .NULL., tlNoReset)
      =THIS.AddObject("SetMultiLocks", "SetMultiLocks", .NULL., tlNoReset)
      =THIS.AddObject("SetNear", "SetNear", .NULL., tlNoReset)
      =THIS.AddObject("SetNotify", "SetNotify", .NULL., tlNoReset)
      =THIS.AddObject("SetNull", "SetNull", .NULL., tlNoReset)
      =THIS.AddObject("SetOdometer", "SetOdometer", .NULL., tlNoReset)
      =THIS.AddObject("SetOLEObject", "SetOLEObject", .NULL., tlNoReset)
      =THIS.AddObject("SetOptimize", "SetOptimize", .NULL., tlNoReset)
      =THIS.AddObject("SetPalette", "SetPalette", .NULL., tlNoReset)
      =THIS.AddObject("SetPath", "SetPath", .NULL., tlNoReset)
      =THIS.AddObject("SetPrinter", "SetPrinter", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetPoint", "SetPoint", .NULL., tlNoReset)
      =THIS.AddObject("SetProcedure", "SetProcedure", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetReadBorder", "SetReadBorder", .NULL., tlNoReset)
      =THIS.AddObject("SetRefresh", "SetRefresh", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetReprocess", "SetReprocess", .NULL., tlNoReset)
      =THIS.AddObject("SetResource", "SetResource", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetSafety", "SetSafety", .NULL., tlNoReset)
      =THIS.AddObject("SetSeconds", "SetSeconds", .NULL., tlNoReset)
      =THIS.AddObject("SetSeparator", "SetSeparator", .NULL., tlNoReset)
      =THIS.AddObject("SetSpace", "SetSpace", .NULL., tlNoReset)
      =THIS.AddObject("SetStatus", "SetStatus", .NULL., tlNoReset)
      =THIS.AddObject("SetStatusBar", "SetStatusBar", .NULL., tlNoReset)
      =THIS.AddObject("SetStep", "SetStep", .NULL., tlNoReset)
      =THIS.AddObject("SetSysFormats", "SetSysFormats", .NULL., tlNoReset)
      =THIS.AddObject("SetSysMenu", "SetSysMenu", .NULL., tlNoReset)
      =THIS.AddObject("SetTalk", "SetTalk", .NULL., tlNoReset)
      =THIS.AddObject("SetTrBetween", "SetTrBetween", .NULL., tlNoReset)
      =THIS.AddObject("SetTopic", "SetTopic", .NULL., .NULL., tlNoReset)
      =THIS.AddObject("SetTypeAhead", "SetTypeAhead", .NULL., tlNoReset)
      =THIS.AddObject("SetUdfParms", "SetUdfParms", .NULL., tlNoReset)
      =THIS.AddObject("SetUnique", "SetUnique", .NULL., tlNoReset)
      =THIS.AddObject("SetView", "SetView", .NULL., tlNoReset)
      =THIS.AddObject("SetWindowOfMemo", "SetWindowOfMemo",;
                      .NULL., tlNoReset)
      * SetClassLib must be last if this is a VCX.  Could be smarter
      * and keep itself in memory or ignore this if we're a VCX.
      =THIS.AddObject("SetClassLib", "SetClassLib", .NULL., .NULL., tlNoReset)
      RETURN .F.
   ENDPROC  && Init
ENDDEFINE  && CLASS SetVfpDefaults AS Custom


*************************************************************
* ON Parent Classes
*************************************************************

DEFINE CLASS On AS Custom  && abstract class
   PROTECTED cOldOn,;
             cNewOn,;
             lNoReset

   FUNCTION GetOld
      RETURN THIS.cOldOn
   ENDFUNC  && GetOld

   FUNCTION GetNew
      RETURN THIS.cNewOn
   ENDFUNC  && GetNew

   PROTECTED PROCEDURE Init(tcOn, tcValue)
      THIS.cOldOn = ON(tcOn)
      THIS.cNewOn = NVL(tcValue, "")
   ENDPROC  && Init
ENDDEFINE  && CLASS On AS Custom  && abstract class


*************************************************************
* ON Classes
*************************************************************

DEFINE CLASS OnError AS On
   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      =DODEFAULT("ERROR", tcValue)
      IF EMPTY(THIS.cNewOn)
         ON ERROR
      ELSE
         LOCAL lcError
         lcError = THIS.cNewOn
         ON ERROR &lcError  && macro alert
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE EMPTY(THIS.cOldOn)
            ON ERROR
         OTHERWISE
            LOCAL lcError
            lcError = THIS.cOldOn
            ON ERROR &lcError  && macro alert
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS OnError AS On


DEFINE CLASS OnKey AS On
   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      =DODEFAULT("KEY", tcValue)
      IF EMPTY(THIS.cNewOn)
         ON KEY
      ELSE
         LOCAL lcKey
         lcKey = THIS.cNewOn
         ON KEY &lcKey  && macro alert
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE EMPTY(THIS.cOldOn)
            ON KEY
         OTHERWISE
            LOCAL lcKey
            lcKey = THIS.cOldOn
            ON KEY &lcKey  && macro alert
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS OnKey AS On


DEFINE CLASS OnKeyLabel AS On
   PROTECTED cLabel

   FUNCTION GetLabel
      RETURN THIS.cLabel
   ENDFUNC  && GetLabel

   PROTECTED PROCEDURE Init(tcLabel, tcValue, tlNoReset)
   * Override parent class.
      THIS.cLabel = tcLabel
      THIS.cOldOn = ON("KEY", tcLabel)
      THIS.cNewOn = NVL(tcValue, "")
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      IF EMPTY(THIS.cNewOn)
         ON KEY LABEL (THIS.cLabel)
      ELSE
         LOCAL lcKey
         lcKey = THIS.cNewOn
         ON KEY LABEL (THIS.cLabel) &lcKey  && macro alert
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE EMPTY(THIS.cOldOn)
            ON KEY LABEL (THIS.cLabel)
         OTHERWISE
            LOCAL lcKey
            lcKey = THIS.cOldOn
            ON KEY LABEL (THIS.cLabel) &lcKey  && macro alert
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS OnKeyLabel AS On


DEFINE CLASS OnShutDown AS On
   PROTECTED PROCEDURE Init(tcValue, tlNoReset)
      IF tlNoReset
         THIS.lNoReset = .T.
      ENDIF
      =DODEFAULT("SHUTDOWN", tcValue)
      IF EMPTY(THIS.cNewOn)
         ON SHUTDOWN
      ELSE
         LOCAL lcShutDown
         lcShutDown = THIS.cNewOn
         ON SHUTDOWN &lcShutDown  && macro alert
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE THIS.lNoReset
            * Do nothing.
         CASE EMPTY(THIS.cOldOn)
            ON SHUTDOWN
         OTHERWISE
            LOCAL lcShutDown
            lcShutDown = THIS.cOldOn
            ON SHUTDOWN &lcShutDown  && macro alert
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS OnShutDown AS On


*************************************************************
* Save/Restore Table Parent Classes
*************************************************************

DEFINE CLASS SaveArea AS Custom  && abstract class
   PROTECTED nSelect

   FUNCTION GetSelect
      RETURN THIS.nSelect
   ENDFUNC  && GetSelect

   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      DO CASE
         CASE EMPTY(tuArea) OR ISNULL(tuArea)
            THIS.nSelect = SELECT(0)
         CASE TYPE("tuArea") == "N"
            THIS.nSelect = MAX(0, tuArea)
         OTHERWISE  && assumes character or error will prevent init
            THIS.nSelect = SELECT(tuArea)
      ENDCASE
      IF EMPTY(THIS.nSelect)
         ERROR cnVF_ERR_TABLE_NUMINVALID
         RETURN .F.
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SaveArea AS Custom  && abstract class


DEFINE CLASS SaveUsedArea AS SaveArea  && abstract class
   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      DO CASE
         CASE NOT DODEFAULT(tuArea)
            RETURN .F.  && early exit
         CASE NOT USED(THIS.nSelect)
            ERROR cnVF_ERR_TABLE_NOTOPEN
            RETURN .F.  && early exit
      ENDCASE
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      RETURN USED(THIS.nSelect)
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveUsedArea AS SaveArea  && abstract class


*************************************************************
* Set/Restore Table Classes
*************************************************************

DEFINE CLASS SaveSelect AS SaveArea
   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      RETURN DODEFAULT(tuArea)
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      SELECT (THIS.nSelect)
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveSelect AS SaveArea


DEFINE CLASS SetSelect AS SaveSelect
   PROTECTED PROCEDURE Init(tuNewArea)  && character or numeric
      IF DODEFAULT()  && current area
         SELECT (tuNewArea)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SetSelect AS SaveSelect


DEFINE CLASS SaveBuffering AS SaveUsedArea
   PROTECTED nBuffering

   FUNCTION GetOld
      RETURN THIS.nBuffering
   ENDFUNC  && GetOld

   PROTECTED PROCEDURE Init(tuArea)
      IF DODEFAULT(tuArea)
         THIS.nBuffering = CURSORGETPROP("Buffering", THIS.nSelect)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         =CURSORSETPROP("Buffering", THIS.nBuffering, THIS.nSelect)
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveBuffering AS SaveUsedArea


DEFINE CLASS SetBuffering AS SaveBuffering
   PROTECTED nDefault
   nDefault = 1

   PROTECTED PROCEDURE GetDefault
      RETURN THIS.nDefault
   ENDPROC  && GetDefault

   PROTECTED PROCEDURE Init(tnBuffering, tuNewArea)
      IF DODEFAULT(tuNewArea)
         =CURSORSETPROP("Buffering",;
                        NVL(tnBuffering, THIS.nDefault),;
                        THIS.nSelect)
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SetBuffering AS SaveBuffering


DEFINE CLASS SaveRecno AS SaveUsedArea
   PROTECTED nRecno

   FUNCTION GetOld
      RETURN THIS.nRecno
   ENDFUNC  && GetOld

   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      IF DODEFAULT(tuArea)
         THIS.nRecno = IIF(EOF(THIS.nSelect),;
                            .NULL., RECNO(THIS.nSelect))
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE NOT DODEFAULT()
            * Do nothing.
         CASE ISNULL(THIS.nRecno)  && EOF()
            =THIS.AddObject("SetSelect", "SetSelect", THIS.nSelect)
            LOCATE FOR .F.  && EOF()
         CASE THIS.nRecno <= RECCOUNT(THIS.nSelect)
            GO THIS.nRecno IN (THIS.nSelect)
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveRecno AS SaveUsedArea


DEFINE CLASS SaveOrder AS SaveUsedArea
* Only handles CDX tags, not individual IDX.
   PROTECTED cOrder, lDescending

   FUNCTION GetOld
      RETURN THIS.cOrder
   ENDFUNC  && GetOld

   FUNCTION GetDescending
      RETURN THIS.lDescending
   ENDFUNC  && GetDescending

   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      IF DODEFAULT(tuArea)
         LOCAL lnSelect
         lnSelect    = THIS.nSelect
         THIS.cOrder = ORDER(lnSelect)
         IF NOT EMPTY(THIS.cOrder)
            THIS.lDescending = DESCENDING(TAGNO(ORDER(lnSelect),;
                                                 CDX(1, lnSelect),;
                                                 lnSelect),;
                                           lnSelect)
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      DO CASE
         CASE NOT DODEFAULT()
            * Do nothing.
         CASE EMPTY(THIS.cOrder)
            SET ORDER TO 0 IN (THIS.nSelect)
         CASE THIS.lDescending
            SET ORDER TO (THIS.cOrder) IN (THIS.nSelect);
                         DESCENDING
         OTHERWISE
            SET ORDER TO (THIS.cOrder) IN (THIS.nSelect);
                         ASCENDING
      ENDCASE
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveOrder AS SaveUsedArea


DEFINE CLASS SetOrder AS SaveOrder
   PROTECTED PROCEDURE Init(tuOrder, tuNewArea, tlDescending)
      DO CASE
         CASE NOT DODEFAULT(tuNewArea)
            RETURN .F.  && early exit
         CASE EMPTY(tuOrder) OR ISNULL(tuOrder)
            SET ORDER TO 0 IN (THIS.nSelect)
         CASE tlDescending
            SET ORDER TO (tuOrder) IN (THIS.nSelect);
                         DESCENDING
         OTHERWISE
            SET ORDER TO (tuOrder) IN (THIS.nSelect)
      ENDCASE
   ENDPROC  && Init
ENDDEFINE  && CLASS SetOrder AS SaveOrder


DEFINE CLASS SaveFilter AS SaveUsedArea
   PROTECTED cFilter

   FUNCTION GetOld
      RETURN THIS.cFilter
   ENDFUNC  && GetOld

   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      IF DODEFAULT(tuArea)
         THIS.cFilter = FILTER(THIS.nSelect)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         =THIS.AddObject("SetSelect", "SetSelect", THIS.nSelect)
         IF EMPTY(THIS.cFilter)
            SET FILTER TO
         ELSE
            LOCAL lcFilter
            lcFilter = THIS.cFilter
            SET FILTER TO &lcFilter  && macro alert
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveFilter AS SaveUsedArea


DEFINE CLASS SetFilter AS SaveFilter
   PROTECTED PROCEDURE Init(tcFilter, tuNewArea, tcAdditive)
   * tcAdditive ::= "AND" | "OR"
      IF DODEFAULT(tuNewArea)
         LOCAL loSelect
         loSelect = CREATEOBJECT("SetSelect", THIS.nSelect)
         DO CASE
            CASE EMPTY(tcFilter) OR ISNULL(tcFilter)
               SET FILTER TO
            CASE EMPTY(tcAdditive)
               SET FILTER TO &tcFilter  && macro alert
            OTHERWISE
               LOCAL lcFilter
               lcFilter = "(" + FILTER() + ") " + tcAdditive+;
                          " (" + tcFilter + ")"
               SET FILTER TO &lcFilter  && macro alert
         ENDCASE
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SetFilter AS SaveFilter


DEFINE CLASS SaveRelation AS SaveUsedArea
* Also handles SET SKIP.
   PROTECTED cRelation,;
             cSkip

   FUNCTION GetOld
      RETURN THIS.cRelation
   ENDFUNC  && GetOld

   PROTECTED PROCEDURE Init(tuArea)  && character or numeric
      IF DODEFAULT(tuArea)
         LOCAL loSelect
         loSelect = CREATEOBJECT("SetSelect", THIS.nSelect)
         THIS.cRelation = SET("RELATION")
         THIS.cSkip     = SET("SKIP")
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      IF DODEFAULT()
         LOCAL loSelect
         loSelect = CREATEOBJECT("SetSelect", THIS.nSelect)
         IF EMPTY(THIS.cRelation)
            SET RELATION TO
         ELSE
            LOCAL lcTemp
            lcTemp = THIS.cRelation
            SET RELATION TO &lcTemp  && macro alert
            IF NOT EMPTY(THIS.cSkip)
               lcTemp = THIS.cSkip
               SET SKIP TO &lcTemp   && macro alert
            ENDIF
         ENDIF
      ENDIF
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveRelation AS SaveUsedArea


DEFINE CLASS SetRelation AS SaveRelation
   PROTECTED PROCEDURE Init(tcRelation, tuNewArea, tcSkip)
      IF DODEFAULT(tuNewArea)
         LOCAL loSelect
         loSelect = CREATEOBJECT("SetSelect", THIS.nSelect)
         IF EMPTY(tcRelation) OR ISNULL(tcRelation)
            SET RELATION TO
         ELSE
            SET RELATION TO &tcRelation  && macro alert
            IF NOT EMPTY(tcSkip)
               SET SKIP TO &tcSkip  && macro alert
            ENDIF
         ENDIF
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SetRelation AS SaveRelation


DEFINE CLASS SaveTable AS SaveUsedArea
   PROTECTED cAlias,;
             cFile,;
             cLock

   PROTECTED PROCEDURE Init(tuArea, tlNoDependencies)
      IF DODEFAULT(tuArea)
         LOCAL loFullPath, loSelect
         loSelect   = CREATEOBJECT("SetSelect", THIS.nSelect)
         loFullPath = CREATEOBJECT("SetFullPath", "ON")
         =THIS.AddObject("SaveBuffering", "SaveBuffering")
         =THIS.AddObject("SaveRecno", "SaveRecno")
         =THIS.AddObject("SetDataSession", "SetDataSession")
         IF NOT tlNoDependencies
            * Order and filter could have references to other tables.
            =THIS.AddObject("SaveOrder", "SaveOrder")
            =THIS.AddObject("SaveFilter", "SaveFilter")
            =THIS.AddObject("SaveRelation", "SaveRelation")
         ENDIF
         THIS.cAlias = ALIAS()
         THIS.cFile  = DBF()
         THIS.cLock  = SYS(cnVF_SYS_LOCKSTATUS)
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
   * Override parent class which checks for an open table.
      LOCAL loSelect
      loSelect = CREATEOBJECT("SetSelect", THIS.nSelect)
      IF NOT ALIAS() == THIS.cAlias
         =THIS.RemoveObject("SetDataSession", "SetDataSession")
         IF USED(THIS.cAlias)  && close if open in another area
            USE IN (THIS.cAlias)
         ENDIF
         IF THIS.cLock == "Exclusive"
            USE (THIS.cFile) ALIAS (THIS.cAlias) AGAIN EXCLUSIVE
         ELSE
            USE (THIS.cFile) ALIAS (THIS.cAlias) AGAIN SHARED
            DO CASE
               CASE THIS.cLock == "File Locked"
                  =FLOCK()
               CASE THIS.cLock == "Record Locked"
                  =THIS.RemoveObject("SaveRecno", "SaveRecno")
                  =RLOCK()
               OTHERWISE  && should never happen
                  ERROR "CASE...OTHERWISE: Unexpected."
            ENDCASE
         ENDIF
      ENDIF  && NOT ALIAS() == THIS.cAlias
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SaveTable AS SaveUsedArea


DEFINE CLASS SaveAllTables AS Custom
   ADD OBJECT PROTECTED SaveSelect AS SaveSelect

   PROTECTED PROCEDURE Init
      LOCAL lnCounter, laUsed[1]
      IF AUSED(laUsed) > 0
         * AUSED sorts from most recently opened to least recently opened.
         * Destruction is reversed; first constructed are last destructed,
         * so save the dependencies before the tables so all tables are
         * open when any potential dependencies are restored.
         FOR lnCounter = 1 TO ALEN(laUsed, 1)
            =THIS.AddObject("SaveRel" + LTRIM(STR(lnCounter)),;
                            "SaveRelation",;
                            laUsed[lnCounter, 2])
            =THIS.AddObject("SaveFil" + LTRIM(STR(lnCounter)),;
                            "SaveFilter",;
                            laUsed[lnCounter, 2])
         ENDFOR
         * Relations are dependent on order.
         FOR lnCounter = 1 TO ALEN(laUsed, 1)
            =THIS.AddObject("SaveOrd" + LTRIM(STR(lnCounter)),;
                            "SaveOrder",;
                            laUsed[lnCounter, 2])
         ENDFOR
         * All dependencies are dependent on tables.
         FOR lnCounter = 1 TO ALEN(laUsed, 1)
            =THIS.AddObject("SaveTab" + LTRIM(STR(lnCounter)),;
                            "SaveTable",;
                            laUsed[lnCounter, 2],;
                            .T.)  && tables will be restored first
         ENDFOR
      ELSE
         RETURN .F.
      ENDIF
   ENDPROC  && Init
ENDDEFINE  && CLASS SaveAllTables AS Custom


*************************************************************
* Set/Restore Property Classes
*************************************************************

DEFINE CLASS SaveProperty AS Custom
* Use the Update method to save any changes.
* Use the Revert method or destroy the object to discard unsaved changes.
* Array properties: currently saves the first element only.

   PROTECTED oObject,;
             aProperties[1,2],;
             cProperty

   PROTECTED PROCEDURE Init(toObject, tcProperty)  && arguments are optional
      * Object default order: 1 = parameter, 2 = PARENT, (3) _SCREEN.
      * Saves all properties unless tcProperty passed.
      DO CASE
         CASE TYPE("toObject") == "O"
            THIS.oObject = toObject
         CASE TYPE("THIS.PARENT") == "O"
            THIS.oObject = THIS.PARENT
         OTHERWISE
            THIS.oObject = _SCREEN
      ENDCASE
      IF NOT EMPTY(tcProperty)
         THIS.cProperty = ALLTRIM(tcProperty)
      ENDIF
      THIS.Update()
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      THIS.Revert()
   ENDPROC  && Destroy

   PROCEDURE Update
      LOCAL lnCounter,;
            laProperties[1]
      IF EMPTY(THIS.cProperty)
         DIMENSION THIS.aProperties[AMEMBERS(laProperties,THIS.oObject), 2]
         FOR lnCounter = 1 TO ALEN(laProperties)
            IF TYPE("THIS.oObject." + laProperties[lnCounter]) $ "OU"
               THIS.aProperties[lnCounter, 1] = .NULL.
            ELSE
               THIS.aProperties[lnCounter, 1] = laProperties[lnCounter]
               THIS.aProperties[lnCounter, 2] =;
                  EVALUATE("THIS.oObject." + laProperties[lnCounter])
            ENDIF
         ENDFOR
      ELSE
         DIMENSION THIS.aProperties[1, 2]
         THIS.aProperties[1, 1] = THIS.cProperty
         THIS.aProperties[1, 2] = EVALUATE("THIS.oObject." + THIS.cProperty)
      ENDIF
   ENDPROC  && Update

   PROCEDURE Revert
      LOCAL lnCounter
      FOR lnCounter = 1 TO ALEN(THIS.aProperties, 1)
         IF (NOT ISNULL(THIS.aProperties[lnCounter, 1])) AND;
            (TYPE("THIS.aProperties[lnCounter, 2]") !=;
                TYPE("THIS.oObject."+THIS.aProperties[lnCounter, 1]) OR;
             THIS.aProperties[lnCounter, 2] !=;
                EVALUATE("THIS.oObject."+;
                         THIS.aProperties[lnCounter, 1]))
            STORE THIS.aProperties[lnCounter, 2];
               TO ("THIS.oObject." + THIS.aProperties[lnCounter, 1])
         ENDIF
      ENDFOR
   ENDPROC  && Revert
ENDDEFINE  && CLASS SaveProperty AS Custom


DEFINE CLASS SetProperty AS SaveProperty
   PROTECTED PROCEDURE Init(toObject, tcProperty, tuValue)
      DODEFAULT(toObject, tcProperty)
      STORE tuValue TO ("THIS.oObject." + THIS.cProperty)
   ENDPROC  && Init
ENDDEFINE  && CLASS SetProperty AS SaveProperty


*************************************************************
* Set/Restore System Variable Classes
*************************************************************

DEFINE CLASS SetSysVar AS Custom
   PROTECTED cSysVar,;
             uValue

   FUNCTION GetOld
      RETURN THIS.uValue
   ENDFUNC  && GetOld

   PROTECTED PROCEDURE Init(tcSysVar, tuValue)
      THIS.cSysVar = tcSysVar
      THIS.uValue  = EVALUATE(tcSysVar)
      STORE tuValue TO (tcSysVar)
   ENDPROC  && Init

   PROTECTED PROCEDURE Destroy
      STORE THIS.uValue TO (THIS.cSysVar)
   ENDPROC  && Destroy
ENDDEFINE  && CLASS SetSysVar AS Custom


*************************************************************
* Timer Classes
*************************************************************

DEFINE CLASS MessageTimer AS Timer
* This class works differently from most in this library because
* it's not meant to be destroyed between message settings.  Instead,
* the timer method resets the message and the class stays available
* for another timed message.  This is similar to what a TIMEOUT clause
* on SET MESSAGE would do.
   PROTECTED Interval,;
             nIntervalDefault,;
             cMessage
   nIntervalDefault = 0
   cMessage = .NULL.

   PROCEDURE SetIntervalDefault(tnSeconds)
      THIS.nIntervalDefault = IIF(EMPTY(tnSeconds),;
                                  0, tnSeconds * 1000)
   ENDPROC

   PROCEDURE SetMessage(tcMessage, tnSeconds)
      IF ISNULL(THIS.cMessage)  && don't get our timed message
         THIS.cMessage = SET("MESSAGE", 1)
      ENDIF
      THIS.Interval = IIF(PARAMETERS() < 2,;
                          THIS.nIntervalDefault,;
                          tnSeconds * 1000)
      THIS.Reset()  && start over in case already in progress
      IF EMPTY(tcMessage)
         SET MESSAGE TO
      ELSE
         SET MESSAGE TO tcMessage
      ENDIF
   ENDPROC

   PROTECTED PROCEDURE Timer  && fires once to clear message
      IF EMPTY(THIS.cMessage)
         SET MESSAGE TO
      ELSE
         SET MESSAGE TO THIS.cMessage
      ENDIF
      THIS.cMessage = .NULL.
      THIS.Interval = 0  && don't fire until new message is set
   ENDPROC

   PROTECTED PROCEDURE Destroy
      IF NOT ISNULL(THIS.cMessage)
         THIS.Timer()
      ENDIF
   ENDPROC
ENDDEFINE  && CLASS MessageTimer AS Timer

*** EnvLib.prg **********************************************

