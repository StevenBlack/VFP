* Program: True.h (Named Constant File)
*  Notice: The author releases all rights to the public domain
*        : subject to the Warranty Disclaimer in the programs
*        : and documentation.
*  Author: Tom Rettig
* Version: TRUE Version 1.0 July 15, 1995 (#defined in True.h)
* Created: December 10, 1994 (Beta 1)
*  Update: January 7, 1995 (Beta 1b)
*  Update: April 7, 1995 (Beta 2b)
*  Update: June 6, 1995 (Release Candidate 2a)
*  Update: July 15, 1995 (First Release)

*************************************************************
* Visual FoxPro Specific
*************************************************************

* Include VFP's constant file.
#INCLUDE "FoxPro.h"

* VFP AERROR() array dimensions.
#DEFINE cnVF_AERR_MAX           7
#DEFINE cnVF_AERR_NUMBER        1
#DEFINE cnVF_AERR_MESSAGE       2
#DEFINE cnVF_AERR_OBJECT        3
#DEFINE cnVF_AERR_WORKAREA      4
#DEFINE cnVF_AERR_TRIGGER       5
#DEFINE cnVF_AERR_EXTRA1        6
#DEFINE cnVF_AERR_EXTRA2        7

* VFP ERRORs used.
#DEFINE cnVF_ERR_FILE_NOTEXIST     1
   * File <insert> does not exist.
#DEFINE cnVF_ERR_PARAM_INVALID    11
   * Function argument value, type, or count is invalid.
#DEFINE cnVF_ERR_TABLE_NUMINVALID 17
   * Table number is invalid.
#DEFINE cnVF_ERR_FIELD_NOTFOUND   47
   * No fields found to process.
#DEFINE cnVF_ERR_TABLE_NOTOPEN    52
   * No table is open.
#DEFINE cnVF_ERR_DISK_SPACE       56
   * Not enough disk space for <insert>.
#DEFINE cnVF_ERR_EXCLUSIVE       110
   * File must be opened exclusively.
#DEFINE cnVF_ERR_ARRAYDIM        230
   * Array dimensions are invalid.
#DEFINE cnVF_ERR_SETARGINVALID   231
   * Invalid argument used with the SET function.
#DEFINE cnVF_ERR_NOTARRAY        232
   * "<insert>" is not an array.
#DEFINE cnVF_ERR_PARAM_TOOFEW   1229
   * Too few arguments.
#DEFINE cnVF_ERR_PARAM_TOOMANY  1230
   * Too many arguments.
#DEFINE cnVF_ERR_DB_NOTOPEN     1520
   * No database is open or set as the current database.
#DEFINE cnVF_ERR_DB_NOTDB       1552
   * File <insert> is not a database.
#DEFINE cnVF_ERR_PROP_INVALID   1560
   * Property value is invalid.
#DEFINE cnVF_ERR_DB_OBJNOTFOUND 1562
   * Cannot find object <insert> in the database.
#DEFINE cnVF_ERR_NAME_ISUSED    1569
   * The name you have chosen is already used for a
   * built in <insert>. Please choose a different name.
#DEFINE cnVF_ERR_OBJ_NAME       1575
   * Object name is invalid.
#DEFINE cnVF_ERR_PROP_DATATYPE  1732
   * Data type is invalid for this property.
#DEFINE cnVF_ERR_PROP_NOTFOUND  1734
   * Property <insert> is not found.               
#DEFINE cnVF_ERR_PROP_READONLY  1743
   * Property <insert> is read-only.
#DEFINE cnVF_ERR_PROP_PROTECTED 1757
   * Property <insert> is protected.
#DEFINE cnVF_ERR_OBJ_TYPE       1773
   * Database object type is invalid.
#DEFINE cnVF_ERR_STR_TOOLONG    1903
   * String is too long to fit.
#DEFINE cnVF_ERR_FILE_NOTCLOSED 1933
   * File '<insert>' is not closed.
#DEFINE cnVF_ERR_FUNC_NOTIMP    1999
   * Function is not implemented.

* VFP limits.
#DEFINE cnVF_FIELD_MAXCOUNT      255
#DEFINE cnVF_FIELD_MAXNAMELEN     10
#DEFINE cnVF_INDEX_MAXKEYLEN     240
#DEFINE cnVF_NUM_MAXPRECISION     16

* VFP SYS() functions used.
#DEFINE cnVF_SYS_EXEDIR         2004
#DEFINE cnVF_SYS_LOCKSTATUS     2011
#DEFINE cnVF_SYS_RELATIVEPATH   2014
#DEFINE cnVF_SYS_UNIQUEID       2015
#DEFINE cnVF_SYS_CROSSPATH      2027

*************************************************************
* TRUE General
*************************************************************
#DEFINE ccCRLF                  CHR(13)+CHR(10)
#DEFINE ccTAB                   CHR( 9)

*************************************************************
* EDC Specific
*************************************************************
#DEFINE ccEDC_VERSION   "EDC Version 1.0 July 15, 1995"

* Control codes.
#DEFINE ccMSG_INSERT1           "<insert1>"
#DEFINE ccMSG_INSERT2           "<insert2>"
#DEFINE ccMSG_INSERT3           "<insert3>"
#DEFINE cnPROP_REMOVE          -1
#DEFINE ccWILDCARD              "*"

* Reserved keywords and names.
* Mostly used in method parameters and messages.
#DEFINE ccALL                   "ALL"
#DEFINE ccCANDIDATE             "CANDIDATE"
#DEFINE ccCOPY                  "COPY"
#DEFINE ccMEMO                  "MEMO"
#DEFINE ccPACK                  "PACK"
#DEFINE ccPRIMARY               "PRIMARY"
#DEFINE ccREFRESH               "REFRESH"
#DEFINE ccREGULAR               "REGULAR"

* EdcLib classes used by DEFINE CLASS.
#DEFINE cxCLASS_EDC             EDC
#DEFINE ccCLASS_EDC            "EDC"      && for PrgToVcx
#DEFINE cxCLASS_MSG             Message
#DEFINE ccCLASS_MSG            "Message"  && for PrgToVcx

* EDC file structures.
#DEFINE cxANALYZE_FIELD         Analyze
#DEFINE cnEDC_FIXEDFIELDS       2         && the rest are extensions
#DEFINE cxEDC_ID                cUniqueID
#DEFINE ccEDC_ID                "cUniqueID"
#DEFINE cxEDC_OBJ               mEdcObject
#DEFINE ccEDC_OBJ               "mEdcObject"

* EDC system objects.
* Unique rows store actual name in cxEDC_ID field instead of SYS(2015).
#DEFINE ccEDC_OBJ_UNIQUETYPE    "EdcUnique "
** Unique type rows are limited to size of EDC_ID field (10),
** must be PROPER() case and defined to length.
#DEFINE ccEDC_OBJ_REGISTRY      "Registry  "

* Wildcard access to property maps.
#DEFINE ccEDC_MAPALL            THIS.cMapName + "*"

* EDC built-in registry properties.
#DEFINE cnEDC_REG_DEFAULT       13  && number of default registry properties
#DEFINE ccEDC_REG_ALTERNATE     "cEdcAlternate"  && alternate EDC file
#DEFINE ccEDC_REG_CREATE        "tEdcCreate"     && creation DateTime
#DEFINE ccEDC_REG_DBC           "cDBC"           && relative path back link
#DEFINE ccEDC_REG_REMOVELOCK    "lRemoveLock"    && prevent field removal
#DEFINE ccEDC_REG_VERSION       "cEdcVersion"
#DEFINE ccEDC_REG_EXTENSIONNAME "cEdcExtensionName"
#DEFINE ccEDC_REG_VENDORNAME    "cEdcVendorName"
#DEFINE ccEDC_REG_METHODOPEN    "cMethodOpen"    && methods are
#DEFINE ccEDC_REG_METHODGET     "cMethodGet"     && named for
#DEFINE ccEDC_REG_METHODSET     "cMethodSet"     && wildcard
#DEFINE ccEDC_REG_METHODLIB     "cMethodLib"     && access
#DEFINE ccEDC_REG_METHODCLASS   "cMethodClass"
#DEFINE ccEDC_REG_METHODINIT    "cMethodInit"    && pass to AddObject()
** Wildcard access to registry methods.
#DEFINE ccEDC_REG_METHODALL     "cMethod*"       && methods and
** Using string for list instead of INLIST() type list
** because these must be checked as case insensitive.
#DEFINE ccEDC_REG_LISTDBF       ccEDC_REG_ALTERNATE+","+ccEDC_REG_DBC
#DEFINE ccEDC_REG_LISTVCX       ccEDC_REG_METHODLIB
#DEFINE ccEDC_REG_LISTFILES     ccEDC_REG_LISTDBF+","+ccEDC_REG_LISTVCX
#DEFINE ccEDC_REG_LISTTYPEC     ccEDC_REG_LISTFILES+","+;
 ccEDC_REG_VERSION+","+ccEDC_REG_METHODGET+","+ccEDC_REG_METHODSET+","+;
 ccEDC_REG_METHODOPEN+","+ccEDC_REG_METHODALL
#DEFINE ccEDC_REG_LISTTYPEL ccEDC_REG_REMOVELOCK
#DEFINE ccEDC_REG_LISTTYPET ccEDC_REG_CREATE
#DEFINE ccEDC_REG_LISTALL   ccEDC_REG_LISTTYPEC+","+;
 ccEDC_REG_LISTTYPEL+","+ccEDC_REG_LISTTYPET
** Method argument substitution strings.  Case insensitive.
#DEFINE ccARG_DBC               "<dbc>"
#DEFINE ccARG_DBCID             "<dbc id>"
#DEFINE ccARG_EDC               "<edc>"
#DEFINE ccARG_OBJTYPE           "<object type>"
#DEFINE ccARG_OBJNAME           "<object name>"
#DEFINE ccARG_PROPNAME          "<property name>"
#DEFINE ccARG_PROPVALUE         "<property value>"
#DEFINE ccARG_REGISTRY          "<registry>"

* Extended property header.
#DEFINE ccHEAD_OFF              "."
#DEFINE ccHEAD_ON               "+"
#DEFINE cnHEAD_SIZE             36
#DEFINE cnHEAD_TYPE              1  && system use only
#DEFINE cnHEAD_NULL              2  && system use only
#DEFINE cnHEAD_READLOCK          3  && user access 3..14
#DEFINE cnHEAD_WRITELOCK         4  && overrides cnHEAD_UPDATE
#DEFINE cnHEAD_REMOVELOCK        5
#DEFINE cnHEAD_UPDATE            6
#DEFINE cnHEAD_RESERVED4         7
#DEFINE cnHEAD_RESERVED3         8
#DEFINE cnHEAD_RESERVED2         9
#DEFINE cnHEAD_RESERVED1        10
#DEFINE cnHEAD_USER4            11
#DEFINE cnHEAD_USER3            12
#DEFINE cnHEAD_USER2            13
#DEFINE cnHEAD_USER1            14
#DEFINE cnHEAD_SWITCHSIZE       14
#DEFINE cnHEAD_TIMESIZE         22  && max size with century and 12-hour
** Bytes 15..36 are used for PADR(TTOC(DATETIME()), cnHEAD_TIMESIZE)

* Alternate EDC update property array dimensions.
* Rows are dynamic.
#DEFINE cnALT_COLUMNS           2
#DEFINE cnALT_FIELD             1
#DEFINE cnALT_ALIAS             2

* Object method update property array dimensions.
* Rows are dynamic.
#DEFINE cnOBJ_COLUMNS           5
#DEFINE cnOBJ_OWNER             1
#DEFINE cnOBJ_OBJECT            2
#DEFINE cnOBJ_GETMETHOD         3
#DEFINE cnOBJ_SETMETHOD         4
#DEFINE cnOBJ_RELEASE           5  && name of class library to release

* Extended aObjectError[] array dimensions.
#DEFINE cnAERR_MAX              cnVF_AERR_MAX + 3
#DEFINE cnAERR_METHOD           cnVF_AERR_MAX + 1
#DEFINE cnAERR_LINE             cnVF_AERR_MAX + 2
#DEFINE cnAERR_SOURCE           cnVF_AERR_MAX + 3

* VFP DBC object types to length of ObjectType field.  Case sensitive.
#DEFINE cnVF_OBJ_TYPESIZE       10
#DEFINE ccVF_OBJ_CONNECTION     "Connection"
#DEFINE ccVF_OBJ_DATABASE       "Database  "
#DEFINE ccVF_OBJ_FIELD          "Field     "
#DEFINE ccVF_OBJ_INDEX          "Index     "
#DEFINE ccVF_OBJ_RELATION       "Relation  "
#DEFINE ccVF_OBJ_TABLE          "Table     "
#DEFINE ccVF_OBJ_VIEW           "View      "
#DEFINE cxVF_OBJ_LISTALL        ccVF_OBJ_CONNECTION,;
                                ccVF_OBJ_DATABASE,;
                                ccVF_OBJ_FIELD,;
                                ccVF_OBJ_INDEX,;
                                ccVF_OBJ_RELATION,;
                                ccVF_OBJ_TABLE,;
                                ccVF_OBJ_VIEW

* Known VFP DBC property types.
#DEFINE ccVF_IND_TAGTYPE        CHR(17)
#DEFINE ccVF_OBJ_SUBTYPE        CHR( 2)  && local table, local view, etc.
#DEFINE ccVF_REL_TAG            CHR(13)
#DEFINE ccVF_REL_FKTABLE        CHR(18)
#DEFINE ccVF_REL_FKTAG          CHR(19)
#DEFINE ccVF_TAB_FILEPATH       CHR( 1)
#DEFINE ccVF_TAB_PRIMARYTAG     CHR(20)
#DEFINE ccVF_VIE_COMMAND        CHR(42)

* Known VFP DBC property values.
#DEFINE ccVF_IND_REGULAR        CHR( 0)
#DEFINE ccVF_IND_CANDIDATE      CHR( 1)
#DEFINE ccVF_OBJ_LOCALTABLE     CHR( 1)
#DEFINE ccVF_OBJ_LOCALVIEW      CHR( 6)

* VFP ADBOBJECTS("RELATION") array columns.
#DEFINE cnVF_REL_MAXCOLS        5
#DEFINE cnVF_REL_PARENTTABLE    1
#DEFINE cnVF_REL_CHILDTABLE     2
#DEFINE cnVF_REL_PARENTTAG      3
#DEFINE cnVF_REL_CHILDTAG       4
#DEFINE cnVF_REL_RIINFO         5


*************************************************************
* ENV Specific
*************************************************************
#DEFINE ccENV_VERSION   "ENV Version 1.0 July 15, 1995"

#DEFINE ccSET_ONE       "1"
#DEFINE ccSET_TWO       "2"
#DEFINE ccSET_BOTH      "3"


*************************************************************
* PTOV Specific
*************************************************************
#DEFINE ccPTOV_VERSION  "PrgToVcx Version 1.0 July 15, 1995"

#DEFINE ccCRLF_DEF              "CHR(13)+CHR(10)"
#DEFINE ccVISUAL_DESCRIPTION    "*>*"
#DEFINE ccVISUAL_DELIMITER      ","
#DEFINE cnCUSTOM_HEIGHT         17    && pixels
#DEFINE cnCUSTOM_WIDTH          100   && pixels

*** TRUE.h **************************************************
