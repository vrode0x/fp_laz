unit vr_utils;

{$mode delphi}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$I vrode.inc}
interface

uses sysutils, Classes, Types, strutils, Math, Pipes, LazUTF8,
  LazFileUtils, LCLVersion,
  FileUtil, LazUTF8Classes,
  LConvEncoding, Masks, vr_types, UTF8Process, process, vr_intfs, vr_SysUtils
  {$IFDEF WINDOWS}, windows, vr_WinAPI{$ELSE}{, LCLIntf}{$ENDIF};

{%Region Types Const Vars}
type
  ICustomData = vr_intfs.ICustomData;
  IVariantObject = vr_intfs.IVariantObject;
  IFPObject = vr_intfs.IFPObject;
  IFPStringBuilder = vr_intfs.IFPStringBuilder;
  IFPTimer = vr_intfs.IFPTimer;
  IFPInterfaceList = vr_intfs.IFPInterfaceList;
  IFPStream = vr_intfs.IFPStream;
  IFPFileStream = vr_intfs.IFPFileStream;
  IFPStringStream = vr_intfs.IFPStringStream;
  IFPStack = vr_intfs.IFPStack;
  IFPList = vr_intfs.IFPList;
  IFPIntegerList = vr_intfs.IFPIntegerList;
  IFPStrings = vr_intfs.IFPStrings;
  IFPStringListUTF8 = vr_intfs.IFPStringListUTF8;
  IFPStringList = vr_intfs.IFPStringList;
  IFPFileNameList = vr_intfs.IFPFileNameList;
  IFPDataStringList = vr_intfs.IFPDataStringList;
  IFPThreadStrings = vr_intfs.IFPThreadStrings;
  IFPThreadStringList = vr_intfs.IFPThreadStringList;
  IFPThreadFileNameList = vr_intfs.IFPThreadFileNameList;
  IFPThreadDataStringList = vr_intfs.IFPThreadDataStringList;
  IFPCustomIniFile = vr_intfs.IFPCustomIniFile;
  IFPIniFileUTF8 = vr_intfs.IFPIniFileUTF8;
  IFPIniFile = vr_intfs.IFPIniFile;
  IFPThreadIniFileUTF8 = vr_intfs.IFPThreadIniFileUTF8;
  IFPThreadIniFile = vr_intfs.IFPThreadIniFile;
  IFPIniCachedFile = vr_intfs.IFPIniCachedFile;
  IWebBrowser = vr_intfs.IWebBrowser;

  TDocEditType = vr_types.TDocEditType;
  TStringFormatHow = vr_types.TStringFormatHow;
  TProcessId = vr_types.TProcessId;
  PProcessId = vr_types.PProcessId;
  PPoint = vr_types.PPoint;

  TIntArray = vr_types.TIntArray;
  PIntArray = vr_types.PIntArray;
  TStringArray = vr_types.TStringArray;
  PStringArray = vr_types.PStringArray;
  TFileNameArray = vr_types.TFileNameArray;
  TWideFileNameArray = vr_types.TWideFileNameArray;
  TCharsSet = vr_types.TCharsSet;
  TObjectMethod = vr_types.TObjectMethod;

  TDataProc = vr_types.TDataProc;
  TDataMethod = vr_types.TDataMethod;
  TPointerProc = vr_types.TPointerProc;
  TPointerMethod = vr_types.TPointerMethod;
  THandleProc = vr_types.THandleProc;
  THandleMethod = vr_types.THandleMethod;
  TInterfaceProc = vr_types.TInterfaceProc;
  TInterfaceMethod = vr_types.TInterfaceMethod;

  TNotifyProcedure = vr_types.TNotifyProcedure;
  TNotifyStringProc = vr_types.TNotifyStringProc;
  TNotifyStringMethod = vr_types.TNotifyStringMethod;
  TNotifyProc = vr_types.TNotifyProc;
  TNotifyMethod = vr_types.TNotifyMethod;

  TNewLineMethod = vr_types.TNewLineMethod;
  TEnvironmentScope = vr_types.TEnvironmentScope;

  TEnumFilesAction = (efaNone, efaStop, efaSkip);
  TEnumFilesMethod = procedure(var sr: TSearchRec; const APath: string; var AnAction: TEnumFilesAction) of object;
  TEnumFilesProc = procedure(var sr: TSearchRec; const APath: string;
      var AnAction: TEnumFilesAction; const AData: Pointer);

  TOsEvent = vr_types.TOSEvent;

const
  {$IFDEF WINDOWS}
  DefaultTextLineBreakStyleConst = tlbsCRLF;{$ENDIF}
  {$IFDEF UNIX}
  {$IFDEF DARWIN}
  DefaultTextLineBreakStyleConst = tlbsCR;{$ELSE}
  DefaultTextLineBreakStyleConst = tlbsLF;{$ENDIF}
  {$ENDIF}

  {$IFDEF UTILS_MAX}
  FilenamesCaseSensitive = FileUtil.FilenamesCaseSensitive;
  {$ELSE}
  FilenamesCaseSensitive = {$if defined(Windows) or defined(darwin)}False{$else}True{$endif};
  {$ENDIF}

type
  TExecProgramFlag = vr_types.TExecProgramFlag;
  TExecProgramFlags = vr_types.TExecProgramFlags;
const
  EXEC_OK       = vr_types.EXEC_OK;
  EXEC_FAIL     = vr_types.EXEC_FAIL;
  EXEC_TIME_OUT = vr_types.EXEC_TIME_OUT;

  epfNone = TExecProgramFlag.epfNone;
  epfWait = TExecProgramFlag.epfWait;
  epfNewGroup = TExecProgramFlag.epfNewGroup;
  epfInheritHandles = TExecProgramFlag.epfInheritHandles;
  epfNewConsole = TExecProgramFlag.epfNewConsole;
  epfHidden = TExecProgramFlag.epfHidden;
  //epfSuspended,
  epfKillIfTimeOut = TExecProgramFlag.epfKillIfTimeOut;


var
  ParamValueDelim: Char = ':';
  IsParamCaseSensetive: Boolean = False;
  LOCAL_HOST: string = 'localhost';
{%EndRegion Types Const Vars}

{%Region System}

const
  ParamStrUTF8: function(Param: Integer): string = LazUTF8.paramStrUTF8;

procedure FreeThenNil(var obj);

function GetTickCount: QWord;
const GetTickCount64: function: QWord = GetTickCount;

function sys_ParamValue(const AParam: string; out AValue: string): Boolean;
function sys_ParamExists(const AParam: string): Boolean;
function sys_GetCommandLine(const AParamStart: Integer = 1; const AParamEnd: Integer = MaxInt): string;

function GetUserPath: string;
function GetPersonalPath: string;
function GetProgramFilesPath: string;
{%EndRegion System}

{%Region Application}

type
  TAppState = (apsNone,
    apsLoadedBeforeFirstProject,//in TCustomProject.Open() setted
    apsLoaded,//in AppClasses.TAppEvents.OnIdle setted
    apsAllModulesLoaded,//in TCustomMainForm.FormShow (if no MainForm then user must set)
    apsCloseQuery,//in TCustomMainForm.CloseQuery setted
    apsClosing,//in TCustomMainForm.DoClose() setted
    apsClosed,//in TCustomMainForm.DoClose() setted
    apsActiveProjectChanging, //in TCustomProject.Open() setted
    apsRunAsTool //MainForm not created
    );
  TAppStates = set of TAppState;
var
  AppStates: TAppStates = [];
  AppCloseEvent: PRTLEvent = nil;//in AppClasses.TAppEvents.OnAppClose setted

function IsAppState(const AState: TAppState): Boolean; inline;

function SetAppPath(const APath: string; AShowError: Boolean = False): Boolean;
function AppPath: string;

function AppConfigPath(const AIsGlobal: Boolean = False; const AIsForce: Boolean = True): string;
function AppTempPath(const IsForce: Boolean = True): string;

{%EndRegion Application}

{%Region String}
function NameOfString(const S: string; ASeparator: string;
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;
function NameOfString(const S: string; ASeparator: Char = '=';
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;
function NameOfString(const S: string; ASeparator: TCharsSet;
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;

function ValueOfString(const S: string; ASeparator: string;
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;
function ValueOfString(const S: string; ASeparator: Char = '=';
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;
function ValueOfString(const S: string; ASeparator: TCharsSet;
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;

procedure NameValueOfString(const S: string; var AName, AValue: string;
    ASeparator: string; IsFromRight: Boolean = False; IsTrim: Boolean = True); overload;
procedure NameValueOfString(const S: string; var AName, AValue: string;
    ASeparator: Char = '='; IsFromRight: Boolean = False; IsTrim: Boolean = True); overload;
function NameValueOfString(const S: string; var AName, AValue: string;
    ASeparator: TCharsSet; IsFromRight: Boolean = False; IsTrim: Boolean = True): Char; overload;

function str_PosText(const SubStr, S: string): Integer;
function PosText(const SubStr, S: string): Integer;

function PosCount(const SubStr: string; const AStr: string): Integer; overload;
function PosCount(ch: Char; const Str: string): Integer; overload;

const
  TrimLeftSet: function(const S: String;const CSet:TSysCharSet): String = StrUtils.TrimLeftSet;
  TrimRightSet: function(const S: String;const CSet:TSysCharSet): String = StrUtils.TrimRightSet;
  TrimSet: function(const S: String;const CSet:TSysCharSet): String = StrUtils.TrimSet;

function TrimCharString(const S, AChars: string): string;
function TrimLeftCharString(const S, AChars: string): string;
function TrimRightCharString(const S, AChars: string): string;
function TrimString(const S, AString: string): string;
function TrimLeft_IncPos(var S: string; var iPos: Integer; const ATrimChars: string = '';
    const AIgnoreBlankChars: Boolean = False): Boolean;

function str_QuotedChar(const S: string; const AStart: Char = '"';
    AEnd: Char = #0; const AForce: Boolean = False): string;
function str_QuotedCharIfSpace(const S: string; const AQuot: Char = '"'): string;
function str_Quoted(const S, AQuot: string; const AForce: Boolean = False): string;
function str_QuotedIfSpace(const S, AQuot: string): string;

function StartsText(const ASubText, AText: string): Boolean;
function EndsText(const ASubText, AText: string): Boolean;
function StartsStr(const ASubText, AText: string): Boolean;
function EndsStr(const ASubText, AText: string): Boolean;

function PosR(const SubStr, Str: string): Integer;
function PosRChar(ch: Char; const Str: string): Integer;

function PosSet(const ASet: TCharsSet; const S: string; AIndex: Integer = 1): Integer;
function PosRSet(const ASet: TCharsSet; const Str: string): Integer;//ToDo ? ; AIndex: Integer = 1)

function IsInteger(const S: string): Boolean; overload;
function IsNumber(const S: string): Boolean; overload;
function IsNumber(const ch: Char): Boolean; overload;
function IsNumberWithDots(const S: string): Boolean;

function str_CharsInSet(const S: string; const AChars: TCharsSet): Boolean;
function IsCharUpper(const AChar: Char): Boolean;
function IsCharLower(const AChar: Char): Boolean;
function IsCharLetter(const AChar: Char): Boolean;
function IsCharLetterOrNumber(const AChar: Char): Boolean;
const
   IsCharNumber: function(const AChar: Char): Boolean = IsNumber;

function str_AddPrefix(const S, APrefix: string; CaseSens: Boolean = True): string;
function str_AddSufix(const S, ASuffix: string; CaseSens: Boolean = True): string;
function str_RemovePrefix(const S, APrefix: string; CaseSens: Boolean = True): string;
function str_RemoveSufix(const S, ASuffix: string; CaseSens: Boolean = True): string;

function str_StartsStr(const ASubText, AText: string): Boolean;
function str_EndsStr(const ASubText, AText: string): Boolean;
function str_StartsText(const ASubText, AText: string): Boolean;
function str_EndsText(const ASubText, AText: string): Boolean;

function str_LowerCase(const s: string): string;
function str_UpperCase(const s: string): string;

function str_CompareStr(const S1, S2: string): Integer;
function str_CompareText(const S1, S2: string): Integer;

type
  TCompareStringExOption = (cseoNone, cseoIgnoreCase, cseoDigitAsNumbers
      , cseoWordSort //'- greater then Number,Letter
      );
  TCompareStringExOptions = set of TCompareStringExOption;

function pch_CompareEx(S1, S2: PChar; Len1, Len2: Integer;
    const AOpts: TCompareStringExOptions = []): Integer;
function str_CompareEx(const S1, S2: string; const AOpts: TCompareStringExOptions = []): Integer;

function str_SameStr(const S1, S2: string): Boolean;
function str_SameText(const S1, S2: string): Boolean;

function SameStr(const S1, S2: string): Boolean;

function str_CopyBetween(const S: string; const AStart, AEnd: Integer;
      AIsIncludeEndChar: Boolean = True): string;

function str_DefineLineBreak(const S: string;
    const ADefault: TTextLineBreakStyle = DefaultTextLineBreakStyleConst): TTextLineBreakStyle;

function str_LCopy(const Source: PChar; const MaxLen: Integer): string; overload;
function str_LCopy(const Source: PWideChar; MaxLen: Integer): WideString; overload;

//S='func(p1);' AChars='()' - Result=['func','p1',';']
procedure str_SplitByChar(const S: string; AChar: Char; out arr: TStringDynArray;
    const AIsTrim: Boolean = True);
function str_SplitAccordingChars(const S, AChars: string; out arr: TStringArray;
    const AIsTrim: Boolean = True): Boolean;

procedure str_SplitByChars(const S: string; AChars: TCharsSet; out
    arr: TStringArray; const AIsTrim: Boolean);
procedure pch_SplitByChars(pc: PChar; const AChars: TSysCharSet; out arr: TStringDynArray;
    const AIsTrim: Boolean = True);
function pch_GetStrBetween(const pch1, pch2: PChar): string; inline;

procedure str_AddLine(var S: string; const ALine: string; const ALineBreak: string = sLineBreak); overload;
procedure str_AddLine(var S: string; const ALine: string; const ALineBreak: Char); overload;
function  str_AddedLine(const S, ALine: string; const ALineBreak: string = sLineBreak): string; overload;
function  str_AddedLine(const S, ALine: string; const ALineBreak: Char): string; overload;

function str_FirstChar(const S: string): Char; inline;
function str_LastChar(const S: string): Char; inline;
function str_Char(const S: string; AIndex: Integer): Char;
function str_CharR(const S: string; AIndex: Integer): Char;

function str_RandomChars(const Len: Integer; const IsUpperCase: Boolean = False): string;
const
  stringOfRandomChar: function(const Len: Integer; const IsUpperCase: Boolean = False): string = str_RandomChars;
  str_OfRandomChar: function(const Len: Integer; const IsUpperCase: Boolean = False): string = str_RandomChars;

function str_Replace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string; overload;
procedure str_Replace(const AReplace: string; var S: string; AStart, ACount: Integer); overload;
function str_Insert(const AInsert, S: string; AIndex: Integer): string;

function str_TryLoadFromFile(const AFileName: string; out S: string; AIsShowError: Boolean = False): Boolean;
function str_LoadFromFile(const AFileName: string; const AIsAdjustLineBreaks: Boolean = False): string;
function str_LoadFromFileEx(var AEncoding: string; const AFileName: string;
    const AIsAdjustLineBreaks: Boolean = False): string;
function str_LoadUtf8FromFile(const AFileName: string;
    const AIsAdjustLineBreaks: Boolean = False): string;
function str_SaveToFile(const AFileName: string; const S: string;
    const AIsShowError: Boolean = False): Boolean;
function str_SaveToFileEx(const AFileName: string; const S, AEncoding: string;
      AIsShowError: Boolean = False): Boolean;
function str_LoadFromFileAsInt(const AFileName: string; const ADefault: Integer = 0): Integer;
function str_AddToFile(const AFileName: string; const S: string; AsLine: Boolean = True): Boolean;

function Hex2Int(const AValue : string): Integer;
const
  str_Hex2Int: function(const AValue : string): Integer = Hex2Int;
function str_DecodeHexChars(const S: string): string;
function str_EncodeToHex(const S: string; const Allowed: TSysCharSet): string;

function WideLoadFromFile(const AFileName: string): WideString; overload;
function WideSaveToFile(const AFileName: string; const S: WideString;
    const AIsShowError: Boolean = False): Boolean; overload;

function str_RemoveEmptyChars(const S: string): string;

function IncludeTrailingUnixSlash(const APath: string): string;
function ExcludeTrailingUnixSlash(const APath: string): string;
function IncludeLeadingUnixSlash(const APath: string): string;
function ExcludeLeadingUnixSlash(const APath: string): string;
function str_ToUnixSlash(const S: string): string;
function str_ToWinSlash(const S: string): string;
function str_ToSysSlash(const S: string): string;

function str_IsUTF8(const S: string): Boolean;
// Stream's position doesn't change.
function utf8_HasBOM(S : TStream): boolean; overload;
function utf8_HasBOM(S : string): boolean; overload;

{$IF lcl_fullversion < 2000000}
const
  UTF8CodepointSize: function(p: PChar): integer = LazUTF8.UTF8CharacterLength;{$ENDIF}

//AEncoding: 'utf8bom','ucs2le','ucs2be'
function AddEncodingBOM(const AEncoding, AText: string): string;
function RemoveEncodingBOM(const AEncoding, AText: string): string;

function utf_8To16(const S: string): UnicodeString;
function utf_16To8(const S: UnicodeString): string;

type
  TNextDelimStingGetterOption = (ndsoNone, ndsoTrim, ndsoTrimOnlySpace{' ',#9},
      ndsoDeqoute, ndsoSkipEmpty,
      ndsoStrict{ignore delim in ""});
  TNextDelimStingGetterOptions = set of TNextDelimStingGetterOption;

  INextDelimStringGetter = interface
    ['{7037A38D-CC97-4DCC-9897-7F71DA67FD5F}']
    function Next(out S: string): Boolean;
    function LastPos: Integer;
    function CurrPos: Integer;
  end;

function New_NextDelimStringGetter(const S, ADelim: string;
    const AOptions: TNextDelimStingGetterOptions = []; const AQuoteChars: string = '"'): INextDelimStringGetter;

function new_StringBuilder(const ABlockLen: Integer = 1000): IFPStringBuilder;

{%EndRegion String}

{%Region StringArray}
const
  F_arrS_SORTED   = $00000001;
  F_arrS_CASESENS = $00000002;
  F_arrS_FILENAME = $00000004;

procedure arrS_Insert(var arr: TStringArray; AIndex: Integer; const S: string);
procedure arrS_Delete(var arr: TStringArray; AIndex: Integer);
function arrS_Remove(var arr: TStringArray; const S: string): Integer;
function  arrS_Add(var arr: TStringArray; const S: string): Integer;
function  arrS_AddArray(var arr: TStringArray; const arr1: TStringArray;
    AIgnoreDuplicat: Boolean = False; ACaseSensetive: Boolean = False): Integer;
procedure arrS_RemoveDuplicat(var arr: TStringArray);
procedure arrS_RemoveEmpty(var arr: TStringArray);
function arrS_FromOpenArray(const arr: array of string): TStringArray;
function arrS_Copy(const arr: TStringArray; AStart, AEnd: Integer): TStringArray;

function arrS_Find(const arr: TStringArray; out AIndex: Integer;
    const S: string; CaseSens: Boolean): Boolean;
function arrS_IndexOf(const arr: TStringArray;
    const S: string; F_arrS: Cardinal = 0): Integer;

procedure arrS_SetCommaText(out arr: TStringArray; const ACommaText: string;
     ADelim: Char = ','; F_arrS: Cardinal = 0);
function arrS_GetCommaText(const arr: TStringArray; ADelim: Char = ',';
     F_arrS: Cardinal = 0): string;

procedure arrS_Sort(var arr: TStringArray; ACaseSensetive: Boolean);

procedure arrS_LoadFromStrs(out arr: TStringArray; const strs: TStrings);
procedure arrS_SaveToStrs(const arr: TStringArray; const strs: TStrings;
    const AIndexToObject: Boolean = False); overload;
procedure arrS_SaveToStrs(const arr: array of string; const strs: TStrings;
    const AIndexToObject: Boolean = False); overload;
{%EndRegion StringArray}

{%Region Integer Array TIntArray}
type
  TIntArraySortCompare = function(arrI: TIntArray;
      Index1, Index2: Integer; AData: Pointer): Integer;

function arrI_IndexOf(var arr: TIntArray; const AInt: PtrInt): Integer;
function arrI_IndexOfSorted(var arr: TIntArray; const AInt: PtrInt): Integer;
procedure arrI_Insert(var arr: TIntArray; AIndex: Integer; const AInt: PtrInt);
procedure arrI_Delete(var arr: TIntArray; const AIndex: Integer);
function arrI_Remove(var arr: TIntArray; const AInt: PtrInt): Integer;
function arrI_RemoveSorted(var arr: TIntArray; AInt: PtrInt): Integer;
function arrI_Add(var arr: TIntArray; const AInt: PtrInt): Integer;
function arrI_AddSorted(var arr: TIntArray; const AInt: PtrInt): Integer;
function arrI_AddUnique(var arr: TIntArray; const AInt: PtrInt): Integer;
procedure arrI_Sort(var arr: TIntArray; CompareFn: TIntArraySortCompare = nil;
    AData: Pointer = nil);
procedure arrI_Fill(var arr: TIntArray; AInt: PtrInt; const AInc: Integer = 0);
function arrI_Find(var arr: TIntArray; const AInt: PtrInt; out Index: Integer): Boolean;
procedure arrI_AddArray(var arr: TIntArray; const arrAdding: TIntArray;
    AIsNotDublicate: Boolean = False);
procedure arrI_Assign(out arr: TIntArray; const arrSource: TIntArray);
procedure arrI_Expand(var arr: TIntArray; const ACount: Integer; const AValue: PtrInt = 0);
function arrI_AsJsonString(const arr: TIntArray): string;

function arrI_GetCommaText(const arr: TIntArray): string;
procedure arrI_SetCommaText(out arr: TIntArray; const ACommaText: string);
{%EndRegion Integer Array TIntArray}

{%Region  Arrays Misc}

function BoolArrayToIntArray(const ABools: TBooleanDynArray): TIntArray;
function IntArrayToBoolArray(const arr: TIntArray): TBooleanDynArray;

function ByteArrayToIntArray(const ABytes: TByteDynArray): TIntArray;
function IntArrayToByteArray(const arr: TIntArray): TByteDynArray;

function ByteArrayToCommaText(const arr: TByteDynArray): string;
function BoolArrayToCommaText(const arr: TBooleanDynArray): string;

function CommaTextToByteArray(const S: string): TByteDynArray;
function CommaTextToBoolArray(const S: string): TBooleanDynArray;

function NewByteArray(const ASource: TByteDynArray): TByteDynArray;

{%EndRegion}

{%Region Files}

function file_ExtractDir(const FileName: string): string;
function file_ExtractPath(const FileName: string): string;
function file_ExtractDrive(const FileName: string): string;

function file_Size(const Filename: string): int64;

function file_ExtractExt(const FileName: string): string;
function file_ExtractName(const FileName: string): string;
function file_ExtractNameWithoutExt(const AFilename: string): string;
function file_IsAbsoluteName(const AFileName: string): Boolean;

function file_AddFileProtocol(const AFileName: string;
    AIsAdjustToUnixSlash: Boolean = True; AIsAddThirdSlash: Boolean = True): string;

function file_Open(const AFileName: string; out AHandle: THandle;
    const Mode: Integer; const IsShowError: Boolean = False): Boolean;
procedure file_Close(AHandle: THandle);
function file_Create(const AFileName: string; out AHandle: THandle;
      const IsShowError: Boolean = False; const AShareMode: Integer = fmShareExclusive;
      const ARights: Integer = 0): Boolean;
function file_Clear(const AFileName: string; IsShowError: Boolean = False): Boolean;
function file_CreateEmpty(const AFileName: string; IsShowError: Boolean = False): Boolean;
function file_CreateIfNotExist(const AFileName: string; IsShowError: Boolean = False): Boolean;

function file_GetAttr(const AFileName: String): Longint;
function file_SetAttr(const AFilename: String; Attr: longint): Longint;

function file_GetModified(const AFileName: string): TDateTime;
function file_SetModified(const AFileName: string; dt: TDateTime): Integer;

procedure file_Assign(out f: Text; const AFileName: string); overload;
procedure file_Assign(out f: File; const AFileName: string); overload;
function file_AppendString(const AFileName, S: string; const IsShowError: Boolean = False): Boolean;
function file_AppendLine(const AFileName, S: string; const IsShowError: Boolean = False): Boolean;
function file_Read(Handle : THandle; out Buffer; Count : longint): Longint;
function file_Write(Handle : THandle; const Buffer; Count : Longint): Longint;
//Origin: fsFromBeginning,fsFromCurrent,fsFromEnd
function file_Seek(Handle : THandle; FOffset: Int64; Origin: Longint): Int64;

function file_Exists(const AFilename: string; const AIsShowError: Boolean = False): Boolean;

//SymLink or HardLink
function file_IsLink(const AFileName: string): Boolean;
function file_IsInLinkDir(const AFileName: string; out ADir: string): Boolean;
function file_GetLinkTarget(const ALink: string; out ATarget: string): Boolean; overload;
function file_GetLinkTarget(var ALink: string): Boolean; overload;

//for each file_FindFirst must be called file_FindClose with the same ASearchRec
//Result=0 if Found
function file_FindFirst(const Path: string; const Attr: Longint; out ASearchRec: TSearchRec): Longint;
function file_FindNext(var ASearchRec: TSearchRec): Longint;
procedure file_FindClose(var ASearchRec: TSearchrec);
function file_FindFirstInPath(const AName, APath: string; out AFileName: string): Boolean;

const
  file_CompareName: function(const AFileName1, AFileName2: string): Integer = LazFileUtils.CompareFilenames;
function file_SameName(const AFileName1, AFileName2: string): Boolean;

function file_Rename(const AOldName, ANewName: String; IsShowError: Boolean = False): Boolean;
function file_ExtractNameOnly(const AFilename: string;
    const AWithoutAllExtensions: Boolean = False): string;  //ToDo do patch to LCL
function file_Delete(const AFileName: string; IsShowError: Boolean = False;
    IsConfirm: Boolean = False): Boolean;

type
  TFileCopyFlag = (fcfSkipIfExists, fcfCreateDestDirectory, fcfPreserveTime,
      fcfShowError, fcfSkipIfOlderVersion);
  TFileCopyFlags = set of TFileCopyFlag;

function file_Copy(const ASrcFilename, ADestFilename: string; const fcf: TFileCopyFlags = []): boolean;

function file_HasExt(const AFileName: string; const AExt: string = ''): Boolean;
function file_AddExt(const AFileName, AExt: string; const AOnlyIfNone: Boolean = True): string;

function file_Starts(const ASubDir, AFilename: string): Boolean;
function file_Ends(const ASubName, AFilename: string): Boolean;
function file_PosInName(const ASubStr, AFilename: string): Integer;

function file_ChangeExt(const AFileName, AExt: string): string;
function file_RemoveExt(const AFileName: string): string;
function file_IsReadOnly(const FileName: string): Boolean;
function file_ExpandName(const AFileName: string): string;
function file_QuotedName(const AFileName: string): string;

//if not WINDOWS and (AParam <> '') then called function GetProgramOutput()
procedure file_ParseVersion(const AFileName, AParam: string; out AMax, AMin: Integer;
    const ARevision: PInteger = nil; const ABuild: PInteger = nil);
function file_GetVersionMax(const AFile: string; const AParam: string = ''): Integer;
function file_GetVersionAsString(const AFile: string; const AParam: string = ''): string;
function file_CompareVers(AFileName1, AFileName2: string; const AParam: string = ''): Integer;
function file_CompareVerWith(AFileName1, AVersion: string; const AParam: string = ''): Integer;
function file_CompareVersionStrings(AVer1, AVer2: string): Integer;

function file_GetTempNameInDir(const ADirectory: string;
    const AExt: string = '.tmp'; const APrefix: string = '';
    AIsFullPath: Boolean = True): string;
function file_GetTempName(const AExt: string = '.tmp';
    const APrefix: string = ''; AIsFullPath: Boolean = True): string;
function file_CreateTemp(out AFileName: string; ADeleteOnFreeObject: TPersistent = nil;
    const AExt: string = '.tmp'; const APrefix: string = '';
    const ADirectory: string = ''): Boolean;

function dir_Exists(const ADirectory: string; const IsShowError: Boolean = False): Boolean;
function dir_Force(const ADir: string; IsShowError: Boolean = False): Boolean;
function dir_Create(const ADir: string; IsShowError: Boolean = False): Boolean;
function dir_GetCurrent: string;
const
  dir_SetCurrent: function(const ANewDir: string): Boolean = LazFileUtils.SetCurrentDirUTF8;
function dir_GetParent(var ADir: string): Boolean;

function dir_IsEmpty(const ADir: string; AIgnoreEmptySubDir: Boolean = False): Boolean;
function dir_FileCount(const ADir: string; const AIncludeSubDir: Boolean = False): Integer;
function dir_Remove(const ADir: string; IsShowError: Boolean = False): Boolean; //Only If Empty
function dir_Delete(const ADir: string; OnlyChilds: boolean = False;
      IsConfirm: Boolean = False; IsShowError: Boolean = False): boolean;
function dir_DeleteIfEmpty(const ADir: string; AIgnoreEmptySubDir: Boolean = False): Boolean;

function dir_CopyExt(const ASrcDir, ADestDir: string; fcf: TFileCopyFlags = [];
      const AFileMask: string = ''; faAttr: Longint = faAnyFile): Integer;
function dir_Copy(const ASrcDir, ADestDir: string): Boolean;

function dir_SameName(const ADir1, ADir2: string): Boolean;

{%EndRegion Files}

{%Region URL}

function url_SpacesToHex(const AUrl: string): string;
function url_HexToSpaces(const AUrl: string): string;
function url_Unescape(const AUrl: string): string;
function url_Escape(const AUrl: string): string;

function url_RemoveProtocol(const AUrl: string): string;

{%EndRegion URL}

{%Region Enum Files Functions}

type
  TVrodeEnumFilesOption = (vefoIgnoreFiles, vefoIgnoreDirs,
      vefoFileMasksExclude, vefoDirMasksExclude,
      vefoAddToStrsWithPath, //if true then vefoRelativePath ignore for Strings //backward compat ? remove
      vefoUnixSlashInStrs, vefoExcludeExt,
      vefoRelativePath);
  TVrodeEnumFilesOptions = set of TVrodeEnumFilesOption;

  TVrodeMatchFunc = function(const AName: string; AMaskList: TMaskList): Boolean of object;

  { TVrodeEnumFiles }

  TVrodeEnumFiles = class(TObject)
  private
    FAttr: Integer;
    FDirMasks: string;
    FFileMasks: string;
    FLevel: Integer;
    //FIsDirMasksExclude: Boolean;
    //FIsFileMasksExclude: Boolean;
    //FOnlyDirs: Boolean;
    FOptions: TVrodeEnumFilesOptions;
    Fstrs: TStrings;
    //FIsAddToStrsWithPath: Boolean;
    FPaths: TFileNameArray;
    FProc: TEnumFilesProc;
    FProcData: Pointer;
    FMethod: TEnumFilesMethod;
    FFileMatch: TVrodeMatchFunc;
    FDirMatch: TVrodeMatchFunc;
    function MatchEmpty(const {%H-}AName: string; {%H-}AMaskList: TMaskList): Boolean;
    function MatchInclude(const AName: string; AMaskList: TMaskList): Boolean;
    function MatchExclude(const AName: string; AMaskList: TMaskList): Boolean;
    procedure SetDirMasks(const AValue: string);
    procedure SetFileMasks(const AValue: string);
    procedure SetMatchFunc(var AFunc: TVrodeMatchFunc;
        AMaskList: TMaskList; IsExclude: Boolean);
  protected
    procedure _Enum;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure EnumToList(strs: TStrings; AIsAddToList: Boolean = False);
    procedure EnumToProc(Proc: TEnumFilesProc; AData: Pointer = nil);
    procedure EnumToMethod(AMethod: TEnumFilesMethod);
    procedure AddPath(const APath: string);
    procedure AddArrayOfPath(var arr: TFileNameArray);
    procedure ClearPaths;
    property FileMasks: string read FFileMasks write SetFileMasks;
    //property IsFileMasksExclude: Boolean read FIsFileMasksExclude write FIsFileMasksExclude;
    property DirMasks: string read FDirMasks write SetDirMasks;
    //property IsDirMasksExclude: Boolean read FIsDirMasksExclude write FIsDirMasksExclude;
    property Options: TVrodeEnumFilesOptions read FOptions write FOptions;
    property Attributes: Integer read FAttr write FAttr;
    property Level: Integer read FLevel write FLevel;

  end;

const
  VEFO_DEFAULT = [vefoIgnoreDirs, vefoAddToStrsWithPath];

procedure EnumFilesOfArray(var arr: TFileNameArray; AObjProc: TEnumFilesMethod;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile); overload;

procedure EnumFilesOfArray(var arr: TFileNameArray; AProc: TEnumFilesProc;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile); overload;

procedure EnumFilesOfArray(var arr: TFileNameArray; strs: TStrings;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile; AIsAddToList: Boolean = False); overload;

procedure EnumFilesToMethod(const APath: string; AObjProc: TEnumFilesMethod;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile); overload;

procedure EnumFilesToProc(const APath: string; AProcData: Pointer; AProc: TEnumFilesProc;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile); overload;

procedure EnumFilesToStrs(const APath: string; strs: TStrings;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile; AIsAddToList: Boolean = False); overload;

{%EndRegion Enum Files Functions}

{%Region Misc}

type
  TDebugInfoType = (ditNone, ditInfo, ditError, ditHint, ditWarn, ditClear);
var
  OnShowConfirm: function(const S: string; AIsYesNo: Boolean): Boolean = nil;
  OnShowInfo: procedure(const S: string) = nil;
  OnShowError: procedure(const AError: string; ACaption: string = '') = nil;
  OnShowDebugInfo: procedure(const S, AFileName: string; const AType: TDebugInfoType) = nil;

function ShowConfirm(const S: string; AIsYesNo: Boolean = False): Boolean;
function ShowConfirmFmt(const S: string; AParams: array of const; AIsYesNo: Boolean = False): Boolean; inline;
procedure ShowInfo(const S: string);
procedure ShowInfoFmt(const S: string; AParams: array of const); inline;
procedure ShowError(const AError: string; ACaption: string = '');
procedure ShowErrorFmt(const AError: string; AParams: array of const); inline;
procedure ShowDebugInfo(const S: string; const AFileName: string = ''; AType: TDebugInfoType = ditNone);
procedure ShowDebugInfoFmt(const S, AFileName: string; AParams: array of const;
    AType: TDebugInfoType = ditNone); inline;

procedure ShowLastOSError;
function GetLastOSErrorString: string;
function GetOSErrorString(AErrCode: Integer): string;
function err_GetMessage(E: Exception): string;
function err_GetSysMessage(AErrorCode: Integer): string;
procedure ShowExceptionMsg(E: Exception);

function IfThen(val: Boolean; const ifTrue: string; const ifFalse: string = ''): string; inline; overload;
function IfThen(val: Boolean; const ifTrue: UnicodeString; const ifFalse: UnicodeString = ''): UnicodeString; inline; overload;
function IfThen(val: Boolean; const ifTrue: WideString; const ifFalse: WideString = ''): WideString; inline; overload;
function IfThen(val: Boolean; const ifTrue: Char; const ifFalse: Char = ' '): Char; inline; overload;
function IfThen(val: Boolean; const ifTrue: Pointer; const ifFalse: Pointer = nil): Pointer; inline; overload;
function IfThen(val: Boolean; const ifTrue: Integer; const ifFalse: Integer = 0): Integer; inline; overload;
function IfThen(val: Boolean; const ifTrue: Cardinal; const ifFalse: Cardinal = 0): Cardinal; inline; overload;
function IfThen(val: Boolean; const ifTrue: Byte; const ifFalse: Byte = 0): Byte; inline; overload;
function IfThen(val: Boolean; const ifTrue: Word; const ifFalse: Word = 0): Word; inline; overload;

function flag_Is(const AFlags, AFlag: Integer): Boolean; inline;
function flag_Toggle(var AFlags: Integer; const AFlag: Integer): Integer;
procedure flag_Set(var AFlags: Integer; const AFlag: Integer); inline; overload;
procedure flag_Set(var AFlags: Cardinal; const AFlag: Cardinal); inline; overload;
procedure flag_UnSet(var AFlags: Integer; const AFlag: Integer); overload;
procedure flag_UnSet(var AFlags: Cardinal; const AFlag: Cardinal); overload;
function flag_GetUnSet(const AFlags: Integer; const AFlag: Integer): Integer;
procedure flag_Change(var AFlags: Integer; const AFlag: Integer; IsSet: Boolean);
function flag_GetChanged(const AFlags, AFlag: Integer; IsSet: Boolean): Integer;

function SameMethod(AMethod1, AMethod2: TMethod): Boolean;
function Method(const ACode, AData: Pointer): TMethod; inline;
function IsValueBetween(AValue, AMin, AMax: Integer): Boolean; inline;

function TryStrToDouble(const S: string; out AValue: Double): Boolean;
function DoubleToStr(AValue: Double): string;
function PointerToStr(p: Pointer): string;
function StrToPointer(const S: string): Pointer;
function TryStrToPointer(const S: string; out p : Pointer) : Boolean;
function StrToPointerDef(const S: string; Default: Pointer): Pointer;
function CompareValue(const A, B: Pointer): Integer; inline; overload;
function TryStrToPtrInt(const s: string; Out i : PtrInt) : Boolean;

function Wait(AWaitMSec: Cardinal; AOnWaitFlag: System.PBoolean;
    AFlagValue: Boolean = True; //AInterval: Cardinal = 10;
    AIgnoreAppCloseEvent: Boolean = False): Boolean; overload;
function Wait(AWaitMSec: Cardinal; AOnWaitFlag: PPtrInt = nil;
    AFlagValue: PtrInt = 0; AIfNotFlagValueEqual: Boolean = False;
    AIgnoreAppCloseEvent: Boolean = False): PtrInt; overload;

procedure CallNotifyEventAsync(const AMethod: TNotifyEvent; const AData: Pointer;
       const AWaitMSec: Integer = 0; const AIsSyncronize: Boolean = False);
procedure CallNotifyProcAsync(const AProc: TNotifyProcedure; const AData: Pointer;
       const AWaitMSec: Integer = 0; const AIsSyncronize: Boolean = False);

function res_ExtractToFile(const AResName, AFileName: string; AHInstance: TFPResourceHMODULE = 0): Boolean;
function res_ExtractToString(const AResName: string; out S: string; AHInstance: TFPResourceHMODULE = 0): Boolean;

function IsMainThread: Boolean; overload; inline;
function IsMainThread(AIsRaiseException: Boolean): Boolean;  overload;

function GuidCreate: TGUID;
function GuidCreateString(const AWithoutBrace: Boolean = False): string;

const
  RaiseToDoException: procedure(AMsg: string = '') = vr_SysUtils.RaiseToDoException;

{%EndRegion Misc}

{%Region Point}

function pt_IsBetweenLines(P, P1, P2: TPoint): Boolean;
function pt_IsInRect(P, lt, br: TPoint): Boolean; overload;
function pt_IsInRect(p: TPoint; r: TRect): Boolean; overload;
function pt_IsSame(p1, p2: TPoint): Boolean;
function pt_Min(p1, p2: TPoint): TPoint;
function pt_Max(p1, p2: TPoint): TPoint;

function pt_ToZeroBased(const p: TPoint): TPoint;
function pt_ToOneBased(const p: TPoint): TPoint;

{%EndRegion Point}

{%Region Process}

function ExecuteDocument(const APath: string; const AParams: string = '';
    const ACurrentDirectory: string = ''; const AShowError: Boolean = False): Boolean;
function ExecuteDocumentEx(const APath, AParams, ACurrentDirectory: string;
    const AShowError: Boolean; out AProcessHandle: THandle): Boolean;

//Return EXEC_FAIL if ERROR
function ExecuteProgram(const AProg, AParams: string;
    const ACurrentDirectory: string = '';
    const AFlags: TExecProgramFlags = []; const AMSecWait: Integer = 0;
    const AProcessId: PProcessId = nil; {const AJobHandle: PHandle = nil;}
    const AProcessHandle: PHandle = nil): Integer;

function GetProgramOutput(const APath, ACommaParams: string; strs: TStrings;
    AWaitSecIfNoOutput: Cardinal = 0; const ACurrentDirectory: string = '';
    const AKillIfTimeOut: Boolean = False): Integer; overload;
function GetProgramOutput(const APath, ACommaParams: string; out AResult: string;
    AWaitSecIfNoOutput: Cardinal = 0; const ACurrentDirectory: string = '';
    const AKillIfTimeOut: Boolean = False): Integer; overload;

function GetProcessHandle: THandle;//unix: return PID
function GetProcessHandleByPID(const pid: TProcessId): THandle;//unix: return PID
function GetProcessIDByHandle(const AProcessHandle: THandle): TProcessId;
function CloseProcessHandle(const AProcessHandle: THandle): Boolean;
function TerminateProgram(const AProcessHandle: THandle; const AExitCode: Cardinal): Boolean;
function TerminateProcess(const APid: TProcessId; const AExitCode: Cardinal): Boolean;
function TerminateProcessAndChildren(const APid: TProcessId; const AExitCode: Cardinal): Boolean;
function IsProgramRunning(const AProcessHandle: THandle): Boolean;
function IsProcessRunning(const APid: TProcessId): Boolean;
function GetProcessExitCode(const AProcessHandle: THandle; out ACode: Integer): Boolean;
function IsProgramForeground(const AProcessID: Cardinal): Boolean;

//Only for Windows
function IsJobRunning(const AJob: THandle): Boolean;
function CreateJob(const AProcessHandle: THandle = 0): THandle;
function AddProcessToJob(const AJob: THandle; const AProcessHandle: THandle): Boolean;
function TerminateJob(const AJob: THandle; const AExitCode: Cardinal = 0): Boolean;
function CloseJobHandle(const AJobHandle: THandle): Boolean;

{%EndRegion Process}

{$IFDEF DEBUG_MODE}
//* use CommandLine parameter --debug-log to assign DebugLogFileName in initialization block
procedure WriteDebugLogLn(const S: string);
procedure WriteDebugLogLnFmt(const S: string; const Args : Array of const);

procedure WriteToDebugConsole(const S: string; const AIsAddToLastLine: Boolean = False;
    const AForce: Boolean = False);
procedure ClearDebugConsole;

var
  OnWriteToDebugConsole: procedure(const S: string; const AIsAddToLastLine: Boolean = False;
      const AForce: Boolean = False);
  OnClearDebugConsole: procedure;
  DebugLogFileName: string;
  iDebug: Integer = 0;
  iDebugTemp: Integer = 0;
  iDebugSelS: Integer = 0;
  iDebugData: Pointer = nil;
  iDebugFileName: string;
  fDebug: Boolean = False;
  DebugVarName: string;
  DebugFileName: string;{$ENDIF}

var
  OnAppConfigPath: function(const AIsGlobal: Boolean = False; const AIsForce: Boolean = True): string;

implementation

{$IFDEF WINDOWS}{$IFDEF IGNORE_SHELL_COMPAT}
uses ShellApi;
{$ENDIF}{$ENDIF}

type
  TTrimStringFunc = function(const S: string): string;
  TTrimStringManager = record
    Trim: TTrimStringFunc;
    TrimLeft: TTrimStringFunc;
    TrimRight: TTrimStringFunc;
  end;
  PTrimStringManager = ^TTrimStringManager;

  { TAutoTempFileDeleter }

  TAutoTempFileDeleter = class(TPersistent, IFPObserver) //ThreadSafe
  private
    FList: TStrings;
    FRTLSect: TRTLCriticalSection;
  private
    { IFPObserver }
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; {%H-}Data : Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AFileName: string; const AObject: TPersistent);
  end;

{ TAutoTempFileDeleter }

procedure TAutoTempFileDeleter.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  i: Integer;
begin
  if (Self = nil) or
      (Operation <> ooFree) then Exit;
  System.EnterCriticalsection(FRTLSect);
  try
    for i := FList.Count - 1 downto 0 do
      if FList.Objects[i] = ASender  then
        begin
          file_Delete(FList[i]);
          FList.Delete(i);
        end;
  finally
    System.LeaveCriticalsection(FRTLSect);
  end;
end;

constructor TAutoTempFileDeleter.Create;
begin
  FList := TStringListUTF8.Create;
  InitCriticalSection(FRTLSect);
end;

destructor TAutoTempFileDeleter.Destroy;
var
  i: Integer;
begin
  System.EnterCriticalsection(FRTLSect);
  try
    for i := 0 to FList.Count - 1 do
      file_Delete(FList[i]);
    FList.Free;
    inherited Destroy;
  finally
    System.LeaveCriticalsection(FRTLSect);
    System.DoneCriticalsection(FRTLSect);
  end;
end;

procedure TAutoTempFileDeleter.Add(const AFileName: string;
  const AObject: TPersistent);
begin
  System.EnterCriticalsection(FRTLSect);
  try
    FList.AddObject(AFileName, AObject);
    AObject.FPOAttachObserver(Self);
  finally
    System.LeaveCriticalsection(FRTLSect);
  end;
end;



var
  _AutoTempFileDeleter: TAutoTempFileDeleter = nil;


function _EmptyTrim(const S: string): string;
begin
  Result := S;
end;

var
  _TrimMan: array of TTrimStringManager;
function _GetTrimManager(const AIsNotEmpty: Boolean): TTrimStringManager;
var
  r: PTrimStringManager;
begin
  if Length(_TrimMan) = 0 then
    begin
      SetLength(_TrimMan, 2);
      r := @_TrimMan[0];
      r.Trim := _EmptyTrim;
      r.TrimLeft := _EmptyTrim;
      r.TrimRight := _EmptyTrim;
      r := @_TrimMan[1];
      r.Trim := Trim;
      r.TrimLeft := TrimLeft;
      r.TrimRight := Trimright;
    end;
  Result := _TrimMan[Integer(AIsNotEmpty)];
end;

procedure FreeThenNil(var obj);
begin
  if Pointer(obj) <> nil then
  begin
    TObject(obj).Free;
    Pointer(obj) := nil;
  end;
end;

{$IFDEF VER3}
function GetTickCount: QWord;
begin
  Result := sysutils.GetTickCount64;
end;
{$ELSE VER3}
{$IFDEF WINDOWS}
{$IFNDEF WINCE}
type
  TGetTickCount64 = function : QWord; stdcall;

var
  WinGetTickCount64: TGetTickCount64 = Nil;
{$ENDIF}

function GetTickCount: QWord;
{$IFNDEF WINCE}
var
  lib: THandle;
{$ENDIF}
begin
{$IFNDEF WINCE}
  { on Vista and newer there is a GetTickCount64 implementation }
  if Win32MajorVersion >= 6 then begin
    if not Assigned(WinGetTickCount64) then begin
      lib := LoadLibrary('kernel32.dll');
      WinGetTickCount64 := TGetTickCount64(
                             GetProcAddress(lib, 'GetTickCount64'));
    end;
    Result := WinGetTickCount64();
  end else
{$ENDIF}
    Result := Windows.GetTickCount;
end;
{$ELSE WINDOWS}
function GetTickCount: QWord;
begin
  Result := lazutf8sysutils.GetTickCount64;
  //Result := LazSysUtils.GetTickCount64; //Laz 2.0
end;
{$ENDIF ELSE WINDOWS}
{$ENDIF ELSE VER3}

function sys_ParamValue(const AParam: string; out AValue: string): Boolean;

var
  i, n: Integer;
  S: String;
  f: Boolean;
begin
  Result := True;
  AValue := '';
  n := Length(AParam);
  for i := 1 to ParamCount do
    begin
      S := ParamStr(i);
      if IsParamCaseSensetive then
        f := StartsStr(AParam, S)
      else
        f := StartsText(AParam, S);
      if f then
        begin
          if (Length(S) = n) then
            Exit
          else if (Length(S) >= n + 1) and
            (S[n + 1] = ParamValueDelim) then
          begin
            AValue := Copy(S, n + 2, MaxInt);
            Exit;
          end;
        end
    end;
  Result := False;
end;

function sys_ParamExists(const AParam: string): Boolean;
var
  S: string;
begin
  Result := sys_ParamValue(AParam, S);
end;

function sys_GetCommandLine(const AParamStart: Integer; const AParamEnd: Integer
  ): string;
var
  i, iEnd: Integer;
  S: String;
begin
  Result := '';
  iEnd := min(Paramcount, AParamEnd);
  for i := AParamStart to iEnd do
    begin
      S := ParamStrUTF8(i);
      if Pos(' ', S) > 0 then
        S := str_Quoted(S, '"');
      str_AddLine(Result, S, ' ');
    end;
end;

function GetUserPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir);
end;

function GetPersonalPath: string;
begin
  {$IFDEF WINDOWS}
  Result := IncludeTrailingPathDelimiter(GetSpecialFolderDir(CSIDL_PERSONAL));
  {$ELSE}
  Result := GetUserPath;
  {$ENDIF}
  //MacOS  ~/Library/Application Support/<AppName>
end;

function GetProgramFilesPath: string;
begin
  {$IFDEF WINDOWS}
  Result := IncludeTrailingPathDelimiter(GetProgramFilesDir);
  {$ELSE}
  RaiseToDoException();
  {$ENDIF}
end;

function NameOfString(const S: string; ASeparator: string; IsFromRight: Boolean;
  IsTrim: Boolean): string;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosR(ASeparator, Result)
  else
    i := Pos(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimRight(Copy(Result, 1, i - 1));
end;

function NameOfString(const S: string; ASeparator: Char; IsFromRight: Boolean;
  IsTrim: Boolean): string;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosRChar(ASeparator, Result)
  else
    i := Pos(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimRight(Copy(Result, 1, i - 1));
end;

function NameOfString(const S: string; ASeparator: TCharsSet;
  IsFromRight: Boolean; IsTrim: Boolean): string;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosRSet(ASeparator, Result)
  else
    i := PosSet(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimRight(Copy(Result, 1, i - 1));
end;

function ValueOfString(const S: string; ASeparator: string;
    IsFromRight: Boolean; IsTrim: Boolean): string; overload;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosR(ASeparator, Result)
  else
    i := Pos(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimLeft(Copy(Result, i + Length(ASeparator), MaxInt))
  else
    Result := '';
end;

procedure NameValueOfString(const S: string; var AName, AValue: string;
  ASeparator: string; IsFromRight: Boolean; IsTrim: Boolean);
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  AName := tm.Trim(S);
  if IsFromRight then
    i := PosR(ASeparator, AName)
  else
    i := Pos(ASeparator, AName);
  if i > 0 then
    begin
      AValue := tm.TrimLeft(Copy(AName, i + Length(ASeparator), MaxInt));
      AName := tm.TrimRight(Copy(AName, 1, i - 1));
    end
  else
    AValue := '';
end;

function ValueOfString(const S: string; ASeparator: Char = '=';
    IsFromRight: Boolean = False; IsTrim: Boolean = True): string; overload;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosRChar(ASeparator, Result)
  else
    i := Pos(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimLeft(Copy(Result, i + Length(ASeparator), MaxInt))
  else
    Result := '';
end;

function ValueOfString(const S: string; ASeparator: TCharsSet;
  IsFromRight: Boolean; IsTrim: Boolean): string;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  Result := tm.Trim(S);
  if IsFromRight then
    i := PosRSet(ASeparator, Result)
  else
    i := PosSet(ASeparator, Result);
  if i > 0 then
    Result := tm.TrimLeft(Copy(Result, i + SizeOf(Char), MaxInt))
  else
    Result := '';
end;

procedure NameValueOfString(const S: string; var AName, AValue: string;
  ASeparator: Char; IsFromRight: Boolean; IsTrim: Boolean);
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  AName := tm.Trim(S);
  if IsFromRight then
    i := PosRChar(ASeparator, AName)
  else
    i := Pos(ASeparator, AName);
  if i > 0 then
    begin
      AValue := tm.TrimLeft(Copy(AName, i + Length(ASeparator), MaxInt));
      AName := tm.TrimRight(Copy(AName, 1, i - 1));
    end
  else
    AValue := '';
end;

function NameValueOfString(const S: string; var AName, AValue: string;
  ASeparator: TCharsSet; IsFromRight: Boolean; IsTrim: Boolean): Char;
var
  i: Integer;
  tm: TTrimStringManager;
begin
  tm := _GetTrimManager(IsTrim);
  AName := tm.Trim(S);
  if IsFromRight then
    i := PosRSet(ASeparator, AName)
  else
    i := PosSet(ASeparator, AName);
  if i > 0 then
    begin
      Result := S[i];
      AValue := tm.TrimLeft(Copy(AName, i + SizeOf(Char), MaxInt));
      AName := tm.TrimRight(Copy(AName, 1, i - 1));
    end
  else
    begin
      AValue := '';
      Result := #0
    end;
end;

function str_PosText(const SubStr, S: string): Integer;
begin
  Result := Pos(UTF8LowerCase(SubStr), UTF8LowerCase(S));
end;

function PosText(const SubStr, S: string): Integer;
begin
  Result := Pos(LowerCase(SubStr), LowerCase(S));
end;

function PosCount(const SubStr: string; const AStr: string): Integer;
var
  Len: Integer;
  PSubStr: PChar;
  Str: PChar;
begin
  Result := -1;
  Str := PChar(AStr);
  if not Assigned(Str) then Exit;
  Len := Length(SubStr);
  PSubStr := PChar(SubStr);
  Str := Str - Len;
  while Str <> nil do
    begin
      Inc(Result);
      Str := Str + Len;
      Str := StrPos(Str, PSubStr);
    end;
end;

function PosCount(ch: Char; const Str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Str) do
    if Str[i] = ch then
      begin
        Inc(Result);
      end;
end;

function TrimCharString(const S, AChars: string): string;
var
  Ofs, Len: integer;
begin
  Len := Length(S);
  while (Len > 0) and (Pos(S[Len], AChars) > 0) do
    Dec(Len);
  Ofs := 1;
  while (Ofs <= Len) and (Pos(S[Ofs], AChars) > 0) do
    Inc(Ofs);
  Result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

function TrimLeftCharString(const S, AChars: string): string;
var
  I, L:integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (Pos(S[I], AChars) > 0) do
   Inc(I);
  if I = 1 then
    Result := S
  else
    Result := Copy(S, I, L);
end;

function TrimRightCharString(const S, AChars: string): string;
var
  L: Integer;
begin
  L := Length(S);
  while (L > 0) and (Pos(S[L], AChars) > 0) do
   Dec(L);
  if L = Length(S) then
    Result := S
  else
    Result := Copy(S, 1, L);
end;

function TrimString(const S, AString: string): string;
var
  Len: Integer;
begin
  Result := S;
  Len := Length(AString);
  if str_StartsStr(AString, Result) then
    System.Delete(Result, 1, Len);
  if str_EndsStr(AString, Result) then
    SetLength(Result, Length(Result) - Len);
end;

function TrimLeft_IncPos(var S: string; var iPos: Integer;
  const ATrimChars: string; const AIgnoreBlankChars: Boolean): Boolean;
var
  ch: Char;

  function _CheckTrimChars: Boolean;
  begin
    if ATrimChars <> '' then
      begin
        Result := Pos(ch, ATrimChars) > 0;
        if Result or AIgnoreBlankChars then
          Exit;
      end;
    Result := ch <= #32;
  end;

var
  N, N1: Integer;
begin
  Result := False;
  N := Length(S);
  if N = 0 then Exit;
  N1 := 0;
  for N := 1 to N do
    begin
      ch := S[N];
      if _CheckTrimChars then
        Inc(N1)
      else
        Break;
    end;

  if N1 > 0 then
    begin
      Inc(iPos, N1);
      System.Delete(S, 1, N1);
    end;
  Result := Length(S) > 0;
end;

function str_QuotedChar(const S: string; const AStart: Char; AEnd: Char;
  const AForce: Boolean): string;
var
  Len: Integer;
begin
  if AEnd = #0 then
    AEnd := AStart;
  Len := Length(S);
  if Len = 0 then
    Result := AStart + AEnd
  else if AForce then
    Result := AStart + S + AEnd
  else
    begin
      Result := S;
      if S[1] <> AStart then
        Result := AStart + Result;
      if (Len = 1) or (S[Len] <> AEnd) then
        Result := Result + AEnd;
    end;
end;

function str_QuotedCharIfSpace(const S: string; const AQuot: Char): string;

begin
  if Pos(' ', S) > 0 then
    Result := str_QuotedChar(S, AQuot, AQuot, True)
  else
    Result := S;
end;

function str_Quoted(const S, AQuot: string; const AForce: Boolean): string;

begin
  if S = '' then
    Result := AQuot + AQuot
  else if AForce then
    Result := AQuot + S + AQuot
  else
    begin
      Result := S;
      if not str_StartsStr(AQuot, S) then
        Result := AQuot + Result;
      if not str_EndsStr(AQuot, S) then
        Result := Result + AQuot;
    end;
end;

function str_QuotedIfSpace(const S, AQuot: string): string;
begin
  if Pos(' ', S) > 0 then
    Result := str_Quoted(S, AQuot, True)
  else
    Result := S;
end;

function StartsText(const ASubText, AText: string): Boolean;
begin
  Result := CompareStr(UpperCase(ASubText),
      UpperCase(Copy(AText, 1, Length(ASubText)))) = 0;
end;

function EndsText(const ASubText, AText: string): Boolean;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(AText);
  Len2 := Length(ASubText);
  Result := (Len1 >= Len2) and (CompareStr(UpperCase(ASubText),
        UpperCase(Copy(AText, Len1 - Len2 + 1, Len2))) = 0);
end;

function StartsStr(const ASubText, AText: string): Boolean;
begin
  Result := CompareStr(ASubText, Copy(AText, 1, Length(ASubText))) = 0;
end;

function EndsStr(const ASubText, AText: string): Boolean;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(AText);
  Len2 := Length(ASubText);
  Result := (Len1 >= Len2) and (CompareStr(ASubText,
        Copy(AText, Len1 - Len2 + 1, Len2)) = 0);
end;

function PosR(const SubStr, Str: string): Integer;
var
  i, n, Len: Integer;
  ch: AnsiChar;
begin
  Result := 0;
  Len := Length(SubStr);
  if Len = 0 then Exit;
  n := Len;
  ch := SubStr[n];
  for i := Length(Str) downto 1 do
    begin
      if Str[i] = ch then
        begin
          if n = 1 then
            begin
              Result := i;
              Exit;
            end
          else
            begin
              Dec(n);
              ch := SubStr[n];
            end;
        end
      else if n < Len then
        begin
          n := Len;
          ch := SubStr[n];
        end;
    end;
end;

function PosRChar(ch: Char; const Str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(Str) downto 1 do
    if Str[i] = ch then
      begin
        Result := i;
        Exit;
      end;
end;

function PosSet(const ASet: TCharsSet; const S: string; AIndex: Integer): Integer;
var
  i: Integer;
begin
  if AIndex < 1 then AIndex := 1;
  for i := AIndex to Length(S) do
    if S[i] in ASet then
      begin
        Result := i;
        Exit;
      end;
  Result := 0;
end;

function PosRSet(const ASet: TCharsSet; const Str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Str) do
    if Str[i] in ASet then
      begin
        Result := i;
        Exit;
      end;
end;

function IsInteger(const S: string): Boolean;
begin
  Result := str_CharsInSet(S, ['0'..'9']);
end;

function IsNumber(const S: string): Boolean;
var
  i: integer;
begin
  Result := TryStrToInt(S, i);
end;

function IsNumber(const ch: Char): Boolean;
begin
  Result := (ch in ['0'..'9']);
end;

function IsNumberWithDots(const S: string): Boolean;
begin
  Result := str_CharsInSet(S, ['0'..'9', '.']);
end;

function str_CharsInSet(const S: string; const AChars: TCharsSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  if S = '' then Exit;
  for i := 1 to Length(S) do
    if not (S[i] in AChars) then Exit;
  Result := True;
end;

function IsCharUpper(const AChar: Char): Boolean;
begin
 Result := (Ord(AChar) > 64) and (Ord(AChar) < 91);
end;

function IsCharLower(const AChar: Char): Boolean;
begin
  Result := (Ord(AChar) > 96) and (Ord(AChar) < 123);
end;

function IsCharLetter(const AChar: Char): Boolean;
begin
  Result := IsCharUpper(AChar) or IsCharLower(AChar);
end;

function IsCharLetterOrNumber(const AChar: Char): Boolean;
begin
  Result := IsCharLetter(AChar) or IsCharNumber(AChar);
end;

function str_AddPrefix(const S, APrefix: string; CaseSens: Boolean): string;
var
  IsAdd: Boolean;
begin
  Result := S;
  if CaseSens then
    IsAdd := not str_StartsStr(APrefix, S)
  else
    IsAdd := not str_StartsText(APrefix, S);
  if IsAdd then
    Result := APrefix + Result;
end;

function str_AddSufix(const S, ASuffix: string; CaseSens: Boolean): string;
var
  IsAdd: Boolean;
begin
  Result := S;
  if CaseSens then
    IsAdd := not str_EndsStr(ASuffix, S)
  else
    IsAdd := not str_EndsText(ASuffix, S);
  if IsAdd then
    Result := Result + ASuffix;
end;

function str_RemovePrefix(const S, APrefix: string; CaseSens: Boolean): string;
var
  IsRemove: Boolean;
begin
  Result := S;
  if CaseSens then
    IsRemove := str_StartsStr(APrefix, S)
  else
    IsRemove := str_StartsText(APrefix, S);
  if IsRemove then
    System.Delete(Result, 1, Length(APrefix));
end;

function str_RemoveSufix(const S, ASuffix: string; CaseSens: Boolean): string;
var
  IsRemove: Boolean;
begin
  Result := S;
  if CaseSens then
    IsRemove := str_EndsStr(ASuffix, S)
  else
    IsRemove := str_EndsText(ASuffix, S);
  if IsRemove then
    Result := Copy(Result, 1, Length(Result) - Length(ASuffix));
end;

function str_StartsStr(const ASubText, AText: string): Boolean;
begin
  Result := StartsStr(ASubText, AText);
end;

function str_EndsStr(const ASubText, AText: string): Boolean;
begin
  Result := EndsStr(ASubText, AText);
end;

function str_StartsText(const ASubText, AText: string): Boolean;
begin
  Result := CompareStr(str_LowerCase(ASubText),
      str_LowerCase(Copy(AText, 1, Length(ASubText)))) = 0;
end;

function str_EndsText(const ASubText, AText: string): Boolean;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(AText);
  Len2 := Length(ASubText);
  Result := (Len1 >= Len2) and (CompareStr(str_LowerCase(ASubText),
        str_LowerCase(Copy(AText, Len1 - Len2 + 1, Len2))) = 0);
end;

function str_LowerCase(const s: string): string;
begin
  Result := UTF8LowerCase(s);
end;

function str_UpperCase(const s: string): string;
begin
  Result := UTF8UpperCase(s);
end;

function str_CompareStr(const S1, S2: string): Integer;
begin
  Result := UTF8CompareStr(S1, S2);
end;

function str_CompareText(const S1, S2: string): Integer;
begin
  Result := UTF8CompareText(S1, S2);
end;

function _GetOrdDefaultSort(const AOpts: TCompareStringExOptions; const ch: Char): Byte;
begin  //base\str_CompareExt.txt
  Result := Ord(ch);
  if ch < '!' then
    Exit
  else
    case ch of
      '!'..'-':
        Inc(Result, 1);
      '.':
        Dec(Result, 13);
      '0'..'9':
        Inc(Result, 17);
      ':'..'@':
        Dec(Result, 10);
      'A'..'Z':
        if (cseoIgnoreCase in AOpts) then
          Inc(Result, 36)
        else
          Inc(Result, 10);
      '['..'`':
        Dec(Result, 36);
      'a'..'z':
        Inc(Result, 4);
      '{'..'~':
        Dec(Result, 62);
    end;
end;

function _GetOrdWordSort(const AOpts: TCompareStringExOptions; const ch: Char): Byte;
begin  //base\str_CompareExt.txt
  Result := Ord(ch);
  if ch < '!' then
    Exit
  else
    case ch of
      '!'..'&':
        Inc(Result, 1);
      '''':
        Inc(Result, 86);
      //'('..',':
      //  Dec(Result, 1);
      '-':
        Inc(Result, 81);
      '.':
        Dec(Result, 13);
      '/':
        Dec(Result, 2);
      '0'..'9':
        Inc(Result, 15);
      ':'..'@':
        Dec(Result, 12);
      'A'..'Z':
        if (cseoIgnoreCase in AOpts) then
          Inc(Result, 34)
        else
          Inc(Result, 8);
      '['..'`':
        Dec(Result, 38);
      'a'..'z':
        Inc(Result, 2);
      '{'..'~':
        Dec(Result, 64);
    end;
end;

function pch_CompareEx(S1, S2: PChar; Len1, Len2: Integer;
    const AOpts: TCompareStringExOptions = []): Integer;
type
  TGetOrdFunc = function(const AOpts: TCompareStringExOptions; const ch: Char): Byte;
var
  Offset, Count: Integer;

  function _CountDigits(pch: PChar): Integer;
  begin
    Result := 0;
    while pch^ in ['0'..'9'] do
      begin
        Inc(Result);
        Inc(pch);
      end;
  end;

  function _GetLetterOrd(const ch: Char): Byte;
  begin
    if (cseoIgnoreCase in AOpts) and (ch in ['A'..'Z']) then
      Result := Ord(ch) + 32
    else
      Result := Ord(ch);
  end;

  function _CompareNumbers(pch1, pch2: PChar): Integer;
  var
    Count1: Integer = 0;
    Count2: Integer = 0;
  begin
    while (pch1^ = '0') and ((pch1 + 1)^ in ['0'..'9']) do
      begin
        Inc(Count1);
        Inc(pch1);
      end;
    while (pch2^ = '0') and ((pch2 + 1)^ in ['0'..'9']) do
      begin
        Inc(Count2);
        Inc(pch2);
      end;
    Dec(Count, Max(Count1, Count2));
    Inc(Offset, Min(Count1, Count2));
    //Inc(pch1, Count1);
    //Inc(pch2, Count2);

    Count1 := _CountDigits(pch1);
    Count2 := _CountDigits(pch2);
    Result := Count1 - Count2;
    if Result > 0 then
      Exit(1)
    else if Result < 0 then
      Exit(-1);

    //Count1 = Count2
    for Count1 := 1 to Count2 do
      begin
        Result := Ord(pch1^) - Ord(pch2^);
        if Result > 0 then
          Exit(1)
        else if Result < 0 then
          Exit(-1);
        Inc(pch1); Inc(pch2);
      end;

    Inc(Offset, Count2);
  end;

var
  n, CL1, CL2: Integer;
  W1, W2: WideString;
  _GetOrd: TGetOrdFunc;
begin
  Result := 0;
  Count := Min(Len1, Len2);
  if cseoWordSort in AOpts then
    _GetOrd := @_GetOrdWordSort
  else
    _GetOrd := @_GetOrdDefaultSort;

  //ToDo cseoDigitAsNumbers;
  Offset := 0;
  if (Count > 0) then
    while (Offset < Count) do
      begin
        CL1 := {%H-}UTF8CodepointSize(S1);
        CL2 := {%H-}UTF8CodepointSize(S2);
        if CL1 <> CL2 then
          begin
            if CL1 > CL2 then
              Exit(1)
            else
              Exit(-1);
          end;

        if (CL1 = 1) then
          begin
            if (cseoDigitAsNumbers in AOpts) and
                (S1^ in ['0'..'9']) and (S2^ in ['0'..'9']) then
              begin
                Result := _CompareNumbers(S1, S2);
                if Result <> 0 then
                  Exit;
              end;
            if (_GetLetterOrd(S1^) <> _GetLetterOrd(S2^)) then
              begin
                if _GetOrd(AOpts, S1^) > _GetOrd(AOpts, S2^) then
                  Exit(1)
                else
                  Exit(-1);
              end;
            Inc(S1); Inc(S2); Inc(Offset);
          end
        else if cseoIgnoreCase in AOpts then
          begin
            W1 := WideUpperCase(Utf8ToUtf16(S1, CL1));
            W2 := WideUpperCase(Utf8ToUtf16(S2, CL2));
            Result := WideCompareStr(W1, W2);
            if Result <> 0 then
              Exit;
            Inc(S1, CL1); Inc(S2, CL1); Inc(Offset, CL1);
          end
        else
          begin
            for n := 0 to CL1 - 1 do
              begin
                Result := byte(S1^) - byte(S2^);
                if Result > 0 then
                  Exit(1)
                else if Result < 0 then
                  Exit(-1);
                Inc(S1); Inc(S2);
              end;
            Inc(Offset, CL1);
          end;
      end;

  //if (Offset < Count) then
  //  begin
  //    //Fallback result
  //    Result := byte(S1^) - byte(S2^);
  //    if (Result < 0) then
  //      Result := -2
  //    else
  //      Result := 2;
  //    //writeln('UCS: FallBack Result = ',Result);
  //    //Try t find start of valid UTF8 codepoints
  //    if (not Utf8TryFindCodepointStart(Org1, S1, CL1)) or
  //        not Utf8TryFindCodepointStart(Org2, S2, CL2) then
  //      Exit;
  //
  //    W1 := Utf8ToUtf16(S1, CL1);
  //    W2 := Utf8ToUtf16(S2, CL2);
  //    Result := WideCompareStr(W1, W2);
  //  end
  //else
  //  //Strings are the same up and until size of smallest one
  //  Result := Len1 - Len2;
  //
  //if (Result > 1) then
  //  Result := 1
  //else if (Result < -1) then
  //  Result := -1;
end;

function str_CompareEx(const S1, S2: string; const AOpts: TCompareStringExOptions = []): Integer;
begin
  Result := pch_CompareEx(PChar(S1), PChar(S2), Length(S1), Length(S2), AOpts);
end;

function str_SameStr(const S1, S2: string): Boolean;
begin
  Result := UTF8CompareStr(S1, S2) = 0;
end;

function str_SameText(const S1, S2: string): Boolean;
begin
  Result :=UTF8CompareText(S1, S2) = 0;
end;

function SameStr(const S1, S2: string): Boolean;
begin
  Result := CompareStr(S1, S2) = 0;
end;

function str_CopyBetween(const S: string; const AStart, AEnd: Integer;
  AIsIncludeEndChar: Boolean): string;
var
  iCount: Integer;
begin
  iCount := AEnd - AStart;
  if AIsIncludeEndChar then
    Inc(iCount);
  Result := Copy(S, AStart, iCount);
end;

function str_DefineLineBreak(const S: string;
  const ADefault: TTextLineBreakStyle): TTextLineBreakStyle;
var
  pc: PChar;
begin
  Result := ADefault;
  if (S = '') then Exit;
  pc := PChar(S);
  while pc^ <> #0 do begin
    if pc^ in [#10,#13] then
      begin
        if pc^ = #10 then Result := tlbsLF
        else if (pc + 1)^ = #10 then Result := tlbsCRLF
        else Result := tlbsCR;
        Exit;
      end;
    Inc(pc);
  end;
end;

function str_LCopy(const Source: PChar; const MaxLen: Integer): string;
begin
  if MaxLen <= 0 then
    Result := ''
  else
    begin
      SetLength(Result, MaxLen);
      StrLCopy(PChar(Result), Source,  MaxLen);
      Result := PChar(Result);
    end;
end;

function str_LCopy(const Source: PWideChar; MaxLen: Integer): WideString;
begin
  SetLength(Result{%H-}, MaxLen);
  StrLCopy(PWideChar(Result), Source,  MaxLen);
  Result := PWideChar(Result);
end;

procedure str_SplitByChar(const S: string; AChar: Char; out arr: TStringDynArray;
  const AIsTrim: Boolean);
begin
  pch_SplitByChars(PChar(S), [AChar], arr, AIsTrim);
end;

function str_SplitAccordingChars(const S, AChars: string; out arr: TStringArray;
  const AIsTrim: Boolean): Boolean;
var
  i: Integer;
  n: Integer = 0;
  iStart, iEnd: Integer;
  T: TTrimStringFunc;
begin
  if Length(AChars) = 0 then Exit(False);
  Result := True;
  SetLength(arr{%H-}, Length(AChars) + 1);
  T := _GetTrimManager(AIsTrim).Trim;
  iStart := 1;
  for i := 1 to Length(AChars) do
    begin
      iEnd := PosEx(AChars[i], S, iStart);
      if iEnd = 0 then
        Exit(False)
      else
        begin
          arr[n] := T(str_CopyBetween(S, iStart, iEnd, False));
          Inc(n);
          iStart := iEnd + 1;
        end;
    end;
  arr[n] := T(Copy(S, iStart, MaxInt));
end;

procedure str_SplitByChars(const S: string; AChars: TCharsSet; out
  arr: TStringArray; const AIsTrim: Boolean);
begin
  pch_SplitByChars(PChar(S), AChars, arr, AIsTrim);
end;

procedure pch_SplitByChars(pc: PChar; const AChars: TSysCharSet; out
  arr: TStringDynArray; const AIsTrim: Boolean);
var
  pcStart: PChar;

  procedure _Add;
  var
    Len: Integer;
    pcEnd: PChar;
  begin
    pcEnd := pc;
    if AIsTrim then
      begin
        while pcStart^ in [#1..#32] do
          Inc(pcStart);
        while (pcEnd - 1)^ in [#1..#32] do
          Dec(pcEnd);
      end;
    Len := Length(arr);
    SetLength(arr, Len + 1);
    arr[Len] := str_LCopy(pcStart, pcEnd - pcStart);
    pcStart := pc + 1;
  end;

begin
  if pc = nil then
    begin
      SetLength(arr, 1);
      Exit;
    end;

  pcStart := pc;
  while pc^ <> #0 do
    begin
      if pc^ in AChars then
        _Add;
      Inc(pc);
    end;
  _Add;
end;

function pch_GetStrBetween(const pch1, pch2: PChar): string;
begin
  Result := str_LCopy(pch1, pch2 - pch1);
end;

procedure str_AddLine(var S: string; const ALine: string;
  const ALineBreak: string);
var
  S1: string;
begin
  if S = '' then
    S := ALine
  else if ALine <> '' then
    begin
      if str_EndsText(ALineBreak, S) then S1 := ''
      else S1 := ALineBreak;
      S := S + S1 + ALine;
    end;
end;

procedure str_AddLine(var S: string; const ALine: string; const ALineBreak: Char
  );
var
  S1: string;
begin
  if S = '' then
    S := ALine
  else if ALine <> '' then
    begin
      if str_LastChar(S) = ALineBreak then S1 := ''
      else S1 := ALineBreak;
      S := S + S1 + ALine;
    end;
end;

function str_AddedLine(const S, ALine: string; const ALineBreak: string
  ): string;
begin
  Result := S;
  str_AddLine(Result, ALine, ALineBreak);
end;

function str_AddedLine(const S, ALine: string; const ALineBreak: Char): string;
begin
  Result := S;
  str_AddLine(Result, ALine, ALineBreak);
end;

function str_FirstChar(const S: string): Char;
begin
  if S <> '' then
    Result := S[1]
  else
    Result := #0;
end;

function str_LastChar(const S: string): Char;
begin
  if S <> '' then
    Result := S[Length(S)]
  else
    Result := #0;
end;

function str_Char(const S: string; AIndex: Integer): Char;
begin
  if (AIndex > 0) and (AIndex <= Length(S)) then
    Result := S[AIndex]
  else
    Result := #0;
end;

function str_CharR(const S: string; AIndex: Integer): Char;
var
  Len: Integer;
begin
  Len := Length(S);
  AIndex := Len - AIndex + 1;
  if (AIndex > 0) and (AIndex <= Len) then
    Result := S[AIndex]
  else
    Result := #0;
end;

function str_RandomChars(const Len: Integer; const IsUpperCase: Boolean): string;
const
  S: string = 'qwertyuiopasdfghjklzxcvbnm_0987654321';  // 37;
var
  i, n: Byte;
begin
  SetLength(Result{%H-}, Len);
  Randomize;
  for i := 1 to Len do
    begin
      n := Integer(Random(36)) + 1;
      if IsUpperCase then
        Result[i] := UpCase(S[n])
      else
        Result[i] := S[n];
    end;
end;

function _TryCreateFileStream(const AFileName: string; const AMode: Word;
      out AStream: TFileStreamUTF8; AMSecWait: Cardinal): Boolean;

  procedure _Create;
  begin
    try
      AStream := TFileStreamUTF8.Create(AFileName, AMode);
    except
      AStream := nil;
    end;
  end;

var
  iTick: QWord;
begin
  iTick := GetTickCount + AMSecWait;
  while iTick > GetTickCount do
    begin
      _Create;
      if AStream <> nil then Exit(True);
      Sleep(1);
    end;
  Result := False;
end;

function str_Replace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string; overload;
var
  Srch,OldP,RemS: string; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=str_LowerCase(Srch);
    OldP:=str_LowerCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

procedure str_Replace(const AReplace: string; var S: string; AStart, ACount: Integer); overload;
begin
  UTF8Delete(S, AStart, ACount);
  S := str_Insert(AReplace, S, AStart);//UTF8Insert(AReplace, S, AStart);
end;

function str_Insert(const AInsert, S: string; AIndex: Integer): string;
begin
  if AIndex > Length(S) then
    Result := S + AInsert
  else
    begin
      Result := S;
      UTF8Insert(AInsert, Result, AIndex);
    end;
end;

function str_TryLoadFromFile(const AFileName: string; out S: string;
  AIsShowError: Boolean): Boolean;
var
  iSize: Int64;
  st: TFileStreamUTF8 = nil;
begin
  Result := False;
  S := '';
  if not file_Exists(AFileName) then Exit;

  if _TryCreateFileStream(AFileName, fmOpenRead or fmShareDenyWrite, st, 1000) then
    try
      try
        iSize := st.Size;
        SetLength(S, iSize);
        st.Read(Pointer(S)^, iSize);
        Result := True;
      except
        S := '';
      end;
    finally
      st.Free;
    end;
  //try
  //  try
  //    st := TFileStreamUTF8.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  //    iSize := st.Size;
  //    SetLength(S, iSize);
  //    st.Read(Pointer(S)^, iSize);
  //    Result := True;
  //  except
  //    S := '';
  //  end;
  //finally
  //  st.Free;
  //end;
  if not Result and AIsShowError then
    ShowError(Format(RS_ErrFileRead, [AFileName]));
end;

function str_LoadFromFile(const AFileName: string;
  const AIsAdjustLineBreaks: Boolean): string;
begin
  str_TryLoadFromFile(AFileName, Result, False);
  if AIsAdjustLineBreaks then
    Result := AdjustLineBreaks(Result, str_DefineLineBreak(Result));
end;

function str_LoadFromFileEx(var AEncoding: string; const AFileName: string;
  const AIsAdjustLineBreaks: Boolean): string;
begin
  Result := str_LoadFromFile(AFileName, AIsAdjustLineBreaks);
  if AEncoding = '' then
    begin
      AEncoding := NormalizeEncoding(GuessEncoding(Result));
      if AEncoding = '' then
        AEncoding := EncodingUTF8;
    end;
  Result := RemoveEncodingBOM(AEncoding, Result);
  if not StartsStr(EncodingUTF8, AEncoding) then
    Result := ConvertEncoding(Result, AEncoding, EncodingUTF8);
end;

function str_LoadUtf8FromFile(const AFileName: string;
  const AIsAdjustLineBreaks: Boolean): string;
var
  Encoding: string = '';
begin
  Result := str_LoadFromFileEx(Encoding, AFileName, AIsAdjustLineBreaks);
end;

function str_SaveToFile(const AFileName: string; const S: string;
  const AIsShowError: Boolean): Boolean;
var
  st: TFileStreamUTF8 = nil;
begin
  Result := False;
  if not dir_Force(file_ExtractDir(AFileName), AIsShowError) then Exit;

  if _TryCreateFileStream(AFileName, fmCreate, st, 50) then
    try
      try
        st.Write(Pointer(S)^, Length(S));
        Result := True;
      except
        Result := False;
      end;
    finally
      st.Free;
    end;

  if not Result and AIsShowError then
    ShowError(Format(RS_ErrFileWrite, [AFileName]));
end;

function str_SaveToFileEx(const AFileName: string; const S, AEncoding: string;
  AIsShowError: Boolean): Boolean;
var
  sText: String;
begin
  sText := S;
  if not StartsStr(EncodingUTF8, AEncoding) then
    sText := ConvertEncoding(sText, EncodingUTF8, AEncoding);
  sText := AddEncodingBOM(AEncoding, sText);
  Result := str_SaveToFile(AFileName, sText, AIsShowError);
end;

function str_LoadFromFileAsInt(const AFileName: string; const ADefault: Integer): Integer;
begin
  if not TryStrToInt(Trim(str_LoadFromFile(AFileName)), Result) then
    Result := ADefault;
end;

function str_AddToFile(const AFileName: string; const S: string; AsLine: Boolean): Boolean;
var
  Mode: Integer;
  sBreak: string;
begin
  Result := False;
  if not dir_Force(file_ExtractDir(AFileName)) then Exit;

  try
    if file_Exists(AFileName) then
      Mode := fmOpenWrite
    else
      Mode := fmCreate;
    with TFileStreamUTF8.Create(AFileName, Mode) do
      try
        Seek(0, soEnd);
        Write(Pointer(S)^, Length(S));
        if AsLine then
          begin
            sBreak := sLineBreak;
            Write(PChar(sBreak)^, Length(sBreak));
          end;
        Result := True;
      finally
        Free;
      end;
  except
    Result := False;
  end;
end;

function WideLoadFromFile(const AFileName: string): WideString;
var
  iSize: Int64;
  st: TFileStreamUTF8 = nil;
begin
  Result := '';
  if not file_Exists(AFileName) then Exit;

  try
    try
      st := TFileStreamUTF8.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      iSize := st.Size div SizeOf(WideChar);
      SetLength(Result, iSize);
      st.Read(Pointer(Result)^, iSize);
    except
      Result := '';
    end;
  finally
    st.Free;
  end;
  //if not Result and AIsShowError then
  //  ShowError(Format(S_ErrFileRead, [AFileName]));
end;

function WideSaveToFile(const AFileName: string; const S: WideString;
  const AIsShowError: Boolean): Boolean;
var
  st: TFileStreamUTF8 = nil;
begin
  Result := False;
  if not dir_Force(file_ExtractDir(AFileName), AIsShowError) then Exit;

  try
    try
      st := TFileStreamUTF8.Create(AFileName, fmCreate);
      st.Write(Pointer(S)^, Length(S) * SizeOf(WideChar));
      Result := True;
    except
      Result := False;
    end;
  finally
    st.Free;
  end;
  if not Result and AIsShowError then
    ShowError(Format(RS_ErrFileWrite, [AFileName]));
end;

function str_RemoveEmptyChars(const S: string): string;
var
  Len, i: Integer;
begin
  SetLength(Result{%H-}, Length(S));
  Len := 0;
  for i := 1 to Length(S) do
    if (S[i] > #32) then
      begin
        Inc(Len);
        Result[Len] := S[i];
      end;
  SetLength(Result, Len);
end;

function IncludeTrailingUnixSlash(const APath: string): string;
var
  L: Integer;
begin
  Result := APath;
  L := Length(Result);
  if (L=0) or (Result[L] <> '/') then
    Result := Result + '/';
end;

function ExcludeTrailingUnixSlash(const APath: string): string;
var
  L: Integer;
begin
  L := Length(APath);
  if (L > 0) and (APath[L] = '/') then
    Dec(L);
  Result := Copy(APath, 1, L);
end;

function IncludeLeadingUnixSlash(const APath: string): string;
var
  L: Integer;
begin
  Result := APath;
  L := Length(Result);
  if (L=0) or (Result[1] <> '/') then
    Result := '/' + Result;
end;

function ExcludeLeadingUnixSlash(const APath: string): string;
var
  L: Integer;
begin
  Result := APath;
  L := Length(Result);
  if (L > 0) and (Result[1] = '/') then
    Delete(Result, 1, 1);
end;

function str_ToUnixSlash(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function str_ToWinSlash(const S: string): string;
begin
  Result := StringReplace(S, '/', '\', [rfReplaceAll]);
end;

function str_ToSysSlash(const S: string): string;
begin
{$IFDEF WINDOWS}
  Result := str_ToWinSlash(S);
{$ELSE}
  Result := str_ToUnixSlash(S);
{$ENDIF}
end;


function str_IsUTF8(const S: string): Boolean;
begin
  Result := GuessEncoding(S) = EncodingUTF8;
end;

function utf8_HasBOM(S : TStream) : boolean; overload;
var
  SavePos : Int64;
  Buf : array[1..3] of char;
begin
  SavePos := S.Position;
  Result := False;
  try
    S.Seek(0, soBeginning);
    if S.Read(Buf{%H-}, 3) = 3 then
      Result := (Buf[1] = UTF8BOM[1])
        and (Buf[2] = UTF8BOM[2])
        and (Buf[3] = UTF8BOM[3]);
  finally
    S.Position := SavePos;
  end;
end;

function utf8_HasBOM(S : string): boolean; overload;
begin
  Result := False;
  if Length(S) < 3 then exit ;
  if S[1] <> UTF8BOM[1] then exit;
  if S[2] <> UTF8BOM[2] then exit;
  if S[3] <> UTF8BOM[3] then exit;
  Result := True;
end;

function AddEncodingBOM(const AEncoding, AText: string): string;
begin
  if (AEncoding = EncodingUCS2LE) then
    Result := str_AddPrefix(AText, UTF16LEBOM)
  else if (AEncoding = EncodingUCS2BE) then
    Result := str_AddPrefix(AText, UTF16BEBOM)
  else if (AEncoding = EncodingUTF8BOM) then
    Result := str_AddPrefix(AText, UTF8BOM)
  else
    Result := AText;
end;

function RemoveEncodingBOM(const AEncoding, AText: string): string;
begin
  Result := AText;
  if (AEncoding = EncodingUCS2LE) then
    Result := str_RemovePrefix(Result, UTF16LEBOM)
  else if (AEncoding = EncodingUCS2BE) then
    Result := str_RemovePrefix(Result, UTF16BEBOM)
  else if (AEncoding = EncodingUTF8BOM) then
    Result := str_RemovePrefix(Result, UTF8BOM);
end;

function utf_8To16(const S: string): UnicodeString;
begin
  Result := UTF8ToUTF16(S);
end;

function utf_16To8(const S: UnicodeString): string;
begin
  Result := UTF16ToUTF8(S);
end;

type
  { TNextDelimStingGetter }

  TNextDelimStingGetter = class(TInterfacedObject, INextDelimStringGetter)
  private
    FDelimCurrPos, FDelimLastPos: Integer;
    FText: string;
    FDelim: string;
    FCurrDelimPC: PChar;
    FQuoteChars: string;
    FOptions: TNextDelimStingGetterOptions;
  protected
    { INextDelimStringGetter }
    function Next(out S: string): Boolean;
    function LastPos: Integer;
    function CurrPos: Integer;
  public
    constructor Create(const S, ADelim: string;
      const AOptions: TNextDelimStingGetterOptions; const AQuoteChars: string);
  end;

  //TStringBuilderData = record
  //  Data: TByteDynArray;
  //  Count: Cardinal;
  //end;
  //PStringBuilderData = ^TStringBuilderData;

  { TStringBuilder }

  TStringBuilder = class(TInterfacedObject, IFPStringBuilder)
  private
    FBlockLen: Integer;
    FTailLength: Integer;
    FTail: string;
    FList: TStringList;
    function GetCount: Integer;
  public
    constructor Create(const ABlockLen: Integer = 1000);
    destructor Destroy; override;

    procedure Add(const S: string); overload;
    procedure Add(const AStrings: array of string); overload;
    function ToString: string; override;
    procedure Clear;
    property Count: Integer read GetCount;
  end;

{ TNextDelimStingGetter }

function TNextDelimStingGetter.Next(out S: string): Boolean;

  function _StrPos(str, sub: PChar): PChar;
  var
    lsub : SizeInt;
    ch: Char;
    IsQout: Boolean = False;
  begin
    Result := nil;
    if (str = nil) then
     Exit;
    lsub := strlen(sub);
    ch := str^;
    while ch <> #0 do
      begin
        if IsQout then
          begin
            if ch = '"' then
              IsQout := False;
          end
        else if strlcomp(str, sub, lsub) = 0 then
          begin
             Result := str;
             Exit;
          end
        else if ch = '"' then
          IsQout := True;
        Inc(str);
        ch := str^;
      end;
  end;

var
  pc, DelimPC0: PChar;
  i: Integer;
begin
  Result := False;
  S := '';
  if not Assigned(FCurrDelimPC) or
      (FCurrDelimPC^ = #0) then Exit;
  if FDelim = '' then
    begin
      FCurrDelimPC := nil;
      S := FText;
      Exit(True);
    end;
  DelimPC0 := PChar(FDelim);
  pc := FCurrDelimPC;
  if ndsoStrict in FOptions then
    FCurrDelimPC := StrPos(FCurrDelimPC, DelimPC0)
  else
    FCurrDelimPC := _StrPos(FCurrDelimPC, DelimPC0);
  if not Assigned(FCurrDelimPC) then
    i := StrLen(pc)
  else
    begin
      i := FCurrDelimPC - pc;
      Inc(FCurrDelimPC, Length(FDelim));
    end;
  FDelimLastPos := FDelimCurrPos;
  Inc(FDelimCurrPos, i + Length(FDelim));
  if i = 0 then
    begin
      //if (DelimPC0^ = #0) then
      //  FCurrDelimPC := nil
      //else
        Result := True;
    end
  else
    begin
      Result := True;
      SetLength(S, i);
      StrLCopy(PChar(S), pc, i);
    end;
  if Result {and (FOptions <> [])} then
    begin
      if (ndsoTrimOnlySpace in FOptions) then
        begin
          TrimLeft_IncPos(S, FDelimLastPos, ' '#9, True);
          S := TrimRightSet(S, [' ', #9]);
        end
      else if (ndsoTrim in FOptions) then
        begin
          TrimLeft_IncPos(S, FDelimLastPos);
          S := TrimRight(S);
        end;
      if (ndsoDeqoute in FOptions) then
        begin
          TrimLeft_IncPos(S, FDelimLastPos, FQuoteChars);
          S := TrimRightCharString(S, FQuoteChars);
        end;
      if (S = '') and (ndsoSkipEmpty in FOptions) then
        Result := Next(S);
    end
  //new:6.7 >>>
  //else if S = '' then
  //  Result := False
  ;
end;

function TNextDelimStingGetter.LastPos: Integer;
begin
   Result := FDelimLastPos;
end;

function TNextDelimStingGetter.CurrPos: Integer;
begin
   Result := FDelimCurrPos;
end;

constructor TNextDelimStingGetter.Create(const S, ADelim: string;
  const AOptions: TNextDelimStingGetterOptions; const AQuoteChars: string);
begin
  FDelimCurrPos := 0;
  FDelimLastPos := 0;
  FText := S;
  FCurrDelimPC := PChar(FText);
  FDelim := ADelim;
  FQuoteChars := AQuoteChars;
  FOptions := AOptions;
end;


function New_NextDelimStringGetter(const S, ADelim: string;
  const AOptions: TNextDelimStingGetterOptions; const AQuoteChars: string
  ): INextDelimStringGetter;
begin
  Result := TNextDelimStingGetter.Create(S, ADelim, AOptions, AQuoteChars);
end;

function new_StringBuilder(const ABlockLen: Integer): IFPStringBuilder;
begin
  Result := TStringBuilder.Create(ABlockLen);
end;

{ TStringBuilder }

function TStringBuilder.GetCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    Inc(Result, Length(FList[i]));
  Inc(Result, FTailLength);
end;

constructor TStringBuilder.Create(const ABlockLen: Integer);
begin
  FBlockLen := ABlockLen;
  FList := TStringList.Create;
end;

destructor TStringBuilder.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TStringBuilder.Add(const S: string);
var
  Len: Integer;
begin
  Len := Length(S);
  if Len = 0 then Exit;

  if (FTailLength + Len) >= FBlockLen then
    begin
      if FTailLength = 0 then
        FList.Add(S)
      else
        begin
          SetLength(FTail, FTailLength + Len);
          Move(PChar(S)^, (PChar(FTail) + FTailLength)^, Len);
          FList.Add(FTail);
          FTailLength := 0;
          FTail := '';
        end;
    end
  else if FTailLength = 0 then
    begin
      SetLength(FTail, FBlockLen);
      Move(PChar(S)^, PChar(FTail)^, Len);
      FTailLength := Len;
    end
  else
    begin
      SetLength(FTail, FTailLength + Len);
      Move(PChar(S)^, (PChar(FTail) + FTailLength)^, Len);
      Inc(FTailLength, Len);
    end;
end;

procedure TStringBuilder.Add(const AStrings: array of string);
var
  i: Integer;
begin
  for i := 0 to Length(AStrings) - 1 do
    Add(AStrings[i]);
end;

function TStringBuilder.ToString: string;
var
  i: Integer;
  S: String;
  pc: PChar;
begin
  SetLength(Result{%H-}, Count);
  pc := PChar(Result);
  for i := 0 to FList.Count - 1 do
    begin
      S := FList[i];
      Move(PChar(S)^, pc^, Length(S));
      Inc(pc, Length(S))
    end;
  if FTailLength > 0 then
    Move(PChar(FTail)^, pc^, FTailLength);
end;

procedure TStringBuilder.Clear;
begin
  FList.Clear;
  FTail := '';
  FTailLength := 0;
end;

procedure arrS_Insert(var arr: TStringArray; AIndex: Integer; const S: string);
var
  Len: Integer;
begin
  Len := Length(arr);
  if (AIndex < 0) then AIndex := 0
  else if (AIndex > Len) then AIndex := Len;
  SetLength(arr, Len + 1);
  if AIndex < Len then
    begin
      Move(arr[AIndex], arr[AIndex + 1], SizeOf(string) * (Len - AIndex));
      FillByte(arr[AIndex], SizeOf(string), 0);
    end;
  arr[AIndex] := S;
end;

procedure arrS_Delete(var arr: TStringArray; AIndex: Integer);
var
  Len: Integer;
begin
  Len := Length(arr);
  if (AIndex < 0) or (AIndex >= Len) then Exit;
  Dec(Len);
  arr[AIndex] := '';
  if AIndex < Len then
    begin
      Move(arr[AIndex + 1], arr[AIndex], SizeOf(string) * (Len - AIndex));
      FillByte(arr[Len], SizeOf(string), 0);
    end;
  SetLength(arr, Len);
end;

function arrS_Remove(var arr: TStringArray; const S: string): Integer;
begin
  Result := arrS_IndexOf(arr, S);
  if Result <> -1 then
    arrS_Delete(arr, Result);
end;

function arrS_Add(var arr: TStringArray; const S: string): Integer;
begin
  Result := Length(arr);
  SetLength(arr, Result + 1);
  arr[Result] := S;
end;

function arrS_AddArray(var arr: TStringArray; const arr1: TStringArray;
  AIgnoreDuplicat: Boolean; ACaseSensetive: Boolean): Integer;
var
  Len, Len1: Integer;
  a: TStringArray;
  i: Integer;
  Flags: Cardinal;
begin
  Len1 := Length(arr1);
  Result := Len1;
  Len := Length(arr);
  a := arr;
  SetLength(arr, Len + Len1);
  if ACaseSensetive then Flags := F_arrS_CASESENS else Flags := 0;
  for i := 0 to Len1 - 1 do
    begin
      if AIgnoreDuplicat and
          (arrS_IndexOf(a, arr1[i], Flags) <> -1) then Continue;
      arr[Len] := arr1[i];
      Inc(Len);
    end;
  if Len < Length(arr) then
    begin
      SetLength(arr, Len);
      Result := Len - Length(a);
    end;
end;

procedure arrS_RemoveDuplicat(var arr: TStringArray);
var
  a: TStringArray;
  i, n: Integer;
begin
  SetLength(a{%H-}, 0);
  for i := 0 to Length(arr) - 1 do
    if arrS_IndexOf(a, arr[i]) = -1 then
      begin
        n := Length(a);
        SetLength(a, n + 1);
        a[n] := arr[i];
      end;
  arr := a;
end;

procedure arrS_RemoveEmpty(var arr: TStringArray);
var
  arrNew: TStringArray;
  i, n: Integer;
  S: String;
begin
  SetLength(arrNew{%H-}, Length(arr));
  n := 0;
  for i := 0 to Length(arr) - 1 do
    begin
      S := Trim(arr[i]);
      if S <> '' then
        begin
          arrNew[n] := arr[i];
          Inc(n);
        end;
    end;
  if n < Length(arr) then
    begin
      SetLength(arrNew, n);
      arr := arrNew;
    end;
end;

function arrS_FromOpenArray(const arr: array of string): TStringArray;
var
  i: Integer;
begin
  SetLength(Result{%H-}, Length(arr));
  for i := 0 to Length(arr) - 1 do
    Result[i] := arr[i];
end;

function arrS_Copy(const arr: TStringArray; AStart, AEnd: Integer
  ): TStringArray;
var
  i, Len: Integer;
begin
  SetLength(Result{%H-}, 0);
  if (Length(arr) = 0) or (AStart < 0) then Exit;
  AEnd := Min(AEnd, Length(arr) - 1);
  Len := AEnd - AStart + 1;
  if Len <= 0 then Exit;
  SetLength(Result, Len);
  for i := 0 to Len - 1 do
    begin
      Result[i] := arr[AStart];
      Inc(AStart);
    end;
end;

function arrS_Find(const arr: TStringArray; out AIndex: Integer;
  const S: string; CaseSens: Boolean): Boolean;
var
  L, H, I, C: Integer;
  S0, S1: string;
begin
  Result := False;
  if CaseSens then S0 := S else S0 := str_UpperCase(S);
  L := 0;
  H := Length(arr) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if CaseSens then S1 := arr[I]
    else S1 := str_UpperCase(arr[I]);
    C := CompareStr(S1, S0);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        AIndex := I;
        Exit;
        //if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  AIndex := L;
end;

function arrS_IndexOf(const arr: TStringArray; const S: string; F_arrS: Cardinal
  ): Integer;
var
  i, n: Integer;
begin
  if (F_arrS and F_arrS_SORTED) = 0 then
    begin
      Result := -1;
      for i := 0 to Length(arr) - 1 do
        begin
          if (F_arrS and F_arrS_FILENAME) <> 0 then
            n := file_CompareName(arr[i], S)
          else if (F_arrS and F_arrS_CASESENS) <> 0 then
            n := CompareStr(arr[i], S)
          else
            n := str_CompareText(arr[i], S);
          if n = 0 then
            begin
              Result := i;
              Exit;
            end;
        end;
    end
  else
    if not arrS_Find(arr, Result, S, (F_arrS and F_arrS_CASESENS) <> 0) then
      Result := -1;
end;

procedure arrS_SetCommaText(out arr: TStringArray; const ACommaText: string;
  ADelim: Char; F_arrS: Cardinal);
var
  strs: TStringList;
begin
  if ACommaText = '' then
    begin
      SetLength(arr{%H-}, 0);
      Exit;
    end;
  strs := TStringListUTF8.Create;
  try
    strs.Delimiter := ADelim;
    strs.StrictDelimiter := True;
    strs.CaseSensitive := (F_arrS and F_arrS_CASESENS) <> 0;
    strs.DelimitedText := ACommaText;
    if (F_arrS and F_arrS_SORTED) <> 0 then
      strs.Sort;
    arrS_LoadFromStrs(arr, strs);
  finally
    strs.Free;
  end;
end;

function arrS_GetCommaText(const arr: TStringArray; ADelim: Char;
  F_arrS: Cardinal): string;
var
  strs: TStringList;
begin
  strs := TStringListUTF8.Create;
  try
    strs.Delimiter := ADelim;
    strs.StrictDelimiter := True;
    strs.CaseSensitive := (F_arrS and F_arrS_CASESENS) <> 0;
    arrS_SaveToStrs(arr, strs);
    if (F_arrS and F_arrS_SORTED) <> 0 then
      strs.Sort;
    Result := strs.DelimitedText;
  finally
    strs.Free;
  end;
end;

procedure arrS_Sort(var arr: TStringArray; ACaseSensetive: Boolean);
var
  strs: TStringList;
begin
  strs := TStringListUTF8.Create;
  try
    arrS_SaveToStrs(arr, strs);
    strs.CaseSensitive := ACaseSensetive;
    strs.Sort;
    arrS_LoadFromStrs(arr, strs);
  finally
    strs.Free;
  end;
end;

procedure arrS_LoadFromStrs(out arr: TStringArray; const strs: TStrings);
var
  i: Integer;
begin
  i := strs.Count;
  SetLength(arr{%H-}, i);
  for i := 0 to i - 1 do
    arr[i] := strs[i];
end;

procedure arrS_SaveToStrs(const arr: TStringArray; const strs: TStrings;
  const AIndexToObject: Boolean);
var
  i: Integer;
begin
  strs.Clear;
  i := Length(arr);
  strs.Capacity := i;
  for i := 0 to i - 1 do
    strs.AddObject(arr[i], IfThen(AIndexToObject, {%H-}Pointer(i), nil));
end;

procedure arrS_SaveToStrs(const arr: array of string; const strs: TStrings;
  const AIndexToObject: Boolean);
var
  i: Integer;
begin
  strs.Clear;
  i := Length(arr);
  strs.Capacity := i;
  for i := 0 to i - 1 do
    strs.AddObject(arr[i], IfThen(AIndexToObject, {%H-}Pointer(i), nil));
end;

function arrI_IndexOf(var arr: TIntArray; const AInt: PtrInt): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(arr) - 1 do
    if arr[i] = AInt then
      begin
        Result := i;
        Exit;
      end;
end;

function arrI_IndexOfSorted(var arr: TIntArray; const AInt: PtrInt): Integer;
begin
  arrI_Find(arr, AInt, Result);
end;

procedure arrI_Insert(var arr: TIntArray; AIndex: Integer; const AInt: PtrInt);
var
  Len: Integer;
begin
  Len := Length(arr);
  if (AIndex < 0) then AIndex := 0
  else if (AIndex > Len) then AIndex := Len;
  SetLength(arr, Len + 1);
  if AIndex < Len then
    Move(arr[AIndex], arr[AIndex + 1], SIZE_INT * (Len - AIndex));
  arr[AIndex] := AInt;
end;

procedure arrI_Delete(var arr: TIntArray; const AIndex: Integer);
var
  Len: Integer;
begin
  Len := Length(arr);
  if (AIndex < 0) or (AIndex >= Len) then Exit;
  Dec(Len);
  if AIndex < Len then
    Move(arr[AIndex + 1], arr[AIndex], SIZE_INT * (Len - AIndex));
  SetLength(arr, Len);
end;

function arrI_Remove(var arr: TIntArray; const AInt: PtrInt): Integer;
begin
  Result := arrI_IndexOf(arr, AInt);
  if Result <> -1 then
    arrI_Delete(arr, Result);
end;

function arrI_RemoveSorted(var arr: TIntArray; AInt: PtrInt): Integer;
begin
  if arrI_Find(arr, AInt, Result) then
    arrI_Delete(arr, Result);
end;

function arrI_Add(var arr: TIntArray; const AInt: PtrInt): Integer;
begin
  Result := Length(arr);
  SetLength(arr, Result + 1);
  arr[Result] := AInt;
end;

function _CompareIntArrayItems(arr: TIntArray; Index1, Index2: Integer; {%H-}AData: Pointer): Integer;
begin
  Result := CompareValue(arr[Index1], arr[Index2]);
end;

procedure _arrI_QuickSort(var arr: TIntArray; L, R: Integer;
    CompareFn: TIntArraySortCompare; AData: Pointer);
var
  iL, iR : Integer;
  Mid : Integer;
//  i, j: Integer;

  procedure Swap(i1, i2: Integer);
  var
    t: Integer;
  begin
    t := arr[i1];
    arr[i1] := arr[i2];
    arr[i2] := t;
  end;

begin
  if R - L <= 1 then begin
    if L < R then
      if CompareFn(arr, L, R, AData) > 0 then
        Swap(L, R);
    Exit;
  end;

  repeat
    Mid := (L + R) div 2;
    Swap(mid, L);
    iL := L + 1;
    iR := R;
    repeat
      while (iL <= iR) and
          (CompareFn(arr, L, iL, AData) > 0) do
        Inc(iL);
      while (iR >= iL) and
          (CompareFn(arr, L, iR, AData) < 0) do
        Dec(iR);
      If iL <= iR then
        begin
          if iL < iR then
            Swap(iL, iR);
          Inc(iL);
          Dec(iR);
        end;
    until iL > iR;
    Swap(L, iR);
    if L < iR - 1 then
      _arrI_QuickSort(arr, L, iR - 1, CompareFn, AData);
    L := iR + 1;//iL;
  until L >= R;//iL >= R;
end;

function arrI_AddSorted(var arr: TIntArray; const AInt: PtrInt): Integer;
begin
  if not arrI_Find(arr, AInt, Result) then
    arrI_Insert(arr, Result, AInt);
end;

function arrI_AddUnique(var arr: TIntArray; const AInt: PtrInt): Integer;
begin
  Result := arrI_IndexOf(arr, AInt);
  if Result = -1  then
    Result := arrI_Add(arr, AInt);
end;

procedure arrI_Sort(var arr: TIntArray; CompareFn: TIntArraySortCompare; AData: Pointer);
begin
   if (arr = nil) or (Length(arr) < 2) then Exit;
   if not Assigned(CompareFn) then
     CompareFn := _CompareIntArrayItems;
  _arrI_QuickSort(arr, 0, Length(arr) - 1, CompareFn, AData);
end;

procedure arrI_Fill(var arr: TIntArray; AInt: PtrInt; const AInc: Integer);
var
  i: Integer;
begin
  for i := 0 to Length(arr) - 1 do
    begin
      arr[i] := AInt;
      Inc(AInt, AInc);
    end;
end;

function arrI_Find(var arr: TIntArray; const AInt: PtrInt; out Index: Integer
  ): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Length(arr) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareValue(arr[I], AInt);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        Index := I;
        Exit;
        //if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure arrI_AddArray(var arr: TIntArray; const arrAdding: TIntArray;
  AIsNotDublicate: Boolean);
var
  i, n, iValue: Integer;
begin
  i := Length(arr);
  SetLength(arr, i + Length(arrAdding));
  for n := 0 to Length(arrAdding) - 1 do
    begin
      iValue := arrAdding[n];
      if AIsNotDublicate and
        (arrI_IndexOf(arr, iValue) <> -1) then Continue;
      arr[i] := iValue;
      Inc(i);
    end;
  SetLength(arr, i);
end;

procedure arrI_Assign(out arr: TIntArray; const arrSource: TIntArray);
var
  i: Integer;
begin
  SetLength(arr{%H-}, Length(arrSource));
  for i := 0 to Length(arrSource) - 1 do
    arr[i] := arrSource[i];
end;

procedure arrI_Expand(var arr: TIntArray; const ACount: Integer; const AValue: PtrInt = 0);
var
  iDiff: Integer;
begin
  iDiff := ACount - Length(arr);
  if iDiff <= 0 then Exit;
  SetLength(arr, ACount);
  for iDiff := ACount - iDiff to ACount - 1 do
    arr[iDiff] := AValue;
end;

function arrI_AsJsonString(const arr: TIntArray): string;
var
  b: IFPStringBuilder;
  i: Integer;
begin
  b := new_StringBuilder();
  b.Add('[');
  for i := 0 to Length(arr) - 1 do
    begin
      if i > 0 then
        b.Add(',');
      b.Add(IntToStr(arr[i]));
    end;
  b.Add(']');
  Result := b.ToString;
end;

function arrI_GetCommaText(const arr: TIntArray): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(arr) - 1 do
    str_AddLine(Result, IntToStr(arr[i]), ',');
end;

procedure arrI_SetCommaText(out arr: TIntArray; const ACommaText: string);
var
  S: string;
  i: integer;
  Len: Integer;
  DelimStr: INextDelimStringGetter;
begin
  SetLength(arr{%H-}, 0);
  if ACommaText = '' then Exit;
  DelimStr := New_NextDelimStringGetter(ACommaText, ',', [ndsoTrim]);
  while DelimStr.Next(S) do
    begin
      if not TryStrToInt(S, i) then Continue;
      Len := Length(arr);
      SetLength(arr, Len + 1);
      arr[Len] := i;
    end;
end;

function BoolArrayToIntArray(const ABools: TBooleanDynArray): TIntArray;
var
  i: Integer;
begin
  i := Length(ABools);
  SetLength(Result{%H-}, i);
  for i := 0 to i - 1 do
    Result[i] := Integer(ABools[i]);
end;

function IntArrayToBoolArray(const arr: TIntArray): TBooleanDynArray;
var
  i: Integer;
begin
  i := Length(arr);
  SetLength(Result{%H-}, i);
  for i := 0 to i - 1 do
    Result[i] := arr[i] <> 0;
end;

function ByteArrayToIntArray(const ABytes: TByteDynArray): TIntArray;
var
  i: Integer;
begin
  i := Length(ABytes);
  SetLength(Result{%H-}, i);
  for i := 0 to i - 1 do
    Result[i] := Integer(ABytes[i]);
end;

function IntArrayToByteArray(const arr: TIntArray): TByteDynArray;
var
  i: Integer;
begin
  i := Length(arr);
  SetLength(Result{%H-}, i);
  for i := 0 to i - 1 do
    Result[i] := Byte(arr[i]);
end;

function ByteArrayToCommaText(const arr: TByteDynArray): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(arr) - 1 do
    str_AddLine(Result, IntToStr(arr[i]), ',');
end;

function BoolArrayToCommaText(const arr: TBooleanDynArray): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(arr) - 1 do
    str_AddLine(Result, IntToStr(Byte(arr[i])), ',');
end;

function CommaTextToByteArray(const S: string): TByteDynArray;
var
  arr: TIntArray;
begin
  arrI_SetCommaText(arr, S);
  Result := IntArrayToByteArray(arr);
end;

function CommaTextToBoolArray(const S: string): TBooleanDynArray;
var
  arr: TIntArray;
begin
  arrI_SetCommaText(arr, S);
  Result := IntArrayToBoolArray(arr);
end;

function NewByteArray(const ASource: TByteDynArray): TByteDynArray;
var
  Len: Integer;
begin
  Len := Length(ASource);
  SetLength(Result{%H-}, Len);
  if Len > 0 then
    Move(ASource[0], Result[0], Len);
end;

function file_ExtractDir(const FileName: string): string;
begin
  Result := ExtractFileDir(FileName);
end;

function file_ExtractPath(const FileName: string): string;
begin
  Result := ExtractFilePath(FileName);
end;

function file_ExtractDrive(const FileName: string): string;
begin
  Result := ExtractFileDrive(FileName);
end;

function file_Size(const Filename: string): int64;
begin
  Result := FileSizeUtf8(Filename);
end;

function file_ExtractExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
end;

function file_ExtractName(const FileName: string): string;
begin
  Result := ExtractFileName(FileName);
end;

function file_ExtractNameWithoutExt(const AFilename: string): string;
begin
  Result := {$IFDEF VER3}LazFileUtils.{$ENDIF}ExtractFileNameWithoutExt(AFilename);
end;

function file_IsAbsoluteName(const AFileName: string): Boolean;
begin
  Result := FilenameIsAbsolute(AFileName);
end;

function file_AddFileProtocol(const AFileName: string;
  AIsAdjustToUnixSlash: Boolean; AIsAddThirdSlash: Boolean): string;
var
  sProto: string = 'file://';
begin
  Result := AFileName;
  if (Pos('://', Result) = 0) then //new:6.9 >>>
    Result := sProto + Result
  else
    begin
      if not StartsText(sProto, Result) then Exit;
      if not StartsStr(sProto, Result) then
        str_Replace(sProto, Result, 1, 7);
    end;                          //new:6.9 <<<<
  //if not StartsStr(sProto, Result) then   //new:6.9 >>>
  //  begin
  //    if StartsText(sProto, Result) then
  //      System.Delete(Result, 1, 7);
  //    Insert(sProto, Result, 1);
  //  end;                                  //new:6.9 <<<
  if AIsAddThirdSlash and (Result[8] <> '/') then
    Insert('/', Result, 8);
  if AIsAdjustToUnixSlash then
    Result := str_ToUnixSlash(Result);
end;

function file_Open(const AFileName: string; out AHandle: THandle;
    const Mode: Integer; const IsShowError: Boolean): Boolean;
begin
  Result := False;
  if ((Mode and fmCreate) <> 0) and
        not file_Exists(AFileName) then
    begin
      if not file_Create(AFileName, AHandle, IsShowError) then
        Exit
      else
        file_Close(AHandle);
    end;
  AHandle := FileOpenUTF8(AFileName, Mode);
  Result := (AHandle <> feInvalidHandle);
  if not Result and IsShowError then
    ShowLastOSError;
end;

procedure file_Close(AHandle: THandle);
begin
  FileClose(AHandle);
end;

function file_Create(const AFileName: string; out AHandle: THandle;
  const IsShowError: Boolean; const AShareMode: Integer; const ARights: Integer): Boolean;
begin
  if dir_Force(file_ExtractDir(AFileName), IsShowError) then
    begin
      AHandle := FileCreateUTF8(AFileName, AShareMode, ARights);
      Result := (AHandle <> feInvalidHandle);
      if not Result and IsShowError then
        ShowLastOSError;
    end
  else
    begin
      AHandle := feInvalidHandle;
      Result := False;
    end;
end;

function file_Clear(const AFileName: string; IsShowError: Boolean): Boolean;

begin
  Result := file_Exists(AFileName, IsShowError) and
      file_CreateEmpty(AFileName);
end;

function file_CreateEmpty(const AFileName: string; IsShowError: Boolean
  ): Boolean;
var
  //  F: file;
  h: System.THandle;
begin
  Result := file_Create(AFileName, h, IsShowError);
  if Result then
    file_Close(h);
  //Result := False;
  //try
  //  if Length(AFileName) = 0 then Exit;
  //  file_Assign(F, AFileName);
  //  Rewrite(F);
  //  CloseFile(F);
  //  Result := True;
  //except end;
end;

function file_CreateIfNotExist(const AFileName: string; IsShowError: Boolean
  ): Boolean;
begin
  Result := file_Exists(AFileName);
  if not Result then
    Result := dir_Force(ExtractFileDir(AFileName), IsShowError) and
      file_CreateEmpty(AFileName, IsShowError);
end;

function file_GetAttr(const AFileName: String): Longint;
begin
  Result := FileGetAttrUTF8(AFileName);
end;

function file_SetAttr(const AFilename: String; Attr: longint): Longint;

begin
  Result := FileSetAttrUTF8(AFilename, Attr);
end;

function file_GetModified(const AFileName: string): TDateTime;
begin
  try
    if file_Exists(AFileName) then
      Result := FileDateToDateTime(FileAgeUTF8(AFileName))
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function file_SetModified(const AFileName: string; dt: TDateTime): Integer;

begin
  Result := FileSetDateUTF8(AFileName, DateTimeToFileDate(dt));
end;

procedure file_Assign(out f: Text; const AFileName: string); overload;
begin
  AssignFile(f, UTF8ToSys(AFileName));
end;

procedure file_Assign(out f: File; const AFileName: string); overload;
begin
  AssignFile(f, UTF8ToSys(AFileName));
end;

function file_AppendString(const AFileName, S: string;
  const IsShowError: Boolean): Boolean;
var
  h: System.THandle;
begin
  if S = '' then Exit(True);
  Result := False;
  if file_Open(AFileName, h, fmOpenWrite or fmCreate, IsShowError) then
    try
      file_Seek(h, 0, fsFromEnd);
      file_Write(h, PChar(S)^, Length(S));
      Result := True;
    finally
      file_Close(h);
    end;
end;

function file_AppendLine(const AFileName, S: string; const IsShowError: Boolean
  ): Boolean;
begin
  Result := file_AppendString(AFileName, S + sLineBreak, IsShowError);
end;

function file_Read(Handle: THandle; out Buffer; Count: longint): Longint;

begin
  Result := FileRead(Handle, Buffer, Count);
end;

function file_Write(Handle: THandle; const Buffer; Count: Longint): Longint;

begin
  Result := FileWrite(Handle, Buffer, Count);
end;

function file_Seek(Handle: THandle; FOffset: Int64; Origin: Longint): Int64;

begin
  Result := FileSeek(Handle, FOffset, Origin);
end;

function file_Exists(const AFilename: string; const AIsShowError: Boolean
  ): Boolean;
begin
  Result := (AFilename <> '') and FileExistsUTF8(AFilename);
  if not Result and AIsShowError then
    ShowErrorFmt(RS_FileNotExists, [AFilename]);
end;

function file_IsLink(const AFileName: string): Boolean;
begin
  Result := FileIsSymlink(AFileName) or FileIsHardLink(AFileName);
end;

function file_IsInLinkDir(const AFileName: string; out ADir: string
  ): Boolean;
begin
  ADir := AFileName;
  while dir_GetParent(ADir) do
    begin
      if FileIsSymlink(ADir) or
            FileIsHardLink(ADir) then
        Exit(True);
    end;
  Result := False;
  ADir := '';
end;

{$IFDEF WINDOWS}
//Windows Vista and Higher
function GetFinalPathNameByHandle(hFile: HANDLE; lpszFilePath: PWideChar;
      cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall; external 'kernel32' name 'GetFinalPathNameByHandleW';
{$ENDIF}

function _file_GetLinkTarget(const ALink: string; out ATarget: string): Boolean;
{$IFDEF WINDOWS}
var
  sPath: UnicodeString;
  h: HANDLE;
  buffer: PREPARSE_DATA_BUFFER;
  DataBuffer: array [0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE] of UCHAR;
  returnedDataSize: DWORD;
  ws: WideString;
begin
  Result := False;
  sPath := utf_8To16(ALink);
  FillChar(buffer{%H-}, SizeOf(buffer), 0);
  //buffer.ReparseDataLength := 1024;
  buffer := @DataBuffer;
  h := CreateFileW(
        PWideChar(sPath),
        FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
        nil,
        OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS or FILE_ATTRIBUTE_REPARSE_POINT or FILE_FLAG_OPEN_REPARSE_POINT,
        0);
  if (h <> INVALID_HANDLE_VALUE) then
    try
      Result := True;
      //ATarget := 'D:\Demos';
      Result := DeviceIoControl(
            h,
            FSCTL_GET_REPARSE_POINT,
            nil,
            0,
            buffer,
            MAXIMUM_REPARSE_DATA_BUFFER_SIZE,
            returnedDataSize{%H-},
            nil);
      if Result then
        begin
          ws := str_LCopy(PWideChar(@buffer.PathBuffer), MAX_PATH);
          ATarget := utf_16To8(ws);
          Result := dir_Exists(ATarget) or file_Exists(ATarget);
        end;
    finally
      CloseHandle(h);
    end;
//"\??\C:\MyDir".
end;
{$ELSE}
begin
  Result := False;
  //RaiseToDoException();
end;
{$ENDIF}

function file_GetLinkTarget(const ALink: string; out ATarget: string): Boolean;
var
  sDir, S: string;
begin
  Result := _file_GetLinkTarget(ALink, ATarget);
  if Result then Exit;
  Result := file_IsInLinkDir(ALink, sDir) and
      _file_GetLinkTarget(sDir, ATarget);
  if Result then
    begin
      S := Copy(ALink, Length(sDir) + 1, MaxInt);
      ATarget := ATarget + S;
    end;
end;

function file_GetLinkTarget(var ALink: string): Boolean;
var
  S: string;
begin
  Result := file_GetLinkTarget(ALink, S);
  if Result then
    ALink := S;
end;

function file_FindFirst(const Path: string; const Attr: Longint; out
  ASearchRec: TSearchRec): Longint;
begin
  Result := FindFirstUTF8(Path, Attr, ASearchRec);
end;

function file_FindNext(var ASearchRec: TSearchRec): Longint;
begin
  Result := FindNextUTF8(ASearchRec);
end;

procedure file_FindClose(var ASearchRec: TSearchrec);
begin
  FindCloseUTF8(ASearchRec);
end;

function file_FindFirstInPath(const AName, APath: string; out AFileName: string
  ): Boolean;
var
  sr: TSearchRec;
  sPath: String;
begin
  //ToDo use TMask (file_FindNext) to work more cross platform
  sPath := IncludeTrailingPathDelimiter(APath);
  if file_FindFirst(sPath + AName, faAnyFile, sr) = 0 then
    begin
      AFileName := sPath + sr.Name;
      file_FindClose(sr);
      Exit(True);
    end;
  Result := False;
end;

function dir_Exists(const ADirectory: string; const IsShowError: Boolean
  ): Boolean;
begin
  Result := (ADirectory <> '') and DirectoryExistsUTF8(ADirectory);
  if not Result and IsShowError then
    ShowErrorFmt(RS_FileNotExists, [ADirectory]);
end;

function dir_Force(const ADir: string; IsShowError: Boolean): Boolean;
begin
  try
    Result := (ADir <> '') and ForceDirectoriesUTF8(ADir);
  except  end;
  if not Result and IsShowError then
    ShowError(Format(RS_ErrCreateDir, [ADir]));
end;

function dir_Create(const ADir: string; IsShowError: Boolean): Boolean;
begin
  Result := CreateDirUTF8(ADir);
  if not Result and IsShowError then
    ShowError(Format(RS_ErrCreateDir, [ADir]));
end;

function dir_GetCurrent: string;
begin
  Result := LazFileUtils.GetCurrentDirUTF8;
end;

function dir_GetParent(var ADir: string): Boolean;
var
  sDir: String;
begin
  sDir := ADir;
  ADir := file_ExtractDir(ADir);
  Result := Length(ADir) < Length(sDir);
end;

function dir_IsEmpty(const ADir: string; AIgnoreEmptySubDir: Boolean): Boolean;

var
  sr: TSearchRec;
  sName: string;
  Path: String;
begin
  Result := False;
  Path := IncludeTrailingPathDelimiter(ADir);
  if file_FindFirst(Path + GetAllFilesMask, faAnyFile, sr)= 0 then
    try
      repeat
        sName := sr.Name;
        if (sName='.') or (sName='..') or (sName='') then
          continue;
        if (sr.Attr and faDirectory) > 0 then
          begin
            if not AIgnoreEmptySubDir or
                not dir_IsEmpty(file_ExtractDir(ADir), AIgnoreEmptySubDir) then Exit;
          end
        else
          begin
            Exit;
          end;
      until file_FindNext(sr) <> 0;
    finally
      file_FindClose(sr);
    end;
  Result := True;
end;

function dir_FileCount(const ADir: string; const AIncludeSubDir: Boolean
  ): Integer;
var
  sr: TSearchRec;
  sName: string;
  Path: String;
begin
  Result := 0;
  Path := IncludeTrailingPathDelimiter(ADir);
  if file_FindFirst(Path + GetAllFilesMask, faAnyFile, sr)= 0 then
    try
      repeat
        sName := sr.Name;
        if (sName='.') or (sName='..') or (sName='') then
          continue;
        if (sr.Attr and faDirectory) > 0 then
          begin
            if AIncludeSubDir then
              Inc(Result);
          end
        else
          Inc(Result);
      until file_FindNext(sr) <> 0;
    finally
      file_FindClose(sr);
    end;
end;

function dir_Remove(const ADir: string; IsShowError: Boolean): Boolean;

begin
  Result := RemoveDirUTF8(ADir);
  if not Result and IsShowError then
    ShowError(Format(RS_ErrDeleteFile, [ADir]));
end;

function dir_Delete(const ADir: string; OnlyChilds: boolean;
  IsConfirm: Boolean; IsShowError: Boolean): boolean;
begin
  if not dir_Exists(ADir) then Exit(True);
  Result := False;
  if IsConfirm and not ShowConfirm(Format(RS_DeleteItem, [ADir])) then
    Exit;
  if Trim(ADir) = '' then
    Result := False
  else
    Result := DeleteDirectory(ADir, OnlyChilds);
  if not Result and IsShowError then
    ShowError(Format(RS_ErrDeleteFile, [ADir]));
end;

function dir_DeleteIfEmpty(const ADir: string; AIgnoreEmptySubDir: Boolean
  ): Boolean;
begin
  Result := dir_Exists(ADir) and dir_IsEmpty(ADir, AIgnoreEmptySubDir) and
    dir_Delete(ADir, False);
end;

function dir_CopyExt(const ASrcDir, ADestDir: string; fcf: TFileCopyFlags;
  const AFileMask: string; faAttr: Longint): Integer;
var
  iIndex : Integer;
  sr : TSearchRec;
  SrcPath, DestPath, FileMask, SrcFile, DestFile, sName: string;
begin
  Result := -1;
  if not dir_Exists(ASrcDir) or not dir_Force(ADestDir) then Exit;
  SrcPath := IncludeTrailingPathDelimiter(ASrcDir);
  DestPath := IncludeTrailingPathDelimiter(ADestDir);
  if AFileMask = '' then FileMask := GetAllFilesMask
  else FileMask := AFileMask;
  iIndex := file_FindFirst(SrcPath + FileMask, faAttr, sr);
  try
    while iIndex = 0 do
      begin
        sName := sr.Name;
        SrcFile := SrcPath + sName;
        DestFile := DestPath + sName;
        if (sr.Attr and faDirectory) <> 0 then
          begin
            if (sName <> '' ) and (sName[1] <> '.') then
              dir_CopyExt(SrcFile, DestFile, fcf, FileMask, faAttr);
          end
        else
          begin
            file_Copy(SrcFile, DestFile, fcf);
          end;
         iIndex := file_FindNext(sr);
      end;
    Result := 0;
  finally
    file_FindClose(sr);
  end;
end;

function dir_Copy(const ASrcDir, ADestDir: string): Boolean;
begin
  Result := dir_CopyExt(ASrcDir, ADestDir) = 0;
end;

function dir_SameName(const ADir1, ADir2: string): Boolean;
begin
  Result := file_CompareName(ExcludeTrailingPathDelimiter(ADir1),
      ExcludeTrailingPathDelimiter(ADir2)) = 0;
end;

function file_SameName(const AFileName1, AFileName2: string): Boolean;
begin
  Result := file_CompareName(AFileName1, AFileName2) = 0;
end;

function file_Rename(const AOldName, ANewName: String; IsShowError: Boolean): Boolean;
begin
  try
  Result := RenameFileUTF8(AOldName, ANewName);
  except end;
  if not Result and IsShowError then
    ShowError(Format(RS_ErrRenameFile, [AOldName]));
end;

function file_ExtractNameOnly(const AFilename: string;
  const AWithoutAllExtensions: Boolean): string;
var
  StartPos: Integer;
  ExtPos: Integer;
  EndSep: Set of Char;
begin
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  StartPos:=length(AFilename);
  while (StartPos > 0) and not (AFileName[StartPos] in EndSep) do
    Dec(StartPos);
  Inc(StartPos);
  ExtPos:=length(AFilename);
  while (ExtPos>=StartPos) and (AFilename[ExtPos]<>'.') do
    dec(ExtPos);
  if (ExtPos<StartPos) then ExtPos:=length(AFilename)+1;
  Result:=copy(AFilename,StartPos,ExtPos-StartPos);
  if AWithoutAllExtensions then
    Result := NameOfString(Result, '.');
end;

function file_Delete(const AFileName: string; IsShowError: Boolean;
  IsConfirm: Boolean): Boolean;
begin
  Result := False;
  if IsConfirm and not ShowConfirm(Format(RS_DeleteItem, [AFileName])) then
    Exit;
  try
    Result := (AFileName <> '') and DeleteFileUTF8(AFileName);
  except  end;
  if not Result and IsShowError then
    ShowError(Format(RS_ErrDeleteFile, [AFileName]));
end;

function file_Copy(const ASrcFilename, ADestFilename: string;
  const fcf: TFileCopyFlags): boolean;
var
  sError: String;
  sDestDir: string;
  IsDestFileExits: Boolean;
begin
  IsDestFileExits := file_Exists(ADestFilename);
  if ((fcfSkipIfExists in fcf) and IsDestFileExits) or not file_Exists(ASrcFilename) then
    Result := False
  else
    begin
      try
        if (fcfCreateDestDirectory in fcf) then
          begin
            sDestDir := ExtractFileDir(ADestFileName);
            if not DirectoryExistsUTF8(sDestDir)
              and not ForceDirectoriesUTF8(sDestDir) then
            Exit(False);
          end;
        if (fcfSkipIfOlderVersion in fcf) and IsDestFileExits and
            (file_CompareVers(ASrcFilename, ADestFilename) <= 0) then
          Exit(False);
        Result := FileUtil.CopyFile(ASrcFilename, ADestFilename, (fcfPreserveTime in fcf));
      except
        on E: Exception do
          begin
            Result := False;
            sError := err_GetMessage(E);
          end;
      end;
      if not Result and (fcfShowError in fcf) then
        begin
          if sError = '' then
            sError := Format(RS_ErrCopyFile, [ASrcFilename]);
          ShowError(sError);
        end;
    end;
end;

function file_HasExt(const AFileName: string; const AExt: string): Boolean;

var
  sExt: string;
begin
  if AFileName = '' then Exit(False);
  sExt := file_ExtractExt(AFileName);
  if (AExt = '') then
    Result := sExt <> ''
  else
    Result := file_CompareName(AExt, sExt) = 0;
end;

function file_AddExt(const AFileName, AExt: string; const AOnlyIfNone: Boolean
  ): string;
var
  sExt: String;
begin
  sExt := file_ExtractExt(AFileName);
  if file_CompareName(AExt, sExt) = 0 then
    Result := AFileName
  else if (sExt = '') or not AOnlyIfNone then
    Result := AFileName + AExt
  else
    Result := AFileName;
end;

function file_Starts(const ASubDir, AFilename: string): Boolean;
begin
  Result := file_CompareName(ASubDir, Copy(AFilename, 1, Length(ASubDir))) = 0;
end;

function file_Ends(const ASubName, AFilename: string): Boolean;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(AFilename);
  Len2 := Length(ASubName);
  Result := (Len1 >= Len2) and (file_CompareName(ASubName,
        Copy(AFilename, Len1 - Len2 + 1, Len2)) = 0);
end;

function file_PosInName(const ASubStr, AFilename: string): Integer;
begin
  {$IFDEF FILENAME_CASESENSETIVE}
  Result := Pos(ASubStr, AFilename);
  {$ELSE}
  Result := str_PosText(ASubStr, AFilename);
  {$ENDIF}
end;

function file_ChangeExt(const AFileName, AExt: string): string;
begin
  Result := ChangeFileExt(AFileName, AExt);
end;

function file_RemoveExt(const AFileName: string): string;
begin
  Result := ChangeFileExt(AFileName, '');
end;

function file_IsReadOnly(const FileName: string): Boolean;
begin
  Result := FileIsReadOnlyUTF8(FileName);
end;

function file_ExpandName(const AFileName: string): string;
begin
  Result := ExpandFileNameUTF8(AFileName);
end;

function file_QuotedName(const AFileName: string): string;
begin
  if Pos(' ', AFileName) > 0 then
    Result := str_QuotedChar(AFileName, '"')
  else
    Result := AFileName;
end;

{$IFDEF WINDOWS}
function _file_GetVersionAsStringWin(const AFile: string; const {%H-}AParam: string{%H-}): string;
var
  Info: Pointer;
  InfoData: Pointer;
  InfoSize: LongInt;
  Len: DWORD = 0;
  FName: Pchar;
  Infotype: string;
  LangPtr: Pointer;
begin
  Result := '';
  InfoType := 'FileVersion';
  FName := Pchar(AFile);
  InfoSize := GetFileVersionInfoSize(Fname, Len);
  if (InfoSize > 0) then
  begin
    GetMem(Info, InfoSize);
    try
      if GetFileVersionInfo(FName, Len, InfoSize, Info) then
      begin
        Len := 255;
        if VerQueryValue(Info, '\VarFileInfo\Translation', LangPtr{%H-}, Len) then
          InfoType := Format('\StringFileInfo\%0.4x%0.4x\%s'#0, [LoWord(LongInt(LangPtr^)),
            HiWord(LongInt(LangPtr^)), InfoType]);
        if VerQueryValue(Info, Pchar(InfoType), InfoData{%H-}, len) then
          Result := strPas(InfoData);
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
  end;
end;
{$ENDIF}
function _file_GetVersionAsStringUnix(const AFile: string; const AParam: string = ''): string;
var
  S: string;
  nd: INextDelimStringGetter;
begin
  Result := '';
  GetProgramOutput(AFile, AParam, S, 3000);
  nd := New_NextDelimStringGetter(S, ' ');
  while nd.Next(S) do
    if S[1] in ['0'..'9'] then
      Exit(S);
end;

procedure file_ParseVersion(const AFileName, AParam: string; out AMax,
  AMin: Integer; const ARevision: PInteger; const ABuild: PInteger);
var
  S: String;
  arr: TStringArray;

  procedure _Set(const AIndex: Integer; AVar: PInteger);
  begin
    if AVar = nil then Exit;
    if Length(arr) > AIndex then AVar^ := StrToIntDef(arr[AIndex], 0)
    else AVar^ := 0;
  end;

begin
  S := file_GetVersionAsString(AFileName, AParam);
  str_SplitByChar(S, '.', arr);
  _Set(0, @AMax);
  _Set(1, @AMin);
  _Set(2, ARevision);
  _Set(3, ABuild);
end;

function file_GetVersionMax(const AFile: string; const AParam: string): Integer;
begin
  Result := StrToIntDef(NameOfString(file_GetVersionAsString(AFile, AParam), '.'), 0);
end;

function file_GetVersionAsString(const AFile: string; const AParam: string = ''): string;
begin
  {$IFDEF WINDOWS}
  if AParam = '' then
    Result := _file_GetVersionAsStringWin(AFile, AParam)
  else{$ENDIF}
    Result := _file_GetVersionAsStringUnix(AFile, AParam);
end;

function file_CompareVers(AFileName1, AFileName2: string; const AParam: string
  ): Integer;
begin
  Result := file_CompareVersionStrings(file_GetVersionAsString(AFileName1, AParam),
      file_GetVersionAsString(AFileName2, AParam));
end;

function file_CompareVerWith(AFileName1, AVersion: string; const AParam: string
  ): Integer;
begin
  Result := file_CompareVersionStrings(file_GetVersionAsString(AFileName1, AParam), AVersion);
end;

function file_CompareVersionStrings(AVer1, AVer2: string): Integer;
var
  S, S1: string;
  i, n, n1: Integer;
begin
  if AVer1 = '' then
    begin
      if AVer2 = '' then Result := 0
      else Result := -1;
      Exit;
    end
  else if AVer2 = '' then
    Exit(1);
  Result := CompareStr(AVer1, AVer2);//new:6.9
  if Result = 0 then Exit;//new:6.9
  Result := 0;//new:6.3 := -1;
  S := '.';
  i := min(PosCount(S, AVer1), PosCount(S, AVer2)) + 1;
  while i > 0 do
    begin
      Dec(i);
      NameValueOfString(AVer1, S, AVer1, '.');
      NameValueOfString(AVer2, S1{%H-}, AVer2, '.');
      if not TryStrToInt(S, n) or not TryStrToInt(S1, n1) then Exit;
      Result := CompareValue(n, n1);
      if Result <> 0 then Exit;
    end;
end;

function file_CreateTemp(out AFileName: string;
  ADeleteOnFreeObject: TPersistent; const AExt: string; const APrefix: string;
  const ADirectory: string): Boolean;
var
  sDir: string;
begin
  if ADirectory = '' then sDir := AppTempPath
  else sDir := ADirectory;
  AFileName := file_GetTempNameInDir(sDir, AExt, APrefix);
  Result := file_CreateEmpty(AFileName);
  if not Result then
    AFileName := ''
  else if ADeleteOnFreeObject <> nil then
    _AutoTempFileDeleter.Add(AFileName, ADeleteOnFreeObject);
end;

function file_GetTempNameInDir(const ADirectory: string; const AExt: string;
  const APrefix: string; AIsFullPath: Boolean): string;
var
  i: Integer;
  CurPath{, sExt}: String;
begin
  //if AExt = '' then sExt := '.tmp'
  //else sExt := AExt;
  //CurPath := AppendPathDelim(file_ExpandName(ADirectory)) + APrefix;
  //new:6.9.5 CurPath := IncludeTrailingPathDelimiter(file_RelToFull(ADirectory, dir_GetCurrent)) + APrefix;
  CurPath := IncludeTrailingPathDelimiter(
      {$IFNDEF VER3}LazFileUtils.{$ENDIF}ExpandFileNameUTF8(ADirectory, dir_GetCurrent)) + APrefix;//new:6.9.5
  i := 1;
  repeat
    Result := CurPath + IntToStr(i) + AExt;
    if not (file_Exists(Result) or dir_Exists(Result)) then
      begin
        if not AIsFullPath then
          Result := file_ExtractName(Result);
        Exit;
      end;
    inc(i);
  until false;
end;

function file_GetTempName(const AExt: string; const APrefix: string;
  AIsFullPath: Boolean): string;
begin
  Result := file_GetTempNameInDir(AppTempPath, AExt, APrefix, AIsFullPath);
end;

function url_SpacesToHex(const AUrl: string): string;
begin
  Result := StringReplace(AUrl, ' ', '%20', [rfReplaceAll]);
end;

function url_HexToSpaces(const AUrl: string): string;
begin
  Result := StringReplace(AUrl, '%20', ' ', [rfReplaceAll]);
end;

function url_Unescape(const AUrl: string): string;
begin
  Result := str_DecodeHexChars(AUrl);
end;

const
  //GenDelims = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/', '\', '?', '#', '[', ']'];

function url_Escape(const AUrl: string): string;
begin
  Result := str_EncodeToHex(AUrl, ValidPathChars);
end;

function url_RemoveProtocol(const AUrl: string): string;
var
  i: SizeInt;
  sDelim: String;
begin
 sDelim := ':/';//WEB_PROTOCOL_DELIM;
 i := Pos(sDelim, AUrl);
 if {$IFDEF WINDOWS}i <= 2{$ELSE}i = 0{$ENDIF}then //new:6.9 (Windows C:/) i = 0 then
   Result := AUrl
 else
  begin
    //new:6.9.5
    Inc(i, Length(sDelim));
    while str_Char(AUrl, i) = '/' do
      Inc(i);
    Result := Copy(AUrl, i, MaxInt);
    //new:6.9.5 >>>
    //Result := Copy(AUrl, i + Length(sDelim), MaxInt);
    //while (Result <> '') and (Result[1] = '/') do
    //  System.Delete(Result, 1, 1);
  end;
end;

{ TVrodeEnumFiles }

constructor TVrodeEnumFiles.Create;
begin
  FAttr := faAnyFile;
  FFileMasks := '*.*';
  FLevel := MaxInt;
end;

destructor TVrodeEnumFiles.Destroy;
begin
  ClearPaths;
  inherited;
end;

procedure TVrodeEnumFiles.AddArrayOfPath(var arr: TFileNameArray);
var
  I: Integer;
begin
  for I := 0 to Length(arr) - 1 do
    AddPath(arr[I]);
end;

procedure TVrodeEnumFiles.AddPath(const APath: string);
var
  I: Integer;
begin
  I := Length(FPaths);
  SetLength(FPaths, I + 1);
  FPaths[I] := APath;
end;

procedure TVrodeEnumFiles.ClearPaths;
begin
  FPaths := nil;
end;

procedure TVrodeEnumFiles.EnumToList(strs: TStrings; AIsAddToList: Boolean);
begin
  //FIsAddToStrsWithPath := AIsAddToStrsWithPath;
  //FOnlyDirs := AOnlyDirs;
  Fstrs := strs;
  if not AIsAddToList then
    Fstrs.Clear;
  FProc := nil;
  FMethod := nil;
  _Enum;
end;

procedure TVrodeEnumFiles.EnumToProc(Proc: TEnumFilesProc; AData: Pointer);
begin
  Fstrs := nil;
  FProc := Proc;
  FProcData := AData;
  FMethod := nil;
  _Enum;
end;

procedure TVrodeEnumFiles.EnumToMethod(AMethod: TEnumFilesMethod);
begin
  Fstrs := nil;
  FProc := nil;
  FMethod := AMethod;
  _Enum;
end;

function TVrodeEnumFiles.MatchEmpty(const AName: string; AMaskList: TMaskList): Boolean;
begin
  Result := True;
end;

function TVrodeEnumFiles.MatchInclude(const AName: string; AMaskList: TMaskList): Boolean;
begin
  Result := AMaskList.Matches(AName);
end;

function TVrodeEnumFiles.MatchExclude(const AName: string; AMaskList: TMaskList): Boolean;
begin
  Result := not AMaskList.Matches(AName);
end;

procedure TVrodeEnumFiles.SetDirMasks(const AValue: string);
begin
  //if AValue = '' then
  //  FDirMasks := GetAllFilesMask
  //else
    FDirMasks := AValue;
end;

procedure TVrodeEnumFiles.SetFileMasks(const AValue: string);
begin
  //if AValue = '' then
  //  FFileMasks := GetAllFilesMask
  //else
    FFileMasks := AValue;
end;

procedure TVrodeEnumFiles.SetMatchFunc(var AFunc: TVrodeMatchFunc;
  AMaskList: TMaskList; IsExclude: Boolean);
begin
  if AMaskList.Count = 0 then
    AFunc := MatchEmpty
  else if IsExclude then
    AFunc := MatchExclude
  else
    AFunc := MatchInclude;
end;

procedure TVrodeEnumFiles._Enum;
var
  sPath: string;
  lstFileMasks, lstDirMasks: TMaskList;

  function _EnumPath(const APath: string; ALevel: Integer): TEnumFilesAction;
  var
    sr: TSearchRec;
    Action: TEnumFilesAction;
    sName, sRelPath: string;

    procedure _Add;
    var
      S: String;
    begin
      if Assigned(Fstrs) then
        begin
          if vefoExcludeExt in FOptions then
            sName := file_ChangeExt(sName, '');
          if (vefoAddToStrsWithPath in FOptions) and
              not (vefoRelativePath in FOptions) then
            S := APath + sName
          else
            S := sRelPath + sName;
          if vefoUnixSlashInStrs in FOptions then
            S := str_ToUnixSlash(S);
          Fstrs.Add(S);
        end
      else if Assigned(FMethod) then
        FMethod(sr, IfThen(vefoRelativePath in FOptions, sRelPath, APath), Action)
      else if Assigned(FProc) then
        FProc(sr, IfThen(vefoRelativePath in FOptions, sRelPath, APath), Action, FProcData);
    end;

    procedure ProccessSR;
    begin
      sName := sr.Name;
      if (sName = '.') or (sName = '..') then Exit;
      if ((sr.Attr and faDirectory) <> 0) then
        begin
          if FDirMatch(sName, lstDirMasks) then
            begin
              if not (vefoIgnoreDirs in FOptions) then
                begin
                  _Add;
                  if Action = efaStop then Exit;
                end;
              if Action = efaSkip then
                Action := efaNone
              else
                Action := _EnumPath(APath + sName + PathDelim, ALevel + 1);
            end;
        end
      else
        begin
          if not (vefoIgnoreFiles in FOptions){not FOnlyDirs} and
              FFileMatch(sName, lstFileMasks) then
            begin
              _Add;
              if Action = efaStop then Exit;
            end;
        end;
    end;

  begin
    Result := efaNone;
    if ALevel > FLevel then Exit;
    sRelPath := Copy(APath, Length(sPath) + 1, MaxInt);
    Action := efaNone;
    if FindFirstUTF8(APath + '*.*', FAttr, sr) = 0 then
      begin
        ProccessSR;
        if Action <> efaStop then
          while FindNextUTF8(sr) = 0 do begin
            ProccessSR;
            if Action = efaStop then Break;
          end;
        FindCloseUTF8(sr);
        Result := Action;
      end
    {$IFDEF TEST_MODE}
    else
      begin
        if THandle(sr.FindHandle) <> feInvalidHandle then
          ShowInfo('TVrodeEnumFiles._Enum: sr.FindHandle <> Invalid_Handle_value');//FindCloseUTF8(sr);
      end{$ENDIF};
  end;

var
  i: Integer;
begin
  lstFileMasks := TMaskList.Create(FFileMasks, ';');
  lstDirMasks := TMaskList.Create(FDirMasks, ';');
  try
    SetMatchFunc(FFileMatch, lstFileMasks, (vefoFileMasksExclude in FOptions));
    SetMatchFunc(FDirMatch, lstDirMasks, (vefoDirMasksExclude in FOptions));
    for i := 0 to Length(FPaths) - 1 do
      begin
        sPath := FPaths[i];
        if not dir_Exists(sPath) then Continue;
        sPath := IncludeTrailingPathDelimiter(sPath);
        if _EnumPath(sPath, 0) = efaStop then Break;
      end;
  finally
    lstFileMasks.Free;
    lstDirMasks.Free;
  end;
end;

procedure EnumFilesOfArray(var arr: TFileNameArray; AObjProc: TEnumFilesMethod;
  const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile);
var
  ef: TVrodeEnumFiles;
begin
  if Length(arr) = 0 then Exit;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddArrayOfPath(arr);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToMethod(AObjProc);
  finally
    ef.Free;
  end;
end;

procedure EnumFilesOfArray(var arr: TFileNameArray; AProc: TEnumFilesProc;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile);
var
  ef: TVrodeEnumFiles;
begin
  if Length(arr) = 0 then Exit;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddArrayOfPath(arr);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToProc(AProc);
  finally
    ef.Free;
  end;
end;

procedure EnumFilesOfArray(var arr: TFileNameArray; strs: TStrings;
  const AFileMasks: string; ALevel: Integer; AOpts: TVrodeEnumFilesOptions;
  const ADirMasks: string; Attr: Integer; AIsAddToList: Boolean);
var
  ef: TVrodeEnumFiles;
begin
  if Length(arr) = 0 then
    begin
      if not AIsAddToList then strs.Clear;
      Exit;
    end;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddArrayOfPath(arr);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToList(strs, AIsAddToList);
  finally
    ef.Free;
  end;
end;

procedure EnumFilesToMethod(const APath: string; AObjProc: TEnumFilesMethod;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile);
var
  ef: TVrodeEnumFiles;
begin
  if not dir_Exists(APath) then Exit;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddPath(APath);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToMethod(AObjProc);
  finally
    ef.Free;
  end;
end;

procedure EnumFilesToProc(const APath: string; AProcData: Pointer;
  AProc: TEnumFilesProc; const AFileMasks: string; ALevel: Integer;
  AOpts: TVrodeEnumFilesOptions; const ADirMasks: string; Attr: Integer);
var
  ef: TVrodeEnumFiles;
begin
  if not dir_Exists(APath) then Exit;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddPath(APath);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToProc(AProc, AProcData);
  finally
    ef.Free;
  end;
end;

procedure EnumFilesToStrs(const APath: string; strs: TStrings;
    const AFileMasks: string = ''; ALevel: Integer = MaxInt;
    AOpts: TVrodeEnumFilesOptions = VEFO_DEFAULT; const ADirMasks: string = '';
    Attr: Integer = faAnyFile; AIsAddToList: Boolean = False); overload;
var
  ef: TVrodeEnumFiles;
begin
  if not dir_Exists(APath) then
    begin
      if not AIsAddToList then strs.Clear;
      Exit;
    end;
  ef := TVrodeEnumFiles.Create;
  try
    ef.AddPath(APath);
    ef.Level := ALevel;
    ef.Options := AOpts;
    ef.FileMasks := AFileMasks;
    ef.DirMasks := ADirMasks;
    ef.Attributes := Attr;
    ef.EnumToList(strs, AIsAddToList);
  finally
    ef.Free;
  end;
end;

function ShowConfirm(const S: string; AIsYesNo: Boolean): Boolean;
{$IFDEF WINDOWS}
var
  uType: Integer;
{$ELSE}{$ENDIF}
begin
  if Assigned(OnShowConfirm) then
    Result := OnShowConfirm(S, AIsYesNo)
  else
    begin
    {$IFDEF WINDOWS}
      if AIsYesNo then uType := MB_YESNO else
      uType := MB_OKCANCEL;
      uType := MessageBoxW(0, PWideChar(utf_8To16(S)),
          PWideChar(utf_8To16(ApplicationName)),
          uType or MB_ICONQUESTION);
      if AIsYesNo then Result := uType = IDYES
      else Result := uType = IDOK;
    {$ELSE}
      if not IsMainThread then
        Exit;
    {$ENDIF}
    end;
end;

function ShowConfirmFmt(const S: string; AParams: array of const;
  AIsYesNo: Boolean): Boolean;
begin
  Result := ShowConfirm(Format(S, AParams), AIsYesNo);
end{%H-};

procedure ShowInfo(const S: string);
begin
  if Assigned(OnShowInfo) then
    OnShowInfo(S)
  else
    {$IFDEF WINDOWS}
      MessageBoxW(0, PWideChar(utf_8To16(S)), PWideChar(utf_8To16(ApplicationName)),
          MB_OK or MB_ICONINFORMATION);
    {$ELSE}
      if not IsMainThread then
        Exit;
    {$ENDIF}
end;

procedure ShowInfoFmt(const S: string; AParams: array of const);
begin
  ShowInfo(Format(S, AParams));
end{%H-};

procedure ShowError(const AError: string; ACaption: string);
begin
  //if IsConsole then  //ToDo if IsConsole and GUI
  //  begin
  //    if not IsMainThread then
  //      Exit;
  //    {$IFDEF WINDOWS}
  //    WriteLn(UTF8ToOEM(AError))
  //    {$ELSE}
  //    WriteLn(AError)
  //    {$ENDIF}
  //  end
  //else
  if Assigned(OnShowError) then
    OnShowError(AError, ACaption)
  {$IFDEF WINDOWS}
  else
    begin
      if ACaption = '' then
        ACaption := 'Error';
      MessageBoxW(0, PWideChar(UTF8Decode(AError)),
        PWideChar(UTF8Decode(ACaption)), MB_OK or MB_ICONERROR);
    end;
  {$ELSE}

  {$ENDIF};
end;

procedure ShowErrorFmt(const AError: string; AParams: array of const);
begin
  ShowError(Format(AError, AParams));
end{%H-};

procedure ShowDebugInfo(const S: string; const AFileName: string;
  AType: TDebugInfoType);
begin
  if Assigned(OnShowDebugInfo) then
    OnShowDebugInfo(S, AFileName, AType);
end;

procedure ShowDebugInfoFmt(const S, AFileName: string; AParams: array of const;
  AType: TDebugInfoType);
begin
  ShowDebugInfo(Format(S, AParams), AFileName, AType);
end{%H-};

procedure ShowLastOSError;
begin
  ShowError(GetLastOSErrorString);
end;

function GetLastOSErrorString: string;
begin
  Result := SysToUtf8(SysErrorMessage(GetLastOSError));
end;

function GetOSErrorString(AErrCode: Integer): string;
begin
  Result := SysToUtf8(SysErrorMessage(AErrCode));
end;

function err_GetMessage(E: Exception): string;
begin
  Result := SysToUTF8(E.Message);
end;

function err_GetSysMessage(AErrorCode: Integer): string;
begin
  Result := SysErrorMessageUTF8(AErrorCode);
end;

procedure ShowExceptionMsg(E: Exception);
begin
  ShowError(err_GetMessage(E));
end;

function IfThen(val: Boolean; const ifTrue: string; const ifFalse: string): string;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: UnicodeString; const ifFalse: UnicodeString): UnicodeString;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: WideString; const ifFalse: WideString): WideString;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Char; const ifFalse: Char): Char;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Pointer; const ifFalse: Pointer): Pointer;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Integer; const ifFalse: Integer): Integer;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Cardinal; const ifFalse: Cardinal): Cardinal;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Byte; const ifFalse: Byte): Byte;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function IfThen(val: Boolean; const ifTrue: Word; const ifFalse: Word): Word;
begin
  if val then Result := ifTrue else Result := ifFalse;
end;

function flag_Is(const AFlags, AFlag: Integer): Boolean;
begin
  Result := (AFlags and AFlag) <> 0;
end;

function flag_Toggle(var AFlags: Integer; const AFlag: Integer): Integer;
begin
  if (AFlags and AFlag) = 0 then
    flag_Set(AFlags, AFlag)
  else
    flag_UnSet(AFlags, AFlag);
  Result := AFlags;
end;

procedure flag_Set(var AFlags: Integer; const AFlag: Integer);
begin
  AFlags := AFlags or AFlag;
end;

procedure flag_Set(var AFlags: Cardinal; const AFlag: Cardinal);
begin
  AFlags := AFlags or AFlag;
end;

procedure flag_UnSet(var AFlags: Integer; const AFlag: Integer);
begin
  AFlags := AFlags or AFlag;
  AFlags := AFlags xor AFlag;
end;

procedure flag_UnSet(var AFlags: Cardinal; const AFlag: Cardinal);
begin
  AFlags := AFlags or AFlag;
  AFlags := AFlags xor AFlag;
end;

function flag_GetUnSet(const AFlags: Integer; const AFlag: Integer): Integer;
begin
  Result := AFlags;
  //Flag_UnSet(Result, AFlag);
  Result := Result or AFlag;
  Result := Result xor AFlag;
end;

procedure flag_Change(var AFlags: Integer; const AFlag: Integer; IsSet: Boolean
  );
begin
  if IsSet then
    AFlags := AFlags or AFlag
  else
    begin
      //flag_UnSet(AFlags, AFlag);
      AFlags := AFlags or AFlag;
      AFlags := AFlags xor AFlag;
    end;
end;

function flag_GetChanged(const AFlags, AFlag: Integer; IsSet: Boolean): Integer;
begin
  if IsSet then
    Result := AFlags or AFlag
  else
    Result := flag_GetUnSet(AFlags, AFlag);
end;

function SameMethod(AMethod1, AMethod2: TMethod): Boolean;
begin
  Result := (AMethod1.Data = AMethod2.Data) and (AMethod1.Code = AMethod2.Code);
end;

function Method(const ACode, AData: Pointer): TMethod;
begin
  Result.Code := ACode;
  Result.Data := AData;
end;

function IsValueBetween(AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function TryStrToDouble(const S: string; out AValue: Double): Boolean;
begin
  Result := TryStrToFloat(S, AValue);
end;

function DoubleToStr(AValue: Double): string;
begin
  Result := FloatToStr(AValue);
end;

function PointerToStr(p: Pointer): string;
begin
  {$IFDEF CPU64}
  Result := IntToStr({%H-}Int64(p));
  {$ELSE}
  Result := IntToStr({%H-}Integer(p));
  {$ENDIF}
end;

function StrToPointer(const S: string): Pointer;
begin
  {$IFDEF CPU64}
  Result := {%H-}Pointer(StrToInt64(S));
  {$ELSE}
  Result := {%H-}Pointer(StrToInt(S));
  {$ENDIF}
end;

function TryStrToPointer(const S: string; out p: Pointer): Boolean;
begin
  {$IFDEF CPU64}
  Result := TryStrToInt64(S, {%H-}Int64(p));
  {$ELSE}
  Result := TryStrToInt(S, {%H-}Integer(p));
  {$ENDIF}
end;

function StrToPointerDef(const S: string; Default: Pointer): Pointer;
begin
  {$IFDEF CPU64}
  Result := {%H-}Pointer(StrToInt64Def(S, {%H-}Int64(Default)));
  {$ELSE}
  Result := {%H-}Pointer(StrToIntDef(S, {%H-}Integer(Default)));
  {$ENDIF}
end;

function CompareValue(const A, B: Pointer): Integer;
begin
  result:=1;
  if a=b then
    result:=0
  else
   if a<b then
     result:=-1
end;

function TryStrToPtrInt(const s: string; out i: PtrInt): Boolean;
begin
{$IFDEF CPU64}
  Result := TryStrToInt64(s, i);
{$ELSE}
  Result := TryStrToInt(s, i);
{$ENDIF}
end;

function Hex2Int(const AValue: string): Integer;
var
  I : Integer;
begin
  Result := 0;
  I := 1;
  if AValue = '' then Exit;
  if AValue[ 1 ] = '$' then Inc( I );
  while I <= Length( AValue ) do
    begin
      if  (AValue[ I ] >= '0') and (AValue[ I ] <= '9') then
        Result := (Result shl 4) or (Ord(AValue[I]) - Ord('0'))
      else if  (AValue[ I ] >= 'A') and  (AValue[ I ] <= 'F') then
        Result := (Result shl 4) or (Ord(AValue[I]) - Ord('A') + 10)
      else if  (AValue[ I ] >= 'a') and  (AValue[ I ] <= 'f') then
        Result := (Result shl 4) or (Ord(AValue[I]) - Ord('a') + 10)
      else
        break;
      Inc(I);
    end;
end;

function str_DecodeHexChars(const S: string): string;
var
  pc, pcResult: PChar;
  S1, S2, S3: string;
  I: Integer;
begin
  SetLength(Result{%H-}, Length(S));
  SetLength(S1{%H-}, 2);
  SetLength(S2{%H-}, 4);
  pc := PChar(S);
  pcResult := PChar(Result);
  while pc^ <> #0 do
    begin
      if (pc^ = '%') and ((pc + 1)^ <> #0) and ((pc + 2)^ <> #0) then
        begin
          if ((pc + 1)^ in ['u', 'U']) then
            begin
              Inc(pc);
              StrLCopy(PChar(S2), pc + 1, 4);
              I := Hex2Int(S2);
              if I = 0 then
                pcResult^ := ' '
              else
                begin
                  S3 := UTF8Encode(UnicodeString(WideChar(I)));
                  if S3 = '' then
                    pcResult^ := ' '
                  else
                    begin
                      StrLCopy(pcResult, PChar(S3), Length(S3));
                      Inc(pcResult, Length(S3) - 1);
                    end;
                end;
              Inc(pc, 4);
            end
          else
            begin
              StrLCopy(PChar(S1), pc + 1, 2);
              I := Hex2Int(S1);
              if I = 0 then
                pcResult^ := ' '
              else
                pcResult^ := Char(Byte(I));
              Inc(pc, 2);
            end;
        end
      else
        pcResult^ := pc^;
      Inc(pcResult);
      Inc(pc);
    end;
  SetLength(Result, pcResult - PChar(Result));
end;

function str_EncodeToHex(const S: string; const Allowed: TSysCharSet): string;

var
  i, L: Integer;
  P: PChar;
  iSkip: Integer = 0;
  ch: Char;
begin
  L := Length(s);
  for i := 1 to Length(s) do
    begin
      ch := S[i];
      if ch > #127 then
        begin //ToDo utf8 - %u043d
          {$IFDEF DEBUG_MODE}
          WriteDebugLogLn('str_EncodeToHex(): utf8 string');{$ENDIF}
          Exit(S);//RaiseToDoException();
        end
      else if iSkip > 0 then
        Dec(iSkip)
      else if ch = '%' then
        iSkip := 2
      else if not (ch in Allowed) then
        Inc(L,2);
    end;
  if L = Length(s) then
  begin
    Result := s;
    Exit;
  end;

  SetLength(Result, L);
  P := @Result[1];
  for i := 1 to Length(s) do
  begin
    if not (s[i] in Allowed) then
    begin
      P^ := '%'; Inc(P);
      StrFmt(P, '%.2x', [ord(s[i])]); Inc(P);
    end
    else
      P^ := s[i];
    Inc(P);
  end;
end;

function Wait(AWaitMSec: Cardinal; AOnWaitFlag: System.PBoolean;
  AFlagValue: Boolean; AIgnoreAppCloseEvent: Boolean): Boolean;
var
  iTick: QWord;
begin
  Result := False;
  if AOnWaitFlag = nil then Exit;
  iTick := GetTickCount + AWaitMSec;
  while iTick > GetTickCount do
    begin
      if AOnWaitFlag^ = AFlagValue then
        Exit(True);

      if not AIgnoreAppCloseEvent and (apsClosing in AppStates) then
        Break
      else if GetCurrentThreadId = MainThreadID then
        CheckSynchronize()
      else
        Sleep(10);
    end;
  Result := AOnWaitFlag^ = AFlagValue;
end;

function Wait(AWaitMSec: Cardinal; AOnWaitFlag: PPtrInt;
  AFlagValue: PtrInt; AIfNotFlagValueEqual: Boolean;
  AIgnoreAppCloseEvent: Boolean): PtrInt;
var
  iTick: QWord;
begin
  Result := AFlagValue;
  if AOnWaitFlag = nil then Exit;
  iTick := GetTickCount + AWaitMSec;
  while iTick > GetTickCount do
    begin
      if AIfNotFlagValueEqual then
        begin
          if AOnWaitFlag^ <> AFlagValue then
            Exit(AOnWaitFlag^);
        end
      else if AOnWaitFlag^ = AFlagValue then
        Exit(AFlagValue);

      if not AIgnoreAppCloseEvent and (apsClosing in AppStates) then
        Break
      else if GetCurrentThreadId = MainThreadID then
        CheckSynchronize()
      else
        Sleep(10);
    end;
  Result := AOnWaitFlag^;
end;

type
  { TAsyncMethodThread }

  TAsyncMethodThread = class(TThread)
  private
    FMethod: TNotifyEvent;
    FProc: TNotifyProcedure;
    FData: Pointer;
    FWaitMSec: Integer;
    FIsSyncronize: Boolean;
    procedure DoMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(const AData: Pointer; const AWaitMSec: Integer;
      const AIsSyncronize: Boolean; const AMethod: TNotifyEvent;
      const AProc: TNotifyProcedure);
  end;

{ TAsyncMethodThread }

procedure TAsyncMethodThread.DoMethod;
begin
  if Assigned(FMethod) then
    FMethod(TObject(FData))
  else if Assigned(FProc) then
    FProc(TObject(FData));
end;

procedure TAsyncMethodThread.Execute;
var
  i: Integer;
begin
  //ToDo not always workon AppCloseEvent
  //if FWaitMSec > 0 then
  //  RTLeventWaitFor(AppCloseEvent, FWaitMSec);
  if FWaitMSec >= 1000 then
    i := 1000
  else
    i := FWaitMSec;
  while FWaitMSec > 0 do
    begin
      Sleep(i);
      Dec(FWaitMSec, i);
      if (apsClosing in AppStates) then Break;
    end;
  if FIsSyncronize then
    Synchronize(DoMethod)
  else
    DoMethod;
end;

constructor TAsyncMethodThread.Create(const AData: Pointer;
  const AWaitMSec: Integer; const AIsSyncronize: Boolean;
  const AMethod: TNotifyEvent; const AProc: TNotifyProcedure);
begin
  FMethod := AMethod;
  FProc := AProc;
  FData := AData;
  FWaitMSec := AWaitMSec;
  FIsSyncronize := AIsSyncronize;
  FreeOnTerminate := True;
  inherited Create(False, 16 * 1024);
end;


procedure CallNotifyEventAsync(const AMethod: TNotifyEvent;
  const AData: Pointer; const AWaitMSec: Integer; const AIsSyncronize: Boolean);
begin
  TAsyncMethodThread.Create(AData, AWaitMSec, AIsSyncronize, AMethod, nil);
end;

procedure CallNotifyProcAsync(const AProc: TNotifyProcedure;
  const AData: Pointer; const AWaitMSec: Integer; const AIsSyncronize: Boolean);
begin
  TAsyncMethodThread.Create(AData, AWaitMSec, AIsSyncronize, nil, AProc);
end;

function res_ExtractToFile(const AResName, AFileName: string;
      AHInstance: TFPResourceHMODULE = 0): Boolean;
var
  rs: TResourceStream = nil;
  F: TFileStream = nil;
  Len: Int64;
begin
  Result := False;
  if AHInstance = 0 then
    AHInstance := HINSTANCE;
  rs := TResourceStream.Create(AHInstance, AResName, RT_RCDATA);
  try
    try
      F := TFileStream.Create(AFileName, fmCreate);
      Len := rs.Size;
      Result := F.CopyFrom(rs, Len) = Len;
    except end;
  finally
    rs.Free;
    F.Free;
  end;
end;

function res_ExtractToString(const AResName: string; out S: string;
      AHInstance: TFPResourceHMODULE = 0): Boolean;
var
  rs: TResourceStream = nil;
  F: TStringStream = nil;
  Len: Int64;
begin
  Result := False;
  if AHInstance = 0 then
    {$IFDEF WIDGETSET_DLL}
    AHInstance := LibraryHInstance;
    {$ELSE}
    AHInstance := HINSTANCE;{$ENDIF}
  rs := TResourceStream.Create(AHInstance, AResName, RT_RCDATA);
  try
    try
      F := TStringStream.Create('');
      Len := rs.Size;
      Result := F.CopyFrom(rs, Len) = Len;
      if Result then
        S := F.DataString;
    except end;
  finally
    rs.Free;
    F.Free;
  end;
end;

function IsMainThread: Boolean;
begin
  Result := GetCurrentThreadId = MainThreadID;
end;

function IsMainThread(AIsRaiseException: Boolean): Boolean;
begin
  Result := GetCurrentThreadId = MainThreadID;
  if not Result and AIsRaiseException then
    raise Exception.Create('IsMainThread(): False'); //ToDo silent in thread
end;

function GuidCreate: TGUID;
begin
  CreateGUID(Result);
end;

function GuidCreateString(const AWithoutBrace: Boolean): string;
begin
  Result := GUIDToString(GuidCreate);
  if AWithoutBrace then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function pt_IsBetweenLines(P, P1, P2: TPoint): Boolean;
begin
  Result := ((P1.Y < P.Y) and (P.Y < P2.Y)) or
    ((P1.Y = P.Y) and (P1.X <= P.X)) or
    ((P2.Y = P.Y) and (P.X <= P2.X));
end;

function pt_IsInRect(P, lt, br: TPoint): Boolean; overload;
begin
  Result := (lt.Y <= P.Y) and (lt.x <= P.x) and
            (P.Y <= br.Y) and (P.x <= br.x);
end;

function pt_IsInRect(p: TPoint; r: TRect): Boolean; overload;
begin
  Result := (r.Top <= p.y) and (r.Left <= p.x) and
            (p.y <= r.Bottom) and (p.x <= r.Right);
end;

function pt_IsSame(p1, p2: TPoint): Boolean;
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;

function pt_Min(p1, p2: TPoint): TPoint;
begin
  if p1.y = p2.y then
    begin
      Result.y := p1.y;
      Result.x := Min(p1.x, p2.x);
    end
  else if p1.y < p2.y then
    Result := p1
  else
    Result := p2;
end;

function pt_Max(p1, p2: TPoint): TPoint;
begin
  if p1.y = p2.y then
    begin
      Result.y := p1.y;
      Result.x := Max(p1.x, p2.x);
    end
  else if p1.y > p2.y then
    Result := p1
  else
    Result := p2;
end;

function pt_ToZeroBased(const p: TPoint): TPoint;
begin
  Result.x := p.x - 1;
  Result.y := p.y - 1;
end;

function pt_ToOneBased(const p: TPoint): TPoint;
begin
  Result.x := p.x + 1;
  Result.y := p.y + 1;
end;

function _UnixOpenDocument(APath: string): Boolean;
var
  lApp: String;
begin
  if not FileExistsUTF8(APath) then Exit(False);

  lApp:=FindFilenameOfCmd('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
  if lApp='' then
    lApp:=FindFilenameOfCmd('kfmclient'); // KDE command
  if lApp='' then
    lApp:=FindFilenameOfCmd('gnome-open'); // GNOME command
  if lApp='' then
    Exit(False);

  Result := True;
  if (APath<>'') and (APath[1]<>'"') then
    APath :=QuotedStr(APath);
  RunCmdFromPath(lApp, APath);
end;

function ExecuteDocument(const APath: string; const AParams: string;
  const ACurrentDirectory: string; const AShowError: Boolean): Boolean;

begin
  {$IFDEF WINDOWS}
  Result := ExecuteDocumentW(UTF8Decode(file_QuotedName(APath)),
      UTF8Decode(AParams),
      UTF8Decode(ACurrentDirectory), AShowError);
  {$ELSE}
  _UnixOpenDocument(APath);
  {$ENDIF}
end;

function ExecuteDocumentEx(const APath, AParams, ACurrentDirectory: string;
  const AShowError: Boolean; out AProcessHandle: THandle): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := ExecuteDocumentExW(UTF8Decode(file_QuotedName(APath)),
      UTF8Decode(AParams),
      UTF8Decode(ACurrentDirectory), AShowError, AProcessHandle);
  {$ELSE}
  AProcessHandle := 0;
  _UnixOpenDocument(APath);
  {$ENDIF}
end;

function ExecuteProgram(const AProg, AParams: string;
  const ACurrentDirectory: string; const AFlags: TExecProgramFlags;
  const AMSecWait: Integer; const AProcessId: PProcessId;
  const AProcessHandle: PHandle): Integer;
var
  p: TProcessUTF8;
  opts: TProcessOptions;
  iWait: Integer = 0;
begin
  p := TProcessUTF8.Create(nil);
  try
    p.Executable := AProg;
    SplitCmdLineParams(AParams, p.Parameters);
    p.CurrentDirectory := ACurrentDirectory;
    p.InheritHandles := epfInheritHandles in AFlags;
    opts := [];
    if epfNewGroup in AFlags then
      Include(Opts, poNewProcessGroup)
    else if epfNewConsole in AFlags then
      Include(Opts, poNewConsole)
    else if epfHidden in AFlags then
      p.ShowWindow := swoHIDE;
    p.Options := opts;

    try
      p.Execute;
      if AProcessHandle <> nil then
        AProcessHandle^ := p.ProcessHandle;
      if AProcessId <> nil then
        AProcessId^ := p.ProcessID;
    except
      Exit(EXEC_FAIL);
    end;

    if AMSecWait = -1 then
      iWait := MaxInt
    else
      iWait := AMSecWait;
    if (epfWait in AFlags) and (iWait > 0) then
      begin
        while p.Running do
          begin
            if (apsClosing in AppStates) and
                (System.GetCurrentThreadId <> MainThreadID) then
              begin
                p.Terminate(EXEC_FAIL);
                Exit(EXEC_FAIL);
              end;
            Dec(iWait, 10);
            if iWait < 0 then
              begin
                if epfKillIfTimeOut in AFlags then
                  p.Terminate(EXEC_TIME_OUT);
                Result := EXEC_TIME_OUT;
                Exit;
              end;
          end;//while p.Running
        Result := p.ExitStatus;
      end
    else
      Result := EXEC_OK;

  finally
    p.Free;
  end;
  //{$IFDEF WINDOWS}
  //Result := ExecuteProgramExW(UTF8Decode(AProg), UTF8Decode(AParams),
  //    UTF8Decode(ACurrentDirectory), AFlags, AMSecWait,
  //    AProcessId, AJobHandle, AProcessHandle);
  //{$ELSE}
  ////https://wiki.lazarus.freepascal.org/Executing_External_Programs ((Process.)RunCommand)
  //RaiseToDoException('ExecuteProgram');
  //{$ENDIF}
end;

function GetProgramOutput(const APath, ACommaParams: string; strs: TStrings;
  AWaitSecIfNoOutput: Cardinal; const ACurrentDirectory: string;
  const AKillIfTimeOut: Boolean): Integer;
const
   READ_BYTES = 2048;
var
  p: TProcessUTF8;
  M: TMemoryStream;
  n: LongInt;
  BytesRead: LongInt;
  ws: UnicodeString;
  S: string;
  iWait: Integer;
  st: TInputPipeStream;
  IsTimeOut: Boolean = False;
begin
  Result := 0;
  if strs <> nil then
    strs.Clear;
  BytesRead := 0;
  M := TMemoryStream.Create;
  p := TProcessUTF8.Create(nil);
  try
    p.Executable := APath;
    if ACurrentDirectory = '' then
      p.CurrentDirectory := file_ExtractDir(APath)
    else
      p.CurrentDirectory := ACurrentDirectory;
    p.Parameters.CommaText := ACommaParams;
    p.Options := [poUsePipes, poStderrToOutPut, poNewConsole];
    p.StartupOptions := [suoUseShowWindow];
    p.ShowWindow := swoHIDE;
    try
      p.Execute;
    except
      Exit(EXEC_FAIL);
    end;
    st := P.Output;
    //if P.Running and AKillIfTimeOut then   //new:6.6 ChildProcesses no need -> AppClosing enough
    //  begin
    //    ChildProcesses.Add(P.ProcessHandle);
    //  end;
    if AWaitSecIfNoOutput > 0 then
      iWait := AWaitSecIfNoOutput * 1000;
    while P.Running do
      begin
        if (apsClosing in AppStates) and//new:6.6
            ((AWaitSecIfNoOutput <= 0) or (System.GetCurrentThreadId <> MainThreadID)) //new:6.9
              then
          begin
            p.Terminate(EXEC_FAIL);
            Exit(EXEC_FAIL);
          end;
        M.SetSize(BytesRead + READ_BYTES);
        if st.NumBytesAvailable > 0 then
          n := st.Read((M.Memory + BytesRead)^, READ_BYTES)
        else
          n := 0;
        if n > 0 then
          begin
            Inc(BytesRead, n);
            if AWaitSecIfNoOutput > 0 then
              iWait := AWaitSecIfNoOutput * 1000;
          end
        else
          begin
            Sleep(100);
            if AWaitSecIfNoOutput > 0 then
              begin
                Dec(iWait, 100);
                if iWait < 0 then
                  begin
                    IsTimeOut := True;
                    Break;
                  end;
              end;
          end;
      end;
    repeat
      M.SetSize(BytesRead + READ_BYTES);
      if st.NumBytesAvailable > 0 then
        begin
          n := st.Read((M.Memory + BytesRead)^, READ_BYTES);
          if n > 0 then
            Inc(BytesRead, n);
        end
      else
        Break;
    until n <= 0;
    M.SetSize(BytesRead);
    if strs <> nil then
      begin
        strs.LoadFromStream(M);
        {$IFDEF WINDOWS}
        for n := 0 to strs.Count - 1 do
          begin
            S := strs[n];
            if str_IsUTF8(S) then Continue; //ToDo ? Add in Win8 (Check in Previous Versions)
            SetLength(ws{%H-}, Length(S));
            OemToCharBuffW(PChar(S), PWideChar(ws), Length(S));
            strs[n] := UTF8Encode(ws);
          end;
        {$ELSE}{$ENDIF}
      end;
    Result := p.ExitStatus;
    if AKillIfTimeOut then
      begin
        if IsTimeOut then
          begin
            p.Terminate(EXEC_TIME_OUT);//new:6.9.3 (EXEC_FAIL);
            Result := EXEC_TIME_OUT;//new:6.9.3 EXEC_FAIL;
          end;
        //new:6.6 if not (apsClosing in AppStates) then //ChildProcesses may be already freed in finalization section
        //new:6.6   ChildProcesses.Remove(p.ProcessHandle);
      end;
  finally
    p.Free;
    M.Free;
  end;
end;

function GetProgramOutput(const APath, ACommaParams: string; out AResult: string;
    AWaitSecIfNoOutput: Cardinal = 0; const ACurrentDirectory: string = '';
    const AKillIfTimeOut: Boolean = False): Integer; overload;
var
  strs: TStringList;
begin
  AResult := '';
  strs := TStringList.Create;
  try
    Result := GetProgramOutput(APath, ACommaParams, strs, AWaitSecIfNoOutput,
        ACurrentDirectory, AKillIfTimeOut);
    AResult := Trim(strs.Text);
  finally
    strs.Free;
  end;
end;

function GetProcessHandle: THandle;
begin
  {$IFDEF WINDOWS}
  Result := OpenProcess(PROCESS_ALL_ACCESS, True, GetProcessID);
  {$ELSE}
  Result := GetProcessID;
  {$ENDIF}
end;

function GetProcessHandleByPID(const pid: TProcessId): THandle;
begin
  {$IFDEF WINDOWS}
  Result := OpenProcess(PROCESS_ALL_ACCESS, True, pid);
  {$ELSE}
  Result := pid;
  {$ENDIF}
end;

function GetProcessIDByHandle(const AProcessHandle: THandle): TProcessId;
begin
  {$IFDEF WINDOWS}
  Result := GetProcessIdByProcessHandle(AProcessHandle);
 // PROCESS_BASIC_INFORMATION pbi = {} ;
	//ULONG ulSize ;
 //
 // LONG ( WINAPI *NtQueryInformationProcess )( HANDLE ProcessHandle, ULONG ProcessInformationClass, PVOID ProcessInformation, ULONG ProcessInformationLength, PULONG ReturnLength ) ;
 // *(FARPROC *)&NtQueryInformationProcess = GetProcAddress( GetModuleHandle("ntdll"), "NtQueryInformationProcess" ) ;
 //
 // if( NtQueryInformationProcess != NULL && NtQueryInformationProcess( process_handle, 0, &pbi, sizeof( pbi ), &ulSize ) >= 0 && ulSize == sizeof( pbi ) )
	//  return pbi.UniqueProcessId ;
 // return 0 ;
  {$ELSE}
  Result := AProcessHandle;
  {$ENDIF}
end;

function CloseProcessHandle(const AProcessHandle: THandle): Boolean;
begin
  {$IFDEF WINDOWS}
  //if close twice - WinAPI error
  Result := (AProcessHandle > 0) and CloseHandle(AProcessHandle);
  {$ELSE}
  Result := True;//only for windows
  {$ENDIF}
end;

function TerminateProgram(const AProcessHandle: THandle;
  const AExitCode: Cardinal): Boolean;
begin
  {$IFDEF WINDOWS}   //? CloseProcessHandle(AHandle);
  Result := Windows.TerminateProcess(AProcessHandle, AExitCode);
  {$ELSE}
  RaiseToDoException('TerminateProgram');
  {$ENDIF}
end;

function TerminateProcess(const APid: TProcessId; const AExitCode: Cardinal): Boolean;
var
  h: THandle;
begin
  h := GetProcessHandleByPID(APid);
  Result := TerminateProgram(h, AExitCode);
  CloseProcessHandle(h);
end;

function TerminateProcessAndChildren(const APid: TProcessId; const AExitCode: Cardinal): Boolean;{$IFDEF WINDOWS}

  procedure _TerminateChildren(APid: TProcessId);
  var
    Snap: HANDLE;
    Entry: TProcessEntry32;
  begin
    Snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, APid);
    if Snap <> INVALID_HANDLE_VALUE then
      begin
        Entry.dwSize := SizeOf(Entry);
        if Process32First(Snap, Entry) then
          begin
            if Entry.th32ParentProcessID = APid then
              TerminateProcess(Entry.th32ProcessID, AExitCode);
            while Process32Next(Snap, Entry) do
              begin
                if Entry.th32ParentProcessID = APid then
                  TerminateProcess(Entry.th32ProcessID, AExitCode);
              end;
          end;
        CloseHandle(Snap);
      end;
  end;

begin
  if APid <= 0 then Exit;
  _TerminateChildren(APid);
  Result := TerminateProcess(APid, AExitCode);
end;
{$ELSE}
begin
  //ToDo Children
  Result := TerminateProcess(APid, AExitCode);
end;
{$ENDIF}

function IsProgramRunning(const AProcessHandle: THandle): Boolean;
var
  iExitCode: DWORD;
begin
  if AProcessHandle = 0 then Exit(False);
  {$IFDEF WINDOWS}
  Result := GetExitCodeProcess(AProcessHandle, iExitCode{%H-}) and
      (iExitCode = STILL_ACTIVE);
  {$ELSE}
  RaiseToDoException('IsProgramRunning');
  {$ENDIF}
end;

function IsProcessRunning(const APid: TProcessId): Boolean;
var
  h: THandle;
begin
  h := GetProcessHandleByPID(APid);
  Result := IsProgramRunning(h);
  CloseProcessHandle(h);
end;

function GetProcessExitCode(const AProcessHandle: THandle; out ACode: Integer
  ): Boolean;
{$IFDEF WINDOWS}
begin
  GetExitCodeProcess(AProcessHandle, DWORD({%H-}ACode));
  Result := (ACode <> Still_Active);
end;
{$ENDIF}
{$IFDEF UNIX}
//var
//  res: cint;
begin
  RaiseToDoException();//Check
  {$IFDEF ANDROID}
  {$ELSE}
  //repeat
  //  res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
  //until (res<>-1) or (fpgeterrno<>ESysEINTR);
  //result:=res=Handle;
  //If Result then
  // begin
  //    if wifexited(FExitCode) then
  //      FExitCode:=wexitstatus(FExitCode);
  //    // else pass errorvalue unmodified like shell does, bug #22055
  //   end
  // else
  //  FexitCode:=cardinal(-1); // was 0, better testable for abnormal exit.
   {$ENDIF}
end;
{$ENDIF}

function IsJobRunning(const AJob: THandle): Boolean;
{$IFDEF WINDOWS}
var
  //Info: JOBOBJECT_BASIC_PROCESS_ID_LIST;
  Info1: JOBOBJECT_BASIC_ACCOUNTING_INFORMATION;
begin
  //!!! May be False when Parent Process Handle is still active
  //!!! use if IsJobRunning(hJob) or IsProgramRunning(hProcess);
  //FillChar(Info{%H-}, SizeOf(Info), 0);
  //Result := QueryInformationJobObject(AJob, JobObjectBasicProcessIdList,
  //      @Info, SizeOf(Info), nil) and
  //    (Info.NumberOfAssignedProcesses > 0);

  FillChar(Info1{%H-}, SizeOf(Info1), 0);
  Result := QueryInformationJobObject(AJob, JobObjectBasicAccountingInformation,
        @Info1, SizeOf(Info1), nil) and
      (Info1.ActiveProcesses > 0);
end;
{$ELSE}
begin
  RaiseToDoException('IsJobRunning()');
end;
{$ENDIF}

function CreateJob(const AProcessHandle: THandle): THandle;
{$IFDEF WINDOWS}
begin
  Result := CreateJobObject(nil, nil);
  if AProcessHandle > 0 then
    AddProcessToJob(Result, AProcessHandle);
end;
{$ELSE}
begin
  //https://linux.die.net/man/2/getpgrp
  //int setpgid(pid_t pid, pid_t pgid);
  //pid_t getpgid(pid_t pid);
  RaiseToDoException('CreateJob()');
end;
{$ENDIF}

function AddProcessToJob(const AJob: THandle; const AProcessHandle: THandle): Boolean;
{$IFDEF WINDOWS}
begin
  Result := AssignProcessToJobObject(AJob, AProcessHandle);
end;
{$ELSE}
begin
  RaiseToDoException('AddProcessToJob()');
end;
{$ENDIF}

function TerminateJob(const AJob: THandle; const AExitCode: Cardinal): Boolean;
{$IFDEF WINDOWS}
begin
  Result := TerminateJobObject(AJob, AExitCode);
end;
{$ELSE}
begin
  RaiseToDoException('TerminateJob()');
end;
{$ENDIF}

function CloseJobHandle(const AJobHandle: THandle): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := (AJobHandle > 0) and CloseHandle(AJobHandle);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function IsProgramForeground(const AProcessID: Cardinal): Boolean;
{$IFDEF WINDOWS}
begin
  Result := IsHwndBelongToPID(GetForegroundWindow, AProcessID);
end;
{$ELSE}
begin
  RaiseToDoException('IsProgramForeground');
end;
{$ENDIF}

function IsAppState(const AState: TAppState): Boolean;
begin
  Result := AState in AppStates;
end;

var
  _AppPathAlt: string;
function SetAppPath(const APath: string; AShowError: Boolean): Boolean;
begin
  Result := dir_Exists(APath);
  if Result then
    _AppPathAlt := APath
  else
    begin
      _AppPathAlt := '';
      if AShowError then
        ShowErrorFmt(RS_FileNotExists, [APath]);
    end;
end;

function AppPath: string;
begin
  if _AppPathAlt = '' then
    begin
      {$IFDEF DARWIN}
      _AppPathAlt := ExtractFilePath(ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStrUTF8(0)))));
      {$ELSE}
      _AppPathAlt := ExtractFilePath(ParamStrUTF8(0));
      {$ENDIF}
    end;
  Result := _AppPathAlt;
end;

function AppConfigPath(const AIsGlobal: Boolean; const AIsForce: Boolean): string;
begin
  if Assigned(OnAppConfigPath) then
    Result := OnAppConfigPath(AIsGlobal, AIsForce)
  else
    Result := GetAppConfigDirUTF8(AIsGlobal, AIsForce);
end;

function AppTempPath(const IsForce: Boolean): string;
begin
  Result := GetTempDir(True) + file_ExtractNameOnly(ParamStrUTF8(0)) + PathDelim;
  //Result := AppDataAppPath + 'Temp' + PathDelim;
  if IsForce then
    dir_Force(Result);
end;

{$IFDEF DEBUG_MODE}
procedure WriteDebugLogLn(const S: string);
begin
  if DebugLogFileName <> '' then
    str_AddToFile(DebugLogFileName, S, True);
end;

procedure WriteDebugLogLnFmt(const S: string; const Args: array of const);
begin
  if DebugLogFileName <> '' then
    str_AddToFile(DebugLogFileName, Format(S, Args), True);
end;

procedure WriteToDebugConsole(const S: string; const AIsAddToLastLine: Boolean;
  const AForce: Boolean);
begin
  if Assigned(OnWriteToDebugConsole) then
    OnWriteToDebugConsole(S, AIsAddToLastLine, AForce);
end;

procedure ClearDebugConsole;
begin
  if Assigned(OnClearDebugConsole) then
    OnClearDebugConsole();
end;
{$ENDIF}
initialization
  AppCloseEvent := RTLEventCreate;
  _AutoTempFileDeleter := TAutoTempFileDeleter.Create;
  {$IFDEF DEBUG_MODE}
  if sys_ParamExists('--debug-log') then
    begin
      DebugLogFileName := ExtractFilePath(ParamStrUTF8(0)) + 'DebugLog.txt';
      if file_Exists(DebugLogFileName) then
        file_Delete(DebugLogFileName);
    end;
  {$ENDIF}

finalization
  RTLeventdestroy(AppCloseEvent);
  FreeAndNil(_AutoTempFileDeleter);
  {$IFDEF DEBUG_MODE}
  if (DebugLogFileName <> '') and file_Exists(DebugLogFileName) then
    ExecuteDocument(DebugLogFileName);
  {$ENDIF}

end.

