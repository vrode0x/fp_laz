unit vr_classes;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, LazMethodList, IniFiles, contnrs,
  vr_types, vr_utils, vr_intfs, vr_fpclasses, vr_SysUtils,
  RtlConsts, Math, vr_timer, vr_variant;

type
  //THashedStringList = IniFiles.THashedStringList;
  TNotifyProc = vr_types.TNotifyProc;
  TNotifyMethod = vr_types.TNotifyMethod;

  { TStringListHashed }

  TStringListHashed = class(THashedStringList)
  public
    function Find(const S: string; out Index: Integer): Boolean; override;
  end;

  { TIFPObject }

  TIFPObject = class(TInterfacedObject, IFPObject)
  protected
    { IFPObject }
    function GetFPObject: TObject;
  end;

  { TvrList }

  TvrList = class(TList)
  public
    function AddUnique(AItem: Pointer): Integer;
  end;

  { TIFPList }

  TIFPList = class(TvrList, IFPList)
  protected
    FRefCount: LongInt;
    { IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    { IFPObject }
    function GetFPObject: TObject;
    { IFPList}
     procedure AddList(const AList: IFPList);
  end;

  { TThreadObjectList }

  TThreadObjectList = class
  private
    FList: TObjectList;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create(const AFreeObjects: Boolean = True);
    destructor Destroy; override;
    procedure Add(Item: TObject);
    procedure Clear;
    procedure Remove(Item: TObject);
    function  LockList: TObjectList;
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  { TThreadMethodListBase }

  TThreadMethodListBase = class(TObject)
  private
    FList: TMethodList;
    FLock: TRTLCriticalSection;
  protected
    property List: TMethodList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AMethod: TMethod; AsLast: Boolean = True);
    procedure Remove(const AMethod: TMethod);
    procedure RemoveAllMethodsOfObject(const AnObject: TObject);
    procedure CallNotifyEvents(Sender: TObject);
    function NextDownMethod(var AIndex: integer; out AMethod: TMethod): boolean;
    function Count: Integer;
    function  LockList: TMethodList;
    procedure UnlockList;
  end;

  TThreadMethodList = class(TThreadMethodListBase)
  end;

  { TvrInterfaceList }

  TvrInterfaceList = class(TInterfaceList, IFPInterfaceList)
  private
    { IFPInterfaceList }
    function GetFPObject: TObject;
    procedure AssignIntfList(const AList: IFPInterfaceList);
    procedure IFPInterfaceList.Assign = AssignIntfList;
  public
    procedure Assign(const AList: TInterfaceList);
    function AddUnique(const AItem: IUnknown) : Integer;
    function ExportToArray: TInterfaceArray;
  end;

  { TStringStreamUTF8 }

  TStringStreamUTF8 = class(TStringStream)
  private
  protected
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadUntil(out AResult: string; AChar: Char): LongInt;
  end;

  { TIStringStreamUTF8 }

  TIStringStreamUTF8 = class(TStringStreamUTF8, IFPStringStream)
  private
    FRefCount: LongInt;
  protected
    { IFPStringStream }
    {$IF FPC_FULLVERSION >= 030200} //ToDo not compile if comment >>>
    function ReadAnsiString: string;//Exists in TStream private: if protected -> OK
    function GetUnicodeDataString: UnicodeString; //<<<
    function GetOwnsEncoding: Boolean;
    function GetEncoding: TEncoding;
    {$ENDIF}
    function GetDataString: string;
    { IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    { IFPObject }
    function GetFPObject: TObject;
  end;

  { TFileStreamUTF8 }

  TFileStreamUTF8 = class(TFileStream)//new:6.9.2THandleStream)
  private
    FFileName: string;
  protected
  public
    constructor Create(const AFileName: string; Mode: Word = fmOpenReadWrite); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    //destructor Destroy; override;
    function ReadDateTime: TDateTime;
    function ReadInteger: Integer;
    procedure WriteDateTime(const dt: TDateTime);
    procedure WriteInteger(const i: Integer);
    property FileName: string read FFilename;
  end;

  { TIFileStreamUTF8 }

  TIFileStreamUTF8 = class(TFileStreamUTF8, IFPFileStream)
  private
    FRefCount: LongInt;
  protected
    { IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    { IFPObject }
    function GetFPObject: TObject;
  end;

  { TStringListUTF8 }

  TStringListUTF8 = class(TStringList)
  private
    FTag: PtrInt;
    FTrimOnCompare: Boolean;
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  public
    destructor Destroy; override;{$IF FPC_FULLVERSION >= 030000}
    function Find(const S: string; out Index: Integer): Boolean; override;{$ENDIF}
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure AddStringsWithIndexInObject(ASource: TStrings);
    function AddIfNotFind(const S: string; const AObject: TObject = nil): Boolean;
    function AddIfNotFindEx(const S: string; const AObject: TObject; out AIndex: Integer): Boolean;
    procedure Grow(ANewSize: Integer);
    function Remove(const S: string): Boolean; overload;
    function Remove(const AIndex: Integer; out S: string): Boolean; overload;
    function RemoveEx(const AIndex: Integer; out S: string; out AObject: TObject): Boolean;
    procedure RemoveName(const AName: string);
    procedure RemoveAllObjects(const AObject: TObject);
    procedure SetCount(const ASize: Integer);
    function ExportToArray: TStringArray;
    function ExportByObject(const AObject: TObject): TStringArray;
    procedure ImportArray(const AValue: TStringArray);
    property Tag: PtrInt read FTag write FTag;
    property TrimOnCompare: Boolean read FTrimOnCompare write FTrimOnCompare;
    function FindObject(const S: string; Out AObject: TObject): Boolean;
    procedure FreeObjects;
  end;

  { TIStringListUTF8 }

  TIStringListUTF8 = class(TStringListUTF8, IFPStringListUTF8)
  private
    FRefCount: LongInt;
    { IFPStringListUTF8 }
    {$IF FPC_FULLVERSION >= 030200}//ToDo not compile if comment >>>
    function GetCommaText: string; //exists in TStrings private: if protected -> OK
    Function GetLBS : TTextLineBreakStyle;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;

    procedure SetSortStyle(AValue: TStringsSortStyle);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(b : boolean);
    procedure SetCommaText(const Value: string);
    Procedure SetLBS (AValue : TTextLineBreakStyle);
    procedure SetValue(const Name, Value: string);
    {$ENDIF}            //<<<
    {$IF FPC_FULLVERSION >= 030000}
    function GetSortStyle: TStringsSortStyle;{$ENDIF}
    function GetCaseSensitive: Boolean;
    function GetSorted: Boolean;
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(AValue: Boolean);
  protected
    { IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    { IFPStrings }
    procedure AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False); overload;
    { IFPObject }
    function GetFPObject: TObject;
  public
    constructor Create(const ASorted: Boolean = False; const ACaseSens: Boolean = False); overload;
  end;

  { TFileNameList }

  TFileNameList = class(TIStringListUTF8)
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  public
    procedure AfterConstruction; override;
    procedure ClearNotExistFiles(const ACheckDirs: Boolean = False);
  end;

  TIFileNameList = class(TFileNameList, IFPFileNameList) end;

  TDataStringListRec = record
    FObject: TObject;
    Data: Pointer;
  end;
  PDataStringListRec = ^TDataStringListRec;
  TCompareDataProc = function(AData1, AData2: Pointer): Integer of object;

  { TDataStringList }

  TDataStringList = class(TIStringListUTF8)
  private
    FDataSize: Cardinal;
    FOnCompareData: TCompareDataProc;
    FOnFreeData: TPointerMethod;
    FOnInitData: TPointerMethod;
    procedure FreeListData(Index: Integer);
    procedure FreeListDataAll;
    procedure PutListData(const Index: Integer; Data: PDataStringListRec);
    //procedure PutData(Index: Integer; const Data: Pointer);
  protected
    function GetListData(const Index: Integer): PDataStringListRec;
    function GetData(const Index: Integer): Pointer; virtual;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure DoFreeData(const P: Pointer); virtual;
    function DoCompareData(AData1, AData2: Pointer): Integer; virtual;
    procedure DoInitData(const {%H-}P: Pointer); virtual;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure DoInsertData(const S: string;
        const AObject: TObject; const AData: Pointer; const Index: Integer);
  public
    constructor Create(const ADataSize: Cardinal; const AOnFreeData: TPointerMethod = nil;
        const ASorted: Boolean = False; const AOnInitData: TPointerMethod = nil); virtual;
    destructor Destroy; override;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    function AddData(const S: string; const AObject: TObject; const AData: Pointer): Integer;

    { IFPDataStringList }
    function IndexOfData(AData: Pointer): Integer;
    property Data[Index: Integer]: Pointer read GetData; // write PutData;
    property OnFreeData: TPointerMethod read FOnFreeData write FOnFreeData;
    property OnInitData: TPointerMethod read FOnInitData write FOnInitData;
    property OnCompareData: TCompareDataProc read FOnCompareData write FOnCompareData;
  end;

  { TIDataStringList }

  TIDataStringList = class(TDataStringList, IFPDataStringList){$IF FPC_FULLVERSION >= 030200}
  protected
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(AValue: Boolean);
    function GetLBS: TTextLineBreakStyle;
    procedure SetLBS(AValue: TTextLineBreakStyle);
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; const AValue: string);
    function GetCommaText: string;
    procedure SetCommaText(const AValue: string);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(AValue: Boolean);
    function GetSorted: Boolean;
    procedure SetSorted(AValue: Boolean);
    function GetSortStyle: TStringsSortStyle;
    procedure SetSortStyle(AValue: TStringsSortStyle);{$ENDIF}
  end;

  { TThreadStringListBase }   //!!! Use only inherited classes

  TThreadStringListBase = class(TInterfacedObject{, IFPThreadStrings})
  private
    FList: TIStringListUTF8;//TDataStringList;
    FListIntf: IFPStringListUTF8;//IFPDataStringList;
    function Get(Index: Integer): string;
    function GetObject(Index: Integer): TObject;
    function GetOwnsObjects: boolean;
    function GetSorted: Boolean;
    function GetValue(const AName: string): string;
    procedure Put(Index: Integer; const AValue: string);
    procedure PutObject(Index: Integer; const AValue: TObject);
    procedure SetOwnsObjects(AValue: boolean);
    procedure SetSorted(AValue: Boolean);
    procedure SetValue(const AName: string; const AValue: string);
  protected
    FLock: TRTLCriticalSection;
    property List: TIStringListUTF8 read FList;
    property ListIntf: IFPStringListUTF8 read FListIntf;
    procedure CreateBase(const AList: IFPStringListUTF8);
  public
    destructor Destroy; override;
    function Add(const S: string): Integer;
    procedure Remove(const S: string); overload;
    function Remove(const AIndex: Integer; out S: string): Boolean; overload;
    function RemoveLast(out S: string): Boolean;
    function RemoveLastEx(out S: string; out AObject: TObject): Boolean;
    function RemoveEx(const AIndex: Integer; out S: string; out AObject: TObject): Boolean;
    procedure RemoveName(const AName: string);
    procedure RemoveAllObjects(const AObject: TObject);
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure Insert(AIndex: Integer; const S: string);
    procedure InsertObject(AIndex: Integer; const S: string; AObject: TObject);
    procedure Delete(AIndex: Integer);
    function IsEmpty: Boolean;
    function Count: Integer;
    procedure Clear; virtual;
    procedure SetCount(const ASize: Integer);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False);
    procedure CopyToStrings(const AStrings: IFPStrings);
    function ExportToArray: TStringArray;
    function ExportByObject(const AObject: TObject): TStringArray;
    procedure ImportArray(const AValue: TStringArray);
    function Find(const S: string; Out AIndex: Integer): Boolean;
    function FindObject(const S: string; Out AObject: TObject): Boolean;
    function IndexOf(const S: string): Integer;
    function IndexOfObject(const AObject: TObject): Integer;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Values[const AName: string]: string read GetValue write SetValue;
    property Sorted: Boolean read GetSorted write SetSorted;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  { TThreadStringListUTF8 }

  TThreadStringListUTF8 = class(TThreadStringListBase, IFPThreadStringList)
  protected
    function  LockList_: IFPStringList;
    function IFPThreadStringList.LockList = LockList_;
  public
    constructor Create(const ASorted: Boolean = False; const ACaseSensitive: Boolean = False); virtual;
    function  LockList: TStringListUTF8;
    procedure UnlockList;

    class function LockedGet(const AThreadList: TThreadStringListUTF8; out AList: TStringListUTF8): Boolean;
    class function LockedCreateIfNil(var AList: TThreadStringListUTF8; const ANewItem: string;
        const ASorted: Boolean = False; const ACaseSensitive: Boolean = False): Boolean;
    class function LockedFreeAndNilIfEmpty(var AList: TThreadStringListUTF8; const ARemoveItem: string = ''): Boolean;
  end;

  TThreadStringList = TThreadStringListUTF8;

  { TThreadFileNameList }

  TThreadFileNameList = class(TThreadStringListBase, IFPThreadFileNameList)
  protected
    function  LockList_: IFPFileNameList;
    function IFPThreadFileNameList.LockList = LockList_;
  public
    constructor Create(const ASorted: Boolean = False); virtual;
    //class procedure LockedCreateIfNil(var AList: TThreadStringListUTF8;
    //    const ASorted: Boolean = False; const ACaseSensitive: Boolean = False);
    function  LockList: TFileNameList;
    procedure UnlockList;
  end;

  { TThreadDataStringList }

  TThreadDataStringList = class(TThreadStringListBase, IFPThreadDataStringList)
  private
    FOnInitListData: TPointerMethod;
  protected
    function GetData(Index: Integer): Pointer;
    function  LockList_: IFPDataStringList;
    function IFPThreadDataStringList.LockList = LockList_;
    procedure DoOnInitData(const p: Pointer); virtual;
  public
    constructor Create(const ADataSize: Cardinal; const AOnFreeData: TPointerMethod = nil;
        const ASorted: Boolean = False; const AOnInitData: TPointerMethod = nil); virtual;

    function  LockList: TDataStringList;{$IFDEF TEST_MODE}virtual;{$ENDIF}
    procedure UnlockList;{$IFDEF TEST_MODE}virtual;{$ENDIF}
    property Data[Index: Integer]: Pointer read GetData; //ToDo remove (not safe)
  end;

  { TIniFileUTF8 }

  TIniFileUTF8 = class(TIniFile)
  private
    FRefCount: Integer;
    FUpdateCount: Integer;
    FStream: TStringStreamUTF8;
    FFileName: string;
    //FRefCountObject: TRefCountObject;
    //{ IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): Hresult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  protected
    { IFPObject }
    function GetFPObject: TObject;
    { IFPCustomIniFile }
    procedure ReadSection(const ASection: string; const AStrings: IFPStrings); overload;
    procedure ReadSections(const AStrings: IFPStrings); overload;
    procedure ReadSectionValues(const ASection: string; const AStrings: IFPStrings); overload;
    procedure ReadSectionRaw(const ASection: string; const AStrings: IFPStrings); overload;
    procedure GetSection(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSections(out AStrings: IFPStringListUTF8);
    procedure GetSectionValues(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSectionRaw(const ASection: string; out AStrings: IFPStringListUTF8);
  public
    constructor Create(const AFileName: string; {%H-}AEscapeLineFeeds: Boolean = False); override;
    destructor Destroy; override;
    procedure UpdateFile; override;

    { IIniFileUTF8 }
    procedure WriteDeleteString(const ASection, AIdent, AValue, ADefault: String;
           AToQuote: Boolean = False; ACaseSensetive: Boolean = False);
    procedure WriteDeleteBool(const ASection, AIdent: string;
        AValue, ADefault: Boolean);
    procedure WriteDeleteInteger(const ASection, AIdent: string;
        AValue, ADefault: Integer);
    procedure WriteSection(const ASection: string; const AValues: IFPStrings;
        const AIsClearFirst: Boolean = True);
    procedure ClearSection(const ASection: string);//not erase [Section]
    procedure AddSection(const ASection: string);//add empty [ASection] if not exists
    function CopySection(const ASourceSect, ADestSect: string): Boolean;
    function RenameSection(const OldSect, NewSect: string): Boolean;
    procedure BeginUpdateFile;
    procedure EndUpdateFile;
    procedure Clear;

    //property RefCountObject: TRefCountObject read FRefCountObject implements IUnknown;
  end;

  { TIIniFileUTF8 }

  TIIniFileUTF8 = class(TIniFileUTF8, IFPIniFileUTF8)

  end;

  { TThreadIniFileUTF8 }

  TThreadIniFileUTF8 = class(TInterfacedObject)
  private
    FUseBackup: Boolean;
    //FBackupFile: string;
    procedure SetUseBackup(AValue: Boolean);
  protected
    FIni: TIniFileUTF8;
    FIniIntf: IFPIniFileUTF8;
    FLock: TRTLCriticalSection;
  public
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident: string; const ADefault: string = ''): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; ADefault: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadInt64(const Section, Ident: string; ADefault: Int64): {$IF FPC_FULLVERSION >= 30100}Int64{$ELSE}Longint{$ENDIF};
    procedure WriteInt64(const Section, Ident: string; Value: Int64);
    function ReadBool(const Section, Ident: string; ADefault: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadDateTime(const Section, Ident: string; ADefault: TDateTime): TDateTime;
    function ReadFloat(const Section, Ident: string; ADefault: Double): Double;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime);
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime);
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure WriteTime(const Section, Ident: string; Value: TDateTime);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    function ValueExists(const Section, Ident: string): Boolean;
    procedure UpdateFile;

    procedure ReadSection(const ASection: string; const AStrings: IFPStrings);
    procedure ReadSections(const AStrings: IFPStrings);
    procedure ReadSectionValues(const ASection: string; const AStrings: IFPStrings);
    procedure ReadSectionRaw(const ASection: string; const AStrings: IFPStrings);

    procedure GetSection(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSections(out AStrings: IFPStringListUTF8);
    procedure GetSectionValues(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSectionRaw(const ASection: string; out AStrings: IFPStringListUTF8);

    procedure WriteDeleteString(const ASection, AIdent, AValue, ADefault: String;
           AToQuote: Boolean = False; ACaseSensetive: Boolean = False);
    procedure WriteDeleteBool(const ASection, AIdent: string;
           AValue, ADefault: Boolean);
    procedure WriteDeleteInt(const ASection, AIdent: string;
           AValue, ADefault: Integer);
    function CopySection(const ASourceSect, ADestSect: string): Boolean;
    function RenameSection(const OldSect, NewSect: string): Boolean;
    procedure Clear;

  public
    constructor Create(const AFileName: string; AEscapeLineFeeds: Boolean = False); virtual;
    destructor Destroy; override;

    function Lock: TIniFileUTF8;
    procedure Unlock;
    property UseBackup: Boolean read FUseBackup write SetUseBackup; //ToDo ? Remove
  end;

  { TIIThreadIniFileUTF8 }

  { TIThreadIniFileUTF8 }

  TIThreadIniFileUTF8 = class(TThreadIniFileUTF8, IFPThreadIniFileUTF8)
  protected
    { IFPObject }
    function GetFPObject: TObject;
    { IFPThreadIniFileUTF8 }
    function ReadInt64(const Section, Ident: string; ADefault: Int64): {$IF FPC_FULLVERSION >= 030000}Int64{$ELSE}Longint{$ENDIF};
  protected
    function LockIntf: IFPIniFileUTF8;
    function IFPThreadIniFileUTF8.Lock = LockIntf;
  end;

  {$IFDEF TEST_MODE}
    {.define globally  $DEFINE CACHED_INI_NOT_THREAD}
  {$ENDIF}
  {$IFDEF CACHED_INI_NOT_THREAD}
    { TIIniTestCachedFile }

    TIIniTestCachedFile = class(TIIniFileUTF8, IFPIniTestCachedFile)
    public
      function Lock: IFPIniFileUTF8;
      procedure Unlock;
    end;

    //in vr_intfs  IFPIniCachedFile = IFPIniTestCachedFile;
  {$ELSE}
    //in vr_intfs  IFPIniCachedFile = IFPThreadIniFileUTF8;
  {$ENDIF}


{%Region IniFiles}

function ini_GetCacheFile(const AFileName: string; const ASecTimeOut: Cardinal = 300): IFPIniCachedFile;
//if not exists then create
function ini_ReloadCacheFile(const AFileName: string): IFPIniCachedFile;
function ini_CacheFileExists(const AFileName: string): Boolean;
procedure ini_FreeCacheFile(const AFileName: string);
procedure ini_FreeAllCache;
procedure ini_UpdateCacheFile(const AFileName: string);

function IniReadString(const FileName, Sect, Name: string; const Default: string = ''): string;
procedure IniWriteString(const FileName, Sect, Name, Value: string);
procedure IniWriteDelString(const FileName, Sect, Name, Value, DefValue: string);
function IniReadBool(const FileName, Sect, Name: string; const Default: Boolean): Boolean;
procedure IniWriteBool(const FileName, Sect, Name: string; const Value: Boolean);
procedure IniWriteDelBool(const FileName, Sect, Name: string; const Value, DefValue: Boolean);
function IniReadInt(const FileName, Sect, Name: string; const Default: Integer): Integer;
procedure IniWriteInt(const FileName, Sect, Name: string; const Value: Integer);
procedure IniWriteDelInt(const FileName, Sect, Name: string; const Value, DefValue: Integer);
function IniReadFloat(const FileName, Sect, Name: string; const Default: Extended): Extended;
procedure IniWriteFloat(const FileName, Sect, Name: string; const Value: Extended);
procedure IniReadSection(const FileName, Sect: string; const strs: IFPStrings);
procedure IniReadSectionValues(const FileName, Sect: string; const strs: IFPStrings);

function IniKeyExists(const AFileName, ASect, AKey: string): Boolean;
procedure IniDeleteKey(const AFileName, ASect, AKey: string);
procedure IniEraseSection(const AFileName, ASect: string);

function IniSectionExists(const AFileName, ASect: string): Boolean;
function IniRenameSection(const AFileName, OldSect, NewSect: string): Boolean;
function IniCopySection(const AFileName, ASourceSect, ADestSect: string): Boolean; overload;

{%EndRegion IniFiles}

{%Region new_* functions}

function new_StringList(const ASorted: Boolean = False; const ACaseSens: Boolean = False;
       const AFileName: string = ''): IFPStringList;
function new_FileNameList(const ASorted: Boolean = False; const AFileName: string = ''): IFPFileNameList;
function new_DataStringList(const ADataSize: Cardinal; const AOnFreeData: TPointerMethod = nil;
    const ASorted: Boolean = False; const AOnInitData: TPointerMethod = nil): IFPDataStringList;

function new_ThreadStringList(const ASorted: Boolean = False;
    const ACaseSensitive: Boolean = False): IFPThreadStringList;
function new_ThreadFileNameList(const ASorted: Boolean = False): IFPThreadFileNameList;
function new_ThreadDataStringList(const ADataSize: Cardinal; const ASorted: Boolean = False;
    const AOnFreeData: TPointerMethod = nil; const AOnInitData: TPointerMethod = nil): IFPThreadDataStringList;

function new_Thread(const AOnExecute: TNotifyProc; const AData: Pointer = nil;
    const AFreeOnTerminate: Boolean = True; const ASuspended: Boolean = False): TThread;
function new_ThreadM(const AOnExecute: TNotifyMethod; const AData: Pointer = nil;
    const AFreeOnTerminate: Boolean = True; const ASuspended: Boolean = False): TThread;
function new_Timer(const AMSecInterval: Cardinal; const AOnTimer: TNotifyEvent): IFPTimer;

function new_FileStream(const AFileName: string; AMode: Word = fmOpenReadWrite): IFPFileStream;
function new_StringStream(const S: string): IFPStringStream;

function new_List: IFPList;
function new_InterfaceList: IFPInterfaceList;

function new_IniFile(const AFileName: string): IFPIniFile;
function new_ThreadIniFile(const AFileName: string): IFPThreadIniFile;

{%EndRegion new_* functions}

procedure QueueAsyncCall(const AMethod: TDataMethod;
    const AData: PtrInt; const AInMainThread: Boolean = False); overload;
procedure QueueAsyncCall(const AMethod: TNotifyProc; const Sender: TObject;
    const AData: Pointer; const AInMainThread: Boolean = False); overload;
procedure QueueAsyncCall(const AMethod: TNotifyMethod; const Sender: TObject;
    const AData: Pointer; const AInMainThread: Boolean = False); overload;

procedure RemoveAsyncCalls(const AMethod: TMethod); overload;
procedure RemoveAsyncCalls(const AProc: TProcedure); overload;
procedure RemoveAsyncCalls(const AObject: TObject); overload;

implementation

type

  { TAnonymousThread }

  TAnonymousThread = class(TThread)
  private
    FOnExecProc: TNotifyProc;
    FOnExecMethod: TNotifyMethod;
    FData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor CreateAlt(const AOnExecProc: TNotifyProc; const AData: Pointer;
        const AFreeOnTerminate: Boolean; const ASuspended: Boolean;
        const AOnExecMethod: TNotifyMethod = nil);
  end;

  { TIniFileCached }

  TIniFileCached = class(TIThreadIniFileUTF8)
  private
    FSecTimeOut: Cardinal;
    FTickCount: QWord;
    //FLastThreadId: TThreadID;
    function GetDirty: Boolean;
    procedure SetSecTimeOut(AValue: Cardinal);
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds: Boolean = False); override;
    constructor CreateAlt(const AFileName: string; const ASecTimeOut: Cardinal);
    destructor Destroy; override;
    procedure UpdateIfDirty;
    property SecTimeOut: Cardinal read FSecTimeOut write SetSecTimeOut;
    property Dirty: Boolean read GetDirty;
    //procedure Free(AForce: Boolean = False);
  end;

  { TIniFileCachedManager }

  TIniFileCachedManager = class(TObject)
  private
    FFiles: TFileNameList;
    FTimer: IFPTimer;
    FLock: TRTLCriticalSection;
    //procedure CheckCurrThread(AIni: TIniFileCached);
    //procedure RemoveIni(AIni: TObject); //new:6.9.5
    procedure OnTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function GetIni(const AFileName: string; const ASecTimeOut: Cardinal): IFPIniCachedFile;
    function FindIni(const AFileName: string; out AIni: IFPIniCachedFile): Boolean;
    procedure FreeIni(const AFileName: string);
    function Reload(const AFileName: string): IFPIniCachedFile;
    procedure Clear;
  end;

  { TITimer }

  TITimer = class(TInterfacedObject, IFPTimer)
  private
    FTimer: TCustomTimer;
    { ITImer }
    function GetEnabled: Boolean;
    function GetInterval: Cardinal;
    function GetOnTimer: TNotifyEvent;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInterval(AValue: Cardinal);
    procedure SetOnTimer(AValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TIDataStringList }

{$IF FPC_FULLVERSION >= 030200}
function TIDataStringList.GetStrictDelimiter: Boolean;
begin
  Result := StrictDelimiter;
end;

procedure TIDataStringList.SetStrictDelimiter(AValue: Boolean);
begin
  StrictDelimiter := AValue;
end;

function TIDataStringList.GetLBS: TTextLineBreakStyle;
begin
  Result := TextLineBreakStyle;
end;

procedure TIDataStringList.SetLBS(AValue: TTextLineBreakStyle);
begin
  TextLineBreakStyle := AValue;
end;

function TIDataStringList.GetName(Index: Integer): string;
begin
  Result := Names[Index];
end;

function TIDataStringList.GetValue(const Name: string): string;
begin
  Result := Values[Name];
end;

procedure TIDataStringList.SetValue(const Name: string; const AValue: string);
begin
  Values[Name] := AValue;
end;

function TIDataStringList.GetCommaText: string;
begin
  Result := CommaText;
end;

procedure TIDataStringList.SetCommaText(const AValue: string);
begin
  CommaText := AValue;
end;

function TIDataStringList.GetCaseSensitive: Boolean;
begin
  Result := CaseSensitive;
end;

procedure TIDataStringList.SetCaseSensitive(AValue: Boolean);
begin
  CaseSensitive := AValue;
end;

function TIDataStringList.GetSorted: Boolean;
begin
  Result := Sorted;
end;

procedure TIDataStringList.SetSorted(AValue: Boolean);
begin
  Sorted := AValue;
end;

function TIDataStringList.GetSortStyle: TStringsSortStyle;
begin
  Result := SortStyle;
end;

procedure TIDataStringList.SetSortStyle(AValue: TStringsSortStyle);
begin
  SortStyle := AValue;
end;
{$ENDIF FPC_FULLVERSION >= 030200}

{ TIFPObject }

function TIFPObject.GetFPObject: TObject;
begin
  Result := Self;
end;


{ TAnonymousThread }

procedure TAnonymousThread.Execute;
begin
  if Assigned(FOnExecProc) then
    FOnExecProc(Self, FData)
  else if Assigned(FOnExecMethod) then
    FOnExecMethod(Self, FData);
end;

constructor TAnonymousThread.CreateAlt(const AOnExecProc: TNotifyProc;
  const AData: Pointer; const AFreeOnTerminate: Boolean;
  const ASuspended: Boolean; const AOnExecMethod: TNotifyMethod);
begin
  Create(ASuspended);
  FOnExecProc := AOnExecProc;
  FData := AData;
  FreeOnTerminate := AFreeOnTerminate;
  FOnExecMethod := AOnExecMethod;
end;

var
  __IniCachedMan: TIniFileCachedManager = nil;{$IFDEF DEBUG_MODE}
  __Finilized: Boolean = False;{$ENDIF}
function IniCachedMan: TIniFileCachedManager;
begin
  LockVar;
  try
    if __IniCachedMan = nil then
      begin
        __IniCachedMan := TIniFileCachedManager.Create;{$IFDEF DEBUG_MODE}
        if __Finilized then
          WriteDebugLogLn('vr_classes:IniCachedMan(): __Finilized = True');{$ENDIF}
      end;
    Result := __IniCachedMan;
  finally
    UnLockVar;
  end;
end;

{ TIniFileCached }

procedure TIniFileCached.SetSecTimeOut(AValue: Cardinal);
begin
  FSecTimeOut := Max(FSecTimeOut, AValue);
  FTickCount := GetTickCount {%H-}+ FSecTimeOut * 1000;
end;

type
  TAccessIni = class(TIniFileUTF8) end;
function TIniFileCached.GetDirty: Boolean;
begin
  Result := TAccessIni(FIni).Dirty;
end;

constructor TIniFileCached.Create(const AFileName: string;
  AEscapeLineFeeds: Boolean);
begin
  inherited Create(AFileName, AEscapeLineFeeds);
  SecTimeOut := 300;
  //FLastThreadId := System.GetCurrentThreadId;
  UseBackup := True;
end;

constructor TIniFileCached.CreateAlt(const AFileName: string;
  const ASecTimeOut: Cardinal);
begin
  Create(AFileName, False);
  SecTimeOut := ASecTimeOut;
end;

destructor TIniFileCached.Destroy;
begin
  //new:6.9.5 >>>
  //LockVar;
  //try
  //  if __IniCachedMan <> nil then
  //    __IniCachedMan.RemoveIni(Self);
  //finally
  //  UnLockVar;
  //end;
  inherited Destroy;
end;

procedure TIniFileCached.UpdateIfDirty;
begin
  if Dirty then
    UpdateFile;
end;

//procedure TIniFileCached.Free(AForce: Boolean);
//begin
//  if AForce then
//    inherited Free;
//end;

{$IFDEF CACHED_INI_NOT_THREAD}
{ TIIniTestCachedFile }

function TIIniTestCachedFile.Lock: IFPIniFileUTF8;
begin
  Result := Self;
end;

procedure TIIniTestCachedFile.Unlock;
begin

end;
{$ENDIF}

{ TIniFileCachedManager }

//procedure TIniFileCachedManager.CheckCurrThread(AIni: TIniFileCached);
//var
//  id: TThreadID;
//begin
//  System.LeaveCriticalSection(FLock);
//  try
//    id := System.GetCurrentThreadId;
//    while (AIni.FRefCount > 1) and (id <> AIni.FLastThreadId) do
//      begin
//        Sleep(10);
//      end;
//    if (AIni.FRefCount <= 1) then
//      AIni.FLastThreadId := id;
//  finally
//    System.EnterCriticalSection(FLock);
//  end;
//end;

//procedure TIniFileCachedManager.RemoveIni(AIni: TObject);
//var
//  I: Integer;
//begin
//  System.EnterCriticalSection(FLock);
//  try
//    I := FFiles.IndexOfObject(AIni);
//    if I <> -1 then
//      begin
//        FFiles.Delete(I);
//        TIniFileCached(AIni)._Release;
//      end;
//  finally
//    System.LeaveCriticalSection(FLock);
//  end;
//end;

procedure TIniFileCachedManager.OnTimer(Sender: TObject);
var
  I: Integer;
  iTick: QWord;
  ini: TIniFileCached;
begin
  System.EnterCriticalSection(FLock);
  try
    iTick := GetTickCount;
    for I := FFiles.Count - 1 downto 0 do
      begin
        ini := TIniFileCached(FFiles.Objects[I]);
        if (iTick > ini.FTickCount) then
          begin
            FFiles.Delete(I);
            ini._Release;
          end
        else if ini.Dirty then
          ini.UpdateFile;
      end;
  finally
    System.LeaveCriticalSection(FLock);
  end;
  //if FFiles.Count = 0 then
  //  LockedVarFreeAndNil(__IniCachedMan);
end;

constructor TIniFileCachedManager.Create;
begin
  FFiles := TFileNameList.Create;
  FFiles.Sorted := True;
  FTimer := new_Timer(30000, OnTimer);
  InitCriticalSection(FLock);
end;

destructor TIniFileCachedManager.Destroy;
var
  I: Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    FTimer.Enabled := False;
    for I := 0 to FFiles.Count - 1 do
      TIniFileCached(FFiles.Objects[I])._Release;
    FFiles.Free;
  finally
    System.LeaveCriticalSection(FLock);
    DoneCriticalsection(FLock);
  end;
  inherited Destroy;
end;

function TIniFileCachedManager.GetIni(const AFileName: string;
  const ASecTimeOut: Cardinal): IFPIniCachedFile;
var
  I: Integer;
  ini: TIniFileCached;
begin
  {$IFDEF CACHED_INI_NOT_THREAD}
  Result := nil{$ELSE}
  System.EnterCriticalSection(FLock);
  try
    if FFiles.Find(AFileName, I) then
      begin
        ini := TIniFileCached(FFiles.Objects[I]);
        //CheckCurrThread(ini);
        //new:6.9.5  Result := ini;
        ini.GetInterface(IFPIniCachedFile, Result);//new:6.9.5
        ini.SecTimeOut := ASecTimeOut;
      end
    else
      begin
        //new:6.9.5 >>>
        //ini := TIniFileCached.CreateAlt(AFileName, {$IFDEF TEST_MODE}300{$ELSE}ASecTimeOut{$ENDIF});
        //FFiles.AddObject(AFileName, ini);
        //Result := ini;
        //Result._AddRef;
        //new:6.9.5 >>>
        Result := TIniFileCached.CreateAlt(AFileName, {$IFDEF TEST_MODE}300{$ELSE}ASecTimeOut{$ENDIF});
        Result._AddRef;
        ini := TIniFileCached(Result.GetFPObject);
        FFiles.AddObject(AFileName, ini);
        //new:6.9.5 <<<
      end;
  finally
    System.LeaveCriticalSection(FLock);
  end;{$ENDIF}
end;

function TIniFileCachedManager.FindIni(const AFileName: string; out
  AIni: IFPIniCachedFile): Boolean;
var
  I: Integer;
  ini: TIniFileCached;
begin
  {$IFDEF CACHED_INI_NOT_THREAD}
  Result := False;
  {$ELSE}
  System.EnterCriticalSection(FLock);
  try
    Result := FFiles.Find(AFileName, I);
    if Result then
      begin
        ini := TIniFileCached(FFiles.Objects[I]);
        //CheckCurrThread(ini);
        Result := ini.GetInterface(IFPIniCachedFile, AIni); //new:6.9.5 AIni := ini;
      end
    else
      AIni := nil;
  finally
    System.LeaveCriticalSection(FLock);
  end;{$ENDIF}
end;

procedure TIniFileCachedManager.FreeIni(const AFileName: string);
var
  I: Integer;
  ini: IFPIniCachedFile;
begin
  System.EnterCriticalSection(FLock);
  try
    if FFiles.Find(AFileName, I) then
      begin
        if FFiles.Objects[I].GetInterface(IFPIniCachedFile, ini) then
          ini._Release;
        FFiles.Delete(I);
      end;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TIniFileCachedManager.Reload(const AFileName: string
  ): IFPIniCachedFile;
begin
  System.EnterCriticalSection(FLock);
  try
    FreeIni(AFileName);
    Result := GetIni(AFileName, 300);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TIniFileCachedManager.Clear;
var
  I: Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    for I := FFiles.Count - 1 downto 0 do
      begin
        TIniFileCached(FFiles.Objects[I])._Release;
      end;
    FFiles.Clear;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

{ TITimer }

function TITimer.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TITimer.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

function TITimer.GetOnTimer: TNotifyEvent;
begin
  Result := FTimer.OnTimer;
end;

procedure TITimer.SetEnabled(AValue: Boolean);
begin
  FTimer.Enabled := AValue;
end;

procedure TITimer.SetInterval(AValue: Cardinal);
begin
  FTimer.Interval := AValue;
end;

procedure TITimer.SetOnTimer(AValue: TNotifyEvent);
begin
  FTimer.OnTimer := AValue;
end;

constructor TITimer.Create;
begin
  FTimer := TCustomTimer.Create(nil);
end;

destructor TITimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ TvrList }

function TvrList.AddUnique(AItem: Pointer): Integer;
begin
  Result := IndexOf(AItem);
  if Result = -1 then
    Result := Add(AItem);
end;

{ TIFPList }

function TIFPList.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := LongInt(E_NOINTERFACE);
end;

function TIFPList._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TIFPList._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIFPList.GetFPObject: TObject;
begin
  Result := Self;
end;

procedure TIFPList.AddList(const AList: IFPList);
begin
  TList(Self).AddList(TList(AList.GetFPObject));
end;

{ TThreadObjectList }

constructor TThreadObjectList.Create(const AFreeObjects: Boolean);
begin
  FDuplicates:=dupIgnore;
  InitCriticalSection(FLock);
  FList := TObjectList.Create(AFreeObjects);
end;

destructor TThreadObjectList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

procedure TThreadObjectList.Add(Item: TObject);
begin
  LockList;
  try
    if (Duplicates=dupAccept) or
      // make sure it's not already in the list
      (FList.IndexOf(Item) = -1) then
       FList.Add(Item)
     else if (Duplicates=dupError) then
       FList.Error(SDuplicateItem, PtrUInt(Item));
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList.Clear;
begin
  Locklist;
  try
    FList.Clear;
  finally
    UnLockList;
  end;
end;

procedure TThreadObjectList.Remove(Item: TObject);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

function TThreadObjectList.LockList: TObjectList;
begin
  Result:=FList;
  System.EnterCriticalSection(FLock);
end;

procedure TThreadObjectList.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TThreadMethodListBase }

constructor TThreadMethodListBase.Create;
begin
  inherited Create;
  //FDuplicates := dupIgnore;
  InitCriticalSection(FLock);
  FList := TMethodList.Create;
end;

destructor TThreadMethodListBase.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

procedure TThreadMethodListBase.Add(const AMethod: TMethod; AsLast: Boolean);
begin
  LockList;
  try
    FList.Add(AMethod, AsLast);
  finally
    UnlockList;
  end;
end;

procedure TThreadMethodListBase.Remove(const AMethod: TMethod);
begin
  LockList;
  try
    FList.Remove(AMethod);
  finally
    UnlockList;
  end;
end;

procedure TThreadMethodListBase.RemoveAllMethodsOfObject(const AnObject: TObject);
begin
  LockList;
  try
    FList.RemoveAllMethodsOfObject(AnObject);
  finally
    UnlockList;
  end;
end;

procedure TThreadMethodListBase.CallNotifyEvents(Sender: TObject);
var
  I: Integer;
  m: TMethod;
begin
  I := Count;
  while NextDownMethod(I, m) do
    TNotifyEvent(m)(Sender);
end;

function TThreadMethodListBase.NextDownMethod(var AIndex: integer; out
  AMethod: TMethod): boolean;
begin
  LockList;
  try
    Result := FList.NextDownIndex(AIndex);
    if Result then
      AMethod := FList[AIndex]
    else
      AMethod.Code := nil;
  finally
    UnlockList;
  end;
end;

function TThreadMethodListBase.Count: Integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadMethodListBase.LockList: TMethodList;
begin
  Result := FList;
  System.EnterCriticalSection(FLock);
end;

procedure TThreadMethodListBase.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TvrInterfaceList }

function TvrInterfaceList.GetFPObject: TObject;
begin
  Result := Self;
end;

procedure TvrInterfaceList.AssignIntfList(const AList: IFPInterfaceList);
begin
  if AList = nil then
    Assign(nil)
  else
    Assign(TvrInterfaceList(AList.GetFPObject));
end;

procedure TvrInterfaceList.Assign(const AList: TInterfaceList);
var
  I: Integer;
begin
  Clear;
  if AList = nil then Exit;
  Capacity := AList.Capacity;
  Lock;
  AList.Lock;
  try
    for I := 0 to AList.Count - 1 do
      Add(AList[I]);
  finally
    AList.Unlock;
    Unlock;
  end;
end;

function TvrInterfaceList.AddUnique(const AItem: IUnknown): Integer;
begin
  Lock;
  try
    Result := IndexOf(AItem);
    if Result = -1 then
      Result := Add(AItem);
  finally
    Unlock;
  end;
end;

function TvrInterfaceList.ExportToArray: TInterfaceArray;
var
  I: Integer;
begin
  Lock;
  try
    SetLength(Result, Count);
    for I := 0 to Length(Result) - 1 do
      Result[I] := Items[I];
  finally
    Unlock;
  end;
end;

{ TStringStreamUTF8 }

function TStringStreamUTF8.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=Length(DataString)-Position;
  if Result <= 0 then
    begin
      Result := 0;
      Exit;
    end;
  Result := inherited Read(Buffer, Count);
end;

function TStringStreamUTF8.ReadUntil(out AResult: string; AChar: Char): LongInt;
var
  ch: Char;
begin
  AResult := '';
  Result := 0;
  while (Position < Size) do
    begin
      ch := Char(ReadByte);
      if ch = AChar then Exit;
      Inc(Result);
      AResult += ch;
    end;
end;

{ TIStringStreamUTF8 }
{$IF FPC_FULLVERSION >= 030200}
function TIStringStreamUTF8.ReadAnsiString: string;
begin
  Result := TStream(Self).ReadAnsiString;
end;

function TIStringStreamUTF8.GetUnicodeDataString: UnicodeString;
begin
  Result := UnicodeDataString;
end;

function TIStringStreamUTF8.GetOwnsEncoding: Boolean;
begin
  Result := OwnsEncoding;
end;

function TIStringStreamUTF8.GetEncoding: TEncoding;
begin
  Result := Encoding;
end;

{$ENDIF}
function TIStringStreamUTF8.GetDataString: string;
begin
  Result := DataString;
end;

function TIStringStreamUTF8.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := LongInt(E_NOINTERFACE);
end;

function TIStringStreamUTF8._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TIStringStreamUTF8._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIStringStreamUTF8.GetFPObject: TObject;
begin
  Result := Self;
end;

{ TFileStreamUTF8 }

constructor TFileStreamUTF8.Create(const AFileName: string; Mode: Word);
begin
  Create(AFileName, Mode, 438);
end;

constructor TFileStreamUTF8.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
var
  h: THandle;
  f: Boolean;
begin
  FFileName := AFileName;
  If (Mode and fmCreate) > 0 then
    f := file_Create(AFileName, h, False, Mode, Rights)
  else
    f := file_Open(AFileName, h, Mode);

  If not f then
    begin
      If Mode = fmcreate then
        raise EFCreateError.createfmt(SFCreateError,[AFileName])
      else
        raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
    end
  else
    THandleStream(Self).Create(h);
end;

//destructor TFileStreamUTF8.Destroy;
//begin
//  file_Close(Handle);
//  inherited Destroy;
//end;

function TFileStreamUTF8.ReadDateTime: TDateTime;
begin
  ReadBuffer(Result{%H-}, SizeOf(TDateTime));
end;

function TFileStreamUTF8.ReadInteger: Integer;
begin
  ReadBuffer(Result{%H-}, 4);
end;

procedure TFileStreamUTF8.WriteDateTime(const dt: TDateTime);
begin
  WriteBuffer(dt, SizeOf(TDateTime));
end;

procedure TFileStreamUTF8.WriteInteger(const i: Integer);
begin
  WriteBuffer(i, 4);
end;

{ TIFileStreamUTF8 }

function TIFileStreamUTF8.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := LongInt(E_NOINTERFACE);
end;

function TIFileStreamUTF8._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TIFileStreamUTF8._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIFileStreamUTF8.GetFPObject: TObject;
begin
  Result := Self;
end;

{ TStringListUTF8 }

function TStringListUTF8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if FTrimOnCompare then
    begin
      if CaseSensitive then
        Result := str_CompareStr(Trim(s1), Trim(s2))
      else
        Result := str_CompareText(Trim(s1), Trim(s2));
    end
  else if CaseSensitive then
    Result := str_CompareStr(s1, s2)
  else
    Result := str_CompareText(s1, s2);
end;

destructor TStringListUTF8.Destroy;
begin
  inherited Destroy;
end;
{$IF FPC_FULLVERSION >= 030000}
function TStringListUTF8.Find(const S: string; out Index: Integer): Boolean;
var
  i: Integer;
begin
  if not Sorted then
    begin //{$IFDEF TEST_MODE}
      //ShowError('TStringListUTF8.Find(): not Sorted');
      //{$ENDIF}
      i := IndexOf(S);
      Result := i <> -1;
    end
  else
    Result := inherited Find(S, Index);
end;
{$ENDIF}
procedure TStringListUTF8.LoadFromFile(const FileName: string);
begin   //ToDo like LazUTF8Classes.TStringListUTF8
  //inherited LoadFromFile(UTF8ToSys(FileName));
  Text := str_LoadFromFile(FileName);
end;

procedure TStringListUTF8.SaveToFile(const FileName: string);
begin
  str_SaveToFile(FileName, Text, True); //ToDo like LazUTF8Classes.TStringListUTF8
  //try
  //  inherited SaveToFile(UTF8ToSys(FileName));
  //except
  //  on E: Exception do
  //    ShowExceptionMsg(E);
  //end;
end;

procedure TStringListUTF8.AddStringsWithIndexInObject(ASource: TStrings);
var
  I: Integer;
begin
  Clear;
  Capacity := ASource.Count;
  for I := 0 to ASource.Count - 1 do
    AddObject(ASource[I], TObject(I));
end;

function TStringListUTF8.AddIfNotFind(const S: string; const AObject: TObject): Boolean;
var
  i: Integer;
begin
  Result := AddIfNotFindEx(S, AObject, i);
end;

function TStringListUTF8.AddIfNotFindEx(const S: string;
  const AObject: TObject; out AIndex: Integer): Boolean;
begin
  if not Sorted then
    begin
      AIndex := IndexOf(S);
      if AIndex <> -1 then Exit(False);
      AddObject(S, AObject);
    end
  else if Find (S, AIndex) then
      Exit(False)
  else
    begin
      InsertItem(AIndex, S, AObject);
      Result := True;
    end;
end;

procedure TStringListUTF8.Grow(ANewSize: Integer);
begin
  if ANewSize <= Count then Exit;
  Capacity := ANewSize;
  while Count < ANewSize do
    AddObject('', nil);
end;

function TStringListUTF8.Remove(const S: string): Boolean;
var
  i: Integer;
begin
  i := IndexOf(S);
  Result := i <> -1;
  if Result then
    Delete(i);
end;

function TStringListUTF8.Remove(const AIndex: Integer; out S: string): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
  if Result then
    begin
      S := Strings[AIndex];
      Delete(AIndex);
    end;
end;

function TStringListUTF8.RemoveEx(const AIndex: Integer; out S: string; out
  AObject: TObject): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
  if Result then
    begin
      S := Strings[AIndex];
      AObject := Objects[AIndex];
      Delete(AIndex);
    end;
end;

procedure TStringListUTF8.RemoveName(const AName: string);
var
  i: Integer;
begin
  i := IndexOfName(AName);
  if i <> -1 then
    Delete(i);
end;

procedure TStringListUTF8.RemoveAllObjects(const AObject: TObject);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if Objects[i] = AObject then
      Delete(i);
end;

procedure TStringListUTF8.SetCount(const ASize: Integer);
var
  i: Integer;
begin
  if (ASize < 0) or (ASize = Count) then
    Exit
  else if ASize > Count then
    begin
      Capacity := ASize;
      for i := Count to ASize - 1 do
        Add('');
    end
  else
    begin
      {$IFNDEF VER3}
      if ASize = 0 then
        for i := Count - 1 downto Max(ASize, 0) do
          Delete(i);{$ENDIF}
      Capacity := ASize;
    end;
end;

function TStringListUTF8.ExportToArray: TStringArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := Strings[i];
end;

function TStringListUTF8.ExportByObject(const AObject: TObject): TStringArray;
var
  i, n: Integer;
begin
  n := 0;
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
    if AObject = Objects[i] then
      begin
        Result[n] := Strings[i];
        Inc(n);
      end;
  SetLength(Result, n);
end;

procedure TStringListUTF8.ImportArray(const AValue: TStringArray);
var
  i: Integer;
begin
  Clear;
  Capacity := Length(AValue);
  for i := 0 to Length(AValue) - 1 do
    Add(AValue[i]);
end;

function TStringListUTF8.FindObject(const S: string; out AObject: TObject
  ): Boolean;
var
  i: Integer;
begin
  i := IndexOf(S);
  Result := i <> -1;
  if Result then
    AObject := Objects[i]
  else
    AObject := nil;
end;

procedure TStringListUTF8.FreeObjects;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      Objects[i].Free;
      Objects[i] := nil;
    end;
end;

{ TIStringListUTF8 }
{$IF FPC_FULLVERSION >= 030200}

function TIStringListUTF8.GetCommaText: string;
begin
  Result := CommaText;
end;

function TIStringListUTF8.GetLBS: TTextLineBreakStyle;
begin
  Result := TextLineBreakStyle;
end;

function TIStringListUTF8.GetName(Index: Integer): string;
begin
  Result := Names[Index];
end;

function TIStringListUTF8.GetValue(const Name: string): string;
begin
  Result := Values[Name];
end;

procedure TIStringListUTF8.SetSortStyle(AValue: TStringsSortStyle);
begin
  SortStyle := AValue;
end;

procedure TIStringListUTF8.SetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TIStringListUTF8.SetCaseSensitive(b: boolean);
begin
  CaseSensitive := b;
end;

procedure TIStringListUTF8.SetCommaText(const Value: string);
begin
  CommaText := Value;
end;

procedure TIStringListUTF8.SetLBS(AValue: TTextLineBreakStyle);
begin
  TextLineBreakStyle := AValue;
end;

procedure TIStringListUTF8.SetValue(const Name, Value: string);
begin
  Values[Name] := Value;
end;
{$ENDIF FPC_FULLVERSION >= 030200}

{$IF FPC_FULLVERSION >= 030000}
function TIStringListUTF8.GetSortStyle: TStringsSortStyle;
begin
  Result := SortStyle;
end;
{$ENDIF}
function TIStringListUTF8.GetCaseSensitive: Boolean;
begin
  Result := CaseSensitive;
end;

function TIStringListUTF8.GetSorted: Boolean;
begin
  Result := Sorted;
end;

function TIStringListUTF8.GetStrictDelimiter: Boolean;
begin
  Result := StrictDelimiter;
end;

procedure TIStringListUTF8.SetStrictDelimiter(AValue: Boolean);
begin
  StrictDelimiter := AValue;
end;

function TIStringListUTF8.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
     Result := S_OK
   else
     Result := longint(E_NOINTERFACE);
end;

function TIStringListUTF8._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TIStringListUTF8._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Self.Destroy;
end;

procedure TIStringListUTF8.AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False);
begin
  {$IF FPC_FULLVERSION >= 030000}
  AddStrings(TStrings(AStrings.GetFPObject), AClearFirst);{$ELSE}
  if AClearFirst then
    Clear;
  AddStrings(TStrings(AStrings.GetFPObject));
  {$ENDIF}
end;

function TIStringListUTF8.GetFPObject: TObject;
begin
  Result := Self;
end;

constructor TIStringListUTF8.Create(const ASorted: Boolean;
  const ACaseSens: Boolean);
begin
  inherited Create;
  Sorted := ASorted;
  CaseSensitive := ACaseSens;
end;

{ TFileNameList }

function TFileNameList.DoCompareText(const s1, s2: string): PtrInt;
begin
  Result := file_CompareName(s1, s2);
end;

procedure TFileNameList.AfterConstruction;
begin
  inherited AfterConstruction;
  CaseSensitive := FilenamesCaseSensitive;
end;

procedure TFileNameList.ClearNotExistFiles(const ACheckDirs: Boolean);
var
  i: Integer;
  S: String;
begin
  for i := Count - 1 downto 0 do
    begin
      S := Strings[i];
      if not file_Exists(S) and
          (not ACheckDirs or not dir_Exists(S)) then
        Delete(i);
    end;
end;

{ TStringListHashed }

function TStringListHashed.Find(const S: string; out Index: Integer): Boolean;
begin
  Index := IndexOf(S);
  Result := Index <> -1;
end;

{ TDataStringList }

constructor TDataStringList.Create(const ADataSize: Cardinal;
  const AOnFreeData: TPointerMethod; const ASorted: Boolean;
  const AOnInitData: TPointerMethod);
begin
  inherited Create;
  FDataSize := ADataSize; // + SizeOf(PDataStringListRec);
  FOnFreeData := AOnFreeData;
  Sorted := ASorted;
  FOnInitData := AOnInitData;
end;

destructor TDataStringList.Destroy;
begin
  FreeListDataAll;
  OwnsObjects := False;
  inherited;
end;

procedure TDataStringList.FreeListData(Index: Integer);
var
  v: PDataStringListRec;
begin
  v := GetListData(Index);
  if FDataSize > 0 then
    begin
      DoFreeData(v.Data);
      if OwnsObjects then
        begin
          v.FObject.Free;
          inherited PutObject(Index, nil);
        end;
      FreeMem(v.Data, FDataSize);
    end;
  Dispose(v);
end;

procedure TDataStringList.FreeListDataAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeListData(I);
end;

function TDataStringList.GetListData(const Index: Integer): PDataStringListRec;
begin
  Result := PDataStringListRec(inherited GetObject(Index));
end;

procedure TDataStringList.PutListData(const Index: Integer;
  Data: PDataStringListRec);
begin
  inherited PutObject(Index, TObject(Data));
end;

function TDataStringList.GetObject(Index: Integer): TObject;
begin
  Result := GetListData(Index).FObject;
end;

procedure TDataStringList.PutObject(Index: Integer; AObject: TObject);
begin
  GetListData(Index).FObject := AObject;
end;

function TDataStringList.GetData(const Index: Integer): Pointer;
begin
  Result := GetListData(Index).Data;
end;

procedure TDataStringList.InsertItem(Index: Integer; const S: string);
begin
  DoInsertData(S, nil, nil, Index);
end;

procedure TDataStringList.DoInsertData(const S: string; const AObject: TObject;
  const AData: Pointer; const Index: Integer);
var
  v: PDataStringListRec;
  p: Pointer;
begin
  inherited InsertItem(Index, S);
  New(v);
  PutListData(Index, v);
  v.FObject := AObject;
  if (FDataSize > 0) then
    if AData = nil then
      begin
        GetMem(p, FDataSize);
        v.Data := p;
        FillChar(p^, FDataSize, 0);
        DoInitData(p);
        if Assigned(OnInitData) then
          OnInitData(p);
      end
    else
      v.Data := AData;
end;

procedure TDataStringList.Delete(Index: Integer);
begin
  FreeListData(Index);
  inherited;
end;

procedure TDataStringList.Clear;
begin
  FreeListDataAll;
  inherited;
end;

function TDataStringList.AddData(const S: string; const AObject: TObject;
  const AData: Pointer): Integer;
begin
  if not Sorted then
    Result := Count
  else
    if Find (S, Result) then
      Case DUplicates of
        DupIgnore : Exit;
        DupError : Error(SDuplicateString,0)
      end;
  DoInsertData(S, AObject, AData, Result);
end;

function TDataStringList.IndexOfData(AData: Pointer): Integer;
begin
  Result := 0;
  while (Result < Count) and (DoCompareData(AData, Data[Result]) <> 0) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TDataStringList.DoFreeData(const P: Pointer);
begin
  if Assigned(FOnFreeData) then
    FOnFreeData(P);
end;

function TDataStringList.DoCompareData(AData1, AData2: Pointer): Integer;
begin
  if Assigned(FOnCompareData) then
    Result := FOnCompareData(AData1, AData2)
  else
    Result := -1;
end;

procedure TDataStringList.DoInitData(const P: Pointer);
begin

end;

{ TThreadStringListBase }

function TThreadStringListBase.Get(Index: Integer): string;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList[Index];
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.GetObject(Index: Integer): TObject;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Objects[Index];
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.GetOwnsObjects: boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.OwnsObjects;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.GetSorted: Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Sorted;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.GetValue(const AName: string): string;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Values[AName];
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.Put(Index: Integer; const AValue: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList[Index] := AValue;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.PutObject(Index: Integer; const AValue: TObject);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Objects[Index] := AValue;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.SetOwnsObjects(AValue: boolean);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.OwnsObjects := AValue;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.SetSorted(AValue: Boolean);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Sorted := AValue;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.SetValue(const AName: string;
  const AValue: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Values[AName] := AValue;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.CreateBase(const AList: IFPStringListUTF8);
begin
  InitCriticalSection(FLock);
  FListIntf := AList;
  FList :=  TIStringListUTF8(FListIntf.GetFPObject);
end;

destructor TThreadStringListBase.Destroy;
begin
  System.EnterCriticalSection(FLock);
  try
    FListIntf := nil;//FList.Free;
    inherited Destroy;
  finally
    System.LeaveCriticalSection(FLock);
    DoneCriticalsection(FLock);
  end;
end;

function TThreadStringListBase.Add(const S: string): Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Add(S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.Remove(const S: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Remove(S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.Remove(const AIndex: Integer; out S: string): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Remove(AIndex, S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.RemoveLast(out S: string): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Remove(FList.Count - 1, S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.RemoveLastEx(out S: string; out AObject: TObject
  ): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.RemoveEx(FList.Count - 1, S, AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.RemoveEx(const AIndex: Integer; out S: string;
  out AObject: TObject): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.RemoveEx(AIndex, S, AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.RemoveName(const AName: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.RemoveName(AName);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.RemoveAllObjects(const AObject: TObject);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.RemoveAllObjects(AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.AddObject(const S: string; AObject: TObject): Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.AddObject(S, AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.Insert(AIndex: Integer; const S: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Insert(AIndex, S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.InsertObject(AIndex: Integer;
  const S: string; AObject: TObject);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.InsertObject(AIndex, S, AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.Delete(AIndex: Integer);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Delete(AIndex);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.IsEmpty: Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Count = 0;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.Count: Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Count;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.Clear;
begin
  System.EnterCriticalSection(FLock);
  try
    FList.Clear;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.SetCount(const ASize: Integer);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.SetCount(ASize);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.LoadFromFile(const FileName: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.LoadFromFile(FileName);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.SaveToFile(const FileName: string);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.SaveToFile(FileName);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.AddStrings(AStrings, AClearFirst);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.CopyToStrings(const AStrings: IFPStrings);
begin
  System.EnterCriticalSection(FLock);
  try
    AStrings.AddStrings(FListIntf);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.ExportToArray: TStringArray;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.ExportToArray;
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.ExportByObject(const AObject: TObject): TStringArray;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.ExportByObject(AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TThreadStringListBase.ImportArray(const AValue: TStringArray);
begin
  System.EnterCriticalSection(FLock);
  try
    FList.ImportArray(AValue);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.Find(const S: string; out AIndex: Integer): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.Find(S, AIndex);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.FindObject(const S: string; out
  AObject: TObject): Boolean;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.FindObject(S, AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.IndexOf(const S: string): Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.IndexOf(S);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadStringListBase.IndexOfObject(const AObject: TObject
  ): Integer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := FList.IndexOfObject(AObject);
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

{ TThreadStringListUTF8 }

function TThreadStringListUTF8.LockList_: IFPStringList;
begin
  System.EnterCriticalSection(FLock);
  Result := FListIntf;
end;

constructor TThreadStringListUTF8.Create(const ASorted: Boolean;
  const ACaseSensitive: Boolean);
begin
  CreateBase(TIStringListUTF8.Create);
  FList.Sorted := ASorted;
  FList.CaseSensitive := ACaseSensitive;
end;

function TThreadStringListUTF8.LockList: TStringListUTF8;
begin
  System.EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TThreadStringListUTF8.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

class function TThreadStringListUTF8.LockedGet(
  const AThreadList: TThreadStringListUTF8; out AList: TStringListUTF8
  ): Boolean;
begin
  LockVar;
  try
    Result := AThreadList <> nil;
    if Result then
      AList := AThreadList.LockList
    else
      AList := nil;
  finally
    UnLockVar;
  end;
end;

class function TThreadStringListUTF8.LockedCreateIfNil(
  var AList: TThreadStringListUTF8; const ANewItem: string;
  const ASorted: Boolean; const ACaseSensitive: Boolean): Boolean;
begin
  LockVar;
  try
    Result := AList = nil;
    if Result then
      AList := TThreadStringListUTF8.Create(ASorted, ACaseSensitive);
    if ANewItem <> '' then
      AList.Add(ANewItem);
  finally
    UnLockVar;
  end;
end;

class function TThreadStringListUTF8.LockedFreeAndNilIfEmpty(
  var AList: TThreadStringListUTF8; const ARemoveItem: string): Boolean;
var
  lst: TThreadStringListUTF8 = nil;
  l: TStringListUTF8;
begin
  LockVar;
  try
    if AList = nil then
      Exit(True);
    l := AList.LockList;
    if ARemoveItem <> '' then
      l.Remove(ARemoveItem);
    Result := l.Count = 0;
    if Result then
      begin
        lst := AList;
        AList := nil;
        lst.UnlockList;
      end
    else
      AList.UnlockList;
  finally
    UnLockVar;
  end;
  if lst <> nil then
    lst.Free;
end;

{ TThreadFileNameList }

function TThreadFileNameList.LockList_: IFPFileNameList;
begin
  System.EnterCriticalSection(FLock);
  Result := IFPFileNameList(FListIntf);
end;

constructor TThreadFileNameList.Create(const ASorted: Boolean);
begin
  CreateBase(TIFileNameList.Create);
  FList.Sorted := ASorted;
end;

function TThreadFileNameList.LockList: TFileNameList;
begin
  System.EnterCriticalSection(FLock);
  Result := TFileNameList(FList);
end;

procedure TThreadFileNameList.UnlockList;
begin
  System.LeaveCriticalsection(FLock);
end;

{ TThreadDataStringList }

function TThreadDataStringList.GetData(Index: Integer): Pointer;
begin
  System.EnterCriticalSection(FLock);
  try
    Result := TIDataStringList(List).Data[Index];
  finally
    System.LeaveCriticalSection(FLock);
  end;
end;

function TThreadDataStringList.LockList_: IFPDataStringList;
begin
  System.EnterCriticalSection(FLock);
  Result := IFPDataStringList(ListIntf);
end;

procedure TThreadDataStringList.DoOnInitData(const p: Pointer);
begin
  if Assigned(FOnInitListData) then
    FOnInitListData(p);
end;

constructor TThreadDataStringList.Create(const ADataSize: Cardinal;
  const AOnFreeData: TPointerMethod; const ASorted: Boolean;
  const AOnInitData: TPointerMethod);
begin
  CreateBase(TIDataStringList.Create(ADataSize, AOnFreeData, ASorted, DoOnInitData));
  FOnInitListData := AOnInitData;
  //TIDataStringList(List).OnInitData := DoOnInitData;
  //TIDataStringList(List).OnFreeData := AOnFreeData;
  //List.Sorted := ASorted;
end;

function TThreadDataStringList.LockList: TDataStringList;
begin
  System.EnterCriticalSection(FLock);
  Result := TIDataStringList(List);
end;

procedure TThreadDataStringList.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TIniFileUTF8 }

function TIniFileUTF8.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): Hresult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
     Result := S_OK
   else
     Result := longint(E_NOINTERFACE);
end;

function TIniFileUTF8._AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(FRefCount);
end;

function TIniFileUTF8._Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Self.Destroy;
end;

procedure TIniFileUTF8.ReadSection(const ASection: string;
  const AStrings: IFPStrings);
begin
  ReadSection(ASection, TStrings(AStrings.GetFPObject));
end;

procedure TIniFileUTF8.ReadSections(const AStrings: IFPStrings);
begin
  ReadSections(TStrings(AStrings.GetFPObject));
end;

procedure TIniFileUTF8.ReadSectionValues(const ASection: string;
  const AStrings: IFPStrings);
begin
  ReadSectionValues(ASection, TStrings(AStrings.GetFPObject){$IF FPC_FULLVERSION >= 030000}, []{$ENDIF});
end;

procedure TIniFileUTF8.ReadSectionRaw(const ASection: string;
  const AStrings: IFPStrings);
begin
  ReadSectionRaw(ASection, TStrings(AStrings.GetFPObject));
end;

procedure TIniFileUTF8.GetSection(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  AStrings := new_StringList();
  ReadSection(ASection, AStrings);
end;

procedure TIniFileUTF8.GetSections(out AStrings: IFPStringListUTF8);
begin
  AStrings := new_StringList();
  ReadSections(AStrings);
end;

procedure TIniFileUTF8.GetSectionValues(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  AStrings := new_StringList();
  ReadSectionValues(ASection, AStrings);
end;

procedure TIniFileUTF8.GetSectionRaw(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  AStrings := new_StringList();
  ReadSectionRaw(ASection, AStrings);
end;

constructor TIniFileUTF8.Create(const AFileName: string;
  AEscapeLineFeeds: Boolean);
begin
  {$IFDEF TEST_MODE}
  //if file_SameName(AFileName, 'X:\PF\Vrode\VrodeEditor\vrsed.cfg'{DebugVarName}) then
  //  begin
  //    if (apsClosing in AppStates) then
  //      iDebug := iDebug;
  //    Beep;
  //  end;
  //if AppLoaded then
  //file_AppendLine('T:\ini.txt', AFileName + '  ' +
  //    BoolToStr(IsMainThread, 'True', 'False'));
  {$ENDIF}
  CacheUpdates := True;
  FFileName := AFileName;
  FStream := TStringStreamUTF8.Create(str_LoadFromFile(AFileName));
  {$IF FPC_FULLVERSION >= 030000}
  inherited Create(FStream, [ifoEscapeLineFeeds]);{$ELSE}
  StripQuotes := True;
  inherited Create(FStream, AEscapeLineFeeds);{$ENDIF}
end;

destructor TIniFileUTF8.Destroy;
begin
  //{$IFDEF TEST_MODE}
  //if file_SameName(FFileName, DebugVarName) then
  //  Beep;
  //{$ENDIF}
  inherited Destroy;
  FStream.Free;
end;

procedure TIniFileUTF8.UpdateFile;
begin
  if FUpdateCount > 0 then Exit;
  FStream.Size := 0;
  //FStream.Position := 0;
  inherited UpdateFile;
  str_SaveToFile(FFileName, FStream.DataString{$IFDEF TEST_MODE}, True{$ENDIF});
end;

procedure TIniFileUTF8.WriteDeleteString(const ASection, AIdent, AValue,
  ADefault: String; AToQuote: Boolean; ACaseSensetive: Boolean);
var
  f: Boolean;
begin
  if ACaseSensetive then
    f := CompareStr(AValue, ADefault) = 0
  else
    f := str_CompareText(AValue, ADefault) = 0;
  if f then
    DeleteKey(ASection, AIdent)
  else
    begin
      WriteString(ASection, AIdent, IfThen(AToQuote, '"' + AValue + '"', AValue));
    end;
end;

procedure TIniFileUTF8.WriteDeleteBool(const ASection, AIdent: string;
  AValue, ADefault: Boolean);
begin
  if AValue = ADefault then
    DeleteKey(ASection, AIdent)
  else
    WriteBool(ASection, AIdent, AValue);
end;

procedure TIniFileUTF8.WriteDeleteInteger(const ASection, AIdent: string;
  AValue, ADefault: Integer);
begin
  if AValue = ADefault then
    DeleteKey(ASection, AIdent)
  else
    WriteInteger(ASection, AIdent, AValue);
end;

procedure TIniFileUTF8.WriteSection(const ASection: string;
  const AValues: IFPStrings; const AIsClearFirst: Boolean);
var
  I: Integer;
  sName, sValue: string;
begin
  if AIsClearFirst then
    EraseSection(ASection);
  for I := 0 to AValues.Count - 1 do
    begin
      NameValueOfString(AValues[I], sName{%H-}, sValue{%H-});
      if sValue <> '' then
        WriteString(ASection, sName, sValue);
    end;
end;

procedure TIniFileUTF8.ClearSection(const ASection: string);
var
  strs: TStrings;
  I: Integer;
begin
  strs := TStringListUTF8.Create;
  try
    ReadSection(ASection, strs);
    for I := 0 to strs.Count - 1 do
      DeleteKey(ASection, strs[I]);
  finally
    strs.Free;
  end;
end;

procedure TIniFileUTF8.AddSection(const ASection: string);
begin
  if not SectionExists(ASection) then
    begin
      WriteString(ASection, 'a', '1');
      DeleteKey(ASection, 'a');
    end;
end;

function TIniFileUTF8.CopySection(const ASourceSect, ADestSect: string
  ): Boolean;
var
  strs: TStrings;
  I: Integer;
  sName, sValue: string;
begin
  Result := False;
  if SectionExists(ASourceSect) then
    begin
      strs := TStringList.Create;
      try
        ReadSectionRaw(ASourceSect, strs);
        for I := 0 to strs.Count - 1 do
          begin
            NameValueOfString(strs[I], sName{%H-}, sValue{%H-});
            WriteString(ADestSect, sName, sValue);
          end;
        Result := True;
      finally
        strs.Free;
      end;
    end
  else
    AddSection(ADestSect);
end;

function TIniFileUTF8.RenameSection(const OldSect, NewSect: string): Boolean;
begin
  Result := CopySection(OldSect, NewSect);
  EraseSection(OldSect);
end;

procedure TIniFileUTF8.BeginUpdateFile;
begin
  Inc(FUpdateCount)
end;

procedure TIniFileUTF8.EndUpdateFile;
begin
  {$IFDEF TEST_MODE}
  if FUpdateCount <= 0 then
    raise Exception.Create('TIniFileUTF8.EndUpdate(): FUpdateCount <= 0');{$ENDIF}
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateFile;
end;

procedure TIniFileUTF8.Clear;
var
  strs: TStrings;
  I: Integer;
begin
  strs := TStringListUTF8.Create;
  try
    ReadSections(strs);
    for I := 0 to strs.Count - 1 do
      EraseSection(strs[I]);
  finally
    strs.Free;
  end;
end;

function TIniFileUTF8.GetFPObject: TObject;
begin
  Result := Self;
end;

{ TThreadIniFileUTF8 }

procedure TThreadIniFileUTF8.SetUseBackup(AValue: Boolean);
begin
  if FUseBackup = AValue then Exit;
  FUseBackup := AValue;
  //if AValue then
  //  FBackupFile := file_AddExt(FIni.FFileName, '.bkp')
  //else
  //  file_Delete(FBackupFile);
end;

function TThreadIniFileUTF8.SectionExists(const Section: string): Boolean;
begin
  Lock;
  try
    Result := FIni.SectionExists(Section);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadString(const Section, Ident: string;
  const ADefault: string): string;
begin
  Lock;
  try
    Result := FIni.ReadString(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteString(const Section, Ident, Value: String);
begin
  Lock;
  try
    FIni.WriteString(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadInteger(const Section, Ident: string;
  ADefault: Longint): Longint;
begin
  Lock;
  try
    Result := FIni.ReadInteger(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteInteger(const Section, Ident: string;
  Value: Longint);
begin
  Lock;
  try
    FIni.WriteInteger(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadInt64(const Section, Ident: string;
  ADefault: Int64): {$IF FPC_FULLVERSION >= 030100}Int64{$ELSE}Longint{$ENDIF};
begin
  Lock;
  try
    Result := FIni.ReadInt64(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteInt64(const Section, Ident: string;
  Value: Int64);
begin
  Lock;
  try
    FIni.WriteInt64(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadBool(const Section, Ident: string;
  ADefault: Boolean): Boolean;
begin
  Lock;
  try
    Result := FIni.ReadBool(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteBool(const Section, Ident: string;
  Value: Boolean);
begin
  Lock;
  try
    FIni.WriteBool(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadDateTime(const Section, Ident: string;
  ADefault: TDateTime): TDateTime;
begin
  Lock;
  try
    Result := FIni.ReadDateTime(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ReadFloat(const Section, Ident: string;
  ADefault: Double): Double;
begin
  Lock;
  try
    Result := FIni.ReadFloat(Section, Ident, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteDate(const Section, Ident: string;
  Value: TDateTime);
begin
  Lock;
  try
    FIni.WriteDate(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteDateTime(const Section, Ident: string;
  Value: TDateTime);
begin
  Lock;
  try
    FIni.WriteDateTime(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteFloat(const Section, Ident: string;
  Value: Double);
begin
  Lock;
  try
    FIni.WriteFloat(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteTime(const Section, Ident: string;
  Value: TDateTime);
begin
  Lock;
  try
    FIni.WriteTime(Section, Ident, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.EraseSection(const Section: string);
begin
  Lock;
  try
    FIni.EraseSection(Section);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.DeleteKey(const Section, Ident: String);
begin
  Lock;
  try
    FIni.DeleteKey(Section, Ident);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.ValueExists(const Section, Ident: string): Boolean;
begin
  Lock;
  try
    Result := FIni.ValueExists(Section, Ident);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.UpdateFile;
begin
  Lock;
  try
    try
      FIni.UpdateFile;
      //if UseBackup then
      //  file_Copy(FIni.FFileName, FBackupFile);
    except
      {$IFDEF TEST_MODE}Beep;{$ENDIF}
    end;
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.ReadSection(const ASection: string;
  const AStrings: IFPStrings);
begin
  Lock;
  try
    FIni.ReadSection(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.ReadSections(const AStrings: IFPStrings);
begin
  Lock;
  try
    FIni.ReadSections(AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.ReadSectionValues(const ASection: string;
  const AStrings: IFPStrings);
begin
  Lock;
  try
    FIni.ReadSectionValues(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.ReadSectionRaw(const ASection: string;
  const AStrings: IFPStrings);
begin
  Lock;
  try
    FIni.ReadSectionRaw(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.GetSection(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  Lock;
  try
    FIni.GetSection(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.GetSections(out AStrings: IFPStringListUTF8);
begin
  Lock;
  try
    FIni.GetSections(AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.GetSectionValues(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  Lock;
  try
    FIni.GetSectionValues(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.GetSectionRaw(const ASection: string; out
  AStrings: IFPStringListUTF8);
begin
  Lock;
  try
    FIni.GetSectionRaw(ASection, AStrings);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteDeleteString(const ASection, AIdent, AValue,
  ADefault: String; AToQuote: Boolean; ACaseSensetive: Boolean);
begin
  Lock;
  try
    FIni.WriteDeleteString(ASection, AIdent, AValue, ADefault, AToQuote, ACaseSensetive);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteDeleteBool(const ASection, AIdent: string;
  AValue, ADefault: Boolean);
begin
  Lock;
  try
    FIni.WriteDeleteBool(ASection, AIdent, AValue, ADefault);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.WriteDeleteInt(const ASection, AIdent: string;
  AValue, ADefault: Integer);
begin
  Lock;
  try
    FIni.WriteDeleteInteger(ASection, AIdent, AValue, ADefault);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.CopySection(const ASourceSect, ADestSect: string
  ): Boolean;
begin
  Lock;
  try
    Result := FIni.CopySection(ASourceSect, ADestSect);
  finally
    Unlock;
  end;
end;

function TThreadIniFileUTF8.RenameSection(const OldSect, NewSect: string
  ): Boolean;
begin
  Lock;
  try
    Result := FIni.RenameSection(OldSect, NewSect);
  finally
    Unlock;
  end;
end;

procedure TThreadIniFileUTF8.Clear;
begin
  Lock;
  try
    FIni.Clear;
  finally
    Unlock;
  end;
end;

constructor TThreadIniFileUTF8.Create(const AFileName: string;
  AEscapeLineFeeds: Boolean);
begin
  InitCriticalSection(FLock);
  FIniIntf := TIIniFileUTF8.Create(AFileName, AEscapeLineFeeds);
  FIni := TIniFileUTF8(FIniIntf.GetFPObject);
end;

destructor TThreadIniFileUTF8.Destroy;
begin
  Lock;
  try
    UseBackup := False;
    //FIni.Free;
    inherited Destroy;
  finally
    Unlock;
    DoneCriticalSection(FLock);
  end;
end;

function TThreadIniFileUTF8.Lock: TIniFileUTF8;
begin
  System.EnterCriticalSection(FLock);
  Result := FIni;
end;

procedure TThreadIniFileUTF8.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TIIThreadIniFileUTF8 }

function TIThreadIniFileUTF8.GetFPObject: TObject;
begin
  Result := Self;
end;

function TIThreadIniFileUTF8.ReadInt64(const Section, Ident: string;
  ADefault: Int64): {$IF FPC_FULLVERSION >= 30000}Int64{$ELSE}Longint{$ENDIF};
begin
  Result := inherited ReadInt64(Section, Ident, ADefault);
end;

function TIThreadIniFileUTF8.LockIntf: IFPIniFileUTF8;
begin
  System.EnterCriticalSection(FLock);
  Result := FIniIntf;
end;

//{$IFDEF TEST_MODE}
//{.$DEFINE IGNORE_INI_LIST}
//var
//  _iniIgnore: IFPStrings = nil;{$ENDIF}
function ini_GetCacheFile(const AFileName: string; const ASecTimeOut: Cardinal
  ): IFPIniCachedFile;
begin
  //{$IFDEF TEST_MODE}
  //if file_SameName(AFileName, 'X:\PF\Vrode\VrodeEditor\vrsed.cfg'{DebugVarName}) then
  //  Beep;{$ENDIF}
  {$IFDEF CACHED_INI_NOT_THREAD}
  Result := TIIniTestCachedFile.Create(AFileName);
  {$ELSE}
    {$IFDEF IGNORE_INI_LIST}
    if _iniIgnore = nil then
      begin
        _iniIgnore := new_FileNameList(True);
        _iniIgnore.LoadFromFile('T:\ini_1.txt');
      end;
    if _iniIgnore.IndexOf(AFileName) <> -1 then
      begin
        Result := IniCachedMan.GetIni('T:\1.cfg', ASecTimeOut);
        //_iniIgnore.Remove(AFileName);
      end
    else
      Result := IniCachedMan.GetIni(AFileName, ASecTimeOut);{$ELSE}
    Result := IniCachedMan.GetIni(AFileName, ASecTimeOut);{$ENDIF}
  {$ENDIF}
end;

function ini_ReloadCacheFile(const AFileName: string): IFPIniCachedFile;
begin
  {$IFDEF CACHED_INI_NOT_THREAD}
  Result := False;
  {$ELSE}
  Result := IniCachedMan.Reload(AFileName);{$ENDIF}
end;

function ini_CacheFileExists(const AFileName: string): Boolean;
var
  ini: IFPIniCachedFile;
begin
  {$IFDEF CACHED_INI_NOT_THREAD}
  Result := False;
  {$ELSE}
  Result := IniCachedMan.FindIni(AFileName, ini);{$ENDIF}
end;

procedure ini_FreeCacheFile(const AFileName: string);
begin
  {$IFDEF CACHED_INI_NOT_THREAD}{$ELSE}
   IniCachedMan.FreeIni(AFileName);{$ENDIF}
end;

procedure ini_FreeAllCache;
begin
  IniCachedMan.Clear;
end;

procedure ini_UpdateCacheFile(const AFileName: string);
var
  ini: IFPIniCachedFile;
  F: IFPIniFileUTF8;
begin
  {$IFDEF CACHED_INI_NOT_THREAD}{$ELSE}
  if IniCachedMan.FindIni(AFileName, ini) then
    begin
      F := ini.Lock;
      try
        TIniFileCached(F.GetFPObject).UpdateIfDirty;
      finally
        ini.Unlock;
      end;
    end;{$ENDIF}
end;

function IniReadString(const FileName, Sect, Name: string; const Default: string
  ): string;
begin
  Result := ini_GetCacheFile(FileName).ReadString(Sect, Name, Default);
end;

procedure IniWriteString(const FileName, Sect, Name, Value: string);
begin
  ini_GetCacheFile(FileName).WriteString(Sect, Name, Value);
end;

procedure IniWriteDelString(const FileName, Sect, Name, Value, DefValue: string);
begin
  ini_GetCacheFile(FileName).WriteDeleteString(Sect, Name, Value, DefValue);
end;

function IniReadBool(const FileName, Sect, Name: string; const Default: Boolean): Boolean;
begin
  Result := ini_GetCacheFile(FileName).ReadBool(Sect, Name, Default);
end;

procedure IniWriteBool(const FileName, Sect, Name: string; const Value: Boolean
  );
begin
  ini_GetCacheFile(FileName).WriteBool(Sect, Name, Value);
end;

procedure IniWriteDelBool(const FileName, Sect, Name: string; const Value,
  DefValue: Boolean);
begin
  ini_GetCacheFile(FileName).WriteDeleteBool(Sect, Name, Value, DefValue);
end;

function IniReadInt(const FileName, Sect, Name: string; const Default: Integer
  ): Integer;
begin
  Result := ini_GetCacheFile(FileName).ReadInteger(Sect, Name, Default);
end;


procedure IniWriteInt(const FileName, Sect, Name: string; const Value: Integer);
begin
  ini_GetCacheFile(FileName).WriteInteger(Sect, Name, Value);
end;

procedure IniWriteDelInt(const FileName, Sect, Name: string; const Value,
  DefValue: Integer);
begin
  ini_GetCacheFile(FileName).WriteDeleteInt(Sect, Name, Value, DefValue);
end;

function IniReadFloat(const FileName, Sect, Name: string;
  const Default: Extended): Extended;
begin
  Result := ini_GetCacheFile(FileName).ReadFloat(Sect, Name, Default);
end;

procedure IniWriteFloat(const FileName, Sect, Name: string;
  const Value: Extended);
begin
  ini_GetCacheFile(FileName).WriteFloat(Sect, Name, Value);
end;

procedure IniReadSection(const FileName, Sect: string; const strs: IFPStrings);
begin
  ini_GetCacheFile(FileName).ReadSection(Sect, strs);
end;

procedure IniReadSectionValues(const FileName, Sect: string;
  const strs: IFPStrings);
begin
  ini_GetCacheFile(FileName).ReadSectionRaw(Sect, strs);
end;

function IniKeyExists(const AFileName, ASect, AKey: string): Boolean;
begin
  Result := ini_GetCacheFile(AFileName).ValueExists(ASect, AKey);
end;

procedure IniDeleteKey(const AFileName, ASect, AKey: string);
begin
  ini_GetCacheFile(AFileName).DeleteKey(ASect, AKey);
end;

procedure IniEraseSection(const AFileName, ASect: string);
begin
  ini_GetCacheFile(AFileName).EraseSection(ASect);
end;

function IniSectionExists(const AFileName, ASect: string): Boolean;
begin
  Result := ini_GetCacheFile(AFileName).SectionExists(ASect);
end;

function IniRenameSection(const AFileName, OldSect, NewSect: string): Boolean;
begin
  Result := ini_GetCacheFile(AFileName).RenameSection(OldSect, NewSect);
end;

function IniCopySection(const AFileName, ASourceSect, ADestSect: string
  ): Boolean;
begin
  Result := ini_GetCacheFile(AFileName).CopySection(ASourceSect, ADestSect);
end;

function new_StringList(const ASorted: Boolean; const ACaseSens: Boolean;
  const AFileName: string): IFPStringList;
begin
  Result := TIStringListUTF8.Create(ASorted, ACaseSens);
  if AFileName <> '' then
    Result.LoadFromFile(AFileName);
end;

function new_FileNameList(const ASorted: Boolean; const AFileName: string
  ): IFPFileNameList;
begin
  Result := TIFileNameList.Create(ASorted);
  if AFileName <> '' then
    Result.LoadFromFile(AFileName);
end;

function new_DataStringList(const ADataSize: Cardinal;
  const AOnFreeData: TPointerMethod; const ASorted: Boolean;
  const AOnInitData: TPointerMethod): IFPDataStringList;
begin
  Result := TIDataStringList.Create(ADataSize, AOnFreeData, ASorted, AOnInitData);
end;

function new_Thread(const AOnExecute: TNotifyProc;
  const AData: Pointer; const AFreeOnTerminate: Boolean;
  const ASuspended: Boolean): TThread;
begin
  Result := TAnonymousThread.CreateAlt(AOnExecute, AData, AFreeOnTerminate, ASuspended);
end;

function new_ThreadM(const AOnExecute: TNotifyMethod;
  const AData: Pointer; const AFreeOnTerminate: Boolean;
  const ASuspended: Boolean): TThread;
begin
  Result := TAnonymousThread.CreateAlt(nil, AData, AFreeOnTerminate, ASuspended, AOnExecute);
end;

function new_Timer(const AMSecInterval: Cardinal; const AOnTimer: TNotifyEvent): IFPTimer;
begin
  Result := TITimer.Create;
  Result.Interval := AMSecInterval;
  Result.OnTimer := AOnTimer;
end;

function new_FileStream(const AFileName: string; AMode: Word): IFPFileStream;
begin
  Result := TIFileStreamUTF8.Create(AFileName, AMode);
end;

function new_StringStream(const S: string): IFPStringStream;
begin
  Result := TIStringStreamUTF8.Create(S);
end;

function new_List: IFPList;
begin
  Result := TIFPList.Create;
end;

function new_InterfaceList: IFPInterfaceList;
begin
  Result := TvrInterfaceList.Create;
end;

function new_IniFile(const AFileName: string): IFPIniFile;
begin
  Result := TIIniFileUTF8.Create(AFileName);
end;

function new_ThreadIniFile(const AFileName: string): IFPThreadIniFile;
begin
  Result := TIThreadIniFileUTF8.Create(AFileName);
end;

type
  TQueueProcKind = (qpkNone, qpkDataMethod, qpkNotifyProc, qpkNotifyMethod);
  TQueueCallInfo = record
    Kind: TQueueProcKind;
    Proc: TMethod;
    Data1: Pointer;
    Data2: Pointer;
    InMainThread: Boolean;
  end;
  PQueueCallInfo = ^TQueueCallInfo;

  { TQueueThread }

  TQueueThread = class(TThread)
  private
    FInfo: TQueueCallInfo;
    class var FList: TThreadList;
    procedure DoProc;
  protected
    procedure Execute; override;
  end;

var
  _QueueThread: TQueueThread = nil;

{ TQueueThread }

procedure TQueueThread.DoProc;
begin
  case FInfo.Kind of
    qpkDataMethod: TDataMethod(FInfo.Proc)(PtrInt(FInfo.Data1));
    qpkNotifyProc: TNotifyProc(FInfo.Proc.Code)(TObject(FInfo.Data1), FInfo.Data2);
    qpkNotifyMethod: TNotifyMethod(FInfo.Proc)(TObject(FInfo.Data1), FInfo.Data2);
  end;
end;

procedure TQueueThread.Execute;

  function _Next: Boolean;
  var
    lst: TList;
    p: PQueueCallInfo;
  begin
    LockVar;
    lst := FList.LockList;
    try
      Result := False;
      if lst.Count > 0 then
        begin
          p := PQueueCallInfo(lst[0]);
          FInfo := p^;
          lst.Delete(0);
          Dispose(p);
          Exit(True);
        end;
      _QueueThread := nil;
    finally
      FList.UnlockList;
      if not Result then
        FreeAndNil(FList);
      UnLockVar;
    end;
  end;

begin
  FreeOnTerminate := True;
  while _Next do
    begin
      if FInfo.InMainThread then
        Synchronize(DoProc)
      else
        DoProc;
    end;
end;

procedure _AddQueueProc(const AData1, AData2: Pointer; const AInMainThread: Boolean;
    AKind: TQueueProcKind; AProc: TMethod);
var
  Info: PQueueCallInfo;
begin
  New(Info);
  Info.Kind := AKind;
  Info.Proc := AProc;
  Info.Data1 := AData1;
  Info.Data2 := AData2;
  Info.InMainThread := AInMainThread;
  TThreadList.LockedCreateIfNil(TQueueThread.FList, Info);
  LockVar;
  if _QueueThread = nil then
    _QueueThread := TQueueThread.Create(False);
  UnLockVar;
end;

procedure QueueAsyncCall(const AMethod: TDataMethod; const AData: PtrInt;
  const AInMainThread: Boolean);
begin
  _AddQueueProc({%H-}Pointer(AData), nil, AInMainThread, qpkDataMethod, TMethod(AMethod));
end;

procedure QueueAsyncCall(const AMethod: TNotifyProc; const Sender: TObject;
  const AData: Pointer; const AInMainThread: Boolean);
begin
  _AddQueueProc(Sender, AData, AInMainThread, qpkNotifyProc, Method(@AMethod, nil));
end;

procedure QueueAsyncCall(const AMethod: TNotifyMethod; const Sender: TObject;
  const AData: Pointer; const AInMainThread: Boolean);
begin
  _AddQueueProc(Sender, AData, AInMainThread, qpkNotifyProc, TMethod(AMethod));
end;

procedure RemoveAsyncCalls(const AMethod: TMethod);
var
  lst: TList;
  i: Integer;
  r: PQueueCallInfo;
begin
  if not TThreadList.LockedGet(TQueueThread.FList, lst) then Exit;
  try
    for i := lst.Count - 1 downto 0 do
      begin
        r := PQueueCallInfo(lst[i]);
        if ((AMethod.Code = nil) and (AMethod.Data = r.Proc.Data)) then
          begin
            Dispose(r);
            lst.Delete(i);
          end
        else if SameMethod(r.Proc, AMethod) then
          begin
            Dispose(r);
            lst.Delete(i);
          end;
      end;
  finally
    TQueueThread.FList.UnlockList;
  end;
end;

procedure RemoveAsyncCalls(const AProc: TProcedure);
begin
  RemoveAsyncCalls(Method(@AProc, nil));
end;

procedure RemoveAsyncCalls(const AObject: TObject);
begin
  RemoveAsyncCalls(Method(nil, AObject));
end;

function new_ThreadStringList(const ASorted: Boolean;
  const ACaseSensitive: Boolean): IFPThreadStringList;
begin
  Result := TThreadStringListUTF8.Create(ASorted, ACaseSensitive);
end;

function new_ThreadFileNameList(const ASorted: Boolean): IFPThreadFileNameList;
begin
  Result := TThreadFileNameList.Create(ASorted);
end;

function new_ThreadDataStringList(const ADataSize: Cardinal;
  const ASorted: Boolean; const AOnFreeData: TPointerMethod;
  const AOnInitData: TPointerMethod): IFPThreadDataStringList;
begin
  Result := TThreadDataStringList.Create(ADataSize, AOnFreeData, ASorted, AOnInitData);
end;

finalization
  LockedObjectFreeAndNil(__IniCachedMan);
  {$IFDEF DEBUG_MODE}__Finilized := True;{$ENDIF}


end.

