unit vr_intfs;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses SysUtils, Classes, vr_types{$IFDEF WINDOWS}, windows{$ENDIF};

type
  TOsEvent = vr_types.TOSEvent;

  ICustomData = interface
    ['{1B8A37FB-94FF-48D6-80B2-AB2C6E64515B}']
  end;

  IVariantObject = interface //inherit to work IVariant assignment operator overloading
    ['{7528DBB4-8E67-4B7C-A1E0-31B687128060}']
  end;

  IFPObject = interface
    ['{76937C07-A0E0-41B2-A8BC-DAE634D3FF5D}']
    function GetFPObject: TObject;
  end;

  { IFPStringBuilder }

  IFPStringBuilder = interface
    ['{D6DA0F34-6A4D-4472-A549-5CF0E11D0E5B}']
    function GetCount: Integer;

    procedure Add(const S: string); overload;
    procedure Add(const AStrings: array of string); overload;
    function ToString: string;
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  IFPTimer = interface
    ['{39F38883-F916-4EB6-A126-91012AC8DEB7}']
    function GetEnabled: Boolean;
    function GetInterval: Cardinal;
    function GetOnTimer: TNotifyEvent;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInterval(AValue: Cardinal);
    procedure SetOnTimer(AValue: TNotifyEvent);

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
  end;

  { IFPInterfaceList }

  IFPInterfaceList = interface(IInterfaceList)
    ['{03DC6BB5-6F3B-43EA-A6E9-161CE5D7539B}']
    function GetFPObject: TObject;
    procedure Assign(const AList: IFPInterfaceList);
    function AddUnique(const AItem: IUnknown) : Integer;
    function ExportToArray: TInterfaceArray;
  end;

  { IFPStream }

  IFPStream = interface(IFPObject)
    ['{72C74C0C-F0EB-4786-8A5F-48BC851E9320}']
    function GetPosition: Int64;
    function GetSize: Int64;
    function Read(var Buffer; Count: Longint): Longint;
    procedure SetPosition(const AValue: Int64);
    procedure SetSize64(const AValue: Int64);
    function Write(const Buffer; Count: Longint): Longint;
    function Seek(Offset: Longint; Origin: Word): Longint;  overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;  overload;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    //function CopyFrom(Source: TStream; Count: Int64): Int64;
    //function ReadComponent(Instance: TComponent): TComponent;
    //function ReadComponentRes(Instance: TComponent): TComponent;
    //procedure WriteComponent(Instance: TComponent);
    //procedure WriteComponentRes(const ResName: string; Instance: TComponent);
    //procedure WriteDescendent(Instance, Ancestor: TComponent);
    //procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
    procedure WriteResourceHeader(const ResName: string; {!!!:out} var FixupInfo: Integer);
    procedure FixupResourceHeader(FixupInfo: Integer);
    procedure ReadResHeader;
    function ReadByte : Byte;
    function ReadWord : Word;
    function ReadDWord : Cardinal;
    function ReadQWord : QWord;
    function ReadAnsiString : String;
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : Word);
    procedure WriteDWord(d : Cardinal);
    procedure WriteQWord(q : QWord);
    Procedure WriteAnsiString (const S : String);
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;

  IFPFileStream = interface(IFPStream)
    ['{C0A60D1F-A92C-4701-828F-56E028A3F656}']
  end;

  { IFPStringStream }

  IFPStringStream = interface(IFPStream)
    ['{C0A60D1F-A92C-4701-828F-56E028A3F656}']{$IF FPC_FULLVERSION >= 030200}
    function GetUnicodeDataString: UnicodeString;
    function GetOwnsEncoding: Boolean;
    function GetEncoding: TEncoding;{$ENDIF}
    function GetDataString: string;

    function ReadString(Count: Longint): string;
    procedure WriteString(const AString: string);{$IF FPC_FULLVERSION >= 030200}
    function ReadUnicodeString(Count: Longint): UnicodeString;
    procedure WriteUnicodeString(const AString: UnicodeString);
    property UnicodeDataString: UnicodeString read GetUnicodeDataString;
    property OwnsEncoding: Boolean read GetOwnsEncoding;
    property Encoding: TEncoding read GetEncoding;
    {$ENDIF}

    property DataString: string read GetDataString;
  end;

  IFPStack = interface(IFPObject)
    ['{E05DAE44-5BCC-41CD-BC07-5691EDE488D0}']
    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(AItem: Pointer): Pointer;
    function Pop: Pointer;
    function Peek: Pointer;
  end;

  { IFPList }

  IFPList = interface(IFPObject)
    ['{8274F86E-389D-4E6D-80FB-169CB348C253}']
    procedure AddList(const AList: IFPList);
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    function Extract(item: Pointer): Pointer;
    function First: Pointer;
    function Get(Index: Integer): Pointer;
    function GetCapacity: Integer;
    function GetCount: Integer;
    //function GetEnumerator: TListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Put(Index: Integer; AValue: Pointer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    //property List: PPointerList read GetList;

    function AddUnique(AItem: Pointer): Integer;
  end;

  { IFPIntegerList }

  IFPIntegerList = interface(IFPObject)
    ['{55B2725F-54F6-4819-A80E-1ACBA9D68BC2}']
    //procedure AddList(const AList: IFPIntegerList);
    function Add(AItem: PtrInt): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    function Extract(AItem: PtrInt): Boolean;
    function First: PtrInt;
    function GetCapacity: Integer;
    function GetCommaText: string;
    function GetCount: Integer;
    function GetItem(Index: Integer): PtrInt;
    //function GetEnumerator: TListEnumerator;
    function IndexOf(const Item: PtrInt): Integer;
    procedure Insert(Index: Integer; Item: PtrInt);
    function Last: PtrInt;
    procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(Item: PtrInt): Integer;
    procedure Pack;
    procedure SetCapacity(AValue: Integer);
    procedure SetCommaText(const AValue: string);
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; const AValue: PtrInt);
    procedure Sort;//(Compare: TListSortCompare);
    function LoadFromIntegerArray(const arr: TIntArray): Boolean;
    procedure SaveToIntegerArray(var arr: TIntArray);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: PtrInt read GetItem write SetItem; default;
    //property List: PPointerList read GetList;
    property CommaText: string read GetCommaText write SetCommaText;
  end;

  IGenList<T> = interface(IFPObject)
    function GetCapacity: Integer;
    function GetFirst: T;
    function GetLast: T;
    procedure SetCapacity(AValue: Integer);
    procedure SetFirst(const AValue: T);
    procedure SetLast(const AValue: T);

    function Add(const AItem: T): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    function Extract(const Item: T): T;
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T);
    procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(const Item: T): Integer;
    procedure Pack;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  IGenThreadList<T> = interface(IGenList<T>)

  end;

  { IGenNumberListBase }

  IGenNumberListBase<T> = interface(IGenList<T>)
    ['{6624B15B-CA96-4B61-8D88-1F36688FD3AC}']
    function GetDuplicates: TDuplicates;
    function GetSorted: Boolean;
    procedure SetDuplicates(AValue: TDuplicates);
    function GetCommaText: string;
    procedure SetCommaText(const AValue: string);
    procedure SetSorted(AValue: Boolean);

    procedure Sort;
    function LoadFromStream(const AStream: TStream): Boolean;
    procedure SaveToStream(const AStream: TStream);
    function LoadFromArray(const arr: array of T): Boolean;
    procedure SaveToArray(var arr: TGArray<T>);

    property CommaText: string read GetCommaText write SetCommaText;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
  end;

  IGenNumberList<T> = interface(IGenNumberListBase<T>)
    ['{D9D4026E-7C28-40D3-9322-597CB4727A5F}']
    function Get(Index: Integer): T;
    function GetCount: Integer;
    procedure Put(Index: Integer; const AValue: T);
    procedure SetCount(AValue: Integer);

    //function GetEnumerator: TListEnumerator;
    procedure AddList(const AList: IGenNumberList<T>);

    //property List: PPointerList read GetList;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read Get write Put; default;
  end;

  IGenThreadNumberList<T> = interface(IGenNumberListBase<T>)
    ['{1222AE27-E24C-443A-9A9D-4236081394B7}']

  end;

  { IFPStrings }

  IFPStrings = interface(IFPObject)
    ['{E2F15AED-5C47-4620-8DCE-E808E154F0D7}']
    function Add(const S: string): Integer;
    function AddObject(const S: string; AObject: TObject): Integer;
    //procedure AddStrings(const TheStrings: array of string); overload;
    Procedure AddText(Const S : String);
    procedure Clear;
    procedure Delete(Index: Integer);
    function GetStrictDelimiter: Boolean;
    function Remove(const S: string): Boolean;
    procedure Exchange(Index1, Index2: Integer);
    function Get(Index: Integer): string;
    function GetCapacity: Integer;
    function GetCommaText: string;
    function GetCount: Integer;
    function GetDelimitedText: string;
    function GetLBS: TTextLineBreakStyle;
    function GetName(Index: Integer): string;
    function GetObject(Index: Integer): TObject;
    function GetText: PChar;
    function GetTextStr: string;
    function GetValue(const Name: string): string;
    function GetValueFromIndex(Index: Integer): string;
    function IndexOf(const S: string): Integer;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure LoadFromFile(const FileName: string);
    //procedure LoadFromStream(Stream: IFPStream);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Put(Index: Integer; const AValue: string);
    procedure PutObject(Index: Integer; AValue: TObject);
    procedure SaveToFile(const FileName: string);
    procedure SetCapacity(AValue: Integer);
    procedure SetCommaText(const AValue: string);
    procedure SetDelimitedText(const AValue: string);
    procedure SetLBS(AValue: TTextLineBreakStyle);
    procedure SetStrictDelimiter(AValue: Boolean);
    //procedure SaveToStream(Stream: IFPStream);
    procedure SetText(TheText: PChar);
    procedure GetNameValue(Index : Integer; Out AName,AValue : String);
    function  ExtractName(Const S:String):String;
    procedure SetTextStr(const AValue: string);
    procedure SetValue(const Name: string; const AValue: string);
    procedure SetValueFromIndex(Index: Integer; const AValue: string);

    property TextLineBreakStyle : TTextLineBreakStyle Read GetLBS Write SetLBS;
    //property Delimiter: Char read FDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    Property StrictDelimiter : Boolean Read GetStrictDelimiter Write SetStrictDelimiter;
    //property QuoteChar: Char read FQuoteChar write SetQuoteChar;
    //Property NameValueSeparator : Char Read FNameValueSeparator Write SetNameValueSeparator;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;

    procedure AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False);
  end;

  { IFPStringListUTF8 }

  IFPStringListUTF8 = interface(IFPStrings)
    ['{A7A625C7-888D-439A-B334-48C34F670841}']
    function GetCaseSensitive: Boolean;
    function GetSorted: Boolean;{$IF FPC_FULLVERSION >= 030000}
    function GetSortStyle: TStringsSortStyle;{$ENDIF}
    procedure SetCaseSensitive(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);{$IF FPC_FULLVERSION >= 030000}
    procedure SetSortStyle(AValue: TStringsSortStyle);{$ENDIF}

    function AddIfNotFind(const S: string; const AObject: TObject = nil): Boolean;
    function Remove(const AIndex: Integer; out S: string): Boolean; overload;
    function RemoveEx(const AIndex: Integer; out S: string; out AObject: TObject): Boolean;
    procedure RemoveName(const AName: string);
    procedure RemoveAllObjects(const AObject: TObject);
    function Find(const S: string; Out Index: Integer): Boolean;
    function FindObject(const S: string; Out AObject: TObject): Boolean;
    procedure Sort;
    function ExportToArray: TStringArray;
    function ExportByObject(const AObject: TObject): TStringArray;
    procedure ImportArray(const AValue: TStringArray);
    //procedure CustomSort(CompareFn: TStringListSortCompare);
    //property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;{$IF FPC_FULLVERSION >= 030000}
    property SortStyle : TStringsSortStyle read GetSortStyle write SetSortStyle;{$ENDIF}
  end;

  IFPStringList = IFPStringListUTF8;

  IFPFileNameList = interface(IFPStringListUTF8)
    ['{45BEDB7C-0861-42FE-87FE-19BD1D1841BE}']
    procedure ClearNotExistFiles(const ACheckDirs: Boolean = False);
  end;

  { IDataStringList }

  IFPDataStringList = interface(IFPStringListUTF8)
    ['{F1C9B9A7-A5AA-4FB1-AEA9-8BE46C3AAB35}']
    function GetData(const Index: Integer): Pointer;

    function IndexOfData(AData: Pointer): Integer;
    property Data[Index: Integer]: Pointer read GetData;
    //property OnFreeData: TNotifyPointer read FOnFreeData write FOnFreeData;
  end;

  { IFPThreadStrings }

  IFPThreadStrings = interface
    ['{7B569584-0104-4178-80F7-1303D455BEBE}']
    function Get(Index: Integer): string;
    function GetObject(Index: Integer): TObject;
    function GetOwnsObjects: Boolean;
    function GetSorted: Boolean;
    function GetValue(const AName: string): string;
    procedure Put(Index: Integer; const AValue: string);
    procedure PutObject(Index: Integer; const AValue: TObject);
    procedure SetOwnsObjects(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
    procedure SetValue(const AName: string; const AValue: string);

    function Add(const S: string): Integer;
    procedure Remove(const S: string); overload;
    function Remove(const AIndex: Integer; out S: string): Boolean; overload;
    function RemoveLast(out S: string): Boolean;
    procedure RemoveName(const AName: string);
    procedure RemoveAllObjects(const AObject: TObject);
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure Insert(AIndex: Integer; const S: string);
    procedure InsertObject(AIndex: Integer; const S: string; AObject: TObject);
    procedure Delete(AIndex: Integer);
    function IsEmpty: Boolean;
    function Count: Integer;
    procedure SetCount(const ASize: Integer);
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure AddStrings(const AStrings: IFPStrings; const AClearFirst: Boolean = False);
    procedure CopyToStrings(const AStrings: IFPStrings);
    function ExportToArray: TStringArray;
    function ExportByObject(const AObject: TObject): TStringArray;
    procedure ImportArray(const AValue: TStringArray);
    function Find(const S: string; Out AIndex: Integer): Boolean;
    function IndexOf(const S: string): Integer;
    function FindObject(const S: string; Out AObject: TObject): Boolean;
    function IndexOfObject(const AObject: TObject): Integer;

    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Values[const AName: string]: string read GetValue write SetValue;
    property Sorted: Boolean read GetSorted write SetSorted;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IFPThreadStringList = interface(IFPThreadStrings)
    ['{529C0F4A-9A0F-4A11-983E-C465F84D29B2}']
    function  LockList: IFPStringList;
    procedure UnlockList;
  end;

  IFPThreadFileNameList = interface(IFPThreadStrings)
    ['{719E62EC-1F99-4B6A-B9FA-2B86E2205AEC}']
    function  LockList: IFPFileNameList;
    procedure UnlockList;
  end;

  { IFPThreadDataStringList }

  IFPThreadDataStringList = interface(IFPThreadStrings)
    ['{3B508A20-5C30-4E9E-873E-BB984CE2B6A8}']
    function GetData(Index: Integer): Pointer;

    function  LockList: IFPDataStringList;
    procedure UnlockList;
    property Data[Index: Integer]: Pointer read GetData;
  end;

  { IFPCustomIniFile }

  IFPCustomIniFile = interface(IFPObject)
    ['{9554624F-1C01-4855-9FC9-FB59DF8100B2}']
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident: string; const ADefault: string = ''): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; ADefault: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadInt64(const Section, Ident: string; ADefault: Int64): {$IF FPC_FULLVERSION >= 030000}Int64{$ELSE}Longint{$ENDIF};
    procedure WriteInt64(const Section, Ident: string; Value: Int64);
    function ReadBool(const Section, Ident: string; ADefault: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadDate(const Section, Ident: string; ADefault: TDateTime): TDateTime;
    function ReadDateTime(const Section, Ident: string; ADefault: TDateTime): TDateTime;
    function ReadFloat(const Section, Ident: string; ADefault: Double): Double;
    function ReadTime(const Section, Ident: string; ADefault: TDateTime): TDateTime;
    //function ReadBinaryStream(const Section, Name: string; Value: IFPStream): Integer;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime);
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime);
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure WriteTime(const Section, Ident: string; Value: TDateTime);
    //procedure WriteBinaryStream(const Section, Name: string; Value: IFPStream);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
    function ValueExists(const Section, Ident: string): Boolean;

    procedure ReadSections(const AStrings: IFPStrings);
    //without comments name
    procedure ReadSection(const ASection: string; const AStrings: IFPStrings);
    //without comments name=value
    procedure ReadSectionRaw(const ASection: string; const AStrings: IFPStrings);
    //with comments name=value
    procedure ReadSectionValues(const ASection: string; const AStrings: IFPStrings);

    procedure GetSection(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSections(out AStrings: IFPStringListUTF8);
    procedure GetSectionValues(const ASection: string; out AStrings: IFPStringListUTF8);
    procedure GetSectionRaw(const ASection: string; out AStrings: IFPStringListUTF8);
  end;

  { IFPIniFileUTF8 }

  IFPIniFileUTF8 = interface(IFPCustomIniFile)
    ['{7C11338A-C8DD-4612-A66E-5D30E5886CF8}']
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
  end;

  IFPIniFile = IFPIniFileUTF8;

  IFPThreadIniFileUTF8 = interface(IFPObject)
    ['{73467F72-0F1C-45C9-B80B-971FF484C3CE}']
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident: string; const ADefault: string = ''): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; ADefault: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadInt64(const Section, Ident: string; ADefault: Int64): {$IF FPC_FULLVERSION >= 030100}Int64{$ELSE}Longint{$ENDIF};
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
    procedure UpdateFile;
    function ValueExists(const Section, Ident: string): Boolean;

    procedure ReadSections(const AStrings: IFPStrings);
    //without comments name
    procedure ReadSection(const ASection: string; const AStrings: IFPStrings);
    //without comments name=value
    procedure ReadSectionRaw(const ASection: string; const AStrings: IFPStrings);
    //with comments name=value
    procedure ReadSectionValues(const ASection: string; const AStrings: IFPStrings);

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

    function Lock: IFPIniFileUTF8;
    procedure Unlock;
  end;

  IFPThreadIniFile = IFPThreadIniFileUTF8;

  {$IFDEF CACHED_INI_NOT_THREAD}
  IFPIniTestCachedFile = interface(IFPIniFileUTF8)
    ['{87131744-FF14-4E4D-8CE5-1FDEB2C74D4C}']
    function Lock: IFPIniFileUTF8;
    procedure Unlock;
  end;
  IFPIniCachedFile = IFPIniTestCachedFile;
  {$ELSE}
  IFPIniCachedFile = IFPThreadIniFileUTF8;
  {$ENDIF}

  TOnBeforeLoad = procedure(Sender: TObject; var AUrl: string; var ACancel: Boolean) of object;
  TOnShowContextMenu = procedure(Sender: TObject; constref APoint: TPoint; var ADone: Boolean) of object;
  TWBKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState; const AOSEvent: TOSEvent) of Object;

  { IWebBrowser }

  IWebBrowser = interface
    ['{48EA1A18-F87C-4156-A0E5-03BC77E0E2D3}']
    function GetEnabled: Boolean;
    function GetInitialized: Boolean;
    function GetOnKeyDown: TWBKeyEvent;
    procedure SetEnabled(AValue: Boolean);
    procedure SetOnKeyDown(AValue: TWBKeyEvent);
    function GetOnBeforeLoad: TOnBeforeLoad;
    procedure SetOnBeforeLoad(AValue: TOnBeforeLoad);
    function GetOnLoad: TNotifyEvent;
    procedure SetOnLoad(AValue: TNotifyEvent);
    function GetOnShowContextMenu: TOnShowContextMenu;
    procedure SetOnShowContextMenu(AValue: TOnShowContextMenu);
    function GetOnFocused: TNotifyEvent;
    procedure SetOnFocused(AValue: TNotifyEvent);

    procedure Navigate(const AUrl: string);
    procedure Refresh(const AIgnoreCache: Boolean = False);
    function LoadFromString(const S : string): Integer;
    function SaveToString: string;
    procedure PrintToPDF(const AFileName: string);

    function Focused: Boolean;
    function ClearSelection: Boolean;
    //function SelText: string;
    function SelectAll: Boolean;

    procedure ExecuteJavaScript(const ACode: string);

    property OnKeyDown: TWBKeyEvent read GetOnKeyDown write SetOnKeyDown;//may be not In MainThread (Chromium Embedded)
    property OnBeforeLoad: TOnBeforeLoad read GetOnBeforeLoad write SetOnBeforeLoad;
    property OnLoad: TNotifyEvent read GetOnLoad write SetOnLoad;
    property OnShowContextMenu: TOnShowContextMenu read GetOnShowContextMenu write SetOnShowContextMenu;
    property OnFocused: TNotifyEvent read GetOnFocused write SetOnFocused;

    property Initialized: Boolean read GetInitialized;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    function IsEnabled: Boolean;//check Parent
    //property Visible: Boolean read GetVisible write SetVisible;
    function IsVisible: Boolean;//check Parent
  end;

  IFPHttpClient = interface(IFPObject)
    ['{A80BF710-0E74-4918-9A4F-BF2035256918}']
  end;

implementation

end.


