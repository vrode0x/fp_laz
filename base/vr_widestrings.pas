unit vr_WideStrings;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses sysutils, Classes,
  vr_types, vr_SysUtils, LazUTF8Classes, LazFileUtils,
  {$IFDEF WINDOWS}windows{$ELSE}windows_compat{$ENDIF};

const
  { Each Unicode stream should begin with the code U+FEFF,  }
  {   which the standard defines as the *byte order mark*.  }
  UNICODE_BOM = WideChar($FEFF);
  UNICODE_BOM_SWAPPED = WideChar($FFFE);
  UTF8_BOM = AnsiString(#$EF#$BB#$BF);


type
  CharSet = set of ansichar;
  TStreamCharSet = (csAnsi, csUnicode, csUnicodeSwapped, csUtf8);

function WideCharInSet(W: WideChar; sets: CharSet): boolean;
function WideCharInArray(W: WideChar; sets: array of WideChar): boolean;

function WStrAlloc(Size: Cardinal): PWideChar;
function WStrBufSize(const Str: PWideChar): Cardinal;
function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar;
function WStrNew(const Str: PWideChar): PWideChar;
procedure WStrDispose(Str: PWideChar);

function WStrLen(const Str: PWideChar): Cardinal;
function WStrEnd(const Str: PWideChar): PWideChar;
function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WideStr_LCopy(Source: PWideChar; MaxLen: Cardinal): WideString;
function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;
function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;

function WStrScan(Str: PWideChar; Chr: WideChar): PWideChar;

function WStrPos(Str, SubStr: PWideChar): PWideChar;
function WideStrLPos(Str, SubStr: PWideChar; StrLen: Cardinal): PWideChar;
function WideCharRPos(ch: WideChar; Str: WideString): Integer;

function WStrECopy(Dest, Source: PWideChar): PWideChar;

function WStrComp(Str1, Str2: PWideChar): Integer;
function WStrIComp(Str1, Str2: PWideChar): Integer;
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function WStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;

function WStrLower(Str: PWideChar): PWideChar;
function WStrUpper(Str: PWideChar): PWideChar;
function WStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
function WStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WStrPas(const Str: PWideChar): WideString;

procedure WStrSwapByteOrder(Str: PWideChar);

function WStrPosOccur(Substr, Str: PWideChar; Occur: Integer): PWideChar;
function WStrPosCount(SubStr, Str: PWideChar): Integer;

function WStrPosToEnd(Str, SubStr: PWideChar; PEnd: PWideChar = nil): PWideChar;

function WideUpCase(ch: WideChar): WideChar;
function WideLoCase(ch: WideChar): WideChar;

function WideCharIsWordBreak(AChar: WideChar): Boolean;

function WideLastChar(const S: WideString): PWideChar;
function WideLastDelimiter(const Delimiters, S: WideString): Integer;

//vr_utils.str_LCopy  function WideLCopy(Source: PWideChar; MaxLen: Cardinal): WideString;
function WidePosText(const SubStr, S: WideString): Integer;

function WidePosOccur(const Substr, Str: WideString; Occur: Integer): Integer;

function WideExtractQuoted(const S, Quot: WideString): WideString;
//A Quote character is inserted at the beginning and end of S,
//and each Quote character in the string is doubled.
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;

//removes the quote characters from the beginning and end of a quoted string,
//and reduces pairs of quote characters within the string to a single quote character
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function WideDequotedStr(const S: WideString; AQuote: WideChar): WideString;

function WideRemoveEmptyChars(const S: WideString): WideString;
function WideAdjustLineBreaks(const S: WideString; Style: TTextLineBreakStyle = tlbsCRLF ): WideString;

function WideIndexOfChars(const S, Chars : WideString) : Integer;
function WideParse(var S : WideString; const Separators : WideString ): WideString;

function WideDequotedChars(const S, AQuoteChars: WideString): WideString;
function WideDequotedSimple(const S: WideString; AQuote: WideChar): WideString;
function WideStartsText(const ASubText, AText: WideString): Boolean;
function WideStartsStr(const ASubText, AText: WideString): Boolean;
function WideEndsText(const ASubText, AText: WideString): Boolean;
function WideEndsStr(const ASubText, AText: WideString): Boolean;

procedure UpperHTMLTagsW(var S: WideString);
procedure LowerHTMLTagsW(var S: WideString);

function WideOfRandomChar(Len: Integer; IsUpperCase: Boolean = False): WideString;

procedure WideAddLine(var S: WideString; const Line: WideString; ALineBreak: WideString = sLineBreak);

function WideRPos(const SubStr, Str: WideString): Integer;

function WideTrimChars(const S: WideString; chStart, chEnd: WideChar): WideString;
function WideTrimChar(const S: WideString; ch: WideChar): WideString;

function WideReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString;

function WideFindCharsInText(var FindAt: Integer; Len: Integer; PC: PWideChar;
            const FindChars, BreakChars: WideString; Back: Boolean): Integer;
function WideFindEnclosedText(var StartPos: Integer; var EndPos: Integer;
                PC: PWideChar; FindAt, Len: Integer;
                chStart, chEnd: WideChar; Back: Boolean): Boolean;
function WideFindEnclosedTextExt(var StartPos: Integer; var EndPos: Integer;
                PC: PWideChar; FindAt, Len: Integer;
                chStart, chEnd: WideChar; Back: Boolean; Text: WideString): Boolean;


//const
//  F_CET_DELIM_STR_TRIM      = $00000001;
//  F_CET_DELIM_STR_DEQOUTE   = $00000002;
//procedure WideGetNextDelimStringInit(S, DelimS: PWideChar;
//    F_CET_DELIM_STR: Cardinal = 0; Quote: WideString = '"');
//function WideGetNextDelimString(var S: WideString): Boolean;
//function WideGetNextDelimStringExt(var S: WideString; F_CET_DELIM_STR: Cardinal): Boolean;

function WideStrGetNextWord(var pcWord: PWideChar;
    var Len: Integer; IsQuotWord, IsSkipEmpty: Boolean): Boolean;
function WideGetWordAtPosEx(const S: WideString; var PosS: Integer;
    out PosE: Integer; IsQuotWord: Boolean): WideString;
function WideGetWordAtPos(const S: WideString; Pos: Integer): WideString;

function WideStrGetPrevWord(var pchFind: PWideChar; var Len: Integer;
    pchStart: PWideChar; const BreakChars: WideString = ''): Boolean;

function WideToUnixSlash(const S: WideString): WideString;
function WideToWinSlash(const S: WideString): WideString;

function WideStrRowColToSelStart(Apc: PWideChar; ARow, ACol: Integer; var SelS: Integer): Boolean;
function WideStrSelStartToRowCol(Apc: PWideChar; var ARow, ACol: Integer; ASelS: Integer): Boolean;

function AutoDetectCharacterSet(Stream: TStream): TStreamCharSet;

const
  MaxListSize = 65535;
type
  TWideStringList = class;

  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;
  TWideStringListSortCompare = function(List: TWideStringList; Index1, Index2: Integer): Integer;

  TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
      sdLineBreak, sdStrictDelimiter);

  TWideStrings = TWideStringList;

  { TWideStringList }

  TWideStringList = class(TObject)
  private
    FDefined: TStringsDefined;
    FDelimiter: WideChar;
    FLineBreak: WideString;
    FQuoteChar: WideChar;
    FNameValueSeparator: WideChar;
    FStrictDelimiter: Boolean;

    FList: PWideStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;

    function GetCommaText: WideString;
    function GetDelimitedText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
//    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetDelimitedText(const Value: WideString);
//    procedure SetStringsAdapter(const Value: IWideStringsAdapter);
    procedure SetValue(const Name, Value: WideString);
//    procedure WriteData(Writer: TWriter);
    function GetDelimiter: WideChar;
    procedure SetDelimiter(const Value: WideChar);
    function GetLineBreak: WideString;
    procedure SetLineBreak(const Value: WideString);
    function GetQuoteChar: WideChar;
    procedure SetQuoteChar(const Value: WideChar);
    function GetNameValueSeparator: WideChar;
    procedure SetNameValueSeparator(const Value: WideChar);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): WideString;
//    procedure SetValue(const Name: WideString; const AValue: WideString);
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);


    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TWideStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
//    procedure Error(const Msg: WideString; Data: Integer); overload;
//    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function GetTextStr: WideString;
    procedure SetTextStr(const Value: WideString);
    function ExtractName(const S: WideString): WideString;

    function Get(Index: Integer): WideString;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetObject(Index: Integer): TObject; virtual;
    procedure Put(Index: Integer; const S: WideString);
    procedure PutObject(Index: Integer; AObject: TObject);  virtual;
    procedure SetCapacity(NewCapacity: Integer);
    function CompareStrings(const S1, S2: WideString): Integer;
    procedure InsertItem(Index: Integer; const S: WideString; AObject: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: WideString): Integer;
    function AddObject(const S: WideString; AObject: TObject): Integer;
    procedure AddStrings(Strings: TWideStringList);
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function GetText: PwideChar;
    function Find(const S: WideString; out Index: Integer): Boolean; //virtual;
    function IndexOf(const S: WideString): Integer;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer; //virtual;
    procedure Insert(Index: Integer; const S: WideString);
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject);

    procedure Assign(Source: TWideStringList); //override;
    procedure LoadFromFile(const FileName: WideString);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure SaveToFile(const FileName: WideString);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToStream_BOM(Stream: TStream; WithBOM: Boolean);
    procedure SetText(Text: PwideChar);


    procedure Sort; //virtual;
    procedure CustomSort(Compare: TWideStringListSortCompare); //virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: WideChar read GetDelimiter write SetDelimiter;
    property DelimitedText: WideString read GetDelimitedText write SetDelimitedText;
    property LineBreak: WideString read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: WideChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: WideChar read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
//    property OnChange: TNotifyEvent read FOnChange write FOnChange;
//    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  { TThreadWideStringList }

  TThreadWideStringList = class
  private
    FList: TWideStringList;
    FLock: TRTLCriticalSection;
  public
    constructor Create(ASorted: Boolean = False);
    destructor Destroy; override;
    function Add(const S: WideString): Integer;
    procedure Clear;
    function  LockList: TWideStringList;
    procedure UnlockList;
  end;

 {%Region WideStringArray}

const
  F_arrW_SORTED   = $00000001;
  F_arrW_CASESENS = $00000002;

procedure arrW_LoadFromStrs(var arr: TWideStringArray; strs: TWideStringList);
procedure arrW_SaveToStrs(var arr: TWideStringArray; strs: TWideStringList);

procedure arrW_SetCommaText(var arr: TWideStringArray;
    const ACommaText: WideString; F_arrW: Cardinal);
function arrW_GetCommaText(var arr: TWideStringArray; F_arrW: Cardinal): WideString;

function arrW_Find(var arr: TWideStringArray; var Index: Integer;
    S: WideString; CaseSens: Boolean): Boolean;

function arrW_IndexOf(var arr: TWideStringArray;
    const S: WideString; F_arrW: Cardinal): Integer;

{%EndRegion}

function file_ExtractNameW(const FileName: WideString): WideString;

procedure WideStringsToUTF8(strsWide: TWideStrings; strsUTF8: TStrings; AIsClearList: Boolean = True);
procedure WideStringsAddToUTF8(strsWide: TWideStrings; strsUTF8: TStrings);

implementation

function AutoDetectCharacterSet(Stream: TStream): TStreamCharSet;
var
  ByteOrderMark: WideChar;
  BytesRead: Integer;
  Utf8Test: array[0..2] of AnsiChar;
begin
  // Byte Order Mark
  ByteOrderMark := #0;
  if (Stream.Size - Stream.Position) >= SizeOf(ByteOrderMark) then begin
    BytesRead := Stream.Read(ByteOrderMark, SizeOf(ByteOrderMark));
    if (ByteOrderMark <> UNICODE_BOM) and (ByteOrderMark <> UNICODE_BOM_SWAPPED) then begin
      ByteOrderMark := #0;
      Stream.Seek(-BytesRead, soFromCurrent);
      if (Stream.Size - Stream.Position) >= Length(Utf8Test) * SizeOf(AnsiChar) then begin
        BytesRead := Stream.Read({%H-}Utf8Test[0], Length(Utf8Test) * SizeOf(AnsiChar));
        if Utf8Test <> UTF8_BOM then
          Stream.Seek(-BytesRead, soFromCurrent);
      end;
    end;
  end;
  // Test Byte Order Mark
  if ByteOrderMark = UNICODE_BOM then
    Result := csUnicode
  else if ByteOrderMark = UNICODE_BOM_SWAPPED then
    Result := csUnicodeSwapped
  else if Utf8Test = UTF8_BOM then
    Result := csUtf8
  else
    Result := csAnsi;
end;

type
  CharNextWFunc = function (lpsz: PWideChar): PWideChar; stdcall;

var
  CharNextW : CharNextWFunc;

function CharNextW95(lpsz: PWideChar): PWideChar; stdcall;
begin
  Result := lpsz + 1;
end;

procedure InitCharNextWFunc;
begin
  //ToDo if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  {$IFDEF WINDOWS}
    CharNextW := @Windows.CharNextW
  {$ELSE}//else
    CharNextW := @CharNextW95;
  {$ENDIF}
end;

{ TWideStringList }

constructor TWideStringList.Create;
begin
  InitCharNextWFunc;
end;

destructor TWideStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TWideStringList.Add(const S: WideString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TWideStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Exit;//ToDo Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Exit; //ToDo Error(@SListIndexError, Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TWideStringItem));
end;

procedure TWideStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Exit; //ToDo Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Exit; //ToDo Error(@SListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PWideStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

function TWideStringList.Find(const S: WideString; out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then Exit; //ToDo Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  Result := nil;
  if (Index < 0) or (Index >= FCount) then Exit; //ToDo Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    begin
      //Result := inherited IndexOf(S);
      for Result := 0 to GetCount - 1 do
        if CompareStrings(Get(Result), S) = 0 then Exit;
      Result := -1;
    end
  else
    if not Find(S, Result) then Result := -1;
end;

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  InsertObject(Index, S, nil);
end;

procedure TWideStringList.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
  if Sorted then Exit; //ToDO Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Exit; //ToDO Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString; AObject: TObject);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TWideStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
end;

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Exit; //ToDO Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Exit; //ToDO Error(@SListIndexError, Index);
  FList^[Index].FString := S;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Exit; //ToDO Error(@SListIndexError, Index);
  FList^[Index].FObject := AObject;
end;

procedure TWideStringList.QuickSort(L, R: Integer; SCompare: TWideStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TWideStringItem));
  FCapacity := NewCapacity;
end;

procedure TWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

function StringListCompareStrings(List: TWideStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TWideStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TWideStringList.CustomSort(Compare: TWideStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    QuickSort(0, FCount - 1, Compare);
  end;
end;

function TWideStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  if CaseSensitive then
    Result := WideCompareStr(S1, S2)
  else
    Result := WideCompareText(S1, S2);
end;

procedure TWideStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;


function TWideStringList.GetCommaText: WideString;
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: WideChar;
  LOldQuoteChar: WideChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TWideStringList.GetDelimitedText: WideString;

  function IsDelimiter(const ch: WideChar): boolean;
  begin
    Result := true;
    if not StrictDelimiter and (ch <= ' ') then exit
    else if ch = QuoteChar then exit
    else if ch = Delimiter then exit
    else if ch = #$00 then exit;
    Result := False;
  end;

var
  S: WideString;
  P: PwideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := WideString(QuoteChar) + WideString(QuoteChar)
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PwideChar(S);
      while not IsDelimiter(P^) do
        P := CharNextW(P); // for UTF16
      if (P^ <> #0) then S := WideQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TWideStringList.GetDelimiter: WideChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TWideStringList.GetLineBreak: WideString;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

function TWideStringList.GetName(Index: Integer): WideString;
begin
  Result := ExtractName(Get(Index));
end;

function TWideStringList.GetNameValueSeparator: WideChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

function TWideStringList.GetQuoteChar: WideChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

function TWideStringList.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

function TWideStringList.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TWideStringList.GetValueFromIndex(Index: Integer): WideString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TWideStringList.SetCommaText(const Value: WideString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TWideStringList.SetDelimitedText(const Value: WideString);
var
  P, P1: PwideChar;
  S: WideString;
begin
  try
    Clear;
    P := PwideChar(Value);
    if not StrictDelimiter then
      while WideCharInSet(P^, [#1..' ']) do
        P := CharNextW(P);
    while P^ <> #0 do
    begin
      if P^ = WideChar(QuoteChar) then
        S := WideExtractQuotedStr(P, WideChar(QuoteChar))
      else
      begin
        P1 := P;
        while ((not FStrictDelimiter and (P^ > ' ')) or
              (FStrictDelimiter and (P^ <> #0))) and (P^ <> WideChar(Delimiter)) do
          P := CharNextW(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not FStrictDelimiter then
        while WideCharInSet(P^, [#1..' ']) do
          P := CharNextW(P);

      if P^ = WideChar(Delimiter) then
      begin
        P1 := P;
        if CharNextW(P1)^ = #0 then
          Add('');
        repeat
          P := CharNextW(P);
        until not (not FStrictDelimiter and WideCharInSet(P^, [#1..' ']));
      end;
    end;
  finally
    //EndUpdate;
  end;
end;

procedure TWideStringList.SetDelimiter(const Value: WideChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TWideStringList.SetLineBreak(const Value: WideString);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

procedure TWideStringList.SetNameValueSeparator(const Value: WideChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

procedure TWideStringList.SetQuoteChar(const Value: WideChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

procedure TWideStringList.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

procedure TWideStringList.SetValue(const Name, Value: WideString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TWideStringList.SetValueFromIndex(Index: Integer;
  const Value: WideString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

procedure TWideStringList.AddStrings(Strings: TWideStringList);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings[I], Strings.Objects[I]);
end;

function TWideStringList.GetText: PwideChar;
begin
  Result := WStrNew(PwideChar(GetTextStr));
end;

function TWideStringList.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TWideStringList.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TWideStringList.Assign(Source: TWideStringList);
begin
  if Source is TWideStringList then
  begin
    //BeginUpdate;
    //try
      Clear;
      FDefined := TWideStringList(Source).FDefined;
      //{$IFDEF COMPILER_7_UP}
      FNameValueSeparator := TWideStringList(Source).FNameValueSeparator;
      //{$ENDIF}
      FQuoteChar := TWideStringList(Source).FQuoteChar;
      FDelimiter := TWideStringList(Source).FDelimiter;
      AddStrings(TWideStringList(Source));
    //finally
    //  EndUpdate;
    //end;
  end;
end;

procedure TWideStringList.LoadFromFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(UTF8Encode(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStringList.LoadFromStream(Stream: TStream);
begin
  LoadFromStream_BOM(Stream, True);
end;

procedure TWideStringList.LoadFromStream_BOM(Stream: TStream; WithBOM: Boolean);
var
  DataLeft: Integer;
  StreamCharSet: TStreamCharSet;
  SW: WideString;
  SA: AnsiString;
begin
//  BeginUpdate;
  try
    if WithBOM then
      StreamCharSet := AutoDetectCharacterSet(Stream)
    else
      StreamCharSet := csUnicode;
    DataLeft := Stream.Size - Stream.Position;
    if (StreamCharSet in [csUnicode, csUnicodeSwapped]) then
    begin
      // BOM indicates Unicode text stream
      if DataLeft < SizeOf(WideChar) then
        SW := ''
      else begin
        SetLength(SW, DataLeft div SizeOf(WideChar));
        Stream.Read(PWideChar(SW)^, DataLeft);
        if StreamCharSet = csUnicodeSwapped then
          WStrSwapByteOrder(PWideChar(SW));
      end;
      SetTextStr(SW);
    end
    else if StreamCharSet = csUtf8 then
    begin
      // BOM indicates UTF-8 text stream
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      SetTextStr(UTF8Decode(SA));
    end
    else
    begin
      // without byte order mark it is assumed that we are loading ANSI text
      SetLength(SA, DataLeft div SizeOf(AnsiChar));
      Stream.Read(PAnsiChar(SA)^, DataLeft);
      SetTextStr(SA{%H-});
    end;
  finally
//    EndUpdate;
  end;
end;

procedure TWideStringList.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    Delete(CurIndex);
    InsertObject(NewIndex, TempString, TempObject);
  end;
end;

procedure TWideStringList.SaveToFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(UTF8Encode(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStringList.SaveToStream(Stream: TStream);
begin
  SaveToStream_BOM(Stream, True);
end;

procedure TWideStringList.SaveToStream_BOM(Stream: TStream; WithBOM: Boolean);
var
  S: WideString;
  BOM: WideChar;
begin
  if WithBOM then begin
    BOM := UNICODE_BOM;
    Stream.WriteBuffer(BOM, SizeOf(WideChar));
  end;
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S) * SizeOf(WideChar));
end;

procedure TWideStringList.SetText(Text: PwideChar);
begin
  SetTextStr(Text);
end;

(*procedure TWideStringList.Error(Msg: PResStringRec; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TWideStringList.Error(const Msg: WideString; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;
*)
function TWideStringList.GetTextStr: WideString;
var
  I, L, Size, Count: Integer;
  P: PwideChar;
  S, LB: WideString;
begin
  Count := GetCount;
  Size := 0;
  LB := sLineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L*SizeOf(WideChar));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L*SizeOf(WideChar));
      Inc(P, L);
    end;
  end;
end;

procedure TWideStringList.SetTextStr(const Value: WideString);
var
  P, Start: PwideChar;
  S: WideString;
begin
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not WideCharInSet(P^, [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
  end;
end;


function TWideStringList.ExtractName(const S: WideString): WideString;
var
  P: Integer;
begin
  Result := S;
  P := Pos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

{ TThreadWideStringList }

constructor TThreadWideStringList.Create(ASorted: Boolean);
begin
  inherited Create;
  InitCriticalSection(FLock);
  FList := TWideStringList.Create;
  FList.Sorted := ASorted;
end;

destructor TThreadWideStringList.Destroy;
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

function TThreadWideStringList.Add(const S: WideString): Integer;
begin
  Locklist;
  try
    Result := FList.Add(S);
  finally
    UnLockList;
  end;
end;

procedure TThreadWideStringList.Clear;
begin
 Locklist;
 try
   FList.Clear;
 finally
   UnLockList;
 end;
end;

function TThreadWideStringList.LockList: TWideStringList;
begin
  Result:=FList;
  System.EnterCriticalSection(FLock);
end;

procedure TThreadWideStringList.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{WideStringArray}

procedure arrW_LoadFromStrs(var arr: TWideStringArray; strs: TWideStringList);
var
  I: Integer;
begin
  I := strs.Count;
  SetLength(arr, I);
  for I := 0 to I - 1 do
    arr[I] := strs[I];
end;

procedure arrW_SaveToStrs(var arr: TWideStringArray; strs: TWideStringList);
var
  I: Integer;
begin
  strs.Clear;
  I := Length(arr);
  strs.Capacity := I;
  for I := 0 to I - 1 do
    strs.Add(arr[I]);
end;

procedure arrW_SetCommaText(var arr: TWideStringArray;
    const ACommaText: WideString; F_arrW: Cardinal);
var
  strs: TWideStringList;
begin
  strs := TWideStringList.Create;
  try
    strs.CaseSensitive := (F_arrW and F_arrW_CASESENS) <> 0;
    strs.CommaText := ACommaText;
    if (F_arrW and F_arrW_SORTED) <> 0 then
      strs.Sort;
    arrW_LoadFromStrs(arr, strs);
  finally
    strs.Free;
  end;
end;

function arrW_GetCommaText(var arr: TWideStringArray; F_arrW: Cardinal): WideString;
var
  strs: TWideStringList;
begin
  strs := TWideStringList.Create;
  try
    strs.CaseSensitive := (F_arrW and F_arrW_CASESENS) <> 0;
    arrW_SaveToStrs(arr, strs);
    Result := strs.CommaText;
  finally
    strs.Free;
  end;
end;

function arrW_Find(var arr: TWideStringArray; var Index: Integer;
    S: WideString; CaseSens: Boolean): Boolean;
var
  L, H, I, C: Integer;
  S1: WideString;
begin
  Result := False;
  if not CaseSens then
    S := WideUpperCase(S);
  L := 0;
  H := Length(arr) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if CaseSens then S1 := arr[I]
    else S1 := WideUpperCase(arr[I]);
    C := WideCompareStr(S1, S);
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

function arrW_IndexOf(var arr: TWideStringArray;
    const S: WideString; F_arrW: Cardinal): Integer;
//var
//  I: Integer;
//  S1, S2: WideString;
begin
  if (F_arrW and F_arrW_SORTED) = 0 then
    begin
      Result := -1;
      //for I := 0 to Length(arr) do
      //  begin

      //  end;
    end
  else
    if not arrW_Find(arr, Result, S, (F_arrW and F_arrW_CASESENS) <> 0) then
      Result := -1;
end;

function file_ExtractNameW(const FileName: WideString): WideString;
begin
  Result := UTF8Decode(ExtractFileName(UTF8Encode(FileName)));
end;

procedure WideStringsToUTF8(strsWide: TWideStrings; strsUTF8: TStrings;
  AIsClearList: Boolean);
begin
  if AIsClearList then
    strsUTF8.Clear;
  WideStringsAddToUTF8(strsWide, strsUTF8);
end;

procedure WideStringsAddToUTF8(strsWide: TWideStrings; strsUTF8: TStrings);
var
  I: Integer;
begin
  for i := 0 to strsWide.Count - 1 do
    begin
      strsUTF8.AddObject(UTF8Encode(strsWide[I]), strsWide.Objects[I]);
    end;
end;

{WideString}

function WideCharInSet(W: WideChar; sets: CharSet): boolean;
begin
  if W <= #$FF then
    Result := Char(W) in sets
  else
    Result := False;
end;

function WideCharInArray(W: WideChar; sets: array of WideChar): boolean;
var
  ind : integer;
begin
  Result := true;
  for ind := 0 to High(sets) do
  begin
    if W = sets[ind] then exit;
  end;
  Result := False;
end;

function WStrAlloc(Size: Cardinal): PWideChar;
begin
  Size := Size * Sizeof(WideChar);
  Inc(Size, SizeOf(Cardinal));
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(PChar(Result), SizeOf(Cardinal));
end;

function WStrBufSize(const Str: PWideChar): Cardinal;
var
  P: PWideChar;
begin
  P := Str;
  Dec(P, SizeOf(Cardinal));
  Result := (Cardinal(Pointer(P)^) - SizeOf(Cardinal)) div sizeof(WideChar);
end;

function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar;
begin
  Result := Dest;
  Move(Source^, Dest^, Count * Sizeof(WideChar));
end;


function WStrNew(const Str: PWideChar): PWideChar;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := WStrLen(Str) + 1;
    Result := WStrMove(WStrAlloc(Size), Str, Size);
  end;
end;

procedure WStrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(PChar(Str), SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

function WStrLen(const Str: PWideChar): Cardinal;
var
  P : PWideChar;
begin
  P := Str;
  while (P^ <> #0) do Inc(P);
  Result := (P - Str);
end;

function WStrEnd(const Str: PWideChar): PWideChar;
begin
  Result := Str;
  while (Result^ <> #0) do Inc(Result);
end;

function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  WStrCopy(WStrEnd(Dest), Source);
  Result := Dest;
end;

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
  Dest^ := #$00;
end;

function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) and (MaxLen > 0) do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
    Dec(MaxLen);
  end;
  Dest^ := #$00;
end;

function WideStr_LCopy(Source: PWideChar; MaxLen: Cardinal): WideString;
begin
  SetLength(Result, MaxLen);
  WStrLCopy(PWideChar(Result), Source,  MaxLen);
  Result := PWideChar(Result);
end;

function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), Length(Source));
end;

function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;

function WStrScan(Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

//function WStrComp(Str1, Str2: PWideChar): Integer;
//begin
//  Result := WStrLComp(Str1, Str2, MaxInt);
//end;

function WStrPos(Str, SubStr: PWideChar): PWideChar;
var
  PSave: PWideChar;
  P: PWideChar;
  PSub: PWideChar;
begin
  // returns a pointer to the first occurance of SubStr in Str
  Result := nil;
  if (Str <> nil) and (Str^ <> #0) and (SubStr <> nil) and (SubStr^ <> #0) then begin
    P := Str;
    While P^ <> #0 do begin
      if P^ = SubStr^ then begin
        // investigate possibility here
        PSave := P;
        PSub := SubStr;
        While (P^ = PSub^) do begin
          Inc(P);
          Inc(PSub);
          if (PSub^ = #0) then begin
            Result := PSave;
            exit; // found a match
          end;
          if (P^ = #0) then
            exit; // no match, hit end of string
        end;
        P := PSave;
      end;
      Inc(P);
    end;
  end;
end;

function WideStrLPos(Str, SubStr: PWideChar; StrLen: Cardinal): PWideChar;
var
  ch: WideChar;
begin
  ch := (Str + StrLen)^;
  (Str + StrLen)^ := #0;
  try
    Result := WStrPos(Str, SubStr);
  finally
    (Str + StrLen)^ := ch;
  end;
end;

function WideCharRPos(ch: WideChar; Str: WideString): Integer;
var
  I: Integer;
//  ch: Char;
begin
  Result := 0;
//  if Length(SubStr) = 0 then Exit;
//  ch := SubStr[1];
  for I := Length(Str) downto 1 do
    if Str[I] = ch then
      begin
        Result := I;
        Exit;
      end;
end;

function WStrECopy(Dest, Source: PWideChar): PWideChar;
begin
  Result := WStrEnd(WStrCopy(Dest, Source));
end;

function WStrComp(Str1, Str2: PWideChar): Integer;
{$IFDEF FPC3}
begin
  Result := strcomp(Str1, Str2);
end;
{$ELSE}
{$IFDEF CPUi386}
asm
         PUSH EDI
         PUSH ESI
         MOV EDI, EDX
         MOV ESI, EAX
         MOV ECX, 0FFFFFFFFH
         XOR EAX, EAX
         REPNE SCASW
         NOT ECX
         MOV EDI, EDX
         XOR EDX, EDX
         REPE CMPSW
         MOV AX, [ESI - 2]
         MOV DX, [EDI - 2]
         SUB EAX, EDX
         POP ESI
         POP EDI
end;
{$ELSE}
begin
  Result := 0;
  RaiseToDoException();
end;
{$ENDIF}
{$ENDIF}

function WStrIComp(Str1, Str2: PWideChar): Integer;
{$IFDEF FPC3}
begin
  Result := stricomp(Str1, Str2);
end;
{$ELSE}
{$IFDEF CPUi386}
asm
         PUSH EDI
         PUSH ESI
         MOV EDI, EDX
         MOV ESI, EAX
         MOV ECX, 0FFFFFFFFH
         XOR EAX, EAX
         REPNE SCASW
         NOT ECX
         MOV EDI, EDX
         XOR EDX, EDX
@@1:     REPE CMPSW
         JE @@4
         MOV AX, [ESI - 2]
         CMP AX, 'a'
         JB @@2
         CMP AX, 'z'
         JA @@2
         SUB AL, 20H
@@2:     MOV DX, [EDI - 2]
         CMP DX, 'a'
         JB @@3
         CMP DX, 'z'
         JA @@3
         SUB DX, 20H
@@3:     SUB EAX, EDX
         JE @@1
@@4:     POP ESI
         POP EDI
end;
{$ELSE}
begin
  Result := 0;
  RaiseToDoException();
end;
{$ENDIF}
{$ENDIF}

function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
{$IFDEF FPC3}
begin
  Result := strlcomp(Str1, Str2, MaxLen);
end;
{$ELSE}
{$IFDEF CPUi386}
asm
         PUSH EDI
         PUSH ESI
         PUSH EBX
         MOV EDI, EDX
         MOV ESI, EAX
         MOV EBX, ECX
         XOR EAX, EAX
         OR ECX, ECX
         JE @@1
         REPNE SCASW
         SUB EBX, ECX
         MOV ECX, EBX
         MOV EDI, EDX
         XOR EDX, EDX
         REPE CMPSW
         MOV AX, [ESI - 2]
         MOV DX, [EDI - 2]
         SUB EAX, EDX
@@1:     POP EBX
         POP ESI
         POP EDI
end;
{$ELSE}
begin
  Result := 0;
  RaiseToDoException();
end;
{$ENDIF}
{$ENDIF}

function WStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
{$IFDEF FPC3}
begin
  Result := strlicomp(Str1, Str2, MaxLen);
end;
{$ELSE}
{$IFDEF CPUi386}
asm
         PUSH EDI
         PUSH ESI
         PUSH EBX
         MOV EDI, EDX
         MOV ESI, EAX
         MOV EBX, ECX
         XOR EAX, EAX
         OR ECX, ECX
         JE @@4
         REPNE SCASW
         SUB EBX, ECX
         MOV ECX, EBX
         MOV EDI, EDX
         XOR EDX, EDX
@@1:     REPE CMPSW
         JE @@4
         MOV AX, [ESI - 2]
         CMP AX, 'a'
         JB @@2
         CMP AX, 'z'
         JA @@2
         SUB AX, 20H
@@2:     MOV DX, [EDI - 2]
         CMP DX, 'a'
         JB @@3
         CMP DX, 'z'
         JA @@3
         SUB DX, 20H
@@3:     SUB EAX, EDX
         JE @@1
@@4:     POP EBX
         POP ESI
         POP EDI
end;
{$ELSE}
begin
  Result := 0;
  RaiseToDoException();
end;
{$ENDIF}
{$ENDIF}

function WStrLower(Str: PWideChar): PWideChar;
begin
  {$IFDEF VER3}
  Result := strlower(Str);{$ELSE}
  Result := nil;
  RaiseToDoException();
  {$ENDIF}
end;

function WStrUpper(Str: PWideChar): PWideChar;
begin
 {$IFDEF VER3}
 Result := strupper(Str);{$ELSE}
 Result := nil;
 RaiseToDoException();
 {$ENDIF}
end;

function WStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
var
  MostRecentFound: PWideChar;
begin
  if Chr = #0 then
    Result := WStrEnd(Str)
  else
  begin
    Result := nil;
    MostRecentFound := Str;
    while True do
    begin
      while MostRecentFound^ <> Chr do
      begin
        if MostRecentFound^ = #0 then
          Exit;
        Inc(MostRecentFound);
      end;
      Result := MostRecentFound;
      Inc(MostRecentFound);
    end;
  end;
end;

function WStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
begin
  Result := Dest;
  WStrLCopy(WStrEnd(Dest), Source, MaxLen - WStrLen(Dest));
end;

function WStrPas(const Str: PWideChar): WideString;
begin
  Result := Str;
end;

procedure WStrSwapByteOrder(Str: PWideChar);
var
  P: PWord;
begin
  P := PWord(Str);
  While (P^ <> 0) do begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
end;

function WStrPosOccur(Substr, Str: PWideChar; Occur: Integer): PWideChar;
var
  I: Integer;
begin
  Result := nil;
  for I := 1 to Occur do
    begin
      Str := WStrPos(Str, SubStr);
      if Str = nil then
        begin
          Result := nil;
          Exit;
        end
      else if I = Occur then begin
          Result := Str;
          Exit;
        end
      else
        Inc(Str);// := Str + 1;
    end;//for
end;

function WStrPosCount(SubStr, Str: PWideChar): Integer;
var
  Len: Integer;
begin
  Result := 0;
  Len := WStrLen(SubStr);
  Str := WStrPos(Str, SubStr);
  while Str <> nil do
    begin
      Inc(Result);
      Inc(Str, Len);
      Str := WStrPos(Str, SubStr);
    end;
end;

function WStrPosToEnd(Str, SubStr: PWideChar; PEnd: PWideChar = nil): PWideChar;
begin
  Result := WStrPos(Str, SubStr);
  if (PEnd <> nil) and (Result <> nil) and (Result > PEnd) then
    Result := nil;
end;

function WideUpCase(ch: WideChar): WideChar;
begin
  Result := WideUpperCase(ch)[1];
end;

function WideLoCase(ch: WideChar): WideChar;
begin
  Result := WideLowerCase(ch)[1];
end;

function WideCharIsWordBreak(AChar: WideChar): Boolean;
begin
  if AChar <= #32 then
    Result := True
  else
    Result := Pos(AChar, '.,;:"`°^!?&,$@§%#~[](){´}<>-=+*/\|''''') > 0;
end;

function WideLastChar(const S: WideString): PWideChar;
begin
  if S = '' then
    Result := nil
  else
    Result := @S[Length(S)];
end;

function WideLastDelimiter(const Delimiters, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (WStrScan(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;


function WidePosText(const SubStr, S: WideString): Integer;
begin
  Result := Pos(WideUpperCase(SubStr), WideUpperCase(S));
end;

function WidePosOccur(const Substr, Str: WideString; Occur: Integer): Integer;
var
  pc: PWideChar;
begin
  Result := 0;
  pc := WStrPosOccur(PWideChar(SubStr), PWideChar(Str), Occur);
  if pc <> nil then
    Result := pc - PWideChar(Str) + 1;
end;

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src, Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := WStrScan(PWideChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := WStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := WStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src)*2);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := WStrScan(Src, Quote);
  until P = nil;
  P := WStrEnd(Src);
  Move(Src^, Dest^, (P - Src)*2);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function WideExtractQuoted(const S, Quot: WideString): WideString;
var
  Len: Integer;
begin
  Result := S;
  Len := Length(Quot);
  if WideStartsStr(Quot, Result) then
    System.Delete(Result, 1, Len);
  if WideEndsStr(Quot, Result) then
    SetLength(Result, Length(Result) - Len);
end;

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := WStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := WStrScan(Src, Quote);
  end;
  if Src = nil then Src := WStrEnd(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := WStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, (Src - P) * SizeOf(WideChar));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := WStrScan(Src, Quote);
    end;
    if Src = nil then Src := WStrEnd(P);
    Move(P^, Dest^, (Src - P - 1) * SizeOf(WideChar));
  end;
end;


function WideDequotedStr(const S: WideString; AQuote: WideChar): WideString;
var
  LText: PWideChar;
begin
  LText := PWideChar(S);
  Result := WideExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function WideRemoveEmptyChars(const S: WideString): WideString;
var
  I, N: Integer;
begin
  SetLength(Result, Length(S));
  I := 0;
  for N := 1 to Length(S) do
    if (S[N] > #32) then
      begin
        Inc(I);
        Result[I] := S[N];
      end;
  SetLength(Result, I);
end;

function WideAdjustLineBreaks(const S: WideString; Style: TTextLineBreakStyle = tlbsCRLF ): WideString;
var
  Source, SourceEnd, Dest: PWideChar;
  DestLen: Integer;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);
  DestLen := Length(S);
  while Source < SourceEnd do
  begin
    case Source^ of
      #10:
        if Style = tlbsCRLF then
          Inc(DestLen);
      #13:
        if Style = tlbsCRLF then
          if Source[1] = #10 then
            Inc(Source)
          else
            Inc(DestLen)
        else
          if Source[1] = #10 then
            Dec(DestLen);
    end;
    Inc(Source);
  end;
  if DestLen = Length(Source) then
    Result := S
  else
  begin
    Source := Pointer(S);
    SetString(Result, nil, DestLen);
    Dest := Pointer(Result);
    while Source < SourceEnd do
      case Source^ of
        #10:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
          end;
        #13:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := #13;
              Inc(Dest);
            end;
            Dest^ := #10;
            Inc(Dest);
            Inc(Source);
            if Source^ = #10 then Inc(Source);
          end;
      else
        Dest^ := Source^;
        Inc(Dest);
        Inc(Source);
      end;
  end;
end;

function WideIndexOfChars(const S, Chars : WideString) : Integer;
var
  I, J : Integer;
begin
  Result := -1;
  for I := 1 to Length( Chars ) do
  begin
    J := pos( Chars[ I ], S );
    if J > 0 then
    begin
      if (Result < 0) or (J < Result) then
         Result := J;
    end;
  end;
end;

function WideParse(var S : WideString; const Separators : WideString ): WideString;
var
  Pos : Integer;
begin
  Pos := WideIndexOfChars( S, Separators );
  if Pos <= 0 then
     Pos := Length(S) + 1;
  Result := Copy(S, 1, Pos - 1);
  System.Delete(S, 1, Pos);
end;

function WideDequotedChars(const S, AQuoteChars: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(AQuoteChars) do
    Result := WideDequotedStr(Result, AQuoteChars[I]);
end;

function WideDequotedSimple(const S: WideString; AQuote: WideChar): WideString;
begin
  Result := S;
  if (Result <> '') and (Result[1] = AQuote) then
    System.Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = AQuote) then
    System.Delete(Result, Length(Result), 1);
end;

function WideStartsText(const ASubText, AText: WideString): Boolean;
var
  //P: PWideChar;
  L, L2: Integer;
begin
  //P := PWideChar(AText);
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
    Result := WideCompareText(ASubText, Copy(AText, 1, L)) = 0;
end;

function WideStartsStr(const ASubText, AText: WideString): Boolean;
var
  //P: PWideChar;
  L, L2: Integer;
begin
  //P := PWideChar(AText);
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
    Result := WideCompareStr(ASubText, Copy(AText, 1, L)) = 0;
end;

function WideEndsText(const ASubText, AText: WideString): Boolean;
var
  SubTextLocation: Integer;
  L: Integer;
begin
  L := Length(ASubText);
  SubTextLocation := Length(AText) - L + 1;
  if (SubTextLocation > 0) and (ASubText <> '') then
    Result := WideCompareText(ASubText, Copy(AText, SubTextLocation, MaxInt)) = 0
  else
    Result := False;
end;

function WideEndsStr(const ASubText, AText: WideString): Boolean;
var
  SubTextLocation: Integer;
  L: Integer;
begin
  L := Length(ASubText);
  SubTextLocation := Length(AText) - L + 1;
  if (SubTextLocation > 0) and (ASubText <> '') then
    Result := WideCompareStr(ASubText, Copy(AText, SubTextLocation, MaxInt)) = 0
  else
    Result := False;
end;

procedure UpperHTMLTagsW(var S: WideString);
var
  I: Integer;
  IsDoUpper: Boolean;
  ch: WideChar;
begin
  IsDoUpper := False;
  for I := 1 to Length(S) do
    begin
      ch := S[I];
      case ch of
        '<': IsDoUpper := (I < Length(S)) and (Pos(S[I + 1], '!?') = 0);
        '>', #0..#32: IsDoUpper := False;
        else
          if IsDoUpper then
            begin
              S[I] := WideUpCase(ch);
            end;
      end;//case
    end;//for
end;

procedure LowerHTMLTagsW(var S: WideString);
var
  I: Integer;
  IsDoLower: Boolean;
  ch: WideChar;
begin
  IsDoLower := False;
  for I := 1 to Length(S) do
    begin
      ch := S[I];
      case ch of
        '<': IsDoLower := (I < Length(S)) and (Pos(S[I + 1], '!?') = 0);
        '>', #0..#32: IsDoLower := False;
        else
          if IsDoLower then
            begin
              S[I] := WideLoCase(ch);
            end;
      end;//case
    end;//for
end;

function WideOfRandomChar(Len: Integer; IsUpperCase: Boolean = False): WideString;
const
  S: WideString = 'qwertyuiopasdfghjklzxcvbnm_0987654321';  // 37;
var
  I, N: Byte;
begin
  SetLength(Result, Len);
  Randomize;
  for I := 1 to Len do
    begin
      N := Integer(Random(36)) + 1;
      if IsUpperCase then
        Result[I] := WideUpCase(S[N])
      else
        Result[I] := S[N];
    end;
end;

procedure WideAddLine(var S: WideString; const Line: WideString; ALineBreak: WideString = sLineBreak);
var
  S1: WideString;
  //Len: Integer;
begin
  //Len := Length(S);
  if Length(S) = 0 then
    S := Line
  else if Length(Line) > 0 then
    //Result := S
  ///else
    begin
      if WideEndsText(ALineBreak, S) then S1 := '' //S[Len] in [#10, #13] then S1 := ''
      else S1 := ALineBreak;
      S := S + S1 + Line;
    end;
end;

function WideRPos(const SubStr, Str: WideString): Integer;
var
  I, N, Len: Integer;
  ch: WideChar;
begin
  Result := 0;
  Len := Length(SubStr);
  if Len = 0 then Exit;
  N := Len;
  ch := SubStr[N];
  for I := Length(Str) downto 1 do
    begin
      if Str[I] = ch then
        begin
          if N = 1 then
            begin
              Result := I;
              Exit;
            end
          else
            begin
              Dec(N);
              ch := SubStr[N];
            end;
        end
      else if N < Len then
        begin
          N := Len;
          ch := SubStr[N];
        end;
    end;
end;

function WideTrimChars(const S: WideString; chStart, chEnd: WideChar): WideString;
var
  Len: Integer;
begin
  Result := S;
  if Result = '' then Exit;
  if Result[1] = chStart then
    System.Delete(Result, 1, 1);
  if (Result <> '') then
    begin
      Len := Length(Result);
      if (Result[Len] = chEnd) then
        SetLength(Result, Len - 1);
    end;
end;

function WideTrimChar(const S: WideString; ch: WideChar): WideString;
begin
  Result := WideTrimChars(S, ch, ch);
end;

function WideReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    Patt := wideUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function WideFindCharsInText(var FindAt: Integer; Len: Integer; PC: PWideChar;
            const FindChars, BreakChars: WideString; Back: Boolean): Integer;
//Result: -1: Not found
//        0: BreakChars found
var
  Step, Stop: Integer;
  ch: WideChar;
  CurrP: PWideChar;
begin
  Result := -1;
  try
    if FindAt <= 0 then
      FindAt := 1;
    CurrP := PC + FindAt - 1;
    if Back then
      begin
        if FindAt = 1  then Exit;
        Step := -1; Stop := 1;
        Dec(FindAt); CurrP := CurrP - 1;
      end
    else
      begin
        if FindAt = Len  then Exit;
        Step := 1; Stop := Len;
      end;

    FindAt := FindAt - Step;
    CurrP := CurrP - Step;
    repeat
      FindAt := FindAt + Step;
      CurrP := CurrP + Step;
      ch := CurrP^;
      if Pos(Ch, BreakChars) > 0 then  begin Result := 0; Exit; end;
      if Pos(Ch, FindChars) > 0 then
        begin
          Result := FindAt;
          Exit;
        end;
    until (FindAt = Stop);
  except end;
end;

function WideFindEnclosedText(var StartPos: Integer; var EndPos: Integer;
                PC: PWideChar; FindAt, Len: Integer;
                chStart, chEnd: WideChar; Back: Boolean): Boolean;
var
  Step, Stop: Integer;
  chFirst, chLast, chFind: WideChar;
  FirstFound: Boolean;
  CurrP: PWideChar;
begin
  Result := False;
  StartPos := FindAt; EndPos := FindAt;
  FirstFound := False;
  CurrP := PC + FindAt - 1;
  if Back then
    begin
      if FindAt = 1  then Exit;
      chFirst := chEnd; chLast := chStart;
      Step := -1; Stop := 1;
      Dec(FindAt); CurrP := CurrP - 1;
    end
  else
    begin
      if FindAt = Len  then Exit;
      chFirst := chStart; chLast := chEnd;
      Step := 1; Stop := Len;
    end;

  chFind := chFirst;
  FindAt := FindAt - Step;
  CurrP := CurrP - Step;
  repeat
    FindAt := FindAt + Step;
    CurrP := CurrP + Step;
    if FirstFound and (CurrP^ = chFirst) then begin
      if Back then EndPos := FindAt + 1 else StartPos := FindAt;
      Continue;
    end;
    if (CurrP^ = chFind) then
      begin
        if FirstFound then
          begin
            if Back then StartPos := FindAt else EndPos := FindAt + 1;
            Result := True; //StrLCopy(strs.Text, StartPos, EndPos - StartPos);
            Exit;
          end
        else
          begin
            FirstFound := True;
            chFind := chLast;
            if Back then EndPos := FindAt + 1 else StartPos := FindAt;
          end;
      end;
  until (FindAt = Stop);
end;

function WideFindEnclosedTextExt(var StartPos: Integer; var EndPos: Integer;
                PC: PWideChar; FindAt, Len: Integer;
                chStart, chEnd: WideChar; Back: Boolean; Text: WideString): Boolean;
var
  I: Integer;
  S: WideString;
begin
  Result := False;
  S := '';  Text := WideUpperCase(Text);
  while WideFindEnclosedText(StartPos, EndPos, PC, FindAt, Len, chStart, chEnd, Back) do
    begin
      FindAt := EndPos{ + 1};
      for I := StartPos to EndPos - 3 do
        S := S + (PC + I)^;
      if WideUpperCase(S) = Text then begin
        Result := True;
        Exit;
      end;
      S := '';
    end;
end;

//var
//  _DelimPosW: Integer;
//  _DelimPCW, _DelimSW: PWideChar;
//  _QuoteW: WideString;
//  _F_CET_DELIM_STR_W: Cardinal;
//procedure WideGetNextDelimStringInit(S, DelimS: PWideChar;
//    F_CET_DELIM_STR: Cardinal = 0; Quote: WideString = '"');
//begin
//  _DelimPosW := 0;
//  _DelimSW := DelimS;
//  _DelimPCW := S;
//  _QuoteW := Quote;
//  _F_CET_DELIM_STR_W := F_CET_DELIM_STR;
//end;
//
//function WideGetNextDelimString(var S: WideString): Boolean;//ToDo Sync with str_GetNextDelim
//var
//  pc: PWideChar;
//  I: Integer;
//begin
//  Result := False;
//  S := '';
//  if not Assigned(_DelimPCW) then Exit;
//  pc := _DelimPCW;
//  _DelimPCW := WStrPos(_DelimPCW, _DelimSW);
//  if not Assigned(_DelimPCW) then
//    I := WStrLen(pc)
//  else
//    begin
//      I := _DelimPCW - pc;
//      Inc(_DelimPCW);
//    end;
//  if I = 0 then
//    _DelimPCW := nil
//  else
//    begin
//      Result := True;
//      SetLength(S, I);
//      WStrLCopy(PWideChar(S), pc, I);
//    end;
//  if Result then
//    begin
//      if (_F_CET_DELIM_STR_W and F_CET_DELIM_STRING_TRIM) <> 0 then
//        S := Trim(S);
//      if (_F_CET_DELIM_STR_W and F_CET_DELIM_STRING_DEQOUTE) <> 0 then
//        S := WideDequotedChars(S, _QuoteW);
//      if S = '' then
//        Result := WideGetNextDelimString(S);
//    end;
//end;
//
//function WideGetNextDelimStringExt(var S: WideString; F_CET_DELIM_STR: Cardinal): Boolean;
//begin
//  Result := WideGetNextDelimString(S);
//  if Result then
//    begin
//      if (F_CET_DELIM_STR and F_CET_DELIM_STRING_TRIM) <> 0 then
//        S := Trim(S);
//      if (F_CET_DELIM_STR and F_CET_DELIM_STRING_DEQOUTE) <> 0 then
//        S := WideDequotedChars(S, _QuoteW);
//      if S = '' then
//        Result := WideGetNextDelimStringExt(S, F_CET_DELIM_STR);
//    end;
//end;

function WideStrGetNextWord(var pcWord: PWideChar;
    var Len: Integer; IsQuotWord, IsSkipEmpty: Boolean): Boolean;
var
  pc: PWideChar;
  ch, Quot: WideChar;
begin
  Result := False;
  Len := 0;
  if pcWord^ = #0 then Exit;
  Result := True;
  Quot := #1;
  pc := pcWord;
  while pc^ <> #0 do
    begin
      ch := pc^;
      Inc(pc);
      if (Quot <> #1) then
        begin
          Inc(Len);
          if ch = Quot then
            begin
              Break;
            end;
          Continue;
        end;
      if WideCharIsWordBreak(ch) then
        begin
          if IsQuotWord and ((ch = '"') or (ch = '''')) then
            begin
              Quot := ch;
              //Continue;
            end
          else
            begin
              if (Len = 0) and IsSkipEmpty and (ch <= #32) then
                begin
                  Inc(pcWord, 1);
                  Len := 0;
                  Continue;
                end;
              Break;
            end;
        end;//if WideCharIsWordBreak(ch)
      Inc(Len);
    end;
  //Result := True;
  //Result := (Len > 0) or (pc^ <> #0);
end;

function WideGetWordAtPosEx(const S: WideString; var PosS: Integer; out
  PosE: Integer; IsQuotWord: Boolean): WideString;
var
  I, SelS, iStart, iEnd: Integer;
  ch: WideChar;
  pcWord: PWideChar;
begin
  Result := '';
  PosE := PosS;
  SelS := PosS;
  if S = '' then Exit;
  iStart := PosS;
  iEnd := Length(S) + 1;
  if (SelS > iEnd) or (SelS < 1) then Exit;

  I := iStart - 1;
  iStart := 1;
  if I > 0 then
    for I := I downto 1 do
      begin
        ch := S[I];
        if WideCharIsWordBreak(ch) then
          begin
            if IsQuotWord then
              if (ch <> #13) and (ch <> #10) then
                Continue;
            iStart := I + 1;
            Break;
          end;
      end;
  pcWord := @S[iStart];
  while WideStrGetNextWord(pcWord, I, IsQuotWord, SelS > iStart) do
    begin
      iStart := pcWord - PWideChar(S) + 1;
      iEnd := iStart + I;
      if (SelS < iStart) then Exit;
      if (SelS <= iEnd) then
        begin
          if I > 0 then
            begin
              SetLength(Result, I);
              Result := WStrLCopy(PWideChar(Result), pcWord, I);
            end;
          PosS := iStart;
          PosE := iEnd;
          Exit;
        end;
      Inc(pcWord, I + 1);
    end;
end;

function WideGetWordAtPos(const S: WideString; Pos: Integer): WideString;
var
  PosE: Integer;
begin
  Result := WideGetWordAtPosEx(S, Pos, PosE, False);
end;

function WideStrGetPrevWord(var pchFind: PWideChar; var Len: Integer;
    pchStart: PWideChar; const BreakChars: WideString = ''): Boolean;

  function IsWordBreak: Boolean;
  begin
    Result := (pchFind^ <= #32) or
      (Pos(pchFind^, BreakChars) > 0);
  end;

begin
//  Result := False;
  Len := 0;
  Dec(pchFind);
  while pchFind >= pchStart do
    begin
      if IsWordBreak then
        begin
          if (Len > 0) then
            Break;
        end
      else
        Inc(Len);
      Dec(pchFind);
    end;
  Result := Len > 0;
  if Result then
    Inc(pchFind);
end;

function WideToUnixSlash(const S: WideString): WideString;
begin
  Result := WideReplace(S, '\', '/', [rfReplaceAll]);
end;

function WideToWinSlash(const S: WideString): WideString;
begin
  Result := WideReplace(S, '/', '\', [rfReplaceAll]);
end;

function WideStrRowColToSelStart(Apc: PWideChar; ARow, ACol: Integer; var SelS: Integer): Boolean;
var
  pc, pc0: PWideChar;
  Row, Col, Len: Integer;
  LineBreak: WideString;   //ARow, ACol: 1-Based
begin                      //SelS 0-Based
  SelS := -1;
  Result := False;
  if Apc = nil then
    Exit
  else
    begin
      Row := 0;  Col := 1;
      pc0 := Apc;
      pc := pc0;
      LineBreak := sLineBreak;
      Len := Length(LineBreak);
      while Assigned(pc) do
        begin
          Inc(Row);
          SelS := pc - pc0;
          if Row = ARow then
            begin
              while Col <> ACol do
                begin
                  pc := pc + 1;
                  Inc(Col);
                  if Pos(pc^, #0#10#13) > 0 then Exit;
                end;
              Result := True;
              Exit;
            end;
          pc := WStrPos(pc, PWideChar(LineBreak)) + Len;
        end;
    end;
end;

function WideStrSelStartToRowCol(Apc: PWideChar; var ARow, ACol: Integer; ASelS: Integer): Boolean;
var
  pc, pc0: PWideChar;
  //Len,
  SelS, SelSLast: Integer;

  function _StrPos: PWideChar;
  var
    ch, ch1: WideChar;
  begin
    Result := nil;
    ch1 := #1;
    while pc^ <> #0 do
    begin
      ch := pc^;
      Inc(pc);
      if (ch = #10) then
        ch1 := #13
      else if (ch = #13) then
        ch1 := #10
      else Continue;
      if pc^ = ch1 then
        Inc(pc);
      Result := pc;
      Exit;
    end;
  end;

begin
  Result := False;
  if ASelS < 1 then Exit;
  ARow := 0; ACol := 0; SelS := 1;
//  Len := Length(sLineBreak);
  pc0 := Apc;//@S[1];
  pc := pc0; //pcLast := pc0;
 try
  while Assigned(pc) do
    begin
      SelSLast := SelS;
      SelS := pc - pc0 + 1;
      if SelS = ASelS then
        begin
          Inc(ARow);
          ACol := 1;
          Result := True;
          Exit;
        end
      else if SelS > ASelS then
        begin
          ACol := ASelS - SelSLast + 1;
          Result := True;
          Exit;
        end;
      Inc(ARow);
      pc := _StrPos; //(pc, sLineBreak);
      //if Assigned(pc) then
      //  Inc(pc, Len);
    end;
 except end;
end;

end.
