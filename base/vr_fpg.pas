unit vr_fpg;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses SysUtils, Classes, vr_types, vr_utils, fgl, vr_intfs, RtlConsts, Math;

type

  { TGThreadObjectList }

  TGThreadObjectList<T{$IF FPC_FULLVERSION >= 030200} : TObject{$ENDIF}> = class(TObject)
  private
  type
    TObjectList_T = TFPGObjectList<T>;
  private
    FList: TObjectList_T;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create(const AFreeObjects: Boolean = True);
    destructor Destroy; override;
    procedure Add(Item: T);
    procedure Clear;
    function IsEmpty: Boolean;
    procedure Remove(Item: T);
    function  LockList: TObjectList_T;
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  { TGItemDataList<T, D> }

  TGItemDataList<T, D> = class(TObject)
  private
  type
    TGItemDataListRec = record
      Item: T;
      Data: D;
    end;
    PDataListRec = ^TGItemDataListRec;
    PData = ^D;
  private
    FList: TFPList;
    FDataSize: Integer;
    function AllocateNewItem(const AItem: T): PDataListRec;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetData(const AIndex: Integer): PData;
    function GetItem(const AIndex: Integer): T;
    procedure SetCapacity(AValue: Integer);
    procedure SetItem(const AIndex: Integer; AValue: T);
  protected
    procedure DoInitData(var AData: D); virtual;
    procedure DoFreeData(var AData: D); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    //Procedure AddList(AList : TList);
    function Add(const AItem: T): Integer;
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    //procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    function Extract(AItem: T): T;
    function First: T;
    //function GetEnumerator: TListEnumerator;
    function IndexOf(const AItem: T): Integer;
    procedure Insert(const AIndex: Integer; const AItem: T);
    function Last: T;
    //procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(const AItem: T): Integer;
    //procedure Pack;
    procedure Sort(ACompare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;// write SetCount;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    property Data[const AIndex: Integer]: PData read GetData;
  end;

  { TGThreadItemDataList }

  TGThreadItemDataList<T, D> = class(TObject)
  private
  type
    //PData = ^D;
    //TGRecordListEnumeratorSpec = TGRecordListEnumerator<T>;
    TItemList = TGItemDataList<T, D>;
  private
    FList: TItemList;
    FLock: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Lock: TItemList;
    procedure UnLock;

    //Procedure AddList(AList : TList);
    function Add(const AItem: T): Integer;
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    //procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    function Extract(AItem: T): T;
    function First: T;
    //function GetEnumerator: TListEnumerator;
    function IndexOf(const AItem: T): Integer;
    procedure Insert(const AIndex: Integer; const AItem: T);
    function Last: T;
    //procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(const AItem: T): Integer;
    //procedure Pack;
    procedure Sort(ACompare: TListSortCompare);
    //property Capacity: Integer read GetCapacity write SetCapacity;
    //property Count: Integer read GetCount;// write SetCount;
    //property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    //property Data[const AIndex: Integer]: PData read GetData;
  end;

{$IF FPC_FULLVERSION >= 030000}
  //!!! if RecordList to vr_fpg from vrFpgPlus not compiled under 2.6.4

  { TGRecordListEnumerator }

  TGRecordListEnumerator<T> = class(TObject)
  private
  type
    PT = ^T;
  protected
    FList: TFPList;
    FPosition: Integer;
    function GetCurrent: PT;
  public
    constructor Create(AList: TFPList);
    function MoveNext: Boolean;
    property Current: PT read GetCurrent;
  end;

  { TGRecordList }

  TGRecordList<T> = class(TObject)
  private
  type
    PT = ^T;
    TGRecordListEnumeratorSpec = TGRecordListEnumerator<T>;
  private
    FList: TFPList;
    FDataSize: Integer;
    function AllocateNewItem: PT;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): PT;
    procedure SetCapacity(AValue: Integer);
  protected
    procedure DoInitItem(var AItems: T); virtual;
    procedure DoFreeItem(var AItems: T); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(constref AItem: T): Integer;
    procedure AddList(AList: TGRecordList<T>);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    //function Extract(AItem: T): T;
    function First: PT;
    function GetEnumerator: TGRecordListEnumeratorSpec;
    function IndexOf(const AItem: PT): Integer;
    procedure Insert(const AIndex: Integer; constref AItem: T);
    function Last: PT;
    //procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(const AItem: PT): Integer;
    //procedure Pack;
    procedure Sort(ACompare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;// write SetCount;
    property Items[const AIndex: Integer]: PT read GetItem; default;
  end;

  { TGThreadRecordList }

  TGThreadRecordList<T> = class(TObject)
  private
  type
    PT = ^T;
    //TGRecordListEnumeratorSpec = TGRecordListEnumerator<T>;
    TRecordList = TGRecordList<T>;
  private
    FList: TRecordList;
    FLock: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Lock: TRecordList;
    procedure UnLock;

    function Add(constref AItem: T): Integer;
    procedure AddList(AList: TRecordList);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    //class procedure Error(const Msg: string; Data: PtrInt); virtual;
    //procedure Exchange(Index1, Index2: Integer);
    //function Expand: TList;
    //function Extract(AItem: T): T;
    function First: T;
    //function GetEnumerator: TGRecordListEnumeratorSpec;
    function IndexOf(const AItem: PT): Integer;
    procedure Insert(const AIndex: Integer; constref AItem: T);
    function Last: T;
    //procedure Move(CurIndex, NewIndex: Integer);
    //procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(const AItem: PT): Integer;
    //procedure Pack;
    procedure Sort(ACompare: TListSortCompare);
    function IsEmpty: Boolean;
    //property Capacity: Integer read GetCapacity write SetCapacity;
    //property Count: Integer read GetCount;// write SetCount;
    //property Items[const AIndex: Integer]: PT read GetItem; default;
  end;

  {$IFDEF TEST_MODE}
  type
    TTestInfo = record
      Handle: PtrInt;
      IsJob: Boolean;
    end;
    PTestInfo = ^TTestInfo;
    TTestRecordList = TGThreadRecordList<TTestInfo>;
  {$ENDIF}

{$ENDIF VER3}

implementation


{ TGThreadObjectList }

constructor TGThreadObjectList<T>.Create(const AFreeObjects: Boolean);
begin
  FDuplicates:=dupIgnore;
  InitCriticalSection(FLock);
  FList := TObjectList_T.Create(AFreeObjects);
end;

destructor TGThreadObjectList<T>.Destroy;
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

procedure TGThreadObjectList<T>.Add(Item: T);
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

procedure TGThreadObjectList<T>.Clear;
begin
  Locklist;
  try
    FList.Clear;
  finally
    UnLockList;
  end;
end;

function TGThreadObjectList<T>.IsEmpty: Boolean;
begin
  Locklist;
  try
    Result := FList.Count = 0;
  finally
    UnLockList;
  end;
end;

procedure TGThreadObjectList<T>.Remove(Item: T);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

function TGThreadObjectList<T>.LockList: TObjectList_T;
begin
  Result:=FList;
  System.EnterCriticalSection(FLock);
end;

procedure TGThreadObjectList<T>.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TGItemDataList<D, T> }

function TGItemDataList<T, D>.AllocateNewItem(const AItem: T): PDataListRec;
begin
  Getmem(Result, FDataSize);
  FillChar(Result^, SizeOf(Result^), 0);
  Result.Item := AItem;
end;

function TGItemDataList<T, D>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TGItemDataList<T, D>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGItemDataList<T, D>.GetData(const AIndex: Integer): PData;
begin
  Result := @PDataListRec(FList[AIndex]).Data;
end;

function TGItemDataList<T, D>.GetItem(const AIndex: Integer): T;
begin
  Result := PDataListRec(FList[AIndex]).Item;
end;

procedure TGItemDataList<T, D>.SetCapacity(AValue: Integer);
begin
  FList.Capacity := AValue;
end;

procedure TGItemDataList<T, D>.SetItem(const AIndex: Integer; AValue: T);
begin
  PDataListRec(FList[AIndex]).Item := AValue;
end;

procedure TGItemDataList<T, D>.DoInitData(var AData: D);
begin

end;

procedure TGItemDataList<T, D>.DoFreeData(var AData: D);
begin

end;

constructor TGItemDataList<T, D>.Create;
begin
  FList := TFPList.Create;
  FDataSize := SizeOf(TGItemDataListRec);
end;

destructor TGItemDataList<T, D>.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TGItemDataList<T, D>.Add(const AItem: T): Integer;
var
  p: PDataListRec;
begin
  p := AllocateNewItem(AItem);
  Result := FList.Add(p);
  DoInitData(p.Data);
end;

procedure TGItemDataList<T, D>.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    Delete(I);
end;

procedure TGItemDataList<T, D>.Delete(const AIndex: Integer);
var
  p: PDataListRec;
begin
  p := FList[AIndex];
  DoFreeData(p.Data);
  Finalize(p.Data);
  Freemem(p, FDataSize);
  FList.Delete(AIndex);
end;

function TGItemDataList<T, D>.Extract(AItem: T): T;
var
  i : Integer;
begin
  i := IndexOf(AItem);
  if i >= 0 then
    begin
      Result := AItem;
      Delete(i);
    end
  else
    Result := T(0);
end;

function TGItemDataList<T, D>.First: T;
begin
  if FList.Count = 0 then
    Result := T(0)
  else
    Result := PDataListRec(FList[0]).Item;
end;

function TGItemDataList<T, D>.IndexOf(const AItem: T): Integer;
var
  C: Integer;
begin
  Result := 0;
  C := FList.Count;
  while (Result < C) and (Items[Result] <> AItem) do
    Inc(Result);
  If Result >= C then
    Result := -1;
end;

procedure TGItemDataList<T, D>.Insert(const AIndex: Integer; const AItem: T);
var
  p: PDataListRec;
begin
  p := AllocateNewItem(AItem);
  p.Item := AItem;
  FList.Insert(AIndex, p);
end;

function TGItemDataList<T, D>.Last: T;
begin
  If FList.Count = 0 then
    Result := T(0)
  else
    Result := PDataListRec(FList[FList.Count - 1]).Item;
end;

function TGItemDataList<T, D>.Remove(const AItem: T): Integer;
begin
  Result := IndexOf(AItem);
  if Result <> -1 then
    Delete(Result);
end;

procedure TGItemDataList<T, D>.Sort(ACompare: TListSortCompare);
begin
  FList.Sort(ACompare);
end;

{ TGThreadItemDataList }

constructor TGThreadItemDataList<T, D>.Create;
begin
  FList := TItemList.Create;
  InitCriticalSection(FLock);
end;

destructor TGThreadItemDataList<T, D>.Destroy;
begin
  Lock;
  try
    FreeAndNil(FList);
    inherited Destroy;
  finally
    UnLock;
    DoneCriticalsection(FLock);
  end;
end;

function TGThreadItemDataList<T, D>.Lock: TItemList;
begin
  System.EnterCriticalsection(FLock);
  Result := FList;
end;

procedure TGThreadItemDataList<T, D>.UnLock;
begin
  System.LeaveCriticalsection(FLock);
end;

function TGThreadItemDataList<T, D>.Add(const AItem: T): Integer;
begin
  Lock;
  try
    Result := FList.Add(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadItemDataList<T, D>.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    UnLock;
  end;
end;

procedure TGThreadItemDataList<T, D>.Delete(const AIndex: Integer);
begin
  Lock;
  try
    FList.Delete(AIndex);
  finally
    UnLock;
  end;
end;

function TGThreadItemDataList<T, D>.Extract(AItem: T): T;
begin
  Lock;
  try
    Result := FList.Extract(AItem);
  finally
    UnLock;
  end;
end;

function TGThreadItemDataList<T, D>.First: T;
begin
  Lock;
  try
    Result := FList.First;
  finally
    UnLock;
  end;
end;

function TGThreadItemDataList<T, D>.IndexOf(const AItem: T): Integer;
begin
  Lock;
  try
    Result := FList.IndexOf(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadItemDataList<T, D>.Insert(const AIndex: Integer; const AItem: T);
begin
  Lock;
  try
    FList.Insert(AIndex, AItem);
  finally
    UnLock;
  end;
end;

function TGThreadItemDataList<T, D>.Last: T;
begin
  Lock;
  try
    Result := FList.Last;
  finally
    UnLock;
  end;
end;

function TGThreadItemDataList<T, D>.Remove(const AItem: T): Integer;
begin
  Lock;
  try
    Result := FList.Remove(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadItemDataList<T, D>.Sort(ACompare: TListSortCompare);
begin
  Lock;
  try
    FList.Sort(ACompare);
  finally
    UnLock;
  end;
end;

{$IF FPC_FULLVERSION >= 030000}
{ TGRecordListEnumerator<T> }

function TGRecordListEnumerator<T>.GetCurrent: PT;
begin
  Result := PT(FList.Items[FPosition]);
end;

constructor TGRecordListEnumerator < T > .Create(AList: TFPList);
begin
  FList := AList;
  FPosition := -1;
end;

function TGRecordListEnumerator<T>.MoveNext: Boolean;
begin
  inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TGRecordList<T> }

function TGRecordList<T>.AllocateNewItem: PT;
begin
  Getmem(Result, FDataSize);
  FillChar(Result^, SizeOf(Result^), 0);
  DoInitItem(Result^);
end;

function TGRecordList<T>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TGRecordList<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGRecordList<T>.GetItem(const AIndex: Integer): PT;
begin
  Result := PT(FList[AIndex]);
end;

procedure TGRecordList<T>.SetCapacity(AValue: Integer);
begin
  FList.Capacity := AValue;
end;

procedure TGRecordList<T>.DoInitItem(var AItems: T);
begin

end;

procedure TGRecordList<T>.DoFreeItem(var AItems: T);
begin

end;

constructor TGRecordList < T > .Create;
begin
  FList := TFPList.Create;
  FDataSize := SizeOf(T);
end;

destructor TGRecordList < T > .Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TGRecordList<T>.Add(constref AItem: T): Integer;
var
  p: PT;
begin
  p := AllocateNewItem;
  p^ := AItem;
  Result := FList.Add(p);
end;

procedure TGRecordList<T>.AddList(AList: TGRecordList<T>);
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    {$IF FPC_FULLVERSION >= 030000}Add(AList[i]^){$ELSE}Add(PT(AList[i])^){$ENDIF};
end;

procedure TGRecordList<T>.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    Delete(i);
end;

procedure TGRecordList<T>.Delete(const AIndex: Integer);
var
  p: PT;
begin
  p := FList[AIndex];
  DoFreeItem(p^);
  Finalize(p^);
  Freemem(p, FDataSize);
  FList.Delete(AIndex);
end;

procedure TGRecordList<T>.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TGRecordList<T>.First: PT;
begin
  if FList.Count = 0 then
    Result := nil
  else
    Result := PT(FList[0]);
end;

function TGRecordList<T>.GetEnumerator: TGRecordListEnumeratorSpec;
begin
  Result := TGRecordListEnumeratorSpec.Create(FList);
end;

procedure TGRecordList<T>.Insert(const AIndex: Integer; constref AItem: T);
var
  p: PT;
begin
  p := AllocateNewItem;
  p^ := AItem;
  FList.Insert(AIndex, p);
end;

function TGRecordList<T>.Last: PT;
begin
  If FList.Count = 0 then
    Result := nil
  else
    Result := PT(FList[FList.Count - 1]);
end;

function TGRecordList<T>.IndexOf(const AItem: PT): Integer;
var
  C: Integer;
begin
  Result := 0;
  C := FList.Count;
  while (Result < C) and (Items[Result] <> AItem) do
    Inc(Result);
  If Result >= C then
    Result := -1;
end;

function TGRecordList<T>.Remove(const AItem: PT): Integer;
begin
  Result := IndexOf(AItem);
  if Result <> -1 then
    Delete(Result);
end;

procedure TGRecordList<T>.Sort(ACompare: TListSortCompare);
begin
  FList.Sort(ACompare);
end;

{ TGThreadRecordList }

constructor TGThreadRecordList<T>.Create;
begin
  FList := TRecordList.Create;
  InitCriticalSection(FLock);
end;

destructor TGThreadRecordList<T>.Destroy;
begin
  Lock;
  try
    FreeAndNil(FList);
    inherited Destroy;
  finally
    UnLock;
    DoneCriticalsection(FLock);
  end;
end;

function TGThreadRecordList<T>.Lock: TRecordList;
begin
  System.EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TGThreadRecordList<T>.UnLock;
begin
  System.LeaveCriticalsection(FLock);
end;

function TGThreadRecordList<T>.Add(constref AItem: T): Integer;
begin
  Lock;
  try
    Result := FList.Add(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadRecordList<T>.AddList(AList: TRecordList);
begin
  Lock;
  try
    FList.AddList(AList);
  finally
    UnLock;
  end;
end;

procedure TGThreadRecordList<T>.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    UnLock;
  end;
end;

procedure TGThreadRecordList<T>.Delete(const AIndex: Integer);
begin
  Lock;
  try
    FList.Delete(AIndex);
  finally
    UnLock;
  end;
end;

function TGThreadRecordList<T>.First: T;
begin
  Lock;
  try
    Result := FList.First^;
  finally
    UnLock;
  end;
end;

function TGThreadRecordList<T>.IndexOf(const AItem: PT): Integer;
begin
  Lock;
  try
    Result := FList.IndexOf(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadRecordList<T>.Insert(const AIndex: Integer; constref AItem: T);
begin
  Lock;
  try
    FList.Insert(AIndex, AItem);
  finally
    UnLock;
  end;
end;

function TGThreadRecordList<T>.Last: T;
begin
  Lock;
  try
    Result := FList.Last^;
  finally
    UnLock;
  end;
end;

function TGThreadRecordList<T>.Remove(const AItem: PT): Integer;
begin
  Lock;
  try
    Result := FList.Remove(AItem);
  finally
    UnLock;
  end;
end;

procedure TGThreadRecordList<T>.Sort(ACompare: TListSortCompare);
begin
  Lock;
  try
    FList.Sort(ACompare);
  finally
    UnLock;
  end;
end;

function TGThreadRecordList<T>.IsEmpty: Boolean;
begin
  Lock;
  try
    Result := FList.Count = 0;
  finally
    UnLock;
  end;
end;

{$ENDIF VER3}


end.
