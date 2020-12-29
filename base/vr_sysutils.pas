unit vr_SysUtils;

{$mode delphi}{$H+}

interface

uses
  SysUtils, types;

procedure LockVar; inline;
procedure UnLockVar;  inline;
function LockedVarCompare(const ATarget: Pointer; const AComparend: Pointer): Integer;
function LockedVarIsNil(const ATarget: Pointer): Boolean;
//? call TObject.Create
//procedure LockedObjectCreate(var AObject; const AClass: TClass; const AFree: Boolean = False);
//procedure LockedObjectCreateIfNil(var AObject; const AClass: TClass);
procedure LockedObjectFreeAndNil(var AObject);
procedure LockedVarSetNil(var v);
function LockedIntefaceGet(const AIntf: IUnknown): IUnknown;
procedure LockedIntefaceSet(var AIntf; const AValue: IUnknown);
function LockedStringGet(const S: string): string; overload;
function LockedStringGet(const S: UnicodeString): UnicodeString;overload;
procedure LockedStringSet(var S: string; const AValue: string);overload;
procedure LockedStringSet(var S: UnicodeString; const AValue: UnicodeString);overload;
function LockedStringEmpty(const S: string): Boolean;overload;
function LockedStringEmpty(const S: UnicodeString): Boolean;overload;
function LockedVarGet(const v): PtrInt;
procedure LockedVarSet(var v; const AValue: PtrInt);
function LockedBoolGet(const v: Boolean): Boolean;
procedure LockedBoolSet(var v: Boolean; const AValue: Boolean);
function LockedByteArrayGet(const arr: TByteDynArray): TByteDynArray;
procedure LockedByteArraySet(var arr: TByteDynArray; const AValue: TByteDynArray);

procedure RaiseToDoException(AMsg: string = '');

implementation

var
  _VarOperationLock: TRTLCriticalSection;

procedure LockVar;
begin
  EnterCriticalSection(_VarOperationLock);
end;

procedure UnLockVar;
begin
  LeaveCriticalSection(_VarOperationLock);
end;

function LockedVarCompare(const ATarget: Pointer; const AComparend: Pointer
  ): Integer;
begin
  LockVar;
  try
    if ATarget = AComparend then
      Result := 0
    else if ATarget < AComparend then
     Result := -1
    else
      Result := 1;
  finally
    UnLockVar;
  end;
end;

function LockedVarIsNil(const ATarget: Pointer): Boolean;
begin
  LockVar;
  try
    Result := ATarget = nil;
  finally
    UnLockVar;
  end;
end;

procedure LockedObjectCreate(var AObject; const AClass: TClass;
  const AFree: Boolean);
var
  tmp: TObject;
begin
  LockVar;
  try
    tmp := TObject(AObject);
    TObject(AObject) := AClass.Create;
  finally
    UnLockVar;
  end;
  if AFree then
    tmp.Free
end;

procedure LockedObjectCreateIfNil(var AObject; const AClass: TClass);
begin
  LockVar;
  try
    if TObject(AObject) = nil then
      TObject(AObject) := AClass.Create;
  finally
    UnLockVar;
  end;
end;

procedure LockedObjectFreeAndNil(var AObject);
var
  tmp: TObject;
begin
  LockVar;
  try
    tmp := TObject(AObject);
    Pointer(AObject) := nil;
  finally
    UnLockVar;
  end;
  tmp.Free;
end;

procedure LockedVarSetNil(var v);
begin
  LockVar;
  try
    Pointer(v) := nil;
  finally
    UnLockVar;
  end;
end;

function LockedIntefaceGet(const AIntf: IUnknown): IUnknown;
begin
  LockVar;
  try
    Result := AIntf;
  finally
    UnLockVar;
  end;
end;

procedure LockedIntefaceSet(var AIntf; const AValue: IUnknown);
begin
  LockVar;
  try
    IUnknown(AIntf) := AValue;
  finally
    UnLockVar;
  end;
end;

function LockedStringGet(const S: string): string;
begin
  LockVar;
  try
    Result := S;
    UniqueString(Result);
  finally
    UnLockVar;
  end;
end;

function LockedStringGet(const S: UnicodeString): UnicodeString;
begin
  LockVar;
  try
    Result := S;
    UniqueString(Result);
  finally
    UnLockVar;
  end;
end;

procedure LockedStringSet(var S: string; const AValue: string);
begin
  LockVar;
  try
    S := AValue;
    UniqueString(S);
  finally
    UnLockVar;
  end;
end;

procedure LockedStringSet(var S: UnicodeString; const AValue: UnicodeString);
begin
  LockVar;
  try
    S := AValue;
    UniqueString(S);
  finally
    UnLockVar;
  end;
end;

function LockedStringEmpty(const S: string): Boolean;
begin
  LockVar;
  try
    Result := S = '';
  finally
    UnLockVar;
  end;
end;

function LockedStringEmpty(const S: UnicodeString): Boolean;
begin
  LockVar;
  try
    Result := S = '';
  finally
    UnLockVar;
  end;
end;

function LockedVarGet(const v): PtrInt;
begin
  LockVar;
  try
    Result := PtrInt(v);
  finally
    UnLockVar;
  end;
end;

procedure LockedVarSet(var v; const AValue: PtrInt);
begin
  LockVar;
  try
    PtrInt(v) := AValue;
  finally
    UnLockVar;
  end;
end;

function LockedBoolGet(const v: Boolean): Boolean;
begin
  LockVar;
  try
    Result := v;
  finally
    UnLockVar;
  end;
end;

procedure LockedBoolSet(var v: Boolean; const AValue: Boolean);
begin
  LockVar;
  try
    v := AValue;
  finally
    UnLockVar;
  end;
end;

function LockedByteArrayGet(const arr: TByteDynArray): TByteDynArray;
begin
  LockVar;
  try
    Result := Copy(arr, 0, MaxInt);
  finally
    UnLockVar;
  end;
end;

procedure LockedByteArraySet(var arr: TByteDynArray; const AValue: TByteDynArray);
begin
  LockVar;
  try
    arr := Copy(AValue, 0, MaxInt);
  finally
    UnLockVar;
  end;
end;

procedure RaiseToDoException(AMsg: string);
begin
  if AMsg = '' then
    AMsg := 'Not implemented';
  raise Exception.Create('TODO: ' + AMsg);
end;


initialization
  InitCriticalSection(_VarOperationLock);

finalization
  DoneCriticalsection(_VarOperationLock);


end.

