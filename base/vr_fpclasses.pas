unit vr_fpclasses;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, RtlConsts, vr_SysUtils, Types;

type

  { TvrThreadListHelper }

  TvrThreadListHelper = class helper for TThreadList
  public
    procedure Insert(const AIndex: Integer; AItem: Pointer);
    function IndexOf(const AItem: Pointer): Integer;
    function IsEmpty: Boolean;
    function GetItem(const AIndex: Integer; out AItem: Pointer): Boolean;
    function Items: TPointerDynArray;

    class function LockedGet(const AThreadList: TThreadList; out AList: TList): Boolean;
    class function LockedCreateIfNil(var AList: TThreadList; ANewItem: Pointer): Boolean;
    class function LockedFreeAndNilIfEmpty(var AList: TThreadList; ARemoveIndex: Integer = -1): Boolean; overload;
    class function LockedFreeAndNilIfEmpty(var AList: TThreadList; ARemoveItem: Pointer = nil): Boolean; overload;
  end;

procedure InitThreads;

  {$IFNDEF VER3}
type
  { TvrThreadHelper }

  TvrThreadHelper = class helper for TThread
  private
    class function GetCurrentThread: TThread; static;
  public
    class property CurrentThread: TThread read GetCurrentThread;
  end;

  {$ENDIF}

implementation

{$IFNDEF VER3}

var
  ExternalThreads: TThreadList;
threadvar
  CurrentThreadVar: TThread;

type
  { this type is used by TThread.GetCurrentThread if the thread does not yet
    have a value in CurrentThreadVar (Note: the main thread is also created as
    a TExternalThread) }

  { TExternalThread }

  TExternalThread = class(TThread)
  protected
    { dummy method to remove the warning }
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;


procedure TExternalThread.Execute;
begin
  { empty }
end;


constructor TExternalThread.Create;
begin
  FThreadID := GetCurrentThreadID;
  ExternalThreads.Add(Self);
  //FExternalThread := True;
  { the parameter is unimportant if FExternalThread is True }
  //inherited Create(False);
  //with ExternalThreads.LockList do
  //  try
  //    Add(Self);
  //  finally
  //    ExternalThreads.UnlockList;
  //  end;
end;


destructor TExternalThread.Destroy;
begin
  ExternalThreads.Remove(Self);
  //inherited;
  //if not ExternalThreadsCleanup then
  //  with ExternalThreads.LockList do
  //    try
  //      Extract(Self);
  //    finally
  //      ExternalThreads.UnlockList;
  //    end;
end;

procedure TExternalThread.AfterConstruction;
begin
  //inherited AfterConstruction;
end;

{ TvrThreadHelper }

class function TvrThreadHelper.GetCurrentThread: TThread; static;
begin
  Result := CurrentThreadVar;
  if not Assigned(Result) then begin
    Result := TExternalThread.Create;
    CurrentThreadVar := Result;
  end;
end;

{$ENDIF NOT VER3}
{ TvrThreadListHelper }

function TvrThreadListHelper.GetItem(const AIndex: Integer; out AItem: Pointer): Boolean;
var
  lst: TList;
begin
  lst := LockList;
  try
    Result := (AIndex >= 0) and (AIndex < lst.Count);
    if Result then
      AItem := lst[AIndex]
    else
      AItem := nil;
  finally
    UnlockList;
  end;
end;

function TvrThreadListHelper.Items: TPointerDynArray;
var
  lst: TList;
  i: Integer;
begin
  lst := LockList;
  try
    SetLength(Result, lst.Count);
    for i := 0 to lst.Count - 1 do
      Result[i] := lst[i];
  finally
    UnlockList;
  end;
end;

class function TvrThreadListHelper.LockedGet(const AThreadList: TThreadList; out
  AList: TList): Boolean;
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

class function TvrThreadListHelper.LockedCreateIfNil(var AList: TThreadList;
  ANewItem: Pointer): Boolean;
begin
  LockVar;
  try
    Result := AList = nil;
    if Result then
      AList := TThreadList.Create;
    if ANewItem <> nil then
      AList.Add(ANewItem);
  finally
    UnLockVar;
  end;
end;

class function TvrThreadListHelper.LockedFreeAndNilIfEmpty(
  var AList: TThreadList; ARemoveIndex: Integer): Boolean;
var
  lst: TList;
begin
  lst := AList.LockList;
  try
    if (ARemoveIndex >= 0) and (ARemoveIndex < lst.Count) then
      lst.Delete(ARemoveIndex);
  finally
    AList.UnlockList;
  end;
  Result := LockedFreeAndNilIfEmpty(AList, nil);
end;

class function TvrThreadListHelper.LockedFreeAndNilIfEmpty(var AList: TThreadList;
  ARemoveItem: Pointer): Boolean;
var
  lst: TThreadList = nil;
  l: TList = nil;
begin
  LockVar;
  try
    if AList = nil then
      Exit(True);
    l := AList.LockList;
    if ARemoveItem <> nil then
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

procedure TvrThreadListHelper.Insert(const AIndex: Integer; AItem: Pointer);
var
  lst: TList;
begin
  lst := LockList;
  try
    if (Duplicates=dupAccept) or
      // make sure it's not already in the list
      (lst.IndexOf(AItem)=-1) then
       lst.Insert(AIndex, AItem)
     else if (Duplicates=dupError) then
       lst.Error(SDuplicateItem,{%H-}PtrUInt(AItem));
  finally
    UnlockList;
  end;
end;

function TvrThreadListHelper.IndexOf(const AItem: Pointer): Integer;
var
  lst: TList;
begin
  lst := LockList;
  try
    Result := lst.IndexOf(AItem);
  finally
    UnlockList;
  end;
end;

function TvrThreadListHelper.IsEmpty: Boolean;
var
  lst: TList;
begin
  lst := LockList;
  try
    Result := lst.Count = 0;
  finally
    UnlockList;
  end;
end;


type
  TThreadManagerPatch = record
    OldBeginThread: TBeginThreadHandler;
    OldEndThread: TEndThreadHandler;
    OldThreadFunc: TThreadFunc;
    Count: Integer;
  end;
var
  ThreadManagerPatch: TThreadManagerPatch;

{$IFNDEF VER3}
function _NewThreadFunc(parameter : pointer) : ptrint;
begin
  CurrentThreadVar := TThread(parameter);
  Result := ThreadManagerPatch.OldThreadFunc(parameter);
end;
{$ENDIF}

function _NewBeginTread(sa : Pointer;stacksize : PtrUInt; ThreadFunction : TThreadFunc;
  p : pointer;creationFlags : dword; var ThreadId : TThreadID) : TThreadID;
begin
  {$IFDEF VER3}
  Result := ThreadManagerPatch.OldBeginThread(sa, stacksize, ThreadFunction,
        p, creationFlags, ThreadId);
  {$ELSE}
  if not Assigned(ThreadManagerPatch.OldThreadFunc) then
    ThreadManagerPatch.OldThreadFunc := ThreadFunction;
  Result := ThreadManagerPatch.OldBeginThread(sa, stacksize, _NewThreadFunc,
      p, creationFlags, ThreadId);
  {$ENDIF}
  Inc(ThreadManagerPatch.Count);
end;

procedure _NewEndThread(ExitCode : DWord);
begin
  Dec(ThreadManagerPatch.Count);
  ThreadManagerPatch.OldEndThread(ExitCode);
end;

procedure _InitTM;
var
  tm: TThreadManager;
begin
  {$IFNDEF VER3}
  ExternalThreads := TThreadList.Create;{$ENDIF}
  if not GetThreadManager(tm{%H-}) or
      (@tm.BeginThread = nil) then Exit;
  ThreadManagerPatch.OldBeginThread := tm.BeginThread;
  ThreadManagerPatch.OldEndThread := tm.EndThread;
  ThreadManagerPatch.OldThreadFunc := nil;
  tm.BeginThread := _NewBeginTread;
  tm.EndThread := _NewEndThread;
  SetThreadManager(tm);
end;

{$IFNDEF VER3}
procedure _Fin;
var
  lst: TList;
  i: Integer;
begin
  lst := ExternalThreads.LockList;
  try
    for i := lst.Count - 1 downto 0 do
      TThread(lst[i]).Free;
  finally
    ExternalThreads.UnlockList;
  end;
  FreeAndNil(ExternalThreads);
end;
{$ENDIF}

procedure _WaitThreadsTerminated(AMSec: Integer);
begin
  while AMSec > 0 do
    begin
      if ThreadManagerPatch.Count <= 0 then
        begin
          Exit;
        end;
      CheckSynchronize();
      Sleep(10);
      Dec(AMSec, 10);
    end;
end;

procedure InitThreads;
begin
  _InitTM;
end;

//{$IFNDEF MOBILE_DEVICE}
initialization
  _InitTM;//{$ENDIF}

finalization
  {$IFNDEF VER3}
  _Fin;{$ENDIF}
  _WaitThreadsTerminated(10000);


end.

