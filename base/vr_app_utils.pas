unit vr_app_utils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, vr_types, vr_SysUtils, vr_utils, vr_classes, contnrs;

const
  {Application Event Type}
  AET_NONE                    = 0;
  AET_BeforeInit              = 1;//Sender: user defined
  AET_AppLoadConfig           = 2;//Sender: user defined
  AET_AppInitialized          = 3;//Sender: TCustomApplication(custapp.pp)
  AET_AppCloseQuery           = 4;//Sender: TForm
  AET_AppSaveConfig           = 5;//Sender: user defined
  AET_AppClose                = 6;//Sender: TForm
  AET_SetSerialKey            = 7;//Sender: 1: Valid Key

  AET_USER                    = 8;

  //*** define user event ***
  // AET_Your_Event = AET_USER + 1;
  // or
  // var AET_Your_Event: Integer;
  // AET_Your_Event = GetNextUnusedAppEventHandlerIndex()
type
  TAppEvent = procedure(const Sender: TObject; var AStop: Boolean; var AResult: PtrInt) of object;
  TAppMultiEvent = procedure(const Sender: TObject; var AStop: Boolean;
      var AResult: PtrInt; const AET_: Integer) of object;
  TCustomEventCallBack = function(AMethod: TMethod; AData: Pointer; var AStop: Boolean): PtrInt;

procedure AddAppMultiEventHandler(AHandler: TAppMultiEvent; AEvents: array of PtrInt; AsFirst: Boolean = False);
procedure AddAppEventHandler(AET_: Integer;
    const AHandler: TAppEvent; const AsFirst: Boolean = False);

procedure RemoveAppMultiEventHandler(const AHandler: TAppMultiEvent); overload;
procedure RemoveAppMultiEventHandler(const AHandler: TAppMultiEvent; AEvents: array of PtrInt); overload;
procedure RemoveAppEventHandler(const AET_: Integer; const AHandler: TAppEvent);
procedure RemoveAppAllEventOfObject(const AObject: TObject);

function GetNextUnusedAppEventHandlerIndex(ACount: Cardinal = 1): Integer; overload;
function GetNextUnusedAppEventHandlerIndex(AEvents: array of PPtrInt): Integer; overload;

function NotifyAppEvent(AET_: Integer; const Sender: TObject): PtrInt;
procedure NotifyAppEventAsync(const AET_: Integer; const Sender: TObject);

procedure AddObjectToAutoFreeList(AObject: TObject);

implementation

const
  FIRST_UNUSED_EVENT_INDEX = 10000;
var
  _NextUnusedAppEventHandlerIndex: Integer = FIRST_UNUSED_EVENT_INDEX + 1;

type
  TAppMethodList = class;
  TAppHandlerRec = record
    Handler: TAppMethodList;
    MultiHandler: TAppMethodList;
  end;
  PAppHandlerRec = ^TAppHandlerRec;

  { TAppMethodList }

  TAppMethodList = class(TThreadMethodList)
  private
    FIsMulti: Boolean;
  public
    constructor Create(AIsMulti: Boolean);
    procedure Notify( const AET_: Integer; const Sender: TObject;
        var AStop: Boolean; var AResult: PtrInt);
  end;

  { TNotifyAppEventThread }

  TNotifyAppEventThread = class(TThread)
  private
    class var FList: TThreadStringList;
    procedure OnAppClose(const Sender: TObject; var AStop: Boolean; var AResult: PtrInt);
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

var
  FAppHandlersConst: TThreadDataStringList = nil;
  FAppHandlersVar: TThreadDataStringList = nil;
  _AsyncNotifier: TNotifyAppEventThread = nil;

{ TAppMethodList }

constructor TAppMethodList.Create(AIsMulti: Boolean);
begin
  inherited Create;
  FIsMulti := AIsMulti;
end;

procedure TAppMethodList.Notify(const AET_: Integer; const Sender: TObject;
  var AStop: Boolean; var AResult: PtrInt);
var
  i: Integer;
  m: TMethod;
begin
  i := Count;
  while NextDownMethod(i, m) do
    begin
      if (apsClosing in AppStates) and (GetThreadID <> MainThreadID) then
        Exit;
      try
        if FIsMulti then
          TAppMultiEvent(m)(Sender, AStop, AResult, AET_)
        else
          TAppEvent(m)(Sender, AStop, AResult);
      except
        //
      end;
      if AStop then Exit;
    end;
end;

{ TNotifyAppEventThread }

procedure TNotifyAppEventThread.OnAppClose(const Sender: TObject;
  var AStop: Boolean; var AResult: PtrInt);
begin
  FList.Clear;
end;

procedure TNotifyAppEventThread.Execute;
var
  AET_: Integer;
  Sender: TObject;

  function _GetData: Boolean;
  var
    lst: TStringListUTF8;
  begin
    lst := FList.LockList;
    try
      Result := lst.Count > 0;
      if Result then
        begin
          AET_ := StrToInt(lst[0]);
          Sender := lst.Objects[0];
          lst.Delete(0);
        end;
    finally
      FList.UnlockList;
    end;
  end;

begin
  while True do
    begin
      if _GetData then
        NotifyAppEvent(AET_, Sender)
      else
        begin
          LockVar;
          try
            if TThreadStringList.LockedFreeAndNilIfEmpty(FList) then
            //if FList.IsEmpty then
              begin
                _AsyncNotifier := nil;
                //FreeAndNil(FList);
                Exit;
              end;
          finally
            UnLockVar;
          end;
        end;
    end;
end;

procedure TNotifyAppEventThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FreeOnTerminate := True;
  AddAppEventHandler(AET_AppClose, OnAppClose);
end;

destructor TNotifyAppEventThread.Destroy;
begin
  RemoveAppEventHandler(AET_AppClose, OnAppClose);
  inherited Destroy;
end;

procedure _InitAppHandlers;
begin
  LockVar;
  try
    if FAppHandlersConst = nil then
      begin
        FAppHandlersConst := TThreadDataStringList.Create(SizeOf(TAppHandlerRec));
        FAppHandlersVar := TThreadDataStringList.Create(SizeOf(TAppHandlerRec));
      end;
  finally
    UnLockVar;
  end;
end;

function _GetEventHandlerList(var AET_: Integer): TThreadDataStringList;
begin
  _InitAppHandlers;
  if AET_ < FIRST_UNUSED_EVENT_INDEX then
    Result := FAppHandlersConst
  else
    begin
      Result := FAppHandlersVar;
      Dec(AET_, FIRST_UNUSED_EVENT_INDEX);
    end;
end;

procedure _GrowAppHandlersList(AET_: Integer; const AList: TThreadDataStringList);
var
  lst: TDataStringList;
begin
  if AET_ >= FIRST_UNUSED_EVENT_INDEX then
    Dec(AET_, FIRST_UNUSED_EVENT_INDEX);
  lst := AList.LockList;
  try
    if AET_ >= lst.Count then
      lst.Grow(AET_ + 1);
  finally
    AList.UnlockList;
  end;
end;

procedure _AddAppEventHandler(AET_: Integer; const AHandler: TMethod; const AsFirst: Boolean;
    const AIsMulti: Boolean);
var
  r: PAppHandlerRec;
  l: ^TAppMethodList;
  lst: TDataStringList;
  tlst: TThreadDataStringList;
begin
  {$IFDEF TEST_MODE}
  if not FUnitInitializied then
    ShowError('AppTypes.pas: AddAppEventHandler called when unit not initialized');
  if (AHandler.Code = nil) then
    begin
      ShowInfo('AppTypes._AddAppEventHandler(): AHandler.Code = nil');
      Exit;
    end;
  {$ENDIF}
  if (AET_ <= 0) or (apsClosing in AppStates) then Exit;
  tlst := _GetEventHandlerList(AET_);
  _GrowAppHandlersList(AET_, tlst);
  lst := tlst.LockList;
  try
    r := lst.Data[AET_];
    if AIsMulti then
      l := @r.MultiHandler
    else
      l := @r.Handler;
    if l^ = nil then
      l^ := TAppMethodList.Create(AIsMulti);
    l^.Add(AHandler, AsFirst);
  finally
    tlst.UnlockList;
  end;
end;

procedure _RemoveAppEventHandler(AET_: Integer; const AHandler: TMethod; const AIsMulti: Boolean);
var
  lst: TDataStringList;
  r: PAppHandlerRec;
  l: TAppMethodList;
  tlst: TThreadDataStringList;
begin
  if (AET_ <= 0) or (FAppHandlersConst = nil) then Exit;
  tlst := _GetEventHandlerList(AET_);
  lst := tlst.LockList;
  try
    if lst.Count > AET_ then
      begin
        r := lst.Data[AET_];
        if AIsMulti then
          l := r.MultiHandler
        else
          l := r.Handler;
        if l <> nil then
          l.Remove(AHandler);
      end;
  finally
    tlst.UnlockList;
  end;
end;

procedure AddAppMultiEventHandler(AHandler: TAppMultiEvent;
  AEvents: array of PtrInt; AsFirst: Boolean);
var
  i: Integer;
begin
  for i := 0 to Length(AEvents) - 1 do
    _AddAppEventHandler(AEvents[i], TMethod(AHandler), AsFirst, True);
end;

procedure AddAppEventHandler(AET_: Integer; const AHandler: TAppEvent;
  const AsFirst: Boolean);
begin
  _AddAppEventHandler(AET_, TMethod(AHandler), AsFirst, False);
end;

procedure RemoveAppMultiEventHandler(const AHandler: TAppMultiEvent);
begin
  RemoveAppMultiEventHandler(AHandler, []);
end;

procedure RemoveAppMultiEventHandler(const AHandler: TAppMultiEvent;
  AEvents: array of PtrInt);
var
  i: Integer;
begin
  if Length(AEvents) = 0 then
    begin
      for i := 1 to _NextUnusedAppEventHandlerIndex - 1 do
        _RemoveAppEventHandler(i, TMethod(AHandler), True);
    end
  else
    for i := 0 to Length(AEvents) - 1 do
      _RemoveAppEventHandler(AEvents[i], TMethod(AHandler), True);
end;

procedure RemoveAppEventHandler(const AET_: Integer; const AHandler: TAppEvent);
begin
  _RemoveAppEventHandler(AET_, TMethod(AHandler), False);
end;

procedure RemoveAppAllEventOfObject(const AObject: TObject);

  procedure _Remove(AList: TThreadDataStringList);
  var
    lst: TDataStringList;
    r: PAppHandlerRec;
    l: TAppMethodList;
    i: Integer;
  begin
    lst := AList.LockList;
    try
      for i := 0 to lst.Count - 1 do
        begin
          r := lst.Data[i];
          l := r.Handler;
          if l <> nil then
            l.RemoveAllMethodsOfObject(AObject);
          l := r.MultiHandler;
          if l <> nil then
            l.RemoveAllMethodsOfObject(AObject);
        end;
    finally
      AList.UnlockList;
    end;
  end;

begin
  _Remove(FAppHandlersConst);
  _Remove(FAppHandlersVar);
end;

function GetNextUnusedAppEventHandlerIndex(ACount: Cardinal): Integer;
begin
  _InitAppHandlers;
  FAppHandlersVar.LockList;
  try
    Inc(_NextUnusedAppEventHandlerIndex, ACount);
    Result := _NextUnusedAppEventHandlerIndex - 1;
    _GrowAppHandlersList(Result, FAppHandlersVar);
  finally
    FAppHandlersVar.UnlockList;
  end;
end;

function GetNextUnusedAppEventHandlerIndex(AEvents: array of PPtrInt): Integer;
var
  i, n, Len: Integer;
begin
  Len := Length(AEvents);
  if Len = 0 then Exit;
  Result := GetNextUnusedAppEventHandlerIndex(Len);
  n := Result;
  for i := Len - 1 downto 0 do
    begin
      AEvents[i]^ := n;
      Dec(n);
    end;
end;

function NotifyAppEvent(AET_: Integer; const Sender: TObject): PtrInt;
var
  Stop: Boolean = False;
  r: PAppHandlerRec;
  tlst: TThreadDataStringList;
  AET_REAL: Integer;

  function _GetData(out AData: PAppHandlerRec): Boolean;
  var
    lst: TDataStringList;
  begin
    Result := False;
    lst := tlst.LockList;
    try
      if (AET_ <= 0) or (AET_ >= lst.Count) then Exit;
      AData := lst.Data[AET_];
      Result := True;
    finally
      tlst.UnlockList;
    end;
  end;

  procedure _Notify(l: TAppMethodList);
  begin
    if l <> nil then
      l.Notify(AET_REAL, Sender, Stop, Result);
  end;

begin
  Result := 0;
  AET_REAL := AET_;
  tlst := _GetEventHandlerList(AET_);
  if LockedVarIsNil(tlst) then Exit;
  if not _GetData(r) then Exit;
  _Notify(r.Handler);
  if Stop then Exit;
  _Notify(r.MultiHandler);
end;

procedure NotifyAppEventAsync(const AET_: Integer; const Sender: TObject);
var
  lst: ^TThreadStringList;
begin
  LockVar;
  try
    lst := @TNotifyAppEventThread.FList;
    if lst^ = nil then
      lst^ := TThreadStringList.Create(False);
    lst^.AddObject(IntToStr(AET_), Sender);
    if _AsyncNotifier = nil then
      _AsyncNotifier := TNotifyAppEventThread.Create(False);
  finally
    UnLockVar;
  end;
end;

var
  _AutoFreeList: TObjectList = nil;
procedure AddObjectToAutoFreeList(AObject: TObject);
begin
  if (apsClosing in AppStates) then
    begin
      AObject.Free;
      Exit;
    end;
  LockVar;
  if _AutoFreeList = nil then
    _AutoFreeList := TObjectList.create(True);
  UnLockVar;
  _AutoFreeList.Add(AObject);
end;

procedure _Fin;

  procedure _ClearEventHandlerList(AList: TThreadDataStringList);
  var
    i: Integer;
    lst: TDataStringList;
    r: PAppHandlerRec;
  begin
    if LockedVarIsNil(AList) then Exit;
    lst := AList.LockList;
    try
      for i := 0 to lst.Count - 1 do
        begin
          r := lst.Data[i];
          FreeAndNil(r.Handler);
          FreeAndNil(r.MultiHandler);
        end;
    finally
      AList.UnlockList;
    end;
    LockedObjectFreeAndNil(AList);
  end;

begin
  FreeAndNil(_AutoFreeList);
  _ClearEventHandlerList(FAppHandlersConst);
  _ClearEventHandlerList(FAppHandlersVar);
end;

//initialization
//  _NextUnusedAppEventHandlerIndex := AET_NEXT_UNUSED + 1;

finalization
  _Fin;

end.

