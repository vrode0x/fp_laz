unit vr_JsonRpcClasses;

{$mode delphi}{$H+}
{$I vrode.inc}
interface
{.$DEFINE FORMS_ASYNC_CALL}
uses
  Classes, SysUtils, vr_utils, vr_classes, vr_JsonRpc, vr_JsonUtils, vr_promise,
  vr_transport, vr_SysUtils, vr_variant
  {$IFDEF FORMS_ASYNC_CALL}, Forms{$ENDIF};

type
  IPromise = vr_promise.IPromise;
  IJsonRpcNotification = vr_JsonRpc.IJsonRpcNotification;
  IJsonRpcRequest = vr_JsonRpc.IJsonRpcRequest;
  IJsonRpcSuccess = vr_JsonRpc.IJsonRpcSuccess;
  IJsonRpcError = vr_JsonRpc.IJsonRpcError;
  TServerMethodsList = TStringListHashed;

  TOnJsonRpcRequest = function(const ARequest: IJsonRpcRequest; const AThread: TThread;
      out AResult: string; var AErrorCode: Integer): Boolean of object;
  TOnJsonRpcRequestASync = function(const ARequest: IJsonRpcRequest; out AResult: string;
      var AErrorCode: Integer): Boolean of object;
  TOnJsonRpcNotification = function(const ANotify: IJsonRpcNotification; const AThread: TThread): Boolean of object;
  TOnJsonRpcNotificationASync = procedure(const ANotify: IJsonRpcNotification) of object;

  TRequestMethod = procedure(const AParams: IJsonData; out AResult: string;
      var AErrorCode: Integer) of object;
  TNotificationMethod = procedure(const AParams: IJsonData; const AClient: Pointer) of object;

  //use prefix for published methods to autodetect if UsePublishedMethods=True
  TPublishedMethodKind = (pmkNone,
      {rt_*}pmkRequestThread,         //TRequestMethod: current thread
      {rq_*}pmkRequestQueue,          //TRequestMethod: QueueAsyncCall(InMainThread=False)
      {rm_*}pmkRequestMainThread,     //TRequestMethod: MainThread TThread.Synchronize()
      {ra_*}pmkRequestASync,          //TRequestMethod: MainThread QueueAsyncCall(InMainThread=True)
      {nt_*}pmkNotificationThread,    //TNotificationMethod: current thread
      {nq_*}pmkNotificationQueue,     //TNotificationMethod QueueAsyncCall(InMainThread=False)
      {nm_*}pmkNotificationMainThread,//TNotificationMethod: MainThread TThread.Synchronize()
      {na_*}pmkNotificationAsync      //TNotificationMethod: MainThread QueueAsyncCall(InMainThread=True)
      );

  TJsonRpcMethodData = record
    Request: IJsonRpcRequest;
    Notification: IJsonRpcNotification;
    Client: Pointer;
    ProcKind: TPublishedMethodKind;
    proc: TMethod;
  end;
  PJsonRpcMethodData = ^TJsonRpcMethodData;


  { TCustomJsonRpc }
  {$M+}
  TCustomJsonRpc = class(TObject)
  private
    FOwnTransport: Boolean;
    FTransport: TCustomTransport;
    FPromiseList: IInterfaceList;
    FUsePublishedMethods: Boolean;
    FMethods: TServerMethodsList;
    FOnRequest: TOnJsonRpcRequest;
    FOnRequestASync: TOnJsonRpcRequestASync;
    FOnNotification: TOnJsonRpcNotification;
    FOnNotificationASync: TOnJsonRpcNotificationASync;
    FSyncData: TJsonRpcMethodData;
    procedure SetUsePublishedMethods(const AValue: Boolean);
    function FindRequestMethod(AName: string; out AMethod: TRequestMethod;
        out AKind: TPublishedMethodKind): Boolean;
    function FindNotificationMethod(AName: string; out AMethod: TNotificationMethod;
        out AKind: TPublishedMethodKind): Boolean;
    procedure AppQueueProc(AData: PtrInt);
    procedure SyncPublishedMethod;
  private
    procedure DoReceiveMessage(const AMsg: string;
        const AClient: Pointer; const AThread: TThread);
    //if not Server then AClient ignore
    procedure DoSend(const AMsg: string; const AClient: Pointer); virtual; abstract;
    procedure DoSendResponse(const AId: string; AMsg: string; const AErrorCode: Integer;
        const AClient: Pointer = nil);
    function DoSendRequest(const AMethod, AParams, AId: string;
        const AClient: Pointer): IPromise;
    //on error: AResult.JsonType=jtNull
    function DoSendRequestWait(out AResult: IJsonData;
        const AMethod, AParams: string;
        const AWaitMsec: Integer; const AClient: Pointer; const AId: string): Boolean;
  private
    procedure DoPublishedRequest(const AProc: TRequestMethod; const ARequest: IJsonRpcRequest;
        const AClient: Pointer);
    procedure DoPublishedNotification(const AProc: TNotificationMethod;
        const ANotification: IJsonRpcNotification; const AClient: Pointer);
  protected
    procedure ReleaseTransport;
    function DoRequest(const ARequest: IJsonRpcRequest; const AThread: TThread;
        const AClient: Pointer): Boolean; virtual;
    function DoRequestASync(const ARequest: IJsonRpcRequest; const AClient: Pointer): Boolean; virtual;
    function DoNotification(const ANotification: IJsonRpcNotification; const AThread: TThread): Boolean; virtual;
    function DoNotificationASync(const ANotification: IJsonRpcNotification): Boolean; virtual;

    property Methods: TServerMethodsList read FMethods;
  public
    constructor Create(const ATransport: TCustomTransport;
        const AOwnTransport: Boolean; const AUsePublishedMethods: Boolean); virtual;
    destructor Destroy; override;

    //events called from FTransport thread in DoReceiveMessage()
    property OnRequest: TOnJsonRpcRequest read FOnRequest write FOnRequest;
    property OnNotification: TOnJsonRpcNotification read FOnNotification write FOnNotification;
    //ASync events called im Main Thread by {Application.}QueueAsyncCall
    property OnRequestASync: TOnJsonRpcRequestASync read FOnRequestASync write FOnRequestASync;
    property OnNotificationASync: TOnJsonRpcNotificationASync read FOnNotificationASync write FOnNotificationASync;

    procedure AddPublishedMethods(const AObject: TObject);
    property UsePublishedMethods: Boolean read FUsePublishedMethods write SetUsePublishedMethods;
  end;
  {$M-}

  { TJsonRpcClient }

  TJsonRpcClient = class(TCustomJsonRpc)
  private
    function GetTransport: TCustomTransportClient; inline;
  protected
    procedure DoSend(const AMsg: string; const {%H-}AClient: Pointer); override;
  public
    constructor Create(const ATransport: TCustomTransport;
        const AOwnTransport: Boolean = False;
        const AUsePublishedMethods: Boolean = False); override;

    procedure SendResponse(const AId: string; AMsg: string; const AErrorCode: Integer);
    procedure SendNotification(const AMethod: string; const AParams: string = '');
    //on resolve: IPromise.Value=IJsonData; on reject: IPromise.Value.Kind=ivkError
    function SendRequest(const AMethod: string; AParams: string = ''): IPromise;
    //on error: Result.JsonType=jtNull
    function SendRequestWait(const AMethod: string; AParams: string = '';
        const AWaitMSec: Integer = 3000): IJsonData; overload;
    function SendRequestWait(out AResult: IJsonData;
        const AMethod: string; AParams: string = '';
        const AWaitMSec: Integer = 3000): Boolean; overload;


    property Transport: TCustomTransportClient read GetTransport;
  end;

  { TJsonRpcServer }

  TJsonRpcServer = class(TCustomJsonRpc)
  private
    function GetTransport: TCustomTransportServer; inline;
  protected
    procedure DoSend(const AMsg: string; const AClient: Pointer); override;
  public
    constructor Create(const ATransport: TCustomTransport;
        const AOwnTransport: Boolean = False; const AUsePublishedMethods: Boolean = False); override;

    procedure SendResponse(const AId: string; AMsg: string; const AErrorCode: Integer;
        const AClient: Pointer = nil);
    procedure SendNotification(const AMethod: string; const AParams: string = '';
        const AClient: Pointer = nil);
    //on resolve: IPromise.Value=IJsonData; on reject: IPromise.Value.Kind=ivkError
    function SendRequest(const AClient: Pointer; const AMethod: string;
        const AParams: string = ''): IPromise;
    //on error: Result.JsonType=jtNull
    function SendRequestWait(const AClient: Pointer; const AMethod: string;
        const AParams: string = ''; const AWaitMsec: Integer = 3000): IJsonData; overload;
    function SendRequestWait(out AResult: IJsonData;
        const AClient: Pointer; const AMethod: string;
        const AParams: string = ''; const AWaitMsec: Integer = 3000): Boolean; overload;
  public
    property Transport: TCustomTransportServer read GetTransport;
  end;


implementation

function _ExtractPromise(const APromiseList: IInterfaceList;
    const AId: string; out APromise: IPromise): Boolean;
var
  i: Integer;
begin
  APromiseList.Lock;
  try
    for i := 0 to APromiseList.Count - 1 do
      begin
        APromise := IPromise(APromiseList[i]);
        if APromise.Data.Str = AId then
          begin
            APromiseList.Delete(i);
            Exit(True);
          end;
      end;
    Result := False;
  finally
    APromiseList.Unlock;
  end;
end;


{ TCustomJsonRpc }

type
  tmethodnamerec = packed record
    name : pshortstring;
    addr : pointer;
  end;
  pmethodnamerec = ^tmethodnamerec;

  tmethodnametable = packed record
   count : dword;
   entries : packed array[0..0] of tmethodnamerec;
  end;
  pmethodnametable =  ^tmethodnametable;

  TPublishedMethodInfo = record
    Self: Pointer;
    Addr: Pointer;
    Kind: TPublishedMethodKind;
  end;
  PPublishedMethodInfo = ^TPublishedMethodInfo;

procedure TCustomJsonRpc.AddPublishedMethods(const AObject: TObject);
var
  methodtable: pmethodnametable;
  i : dword;
  ovmt : PVmt;
  r: pmethodnamerec;
  sName, sPrefix: String;
  k: TPublishedMethodKind;
  Info: PPublishedMethodInfo;
begin
  {$R-}
  ovmt := PVmt(AObject.ClassType);
  while assigned(ovmt) do
    begin
       methodtable:=pmethodnametable(ovmt^.vMethodTable);
       if assigned(methodtable) then
         begin
            for i:=0 to methodtable^.count-1 do
              begin
                r := @methodtable^.entries[i];
                sPrefix := Copy(r.name^, 1, 3);
                sName := Copy(r.name^, 4, MaxInt);
                if (sName = '') or (sPrefix[3] <> '_') then Continue;
                k := pmkNone;
                case sPrefix[1] of
                  'r':
                    case sPrefix[2] of
                      't': k := pmkRequestThread;
                      'q': k := pmkRequestQueue;
                      'm': k := pmkRequestMainThread;
                      'a': k := pmkRequestASync;
                    end;
                  'n':
                    case sPrefix[2] of
                      't': k := pmkNotificationThread;
                      'q': k := pmkNotificationQueue;
                      'm': k := pmkNotificationMainThread;
                      'a': k := pmkNotificationASync;
                    end;
                end;//case sPrefix[1]

                if k <> pmkNone then
                  begin
                    New(Info);
                    Info.Self := AObject;
                    Info.Addr := r.addr;
                    Info.Kind := k;
                    FMethods.AddObject(sName, TObject(Info));
                  end;
              end;
         end;
       ovmt := ovmt^.vParent;
    end;
  {$IFDEF DEBUG_MODE}{$R+}{$ENDIF}
end;

procedure TCustomJsonRpc.SetUsePublishedMethods(const AValue: Boolean);

  procedure _ClearMethods;
  var
    i: Integer;
  begin
    for i := 0 to FMethods.Count - 1 do
      Dispose(PPublishedMethodInfo(FMethods.Objects[i]));
    FMethods.Clear;
  end;

begin
  if FUsePublishedMethods = AValue then Exit;
  FUsePublishedMethods := AValue;
  _ClearMethods;
  if AValue then
    AddPublishedMethods(Self);
end;

function TCustomJsonRpc.FindRequestMethod(AName: string; out
  AMethod: TRequestMethod; out AKind: TPublishedMethodKind): Boolean;
var
  i: Integer;
  Info: PPublishedMethodInfo;
begin
  Result := False;
  AMethod := nil;
  if not UsePublishedMethods then Exit;

  i := FMethods.IndexOf(AName);
  Result := i <> -1;
  if Result then
    begin
      Info := PPublishedMethodInfo(FMethods.Objects[i]);
      AKind := Info.Kind;
      Result := AKind in [pmkRequestThread, pmkRequestQueue, pmkRequestMainThread, pmkRequestASync];
      if Result then
        AMethod := TRequestMethod(Method(Info.Addr, Info.Self));
    end;
end;

function TCustomJsonRpc.FindNotificationMethod(AName: string; out
  AMethod: TNotificationMethod; out AKind: TPublishedMethodKind): Boolean;
var
  i: Integer;
  Info: PPublishedMethodInfo;
begin
  Result := False;
  AMethod := nil;
  if not UsePublishedMethods then Exit;

  i := FMethods.IndexOf(AName);
  Result := i <> -1;
  if Result then
    begin
      Info := PPublishedMethodInfo(FMethods.Objects[i]);
      AKind := Info.Kind;
      Result := AKind in [pmkNotificationThread, pmkNotificationQueue,
          pmkNotificationMainThread, pmkNotificationASync];
      if Result then
        AMethod := TNotificationMethod(Method(Info.Addr, Info.Self));
    end;
end;

procedure TCustomJsonRpc.AppQueueProc(AData: PtrInt);
var
  AsyncData: PJsonRpcMethodData absolute AData;
  Data: TJsonRpcMethodData;
begin
  Data := AsyncData^;
  Dispose(AsyncData);

  case Data.ProcKind of
    pmkRequestASync, pmkRequestQueue:
      DoPublishedRequest(TRequestMethod(Data.proc), Data.Request, Data.Client);
    pmkNotificationAsync, pmkNotificationQueue:
      DoPublishedNotification(TNotificationMethod(Data.proc), Data.Notification, Data.Client);
    pmkNone:
      if Assigned(Data.Request) then
        DoRequestASync(Data.Request, Data.Client)
      else if Assigned(Data.Notification) then
        DoNotificationASync(Data.Notification);
  end
end;

procedure TCustomJsonRpc.SyncPublishedMethod;
begin
  case FSyncData.ProcKind of
    pmkRequestMainThread:
      DoPublishedRequest(TRequestMethod(FSyncData.proc), FSyncData.Request, FSyncData.Client);
    pmkNotificationMainThread:
      DoPublishedNotification(TNotificationMethod(FSyncData.proc),
          FSyncData.Notification, FSyncData.Client);
  end;
end;

procedure TCustomJsonRpc.DoReceiveMessage(const AMsg: string;
  const AClient: Pointer; const AThread: TThread);
var
  FRequest: IJsonRpcRequest;
  FNotification: IJsonRpcNotification;
  FSuccess: IJsonRpcSuccess;
  FError: IJsonRpcError;

  procedure _DoAsync(const AProc: TMethod; const AProcKind: TPublishedMethodKind;
      AInMainThread: Boolean);
  var
    Data: PJsonRpcMethodData;
  begin
    New(Data);
    Data.Request := FRequest;
    Data.Notification := FNotification;
    Data.Client := AClient;
    Data.ProcKind := AProcKind;
    Data.proc := AProc;
    {$IFDEF FORMS_ASYNC_CALL}
    if AInMainThread then
      Application.QueueAsyncCall(AppQueueProc, {%H-}PtrInt(Data));{$ELSE}
    QueueAsyncCall(AppQueueProc, {%H-}PtrInt(Data), AInMainThread);{$ENDIF}
  end;

  procedure _DoInMainThread(const AProc: TMethod; const AProcKind: TPublishedMethodKind);
  begin
    FSyncData.Request := FRequest;
    FSyncData.Notification := FNotification;
    FSyncData.Client := AClient;
    FSyncData.ProcKind := AProcKind;
    FSyncData.proc := TMethod(AProc);
    TThread.Synchronize(AThread, SyncPublishedMethod)
  end;

  procedure _DoRequest;
  var
    proc: TRequestMethod;
    ProcKind: TPublishedMethodKind;
  begin
    if not FindRequestMethod(FRequest.Method, proc, ProcKind) then
      begin
        if not DoRequest(FRequest, AThread, AClient) then
          if Assigned(OnRequestASync) then
            _DoAsync(Method(nil, nil), pmkNone, True)
          else
            DoSendResponse(FRequest.ID, '"' + PRC_ERR_METHOD_NOT_FOUND + '"', CODE_METHOD_NOT_FOUND, AClient);
      end
    else
      case ProcKind of
        pmkRequestThread:
          DoPublishedRequest(proc, FRequest, AClient);
        pmkRequestQueue:
          _DoAsync(TMethod(proc), ProcKind, False);
        pmkRequestMainThread:
          _DoInMainThread(TMethod(proc), ProcKind);
        pmkRequestASync:
          _DoAsync(TMethod(proc), ProcKind, True);
      end;
  end;

  procedure _DoNotification;
  var
    proc: TNotificationMethod;
    ProcKind: TPublishedMethodKind;
  begin
    if not FindNotificationMethod(FNotification.Method, proc, ProcKind) then
      begin
        if not DoNotification(FNotification, AThread) then
          if Assigned(OnNotificationASync) then
            _DoAsync(Method(nil, nil), pmkNone, True);
      end
    else
      case ProcKind of
        pmkNotificationThread:
          proc(FNotification.Params, AClient);
        pmkNotificationQueue:
          _DoAsync(TMethod(proc), ProcKind, False);
        pmkNotificationMainThread:
          _DoInMainThread(TMethod(proc), ProcKind);
        pmkNotificationASync:
          _DoAsync(TMethod(proc), ProcKind, True);
      end;
  end;

  procedure _DoSuccesOrError(const AState: TPromiseState; const AId: string);
  var
    p, p1: IPromise;
    v: IVariant;
  begin
    if _ExtractPromise(FPromiseList, AId, p) then
      begin
        if AState = psFulfilled then
          v := FSuccess.Result
        else
          v := ivar_Err(FError.Message, FError.Code);
        p1 := TPromise.DoChain(p, AState, v);
        if not p1.Settled then
          begin
            p1.Data := p.Data;
            FPromiseList.Add(p1); //ToDo ? not add
          end;
      end;
  end;

var
  msg: IJsonRpcMessage;
begin
  case rpc_Parse(AMsg, msg) of
    jrmtRequest:
      if msg.QueryInterface(IJsonRpcRequest, FRequest) = S_OK then
        begin
          _DoRequest;
          {$IFDEF DEBUG_MODE}
          WriteToDebugConsole(Format('  <<<  {"id": "%s", "%s": %s}',
              [FRequest.ID, FRequest.Method, FRequest.Params.AsJsonString]));{$ENDIF}
        end;
    jrmtNotification:
      if msg.QueryInterface(IJsonRpcNotification, FNotification) = S_OK then
        begin
          _DoNotification;
          {$IFDEF DEBUG_MODE}
          WriteToDebugConsole(Format('  <<<  {"%s": %s}',
              [FNotification.Method, FNotification.Params.AsJsonString]));{$ENDIF}
        end;
    jrmtSuccess:
      if msg.QueryInterface(IJsonRpcSuccess, FSuccess) = S_OK then
        begin
          _DoSuccesOrError(psFulfilled, FSuccess.ID);
          {$IFDEF DEBUG_MODE}
          WriteToDebugConsole(Format('  <<<  {"id": "%s", "result": %s}',
              [FSuccess.ID, FSuccess.Result.AsJsonString]));{$ENDIF}
        end;
    jrmtError, jrmtInvalid:
      if msg.QueryInterface(IJsonRpcError, FError) = S_OK then
        begin
          _DoSuccesOrError(psRejected, FError.ID);
          {$IFDEF DEBUG_MODE}
          WriteToDebugConsole(Format('  <<<  {"id": "%s", "code": %d, "message": "%s", "data": %s}',
              [FError.ID, FError.Code, FError.Message, FError.Data.AsJsonString]));{$ENDIF}
        end;
  end;
end;

function TCustomJsonRpc.DoSendRequest(const AMethod, AParams, AId: string;
  const AClient: Pointer): IPromise;
begin
  Result := TPromise.New(AId);
  FPromiseList.Add(Result);
  DoSend(rpc_Request(AId, AMethod, AParams), AClient);
end;

function TCustomJsonRpc.DoSendRequestWait(out AResult: IJsonData;
  const AMethod, AParams: string; const AWaitMsec: Integer;
  const AClient: Pointer; const AId: string): Boolean;
var
  pr: IPromise;
begin
  pr := TPromise.New(AId);
  FPromiseList.Add(pr);
  DoSend(rpc_Request(AId, AMethod, AParams), AClient);
  pr.Wait(AWaitMsec);
  case pr.State of
    psFulfilled:
      begin
        Result := True;
        AResult := IJsonData(pr.Value.Intf);
      end
    //psRejected:
    //  Result := json_CreateString(pr.Value.Str)
    else
      begin
        Result := False;
        AResult := json_CreateNull;
      end;
  end;
end;

procedure TCustomJsonRpc.DoPublishedRequest(const AProc: TRequestMethod;
  const ARequest: IJsonRpcRequest; const AClient: Pointer);
var
  sResult: string;
  ErrCode: Integer = 0;
begin
  try
    AProc(ARequest.Params, sResult, ErrCode);
  except
    on E: Exception do
      begin
        ErrCode := CODE_INTERNAL_ERROR;
        sResult := E.Message;
      end;
  end;
  DoSendResponse(ARequest.ID, sResult, ErrCode, AClient);
end;

procedure TCustomJsonRpc.DoPublishedNotification(const AProc: TNotificationMethod;
  const ANotification: IJsonRpcNotification; const AClient: Pointer);
begin
  try
    AProc(ANotification.Params, AClient);
  except end;
end;

procedure TCustomJsonRpc.ReleaseTransport;
begin
  if FOwnTransport then
    FTransport.Free;
  FTransport := nil;
end;

function TCustomJsonRpc.DoRequest(const ARequest: IJsonRpcRequest;
  const AThread: TThread; const AClient: Pointer): Boolean;
var
  sResult: string;
  ErrCode: Integer = 0;
begin
  Result := Assigned(FOnRequest) and FOnRequest(ARequest, AThread, sResult, ErrCode);
  if Result then
    DoSendResponse(ARequest.ID, sResult, ErrCode, AClient);
end;

function TCustomJsonRpc.DoRequestASync(const ARequest: IJsonRpcRequest;
  const AClient: Pointer): Boolean;
var
  sResult: string;
  ErrCode: Integer = 0;
begin
  Result := Assigned(FOnRequestASync) and FOnRequestASync(ARequest, sResult, ErrCode);
  if Result then
    DoSendResponse(ARequest.ID, sResult, ErrCode, AClient)
  else
    DoSendResponse(ARequest.ID, sResult,
        IfThen(ErrCode = 0, CODE_INTERNAL_ERROR, ErrCode), AClient);
end;

function TCustomJsonRpc.DoNotification(
  const ANotification: IJsonRpcNotification; const AThread: TThread): Boolean;
begin
  Result := Assigned(OnNotification) and OnNotification(ANotification, AThread);
end;

function TCustomJsonRpc.DoNotificationASync(
  const ANotification: IJsonRpcNotification): Boolean;
begin
  Result := Assigned(OnNotificationASync);
  if Result then
    OnNotificationASync(ANotification);
end;

constructor TCustomJsonRpc.Create(const ATransport: TCustomTransport;
  const AOwnTransport: Boolean; const AUsePublishedMethods: Boolean);
begin
  {$IFDEF DEBUG_MODE}
  if ATransport = nil then
    raise Exception.Create('TCustomJsonRpc.Create(): ATransport = nil');{$ENDIF}
  FOwnTransport := AOwnTransport;
  FTransport := ATransport;
  FTransport.OnReceive := DoReceiveMessage;
  FPromiseList := TInterfaceList.Create;
  FMethods  := TServerMethodsList.Create;
  UsePublishedMethods := AUsePublishedMethods;
end;

destructor TCustomJsonRpc.Destroy;
begin
  FTransport.OnReceive := nil;
  {$IFDEF FORMS_ASYNC_CALL}
  Application.RemoveAsyncCalls(Self);{$ELSE}
  RemoveAsyncCalls(Self);{$ENDIF}
  ReleaseTransport;
  UsePublishedMethods := False;//to clear FMethods.Objects;
  FreeAndNil(FMethods);
  inherited Destroy;
end;

procedure TCustomJsonRpc.DoSendResponse(const AId: string; AMsg: string;
  const AErrorCode: Integer; const AClient: Pointer);
var
  S: String;
begin
  if AErrorCode = 0 then
    S := rpc_Success(AId, AMsg)
  else
    begin
      if AMsg = '' then
        begin
          case AErrorCode of
            CODE_INVALID_REQUEST: AMsg := PRC_ERR_INVALID_REQUEST;
            CODE_METHOD_NOT_FOUND: AMsg := PRC_ERR_METHOD_NOT_FOUND;
            CODE_INVALID_PARAMS: AMsg := RPC_ERR_INVALID_PARAMS;
            CODE_INTERNAL_ERROR: AMsg := RPC_ERR_INTERNAL_ERROR;
            CODE_PARSE_ERROR: AMsg := RPC_ERR_PARSE_ERROR;
          end;
          AMsg := '"' + AMsg + '"';
        end;
      S := rpc_Error(AId, AErrorCode, AMsg);
    end;
  DoSend(S, AClient);
end;


{ TJsonRpcServer }

function TJsonRpcServer.GetTransport: TCustomTransportServer;
begin
  Result := TCustomTransportServer(FTransport);
end;

procedure TJsonRpcServer.DoSend(const AMsg: string; const AClient: Pointer);
begin
  FTransport.Send(AMsg, AClient);
end;

procedure TJsonRpcServer.SendNotification(const AMethod: string;
  const AParams: string; const AClient: Pointer);
begin
  FTransport.Send(rpc_Notification(AMethod, AParams), AClient);
end;

function TJsonRpcServer.SendRequest(const AClient: Pointer;
  const AMethod: string; const AParams: string): IPromise;
begin
  if AClient = nil then
    Exit(TPromise.Resolve(0));

  Result := DoSendRequest(AMethod, AParams, FTransport.NextId, AClient);
end;

function TJsonRpcServer.SendRequestWait(const AClient: Pointer;
  const AMethod: string; const AParams: string; const AWaitMsec: Integer): IJsonData;
begin
  if AClient = nil then
    Exit(json_CreateNull);

  DoSendRequestWait(Result, AMethod, AParams, AWaitMsec, AClient, FTransport.NextId);
end;

function TJsonRpcServer.SendRequestWait(out AResult: IJsonData;
  const AClient: Pointer; const AMethod: string; const AParams: string;
  const AWaitMsec: Integer): Boolean;
begin
  if AClient = nil then
    Exit(False);

  Result := DoSendRequestWait(AResult, AMethod, AParams, AWaitMsec, AClient, FTransport.NextId);
end;

constructor TJsonRpcServer.Create(const ATransport: TCustomTransport;
  const AOwnTransport: Boolean; const AUsePublishedMethods: Boolean);
begin
  {$IFDEF DEBUG_MODE}
  if not (ATransport is TCustomTransportServer) then
    raise Exception.Create('TJsonRpcServer.Create(): ATransport is not TCustomTransportServer');
  {$ENDIF}
  inherited Create(ATransport, AOwnTransport, AUsePublishedMethods);
end;

procedure TJsonRpcServer.SendResponse(const AId: string; AMsg: string;
  const AErrorCode: Integer; const AClient: Pointer);
begin
  DoSendResponse(AId, AMsg, AErrorCode, AClient);
end;

{ TJsonRpcClient }

function TJsonRpcClient.GetTransport: TCustomTransportClient;
begin
  Result := TCustomTransportClient(FTransport);
end;

procedure TJsonRpcClient.DoSend(const AMsg: string; const AClient: Pointer);
begin
  FTransport.Send(AMsg);
end;

constructor TJsonRpcClient.Create(const ATransport: TCustomTransport;
  const AOwnTransport: Boolean; const AUsePublishedMethods: Boolean);
begin
  {$IFDEF DEBUG_MODE}
  if not (ATransport is TCustomTransportClient) then
    raise Exception.Create('TJsonRpcClient.Create(): ATransport is not TCustomTransportClient');
  {$ENDIF}
  inherited Create(ATransport, AOwnTransport, AUsePublishedMethods);
end;

procedure TJsonRpcClient.SendResponse(const AId: string; AMsg: string;
  const AErrorCode: Integer);
begin
  DoSendResponse(AId, AMsg, AErrorCode, nil);
end;

procedure TJsonRpcClient.SendNotification(const AMethod: string;
  const AParams: string);
begin
  FTransport.Send(rpc_Notification(AMethod, AParams));
end;

function TJsonRpcClient.SendRequest(const AMethod: string; AParams: string): IPromise;
begin
  Result := DoSendRequest(AMethod, AParams, FTransport.NextId, nil);
end;

function TJsonRpcClient.SendRequestWait(const AMethod: string; AParams: string;
  const AWaitMSec: Integer): IJsonData;
begin
  DoSendRequestWait(Result, AMethod, AParams, AWaitMSec, nil, FTransport.NextId);
end;

function TJsonRpcClient.SendRequestWait(out AResult: IJsonData;
  const AMethod: string; AParams: string; const AWaitMSec: Integer): Boolean;
begin
  Result := DoSendRequestWait(AResult, AMethod, AParams, AWaitMSec, nil, FTransport.NextId);
end;


end.

