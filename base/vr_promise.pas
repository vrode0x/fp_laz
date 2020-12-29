unit vr_promise;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, vr_classes, Types, vr_utils, vr_fpclasses, vr_variant;

type
  IPromise = interface;
  TVariantDynArray = array of IVariant;

  TPromiseResolveMethod = function(const AValue: IVariant; const AData: IVariant): IPromise of object;
  TPromiseResolveProc = function(const AValue: IVariant; const AData: IVariant): IPromise;
  TPromiseRejectMethod = function(const AReason: string; const AData: IVariant): IPromise of object;
  TPromiseRejectProc = function(const AReason: string; const AData: IVariant): IPromise;
  TPromiseFinallyProc = procedure(const AData: IVariant);
  TPromiseFinallyMethod = procedure(const AData: IVariant) of object;

  TPromiseState = ({psNone,} psPending, {psSealed,} psFulfilled, psRejected);

  { IPromise }

  IPromise = interface
    ['{B2303D03-5F01-4907-A80D-E39DC88767C3}']
    function GetData: IVariant;
    function GetSettled: Boolean;
    function GetState: TPromiseState;
    function GetValue: IVariant;
    procedure SetData(const AValue: IVariant);

    function ThenP(const AOnFulfillment: TPromiseResolveProc;
        const AData: IVariant; const AOnRejection: TPromiseRejectProc = nil;
        const AInMainThread: Boolean = False): IPromise;
    function ThenM(const AOnFulfillment: TPromiseResolveMethod;
        const AData: IVariant; const AOnRejection: TPromiseRejectMethod = nil;
        const AInMainThread: Boolean = False): IPromise;
    function CatchP(const AOnRejection: TPromiseRejectProc; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function CatchM(const AOnRejection: TPromiseRejectMethod; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function FinallyP(const ACallbak: TPromiseFinallyProc; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function FinallyM(const ACallbak: TPromiseFinallyMethod; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;

    function Wait(AWaitMSec: Cardinal = 3000): IVariant;

    property State: TPromiseState read GetState;
    property Settled: Boolean read GetSettled;
    property Value: IVariant read GetValue;
    property Data: IVariant read GetData write SetData;
  end;

  TResolverResolveMethod = procedure(const AValue: IVariant) of object;
  TResolverRejectMethod = procedure(const AReason: string) of object;
  TPromiseCreateProc = procedure(const AResolve: TResolverResolveMethod;
      const AData: IVariant; AReject: TResolverRejectMethod);
  TPromiseCreateMethod = procedure(const AResolve: TResolverResolveMethod;
      const AData: IVariant; AReject: TResolverRejectMethod) of object;


//not overload to use Code Completion on AResolver (Ctrl+Shift+C)
function PromiseP(const AResolver: TPromiseCreateProc; const AData: IVariant;
    const InMainThread: Boolean = False): IPromise; //overload; //inline;
function PromiseM(const AResolver: TPromiseCreateMethod; const AData: IVariant;
    const InMainThread: Boolean = False): IPromise; //overload; //inline;


type
  TPromiseDynArray = array of IPromise;
  TPromiseSettledInfo = record
    FullFilled: TVariantDynArray;
    Rejected: TStringDynArray;
  end;
  PPromiseSettledInfo = ^TPromiseSettledInfo;
  TPromiseSettledInfoArray = array of TPromiseSettledInfo;

  { TPromise }

  TPromise = class
  public
    class function New(const AData: IVariant): IPromise;
    class function Resolve(const AValue: IVariant): IPromise;
    class function Reject(const AReason: string): IPromise;
    class function DoChain(const APromise: IPromise;
        const ANewState: TPromiseState; const AValue: IVariant): IPromise;

    {* Wait for all promises to be resolved, or for any to be rejected.
       If the returned promise resolves, it is resolved with an aggregating array
       of the values from the resolved promises in the same order as defined in
       the iterable of multiple promises. If it rejects, it is rejected with the
       reason from the first promise in the iterable that was rejected.
       : IPromise.Value=IInterfaceList of IVariant *}
    class function All(const APromises: TPromiseDynArray): IPromise;

    {* Wait until all promises have settled (each may resolve, or reject).
       Returns a promise that resolves after all of the given promises have
       either resolved or rejected, with an array of objects that each
       describe the outcome of each promise.
       : Use PromiseSettledInfoFromVariant(IPromise.Value, Info) *}
    class function AllSettled(const APromises: TPromiseDynArray): IPromise;

    {* Wait until any of the promises is resolved or rejected.
       If the returned promise resolves, it is resolved with the value of
       the first promise in the iterable that resolved. If it rejects, it
       is rejected with the reason from the first promise that was rejected. *}
    class function Race(const APromises: TPromiseDynArray): IPromise;
  end;

//use for IPromise.Value returned by TPromise.AllSettled
function PromiseSettledInfoFromVariant(const v: IVariant): TPromiseSettledInfo;
procedure PromiseSettledInfoToVariant(var v: IVariant; constref AInfo: TPromiseSettledInfo);

implementation

type

  TPromiseInternal = class;

  IPromiseInternal = interface
    ['{73497ABA-D609-4A0A-8699-ABA2A6E7DD7E}']
    function GetPromise: TPromiseInternal;
    procedure Call(const APrevPromise: IPromise);
  end;

  { TPromiseInternal }

  TPromiseInternal = class(TInterfacedObject, IPromise, IPromiseInternal)
  private
    FData: IVariant;
    FState: TPromiseState;
    FLock: TRTLCriticalSection;
    FValue: IVariant;
    FThen: IPromise;
    function GetData: IVariant;
    function GetSettled: Boolean;
    function GetState: TPromiseState;
    function GetThenPromise: IPromise;
    function GetValue: IVariant;
    procedure SetData(const AValue: IVariant);
    procedure SetState(const AValue: TPromiseState);
    procedure SetThenPromise(const AValue: IPromise);
    procedure SetValue(const AValue: IVariant);
  protected
    function DoChain(const ANewState: TPromiseState; const AValue: IVariant): IPromise;
    property ThenPromise: IPromise read GetThenPromise write SetThenPromise;
    function ThenPromiseObject: TPromiseInternal;
    procedure DoCall(const {%H-}APrevPromise: TPromiseInternal); virtual;
    procedure ResolvePromise(const AValue: IVariant);
    procedure RejectPromise(const AReason: string);
  protected
    { IPromiseInternal }
    function GetPromise: TPromiseInternal;
    procedure Call(const APrevPromise: IPromise);
  public
    constructor Create(const AData: IVariant); virtual;
    destructor Destroy; override;
    procedure DoSettledLike(const APromise: TPromiseInternal); overload;
    procedure DoSettledLike(const APromise: IPromise); overload;
    { IPromise }
    function ThenP(const AOnFulfillment: TPromiseResolveProc;
        const AData: IVariant; const AOnRejection: TPromiseRejectProc;
        const AInMainThread: Boolean = False): IPromise;
    function ThenM(const AOnFulfillment: TPromiseResolveMethod;
        const AData: IVariant; const AOnRejection: TPromiseRejectMethod;
        const AInMainThread: Boolean = False): IPromise;
    function CatchP(const AOnRejection: TPromiseRejectProc; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function CatchM(const AOnRejection: TPromiseRejectMethod; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function FinallyP(const ACallbak: TPromiseFinallyProc; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;
    function FinallyM(const ACallbak: TPromiseFinallyMethod; const AData: IVariant;
        const AInMainThread: Boolean = False): IPromise;

    function Wait(AWaitMSec: Cardinal = 3000): IVariant;

    property State: TPromiseState read GetState;// write SetState;
    property Settled: Boolean read GetSettled;
    property Value: IVariant read GetValue write SetValue;
    property Data: IVariant read GetData write SetData;
  end;

  { TPromiseResolver }

  TPromiseResolver = class(TPromiseInternal)
  private
    FResolverProc: TPromiseCreateProc;
    FResolverMethod: TPromiseCreateMethod;
    FInMainThread: Boolean;
    procedure Init(const AInMainThread: Boolean);
  private
    FSyncPromise: IPromise;
    procedure CallSync;
    procedure CallResolver;
    procedure OnExecuteThread(const Sender: TObject; const {%H-}AData: Pointer = nil);
  public
    constructor Create(const AResolver: TPromiseCreateProc; const AData: IVariant;
        const AInMainThread: Boolean = False); overload;
    constructor Create(const AResolver: TPromiseCreateMethod; const AData: IVariant;
        const AInMainThread: Boolean = False); overload;
    destructor Destroy; override;
  end;

  { TPromiseThen }

  TPromiseThen = class(TPromiseInternal)
  private
    FOnFulfillmentProc: TPromiseResolveProc;
    FOnRejectionProc: TPromiseRejectProc;
    FOnFulfillmentMethod: TPromiseResolveMethod;
    FOnRejectionMethod: TPromiseRejectMethod;
    FInMainThread: Boolean;
    FSyncValue: IVariant;
    FSyncResult: IPromise;
    procedure DoSyncResolve;
    procedure DoSyncReject;
  protected
    procedure DoCall(const APrevPromise: TPromiseInternal); override;
  public
    constructor Create(const AOnFulfillment: TPromiseResolveProc;
         const AData: IVariant; const AOnRejection: TPromiseRejectProc;
         const AInMainThread: Boolean = False); overload;
    constructor Create(const AOnFulfillment: TPromiseResolveMethod;
         const AData: IVariant; const AOnRejection: TPromiseRejectMethod;
         const AInMainThread: Boolean = False); overload;
  end;

  { TPromiseFinally }

  TPromiseFinally = class(TPromiseInternal)
  private
    FProc: TPromiseFinallyProc;
    FMethod: TPromiseFinallyMethod;
    FInMainThread: Boolean;
    procedure DoSyncCall;
  protected
    procedure DoCall(const APrevPromise: TPromiseInternal); override;
  public
    constructor Create(const ACallbak: TPromiseFinallyProc; const AData: IVariant;
        const AInMainThread: Boolean); overload;
    constructor Create(const ACallbak: TPromiseFinallyMethod; const AData: IVariant;
        const AInMainThread: Boolean); overload;
  end;

  TPromiseSettledVariant = TICustomVariant<TPromiseSettledInfo>;

function PromiseSettledInfoFromVariant(const v: IVariant): TPromiseSettledInfo;
var
  obj: TPromiseSettledVariant;
begin
  if Supports(v, TPromiseSettledVariant, obj) then
    Result := obj.Data;
end;

procedure PromiseSettledInfoToVariant(var v: IVariant; constref AInfo: TPromiseSettledInfo);
begin
  v := TPromiseSettledVariant.Create(AInfo);
end;

function PromiseP(const AResolver: TPromiseCreateProc; const AData: IVariant;
  const InMainThread: Boolean): IPromise;
begin
  Result := TPromiseResolver.Create(AResolver, AData, InMainThread);
end;

function PromiseM(const AResolver: TPromiseCreateMethod; const AData: IVariant;
  const InMainThread: Boolean): IPromise;
begin
  Result := TPromiseResolver.Create(AResolver, AData, InMainThread);
end;

{ TPromiseFinally }

procedure TPromiseFinally.DoSyncCall;
begin
  try
    if Assigned(FProc) then
      FProc(FData)
    else if Assigned(FMethod) then
      FMethod(FData);
  except end;
end;

procedure TPromiseFinally.DoCall(const APrevPromise: TPromiseInternal);
begin
  if FInMainThread and not IsMainThread then
    TThread.Synchronize(TThread.CurrentThread, DoSyncCall)
  else
    DoSyncCall;
  DoSettledLike(APrevPromise);
end;

constructor TPromiseFinally.Create(const ACallbak: TPromiseFinallyProc;
  const AData: IVariant; const AInMainThread: Boolean);
begin
  Create(AData);
  FProc := ACallbak;
  FInMainThread := AInMainThread;
end;

constructor TPromiseFinally.Create(const ACallbak: TPromiseFinallyMethod;
  const AData: IVariant; const AInMainThread: Boolean);
begin
  Create(AData);
  FMethod := ACallbak;
  FInMainThread := AInMainThread;
end;

{ TPromiseThen }

procedure TPromiseThen.DoSyncResolve;
begin
  if Assigned(FOnFulfillmentProc) then
    FSyncResult := FOnFulfillmentProc(FSyncValue, FData)
  else if Assigned(FOnFulfillmentMethod) then
    FSyncResult := FOnFulfillmentMethod(FSyncValue, FData);
end;

procedure TPromiseThen.DoSyncReject;
begin
  if Assigned(FOnRejectionProc) then
    FSyncResult := FOnRejectionProc(FSyncValue, FData)
  else if Assigned(FOnRejectionMethod) then
    FSyncResult := FOnRejectionMethod(FSyncValue, FData);
end;

procedure TPromiseThen.DoCall(const APrevPromise: TPromiseInternal);
var
  sError: String;
  //vValue: IVariant;
  //Result: IPromise = nil;
begin
  try
    FSyncResult := nil;
    FSyncValue := APrevPromise.Value;
    if APrevPromise.State = psRejected then
      begin
        if FInMainThread and not IsMainThread then
          TThread.Synchronize(TThread.CurrentThread, DoSyncReject)
        else
          DoSyncReject;
      end
    else if FInMainThread and not IsMainThread then
      TThread.Synchronize(TThread.CurrentThread, DoSyncResolve)
    else
      DoSyncResolve;
    //vValue := APrevPromise.Value;
    //if APrevPromise.State = psRejected then
    //  begin
    //    if Assigned(FOnRejectionProc) then
    //      Result := FOnRejectionProc(vValue, FData)
    //    else if Assigned(FOnRejectionMethod) then
    //      Result := FOnRejectionMethod(vValue, FData);
    //  end
    //else if Assigned(FOnFulfillmentProc) then
    //  Result := FOnFulfillmentProc(vValue, FData)
    //else if Assigned(FOnFulfillmentMethod) then
    //  Result := FOnFulfillmentMethod(vValue, FData);
  except
    on E: Exception do
      sError := E.Message;
  end;

  if FSyncResult = nil then
    FSyncResult := TPromise.Reject(IfThen(sError = '', 'Then handler return nil', sError))
  else
    FSyncResult.Wait;

  DoSettledLike(FSyncResult);
  FSyncResult := nil;
end;

constructor TPromiseThen.Create(const AOnFulfillment: TPromiseResolveProc;
  const AData: IVariant; const AOnRejection: TPromiseRejectProc;
  const AInMainThread: Boolean);
begin
  Create(AData);
  FOnFulfillmentProc := AOnFulfillment;
  FOnRejectionProc := AOnRejection;
  FInMainThread := AInMainThread;
end;

constructor TPromiseThen.Create(const AOnFulfillment: TPromiseResolveMethod;
  const AData: IVariant; const AOnRejection: TPromiseRejectMethod;
  const AInMainThread: Boolean);
begin
  Create(AData);
  FOnFulfillmentMethod := AOnFulfillment;
  FOnRejectionMethod := AOnRejection;
  FInMainThread := AInMainThread;
end;

{ TPromiseResolver }

procedure TPromiseResolver.Init(const AInMainThread: Boolean);
begin
  _AddRef;//in OnExecuteThread _Release
  FInMainThread := AInMainThread and (GetCurrentThreadId <> MainThreadID);// not IsMainThread();
  if FInMainThread then
    OnExecuteThread(TThread.CurrentThread)
  else
    new_ThreadM(OnExecuteThread);
end;

constructor TPromiseResolver.Create(const AResolver: TPromiseCreateProc;
  const AData: IVariant; const AInMainThread: Boolean);
begin
  Create(AData);
  FResolverProc := AResolver;
  Init(AInMainThread);
end;

constructor TPromiseResolver.Create(const AResolver: TPromiseCreateMethod;
  const AData: IVariant; const AInMainThread: Boolean);
begin
  Create(AData);
  FResolverMethod := AResolver;
  Init(AInMainThread);
end;

destructor TPromiseResolver.Destroy;
begin
  inherited Destroy;
end;

procedure TPromiseResolver.CallSync;
var
  NextPromise: IPromise;
begin
  NextPromise := (FSyncPromise as IPromiseInternal).GetPromise.FThen;
  if NextPromise <> nil then
    (NextPromise as IPromiseInternal).Call(FSyncPromise);
end;

procedure TPromiseResolver.CallResolver;
begin
  if Assigned(FResolverProc) then
    FResolverProc(ResolvePromise, FData, RejectPromise)
  else if Assigned(FResolverMethod) then
    FResolverMethod(ResolvePromise, FData, RejectPromise);
end;

type
  TAccessThread = class(TThread) end;
procedure TPromiseResolver.OnExecuteThread(const Sender: TObject;
  const AData: Pointer);
var
  th: TAccessThread absolute Sender;
  //CurrPromise: TPromiseInternal;
  //NextPromise: TPromiseInternal = nil;
  CurrPromise,  NextPromise: IPromise;
begin
  try
    if FInMainThread then
      TThread.Synchronize(th, CallResolver)
    else
     CallResolver;

    if not Settled then
      begin
        //RejectPromise('Not settled');
        _Release;
        Exit;
      end;

    CurrPromise := Self;
    NextPromise := FThen;
    while True do
      begin
        if NextPromise <> nil then
          begin
            if FInMainThread then
              begin
                FSyncPromise := CurrPromise;
                TThread.Synchronize(th, CallSync);
              end
            else
              (NextPromise as IPromiseInternal).Call(CurrPromise);
            CurrPromise := NextPromise;
            NextPromise := (NextPromise as IPromiseInternal).GetPromise.FThen;
          end
        else
          begin
            CurrPromise := nil;
            if frefcount = 1 then
              begin
                //th.Terminate;
                _Release;//in construnctor _AddRef
                Exit;
              end
            else
              Sleep(10);
          end;
      end;
  //CurrPromise := Self;
  //while True do
  //  begin       //ToDo ? merge with DoChain();
  //    if (NextPromise = nil) then
  //      NextPromise := CurrPromise.ThenPromiseObject;
  //    if NextPromise <> nil then
  //      begin
  //        NextPromise.DoCall(CurrPromise);
  //        CurrPromise := NextPromise;
  //        NextPromise := nil;
  //      end
  //    else if frefcount = 1 then
  //      begin
  //        //th.Terminate;
  //        _Release;//in construnctor _AddRef
  //        Exit;
  //      end
  //    else
  //      Sleep(10);
  //  end;
  except
    //on E: Exception do
    //  RejectPromise(E.Message);
  end;
end;

{ TPromise }

class function TPromise.New(const AData: IVariant): IPromise;
var
  Prom: TPromiseInternal;
begin
  Result := TPromiseInternal.Create(nil);
  Prom := (Result as IPromiseInternal).GetPromise;
  Prom.Data := AData;
end;

class function TPromise.Resolve(const AValue: IVariant): IPromise;
var
  Prom: TPromiseInternal;
begin
  Result := TPromiseInternal.Create(nil);
  Prom := (Result as IPromiseInternal).GetPromise;
  Prom.FState := psFulfilled;
  Prom.FValue := AValue;
end;

class function TPromise.Reject(const AReason: string): IPromise;
var
  Prom: TPromiseInternal;
begin
  Result := TPromiseInternal.Create(nil);
  Prom := (Result as IPromiseInternal).GetPromise;
  Prom.FState := psRejected;
  Prom.FValue := AReason;
end;

class function TPromise.DoChain(const APromise: IPromise;
  const ANewState: TPromiseState; const AValue: IVariant): IPromise;
begin
  Result := (APromise as IPromiseInternal).GetPromise.DoChain(ANewState, AValue);
end;

function _PromiseArrayToVariantList(const arr: TPromiseDynArray): IVariant;
var
  i: Integer;
  lst: IInterfaceList;
begin
  lst := TInterfaceList.Create;
  for i := 0 to Length(arr) - 1 do
    lst.Add(arr[i]);
  Result := lst;
end;

procedure _OnPromiseAll(const AResolve: TResolverResolveMethod;
  const AData: IVariant; AReject: TResolverRejectMethod);
var
  lst: IInterfaceList;
  lstResult: IInterfaceList;
  i, Count: Integer;
  Prom: IPromise;
  vResult: IVariant;
begin
  lst := IInterfaceList(AData.Intf);
  Count := lst.Count;
  lstResult := TInterfaceList.Create;
  for i := 0 to lst.Count - 1 do
    lstResult.Add(lst[i]);
  while True do
    begin
      if Count = 0 then
        Break;
      for i := 0 to lst.Count - 1 do
        if lstResult[i] = nil then
          begin
            Prom := IPromise(lst[i]);
            case Prom.State of
              psRejected:
                begin
                  AReject(Prom.Value);
                  Exit;
                end;
              psFulfilled:
                begin
                  Dec(Count);
                  lstResult[i] := Prom.Value;
                end;
            end;
          end;
      Sleep(10);
    end;
  vResult := lstResult;
  AResolve(vResult);
end;


class function TPromise.All(const APromises: TPromiseDynArray): IPromise;
begin
  Result := PromiseP(_OnPromiseAll, _PromiseArrayToVariantList(APromises));
end;

procedure _OnPromiseAllSettled(const AResolve: TResolverResolveMethod;
  const AData: IVariant; {%H-}AReject: TResolverRejectMethod);
var
  lst: IInterfaceList;
  arrFlag: TBooleanDynArray;
  r: TPromiseSettledInfo;
  i, Count, n: Integer;
  Prom: IPromise;
  vResult: IVariant;
begin
  lst := IInterfaceList(AData.Intf);
  Count := lst.Count;
  SetLength(arrFlag, Count);
  while True do
    begin
      if Count = 0 then
        Break;
      for i := 0 to lst.Count - 1 do
        if not arrFlag[i] then
          begin
            Prom := IPromise(lst[i]);
            case Prom.State of
              psRejected:
                begin
                  arrFlag[i] := True;
                  Dec(Count);
                  n := Length({%H-}r.Rejected);
                  SetLength(r.Rejected, n + 1);
                  r.Rejected[n] := Prom.Value;
                end;
              psFulfilled:
                begin
                  arrFlag[i] := True;
                  Dec(Count);
                  n := Length(r.FullFilled);
                  SetLength(r.FullFilled, n + 1);
                  r.FullFilled[n] := Prom.Value;
                end;
            end;
          end;
      Sleep(10);
    end;
  PromiseSettledInfoToVariant(vResult{%H-}, r);
  AResolve(vResult);
end;

class function TPromise.AllSettled(const APromises: TPromiseDynArray): IPromise;
begin
  Result := PromiseP(_OnPromiseAllSettled, _PromiseArrayToVariantList(APromises));
end;

procedure _OnPromiseRace(const AResolve: TResolverResolveMethod;
  const AData: IVariant; AReject: TResolverRejectMethod);
var
  lst: IInterfaceList;
  Prom: IPromise;
  i: Integer;
begin
  lst := IInterfaceList(AData.Intf);
  while True do
    begin
      for i := 0 to lst.Count - 1 do
        begin
          Prom := IPromise(lst[i]);
          case Prom.State of
            psRejected:
              begin
                AReject(Prom.Value);
                Exit;
              end;
            psFulfilled:
              begin
                AResolve(Prom.Value);
                Exit;
              end;
          end;
        end;
      Sleep(10);
    end;
end;

class function TPromise.Race(const APromises: TPromiseDynArray): IPromise;
begin
  Result := PromiseP(_OnPromiseRace, _PromiseArrayToVariantList(APromises));
end;

{ TPromiseInternal }

function TPromiseInternal.GetSettled: Boolean;
begin
  EnterCriticalsection(FLock);
  try
    Result := FState in [psFulfilled, psRejected];
  finally
    LeaveCriticalsection(FLock);
  end;
end;

function TPromiseInternal.GetData: IVariant;
begin
  EnterCriticalsection(FLock);
  try
    Result := FData;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

function TPromiseInternal.GetState: TPromiseState;
begin
  EnterCriticalsection(FLock);
  try
    Result := FState;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

function TPromiseInternal.GetThenPromise: IPromise;
begin
  EnterCriticalsection(FLock);
  try
    Result := FThen;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

function TPromiseInternal.GetValue: IVariant;
begin
  EnterCriticalsection(FLock);
  try
    Result := FValue;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

procedure TPromiseInternal.SetData(const AValue: IVariant);
begin
  EnterCriticalsection(FLock);
  try
    FData := AValue;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

procedure TPromiseInternal.SetThenPromise(const AValue: IPromise);
begin
  EnterCriticalsection(FLock);
  try
    FThen := AValue;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

procedure TPromiseInternal.SetValue(const AValue: IVariant);
begin
  EnterCriticalsection(FLock);
  try
    FValue := AValue;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

function TPromiseInternal.DoChain(const ANewState: TPromiseState;
  const AValue: IVariant): IPromise;
var
  NextPromise: IPromise;
begin
  Result := Self;
  if not Settled then
    begin
      if ANewState = psPending then Exit;
      SetState(ANewState);
      Value := AValue;
    end;
  NextPromise := FThen;
  while NextPromise <> nil do
    begin
      (NextPromise as IPromiseInternal).Call(Result);
      Result := NextPromise;
      if not Result.Settled then
        Exit;
      NextPromise := (NextPromise as IPromiseInternal).GetPromise.FThen;
    end;
end;

function TPromiseInternal.ThenPromiseObject: TPromiseInternal;
begin
  EnterCriticalsection(FLock);
  try
    if FThen = nil then
      Result := nil
    else
      Result := (FThen as IPromiseInternal).GetPromise;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

procedure TPromiseInternal.DoCall(const APrevPromise: TPromiseInternal);
begin
  RejectPromise('Not implemented');
end;

function TPromiseInternal.GetPromise: TPromiseInternal;
begin
  Result := Self;
end;

procedure TPromiseInternal.Call(const APrevPromise: IPromise);
begin
  DoCall((APrevPromise as IPromiseInternal).GetPromise);
end;

procedure TPromiseInternal.ResolvePromise(const AValue: IVariant);
var
  Prom: IPromise;
begin
  if Settled then
    Exit;
  if Supports(AValue.Intf, IPromise, Prom) and
      (Prom = IPromise(Self)) then
    raise Exception.Create('A promise cannot be resolved with itself');
  //case VarType(AValue) of
  //  var
  //end;
  SetState(psFulfilled);
  Value := AValue;

  //DoResolve(AValue, Self);
end;

procedure TPromiseInternal.RejectPromise(const AReason: string);
begin
  if Settled then
    Exit;
  SetState(psRejected);
  Value := AReason;
end;

procedure TPromiseInternal.SetState(const AValue: TPromiseState);
begin
  EnterCriticalsection(FLock);
  try
    FState := AValue;
  finally
    LeaveCriticalsection(FLock);
  end;
end;

constructor TPromiseInternal.Create(const AData: IVariant);
begin
  FData := AData;
  InitCriticalSection(FLock);
end;

destructor TPromiseInternal.Destroy;
begin
  EnterCriticalsection(FLock);
  try
    inherited Destroy;
  finally
    LeaveCriticalsection(FLock);
    DoneCriticalsection(FLock);
  end;
end;

procedure TPromiseInternal.DoSettledLike(const APromise: TPromiseInternal);
begin
  if Settled then Exit;
  SetState(APromise.State);
  Value := APromise.FValue;
end;

procedure TPromiseInternal.DoSettledLike(const APromise: IPromise);
begin
  DoSettledLike((APromise as IPromiseInternal).GetPromise);
end;

function TPromiseInternal.ThenP(const AOnFulfillment: TPromiseResolveProc;
  const AData: IVariant; const AOnRejection: TPromiseRejectProc;
  const AInMainThread: Boolean): IPromise;
begin
  Result := TPromiseThen.Create(AOnFulfillment, AData, AOnRejection, AInMainThread);
  ThenPromise := Result;
end;

function TPromiseInternal.ThenM(const AOnFulfillment: TPromiseResolveMethod;
  const AData: IVariant; const AOnRejection: TPromiseRejectMethod;
  const AInMainThread: Boolean): IPromise;
begin
  Result := TPromiseThen.Create(AOnFulfillment, AData, AOnRejection, AInMainThread);
  ThenPromise := Result;
end;

function TPromiseInternal.CatchP(const AOnRejection: TPromiseRejectProc;
  const AData: IVariant; const AInMainThread: Boolean): IPromise;
begin
  Result := ThenP(nil, AData, AOnRejection, AInMainThread);
end;

function TPromiseInternal.CatchM(const AOnRejection: TPromiseRejectMethod;
  const AData: IVariant; const AInMainThread: Boolean): IPromise;
begin
  Result := ThenM(nil, AData, AOnRejection, AInMainThread);
end;

function TPromiseInternal.FinallyP(const ACallbak: TPromiseFinallyProc;
  const AData: IVariant; const AInMainThread: Boolean): IPromise;
begin
  Result := TPromiseFinally.Create(ACallbak, AData, AInMainThread);
  ThenPromise := Result;
end;

function TPromiseInternal.FinallyM(const ACallbak: TPromiseFinallyMethod;
  const AData: IVariant; const AInMainThread: Boolean): IPromise;
begin
  Result := TPromiseFinally.Create(ACallbak, AData, AInMainThread);
  ThenPromise := Result;
end;

function TPromiseInternal.Wait(AWaitMSec: Cardinal): IVariant;
var
  i: QWord;
begin
  i := GetTickCount + AWaitMSec;
  while not Settled and
      (i > GetTickCount) do
    begin
      Sleep(10);
      if GetCurrentThreadId = MainThreadID then
        CheckSynchronize;
    end;
  Result := Value;
end;


end.

