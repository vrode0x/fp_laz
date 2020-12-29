unit chrome_jsonrpc;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, chrome_browser, chrome_common,
  vr_utils,  vr_JsonRpcClasses, vr_transport, vr_fpclasses,
  uCEFChromiumWindow, uCEFApplication, uCEFInterfaces, uCEFTypes,
  uCEFProcessMessage, uCEFConstants, uCEFv8Value, uCEFv8Handler, uCEFv8Context,
  uCEFMiscFunctions, uCEFListValue, uCEFValue, uCEFStringVisitor;


type
  TJsonRpcChromeBrowser = class;
  TChromeReceiveStringEvent = procedure(const AStr: string;
      const ASender: TJsonRpcChromeBrowser; const AThread: TThread) of object;


  { TJsonRpcChromeBrowser }

  TJsonRpcChromeBrowser = class(TCustomChromeBrowser)
  private
    FOnReceiveJsonString: TChromeReceiveStringEvent;
    FRpc: TJsonRpcServer;
  protected
    procedure DoEvent(const EVENT_: Integer; const AMsg: ICefProcessMessage); override;
  public
    procedure PostJsonString(const AJson: string);

    property OnReceiveJsonString: TChromeReceiveStringEvent read FOnReceiveJsonString write FOnReceiveJsonString;
    property Rpc: TJsonRpcServer read FRpc;
  end;

  { TJsonRpcChromeRender }

  TJsonRpcChromeRender = class(TCustomChromeRender)
  private
    FOnReceiveJsonStringFunc: ICefv8Value;
  protected
    procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context); override;
    procedure GlobalCEFApp_OnLoadEnd(const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer); override;
    procedure GlobalCEFApp_OnProcessMessageReceived(const browser: ICefBrowser;
        sourceProcess: TCefProcessId; const message: ICefProcessMessage;
        var aHandled: boolean); override;
  end;

  { TChromeTransportClient }

  TChromeTransportClient = class(TCustomTransportClient)
  private
    FJsonRpcChrome: TJsonRpcChromeBrowser;
    procedure OnReceiveJsonString(const AStr: string;
      const {%H-}ASender: TJsonRpcChromeBrowser; const AThread: TThread);
  protected
    function DoConnect: Boolean; override;
    procedure DoSend(const AMsg: string; const {%H-}AClient: Pointer = nil); override;
  public
    constructor Create(const AJsonRpcChrome: TJsonRpcChromeBrowser); overload;
  end;

  { TChromeTransportServer }

  TChromeTransportServer = class(TCustomTransportServer)
  private
    procedure OnReceiveJsonString(const AStr: string;
      const ASender: TJsonRpcChromeBrowser; const AThread: TThread);
  protected
    procedure DoSend(const AMsg: string; const AClient: Pointer); override;
    function GetAlive: Boolean; override;
  public
    procedure AddChrome(const AChrome: TJsonRpcChromeBrowser);
    procedure RemoveChrome(const AChrome: TJsonRpcChromeBrowser);
  end;


implementation

{$IFNDEF DEBUG_MODE}
{$R js.rc}{$ENDIF}

type

  { TDoPostJsonFuncHandler }

  TDoPostJsonFuncHandler = class(TCefv8HandlerOwn)
  protected
    function Execute(const {%H-}name: ustring; const {%H-}obj: ICefv8Value;
        const arguments: TCefv8ValueArray; var {%H-}retval: ICefv8Value;
        var {%H-}exception: ustring): Boolean; override;
  end;

{ TChromeTransportServer }

procedure TChromeTransportServer.OnReceiveJsonString(const AStr: string;
  const ASender: TJsonRpcChromeBrowser; const AThread: TThread);
begin
  DoReceive(AStr, ASender, AThread);
end;

procedure TChromeTransportServer.DoSend(const AMsg: string;
  const AClient: Pointer);
var
  lst: TList;
  i: Integer;
begin
  if AClient <> nil then
    TJsonRpcChromeBrowser(AClient).PostJsonString(AMsg)
  else
    begin
      lst := FClients.LockList;
      try
        for i := 0 to lst.Count - 1 do
          TJsonRpcChromeBrowser(lst[i]).PostJsonString(AMsg);
      finally
        FClients.UnlockList;
      end;
    end;
end;

function TChromeTransportServer.GetAlive: Boolean;
begin
  Result := True;
end;

procedure TChromeTransportServer.AddChrome(const AChrome: TJsonRpcChromeBrowser);
begin
  AChrome.OnReceiveJsonString := OnReceiveJsonString;
  DoAddConnection(AChrome);
end;

procedure TChromeTransportServer.RemoveChrome(const AChrome: TJsonRpcChromeBrowser);
begin
  AChrome.OnReceiveJsonString := nil;
  DoRemoveConnection(AChrome);
end;

{ TChromeTransportClient }

procedure TChromeTransportClient.OnReceiveJsonString(const AStr: string;
  const ASender: TJsonRpcChromeBrowser; const AThread: TThread);
begin
  DoReceive(AStr, nil, AThread);
end;

function TChromeTransportClient.DoConnect: Boolean;
begin
  Result := True;
end;

procedure TChromeTransportClient.DoSend(const AMsg: string;
  const AClient: Pointer);
begin
  FJsonRpcChrome.PostJsonString(AMsg);
end;

constructor TChromeTransportClient.Create(
  const AJsonRpcChrome: TJsonRpcChromeBrowser);
begin
  Create(False);
  FJsonRpcChrome := AJsonRpcChrome;
  FJsonRpcChrome.OnReceiveJsonString := OnReceiveJsonString;
end;

{ TDoPostJsonFuncHandler }

function TDoPostJsonFuncHandler.Execute(const name: ustring;
  const obj: ICefv8Value; const arguments: TCefv8ValueArray;
  var retval: ICefv8Value; var exception: ustring): Boolean;
var
  msg: ICefProcessMessage;
  args: ICefListValue;
begin
  Result := True;
  msg := TCefProcessMessageRef.New(MSG_EVENT);
  args := msg.ArgumentList;
  args.SetInt(0, EVENT_JSON_RECEIVED);
  if Length(arguments) > 0 then
    args.SetValue(1, Cefv8ValueToCefValue(arguments[0]));
  TCefv8ContextRef.Current.Browser.SendProcessMessage(PID_BROWSER, msg);
end;



{ TJsonRpcChromeBrowser }

procedure TJsonRpcChromeBrowser.DoEvent(const EVENT_: Integer;
  const AMsg: ICefProcessMessage);

  procedure _GetJson;
  var
    args: ICefListValue;
    sJson: string;
  begin
    args := AMsg.ArgumentList;
    if args.GetSize > 0 then
      begin
        sJson := UTF8Encode(args.GetString(0));
        if Assigned(FOnReceiveJsonString) then
          FOnReceiveJsonString(sJson, Self, TThread.CurrentThread);
      end;
  end;

begin
  case EVENT_ of
    EVENT_JSON_RECEIVED:
      _GetJson;
    else
      inherited DoEvent(EVENT_, AMsg);
  end;
end;

procedure TJsonRpcChromeBrowser.PostJsonString(const AJson: string);
begin
  ExecRenderCmd(CMD_POST_JSON_STRING, [AJson]);
end;

{ TJsonRpcChromeRender }

procedure TJsonRpcChromeRender.GlobalCEFApp_OnContextCreated(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context);
var
  val: ICefv8Value = nil;
  err: ICefV8Exception = nil;
  Handler: ICefv8Handler;
  Func: ICefv8Value;
  FUNC_DO_SEND_JSON: ustring = '__doPostJsonString';
  S: string;
begin
  inherited GlobalCEFApp_OnContextCreated(browser, frame, context);

{$IFNDEF CUSTOM_BROWSER_JSONRPC}
  {$IFDEF DEBUG_MODE}
  context.Eval(UTF8Decode(str_LoadFromFile('N:\fp\pub\browser\js\simple-jsonrpc-js.js')),
      '', 0, val, err);
  context.Eval(UTF8Decode(str_LoadFromFile('N:\fp\pub\browser\js\init_jrpc.js')),
      '', 0, val, err);
  {$ELSE}
  if res_ExtractToString('SIMPLE_JSON_RPC', S) then
    context.Eval(UTF8Decode(S), '', 0, val, err);
  if res_ExtractToString('INIT_JRPS', S) then
    context.Eval(UTF8Decode(S), '', 0, val, err);
  {$ENDIF}
{$ENDIF}
  Handler  := TDoPostJsonFuncHandler.Create;
  Func := TCefv8ValueRef.NewFunction(FUNC_DO_SEND_JSON, Handler);
  FContext.Global.SetValueByKey(FUNC_DO_SEND_JSON, Func, V8_PROPERTY_ATTRIBUTE_NONE);
  //context.Eval('AppHost = {postMessage: ' + FUNC_DO_SEND_JSON + '}', '', 0, val, err);

  {$IFNDEF CUSTOM_BROWSER_JSONRPC}
  FindFunction(FContext, '__onReceiveJsonString', FOnReceiveJsonStringFunc);{$ENDIF}
end;

procedure TJsonRpcChromeRender.GlobalCEFApp_OnLoadEnd(
  const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  inherited GlobalCEFApp_OnLoadEnd(browser, frame, httpStatusCode);
  {$IFDEF CUSTOM_BROWSER_JSONRPC}
  FindFunction(FContext, '__onReceiveJsonString', FOnReceiveJsonStringFunc);{$ENDIF}
end;

procedure TJsonRpcChromeRender.GlobalCEFApp_OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; var aHandled: boolean);
const
  FIRST_ARG = 2;
  FIRST_RESULT_ARG = 1;
var
  msg: ICefProcessMessage;

  function _DoFunction(const AFunc: ICefv8Value;
      const AnArgs: ICefListValue = nil; const AThis: ICefv8Value = nil): Boolean;
  var
    args: TCefv8ValueArray;
    val: ICefv8Value;
  begin
    args := CefListValueToCefv8ValueArray(AnArgs, FIRST_ARG);
    Result := DoFunction(AFunc, val, args, AThis);
    if Result then
      begin
        msg.ArgumentList.SetValue(FIRST_RESULT_ARG, Cefv8ValueToCefValue(val));
      end;
  end;

var
  sName: ustring;
  args: ICefListValue;
  iCmdId, iCmd: Integer;
  IsSendMsg: Boolean = False;
begin
  sName := message.Name;
  if sName = MSG_CMD then
    begin
      args := message.ArgumentList;
      iCmdId := args.GetInt(0);
      iCmd := args.GetInt(1);//CMD_
      msg := TCefProcessMessageRef.New(IntToStr(iCmdId){%H-});//Command ID
      msg.ArgumentList.SetInt(0, iCmd);
      case iCmd of
        CMD_POST_JSON_STRING:
          begin
            IsSendMsg := _DoFunction(FOnReceiveJsonStringFunc, args);
          end;
        else
          iCmd := 0;
      end;

      if IsSendMsg then
        browser.SendProcessMessage(PID_BROWSER, msg);
      if iCmd > 0 then
        Exit;
    end;

  inherited GlobalCEFApp_OnProcessMessageReceived(browser, sourceProcess, message, aHandled);

end;


end.

