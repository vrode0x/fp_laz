unit chrome_browser;

{$mode delphi}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$I vrode.inc}

interface

uses
  Classes, SysUtils, Controls, LCLIntf, LazUTF8, Forms,
  vr_intfs, vr_SysUtils, vr_utils,
  chrome_common,
  uCEFChromiumWindow, uCEFApplication, uCEFInterfaces, uCEFTypes,
  uCEFProcessMessage, uCEFConstants, uCEFv8Value, uCEFv8Handler, uCEFv8Context,
  uCEFMiscFunctions, uCEFListValue, uCEFValue, uCEFStringVisitor;

type
  TCustomChromeRender = class;
  TCustomChromeRenderClass = class of TCustomChromeRender;
  TChromeBrowserState = (cbsNone, cbsBrowserCreated);
  TChromeBrowserStates = set of TChromeBrowserState;

  { TCustomChromeBrowser }

  TCustomChromeBrowser = class(TChromiumWindow, IWebBrowser)
  private
    FStates: TChromeBrowserStates;
    FUrl: ustring;
    FLoadString: ustring;
    FOnKeyDown: TWBKeyEvent;
    FOnBeforeLoad: TOnBeforeLoad;
    FOnLoad: TNotifyEvent;
    FOnShowContextMenu: TOnShowContextMenu;
    FOnFocused: TNotifyEvent;
    procedure OnIdleLoadUrl(Sender: TObject; var Done: Boolean);
    procedure OnIdleLoadString(Sender: TObject; var Done: Boolean);
    procedure CheckBrowserCreated;
    function IgnoreUrl(const AUrl: ustring): Boolean;
  protected
    function GetInitialized: Boolean;
    function GetOnBeforeLoad: TOnBeforeLoad;
    function GetOnFocused: TNotifyEvent;
    function GetOnKeyDown: TWBKeyEvent;
    function GetOnLoad: TNotifyEvent;
    function GetOnShowContextMenu: TOnShowContextMenu;
    procedure SetOnBeforeLoad(AValue: TOnBeforeLoad);
    procedure SetOnFocused(AValue: TNotifyEvent);
    procedure SetOnKeyDown(AValue: TWBKeyEvent);
    procedure SetOnLoad(AValue: TNotifyEvent);
    procedure SetOnShowContextMenu(AValue: TOnShowContextMenu);
  private
    FSyncResult: Boolean;
    FSyncPos: TPoint;
    FSyncUrl: string;
    FRenderCmdIdThs: ustring;
    FRenderCmdMsgThs: ICefProcessMessage;
    procedure WBProcessMessageReceived(Sender: TObject;
        const browser: ICefBrowser; sourceProcess: TCefProcessId;
        const message: ICefProcessMessage; out Result: Boolean);
    procedure WBPreKeyEvent(Sender: TObject; const browser: ICefBrowser;
        const event: PCefKeyEvent; osEvent: TCefEventHandle; out
        isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure WBRunContextMenu(Sender: TObject; const browser: ICefBrowser;
        const frame: ICefFrame; const params: ICefContextMenuParams;
        const model: ICefMenuModel; const callback: ICefRunContextMenuCallback;
        var aResult: Boolean);
    procedure WBBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest; user_gesture,
      isRedirect: Boolean; out Result: Boolean);
    procedure WBLoadEnd(Sender: TObject; const browser: ICefBrowser;
        const frame: ICefFrame; httpStatusCode: Integer);
    procedure WBGotFocus(Sender: TObject; const browser: ICefBrowser);
  protected
    //function GetRenderClass: TCustomChromeRenderClass; virtual;
    procedure DoRunContextMenu(Data: PtrInt); virtual;
    procedure DoBeforeBrowse; virtual;
    procedure DoLoadEnd(Data: PtrInt); virtual;
    procedure DoGotFocus(Data: PtrInt); virtual;
    procedure DoEvent(const EVENT_: Integer; const AMsg: ICefProcessMessage); virtual;
    procedure DoCmdResult(const IDM_: Integer; const AMsg: ICefProcessMessage;
        out AResult: ICefValue); virtual;
  protected
    procedure ExecRenderCmd(const ACmd: Integer; const AParams: array of const);
    function ExecRenderCmdAsBool(const ACmd: Integer; const AParams: array of const;
        const ADefault: Boolean = False; const AWaitMSec: Integer = 3000): Boolean;
    function ExecRenderCmdAsInt(const ACmd: Integer; const AParams: array of const;
        const ADefault: Integer = 0; const AWaitMSec: Integer = 3000): Integer;
    function ExecRenderCmdAsStr(const ACmd: Integer; const AParams: array of const;
        const ADefault: string = ''; const AWaitMSec: Integer = 3000): string;
    function ExecRenderCmdAndWait(const ACmd: Integer; const AParams: array of const;
        out AResult: ICefValue; const AWaitMSec: Integer = 3000): Boolean;
 public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithParent(AOwner: TComponent; AParent: TWinControl);
    procedure AfterConstruction; override;
    procedure LoadURL(const AUrl : ustring);
  public
    procedure Navigate(const AUrl: string);
    procedure Refresh(const AIgnoreCache: Boolean = False);
    function LoadFromString(const S : string): Integer;
    function SaveToString: string;
    procedure PrintToPDF(const AFileName: string);

    function Focused: Boolean; override;
    function ClearSelection: Boolean;
    function SelectAll: Boolean;

    procedure ExecuteJavaScript(const ACode: string);

    property OnKeyDown: TWBKeyEvent read GetOnKeyDown write SetOnKeyDown;//may be not In MainThread (Chromium Embedded)
    property OnBeforeLoad: TOnBeforeLoad read GetOnBeforeLoad write SetOnBeforeLoad;
    property OnLoad: TNotifyEvent read GetOnLoad write SetOnLoad;
    property OnShowContextMenu: TOnShowContextMenu read GetOnShowContextMenu write SetOnShowContextMenu;
    property OnFocused: TNotifyEvent read GetOnFocused write SetOnFocused;

    //property Initialized: Boolean read GetInitialized;
  end;

  { TCustomChromeRender }

  TCustomChromeRender = class
  protected
    FContext: ICefv8Context;
    procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser;
        const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure GlobalCEFApp_OnContextReleased(const browser: ICefBrowser;
        const frame: ICefFrame; const context: ICefv8Context); virtual;
    procedure GlobalCEFApp_OnLoadEnd(const browser: ICefBrowser;
        const frame: ICefFrame; httpStatusCode: Integer); virtual;
    procedure GlobalCEFApp_OnProcessMessageReceived(const browser: ICefBrowser;
        sourceProcess: TCefProcessId; const message: ICefProcessMessage;
        var aHandled: boolean); virtual;
    procedure GlobalCEFApp_OnWebKitInitializedEvent(); virtual;
  public
    constructor Create; virtual;

    function DoFunction(const AFunc: ICefv8Value; out AResult: ICefv8Value;
      const Args: TCefv8ValueArray = nil; const AThis: ICefv8Value = nil): Boolean;
  end;

  { TCefStringVisitorCommon }

  TCefStringVisitorCommon = class(TCefStringVisitorOwn)
  private
    FStr: ustring;
  protected
    procedure Visit(const str: ustring); override;
  public
    property Str: ustring read FStr;
  end;


//var
//  DefaultChromeRenderClass: TCustomChromeRenderClass = nil;

implementation


{ TCustomChromeBrowser }

procedure TCustomChromeBrowser.OnIdleLoadUrl(Sender: TObject; var Done: Boolean);
begin
  CheckBrowserCreated;
  if Initialized then
    begin
      Application.RemoveOnIdleHandler(OnIdleLoadUrl);
      TChromiumWindow(Self).LoadURL(FUrl);
    end;
end;

procedure TCustomChromeBrowser.OnIdleLoadString(Sender: TObject; var Done: Boolean);
begin
  CheckBrowserCreated;
  if Initialized then
    begin
      Application.RemoveOnIdleHandler(OnIdleLoadString);
      FChromium.LoadString(FLoadString, '');
    end;
end;

procedure TCustomChromeBrowser.CheckBrowserCreated;
begin
  if not (cbsBrowserCreated in FStates) then
    if CreateBrowser then
      Include(FStates, cbsBrowserCreated);
end;

function TCustomChromeBrowser.IgnoreUrl(const AUrl: ustring): Boolean;
begin
  Result := WideCompareText('about:blank', AUrl) = 0;
end;

function TCustomChromeBrowser.GetInitialized: Boolean;
begin
  Result := Initialized;
end;

function TCustomChromeBrowser.GetOnBeforeLoad: TOnBeforeLoad;
begin
  Result := FOnBeforeLoad;
end;

function TCustomChromeBrowser.GetOnFocused: TNotifyEvent;
begin
  Result := FOnFocused;
end;

function TCustomChromeBrowser.GetOnKeyDown: TWBKeyEvent;
begin
  Result := FOnKeyDown;
end;

function TCustomChromeBrowser.GetOnLoad: TNotifyEvent;
begin
  Result := FOnLoad;
end;

function TCustomChromeBrowser.GetOnShowContextMenu: TOnShowContextMenu;
begin
  Result := FOnShowContextMenu;
end;

procedure TCustomChromeBrowser.SetOnBeforeLoad(AValue: TOnBeforeLoad);
begin
  FOnBeforeLoad := AValue;
end;

procedure TCustomChromeBrowser.SetOnFocused(AValue: TNotifyEvent);
begin
  FOnFocused := AValue;
end;

procedure TCustomChromeBrowser.SetOnKeyDown(AValue: TWBKeyEvent);
begin
  FOnKeyDown := AValue;
end;

procedure TCustomChromeBrowser.SetOnLoad(AValue: TNotifyEvent);
begin
  FOnLoad := AValue;
end;

procedure TCustomChromeBrowser.SetOnShowContextMenu(AValue: TOnShowContextMenu);
begin
  FOnShowContextMenu := AValue;
end;

procedure TCustomChromeBrowser.WBProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);

  procedure _DoEvent;
  var
    msg: ICefProcessMessage;
    args: ICefListValue;
    iEvent: Integer;
  begin
    msg := message.Copy;//message ReadOnly
    args := msg.ArgumentList;
    iEvent := args.GetInt(0);
    args.Remove(0);
    DoEvent(iEvent, msg);
  end;

var
  sName: ustring;
begin
  sName := message.Name;
  if sName = MSG_EVENT then
    begin
      Result := True;
      _DoEvent;
    end
  else if LockedStringGet(FRenderCmdIdThs) = sName then //Result of ExecRenderCmdAndWait()
    begin
      LockedIntefaceSet(FRenderCmdMsgThs, message.Copy);
      Result := True;
    end
  else
    Result := False;
end;

procedure TCustomChromeBrowser.WBPreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean);
var
  Key: Word;
begin
  if event.kind <> KEYEVENT_RAWKEYDOWN then
    Exit;
  if Assigned(FOnKeyDown) and Assigned(osEvent) and Initialized then
    begin
      Key := osEvent.wParam;
      FOnKeyDown(Self, Key, GetKeyShiftState, osEvent);
      if Key = 0 then
        Result := True;
    end;
end;

procedure TCustomChromeBrowser.DoRunContextMenu(Data: PtrInt);
begin
  if IsAppState(apsClosing) then Exit;
  if Assigned(FOnShowContextMenu) then
    FOnShowContextMenu(Self, FSyncPos, FSyncResult);
  FSyncResult := True;
end;

procedure TCustomChromeBrowser.WBRunContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel;
  const callback: ICefRunContextMenuCallback; var aResult: Boolean);
begin
  if Assigned(FOnShowContextMenu) then
    begin
      aResult := True;
      GetCursorPos(FSyncPos);//FSyncPos := Point(params.XCoord, params.YCoord);
      FSyncResult := aResult;
      Application.QueueAsyncCall(DoRunContextMenu, 0);
      //TThread.Synchronize(nil, DoRunContextMenu);
      //aResult := FSyncResult;
    end;
end;

procedure TCustomChromeBrowser.DoBeforeBrowse;
begin
  if Assigned(FOnBeforeLoad) then
    FOnBeforeLoad(Self, FSyncUrl, FSyncResult);
end;

procedure TCustomChromeBrowser.WBBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; user_gesture, isRedirect: Boolean; out
  Result: Boolean);
var
  sUrl: String;
begin
  if IgnoreUrl(request.Url) then Exit;
  FSyncUrl := UTF8Encode(request.Url);
  sUrl := FSyncUrl;
  FSyncResult := False;
  TThread.Synchronize(nil, DoBeforeBrowse);
  Result := FSyncResult;
  if Result and (sUrl <> FSyncUrl) then
    request.Url := UTF8Decode(sUrl);
end;

procedure TCustomChromeBrowser.DoLoadEnd(Data: PtrInt);
begin
  if not IsAppState(apsClosing) and Assigned(FOnLoad) then
    FOnLoad(Self);
end;

procedure TCustomChromeBrowser.WBLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  if IgnoreUrl(frame.Url) then Exit;
  Application.QueueAsyncCall(DoLoadEnd, 0);
end;

procedure TCustomChromeBrowser.DoGotFocus(Data: PtrInt);
begin
  if not IsAppState(apsClosing) and Assigned(FOnFocused) then
    FOnFocused(Self);
end;

procedure TCustomChromeBrowser.DoEvent(const EVENT_: Integer;
  const AMsg: ICefProcessMessage);
begin

end;

procedure TCustomChromeBrowser.DoCmdResult(const IDM_: Integer;
  const AMsg: ICefProcessMessage; out AResult: ICefValue);
begin

end;

procedure TCustomChromeBrowser.ExecRenderCmd(const ACmd: Integer;
  const AParams: array of const);
var
  val: ICefValue;
begin
  ExecRenderCmdAndWait(ACmd, AParams, val, 0);
end;

function TCustomChromeBrowser.ExecRenderCmdAsBool(const ACmd: Integer;
  const AParams: array of const; const ADefault: Boolean;
  const AWaitMSec: Integer): Boolean;
var
  val: ICefValue;
begin
  if ExecRenderCmdAndWait(ACmd, AParams, val, AWaitMSec) and
        (val.GetType = VTYPE_BOOL) then
    Result := val.GetBool
  else
    Result := ADefault;
end;

function TCustomChromeBrowser.ExecRenderCmdAsInt(const ACmd: Integer;
  const AParams: array of const; const ADefault: Integer;
  const AWaitMSec: Integer): Integer;
var
  val: ICefValue;
begin
  if ExecRenderCmdAndWait(ACmd, AParams, val, AWaitMSec) and
        (val.GetType = VTYPE_INT) then
    Result := val.GetInt
  else
    Result := ADefault;
end;

function TCustomChromeBrowser.ExecRenderCmdAsStr(const ACmd: Integer;
  const AParams: array of const; const ADefault: string;
  const AWaitMSec: Integer): string;
var
  val: ICefValue;
begin
  if ExecRenderCmdAndWait(ACmd, AParams, val, AWaitMSec) and
        (val.GetType = VTYPE_STRING) then
    Result := utf_16To8(val.GetString)
  else
    Result := ADefault;
end;

var
  _NextCmdId: Cardinal = 0;
function TCustomChromeBrowser.ExecRenderCmdAndWait(const ACmd: Integer;
  const AParams: array of const; out AResult: ICefValue;
  const AWaitMSec: Integer): Boolean;
var
  msg: ICefProcessMessage;
  args: ICefListValue;

  procedure _SetArgs(const ACmdId: Integer);
  var
    i: Integer;
  begin
    args.SetInt(0, ACmdId);
    args.SetInt(1, ACmd);
    for i := 0 to Length(AParams) - 1 do
      AddVarToCefListValue(args, i + 2, AParams[i]);
  end;

  function _Wait: Boolean;
  var
    i: QWord;
    AMsg: ICefProcessMessage;
    args: ICefListValue;
    iCmd: Integer;
  begin
    i := vr_utils.GetTickCount + {$IFDEF DEBUG_MODE}IfThen(GlobalCEFApp.SingleProcess, 15000, AWaitMSec){$ELSE}AWaitMSec{$ENDIF};
    while (i  > vr_utils.GetTickCount) do
      begin
        if not LockedVarIsNil(FRenderCmdMsgThs) then
          begin
            //{$IFDEF DEBUG_MODE}iDebugTemp := vr_utils.GetTickCount - i;{$ENDIF}
            LockVar;
            try
              AMsg := FRenderCmdMsgThs;
              args := AMsg.ArgumentList;
              iCmd := args.GetInt(0);
              args.Remove(0);
            finally
              UnLockVar;
            end;

            DoCmdResult(iCmd, AMsg, AResult);
            Result := AResult <> nil;
            if not Result and (args.GetSize > 0) then
              begin
                AResult := args.GetValue(0);
                Result := True;
              end;
            Exit;
          end;
        {$IFDEF DEBUG_MODE}
        if GlobalCEFApp.SingleProcess then
          Sleep(1);{$ENDIF}
      end;
    Result := False;
  end;

begin
  Result := False;
  //Execute('myFunc()');
  //Execute('window.register()');
  msg := TCefProcessMessageRef.New(MSG_CMD);
  args := msg.ArgumentList;
  if AWaitMSec > 0 then
    begin
      Inc(_NextCmdId);
      LockedStringSet(FRenderCmdIdThs, IntToStr(_NextCmdId){%H-});
      LockedIntefaceSet(FRenderCmdMsgThs, nil);
      _SetArgs(_NextCmdId);
      FChromium.SendProcessMessage(PID_RENDERER, msg);
      Result := _Wait;
      LockedStringSet(FRenderCmdIdThs, '');
      LockedIntefaceSet(FRenderCmdMsgThs, nil);
    end
  else
    begin
      _SetArgs(0);
      FChromium.SendProcessMessage(PID_RENDERER, msg);
    end;
end;

constructor TCustomChromeBrowser.Create(AOwner: TComponent);
begin
  //if _ChromeRender = nil then
  //  _ChromeRender := GetRenderClass.Create;
  inherited Create(AOwner);
end;

constructor TCustomChromeBrowser.CreateWithParent(AOwner: TComponent;
  AParent: TWinControl);
begin
  Create(AOwner);
  Parent := AParent;
end;

procedure TCustomChromeBrowser.AfterConstruction;
begin
  inherited AfterConstruction;
  FChromium.OnProcessMessageReceived := WBProcessMessageReceived;
  FChromium.OnPreKeyEvent := WBPreKeyEvent;
  FChromium.OnRunContextMenu := WBRunContextMenu;
  FChromium.OnBeforeBrowse := WBBeforeBrowse;
  FChromium.OnLoadEnd := WBLoadEnd;
  //FChromium.OnSetFocus := WBSetFocus;
  FChromium.OnGotFocus := WBGotFocus;
end;

procedure TCustomChromeBrowser.LoadURL(const AUrl: ustring);
begin
  Application.AddOnIdleHandler(OnIdleLoadUrl);
  FUrl := AUrl;
end;

procedure TCustomChromeBrowser.WBGotFocus(Sender: TObject; const browser: ICefBrowser);
begin
  Application.QueueAsyncCall(DoGotFocus, 0);
end;

//function TCustomChromeBrowser.GetRenderClass: TCustomChromeRenderClass;
//begin
//  Result := TCustomChromeRender;
//end;

procedure TCustomChromeBrowser.Navigate(const AUrl: string);
begin
  Application.AddOnIdleHandler(OnIdleLoadUrl);
  FUrl := UTF8Decode(AUrl);
end;

procedure TCustomChromeBrowser.Refresh(const AIgnoreCache: Boolean);
begin
  if AIgnoreCache then
    FChromium.ReloadIgnoreCache
  else
    FChromium.Reload;
end;

function TCustomChromeBrowser.LoadFromString(const S: string): Integer;
begin
  Result := 0;
  Application.AddOnIdleHandler(OnIdleLoadString);
  FLoadString := UTF8Decode(S);
end;

function TCustomChromeBrowser.SaveToString: string;
var
  TempFrame: ICefFrame;
  TempVisitor: TCefStringVisitorCommon;
begin
  Result := '';
  if Initialized then
    begin
      TempFrame := FChromium.Browser.MainFrame;
      if (TempFrame <> nil) then
        begin
          TempVisitor := TCefStringVisitorCommon.Create;
          TempFrame.GetSource(TempVisitor);
          Result := UTF8Encode(TempVisitor.Str);
        end;
    end;
end;

procedure TCustomChromeBrowser.PrintToPDF(const AFileName: string);
begin
  ChromiumBrowser.PrintToPDF(utf_8To16(AFileName), '', '');
end;

function TCustomChromeBrowser.Focused: Boolean;
begin
  Result := inherited Focused;
  //Result := Initialized and FChromium.Browser.MainFrame.IsFocused; //FChromium.FrameIsFocused;
end;

function TCustomChromeBrowser.ClearSelection: Boolean;
begin
  Result := True;
  ExecuteJavaScript('document.getSelection().empty()');
end;

function TCustomChromeBrowser.SelectAll: Boolean;
begin
  Result := True;
  FChromium.SelectAll;
end;

procedure TCustomChromeBrowser.ExecuteJavaScript(const ACode: string);
begin
  FChromium.ExecuteJavaScript(UTF8Decode(ACode), '');
end;

{ TCustomChromeRender }

procedure TCustomChromeRender.GlobalCEFApp_OnContextCreated(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context);
begin
  FContext := context;
end;

procedure TCustomChromeRender.GlobalCEFApp_OnContextReleased(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefv8Context);
begin
  FContext := nil;
end;

procedure TCustomChromeRender.GlobalCEFApp_OnLoadEnd(
  const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin

end;

procedure TCustomChromeRender.GlobalCEFApp_OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; var aHandled: boolean);
begin

end;

procedure TCustomChromeRender.GlobalCEFApp_OnWebKitInitializedEvent();
begin

end;

constructor TCustomChromeRender.Create;
begin
  GlobalCEFApp.OnContextCreated := GlobalCEFApp_OnContextCreated;
  GlobalCEFApp.OnContextReleased := GlobalCEFApp_OnContextReleased;
  GlobalCEFApp.OnLoadEnd := GlobalCEFApp_OnLoadEnd;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebKitInitializedEvent;
  GlobalCEFApp.OnProcessMessageReceived := GlobalCEFApp_OnProcessMessageReceived;
end;

function TCustomChromeRender.DoFunction(const AFunc: ICefv8Value; out
  AResult: ICefv8Value; const Args: TCefv8ValueArray; const AThis: ICefv8Value
  ): Boolean;
begin
  if (FContext = nil) or (AFunc = nil) then Exit(False);
  AResult := AFunc.ExecuteFunctionWithContext(FContext, AThis, Args);
  Result := (AResult <> nil) and AResult.IsValid;
end;

{ TCefStringVisitorCommon }

procedure TCefStringVisitorCommon.Visit(const str: ustring);
begin
  FStr := str;
end;


procedure _AfterGlobalCEFAppCreate;
begin
  //if DefaultChromeRenderClass = nil then
  //  DefaultChromeRenderClass := TCustomChromeRender;
  //_ChromeRender := DefaultChromeRenderClass.Create;
end;

//initialization
//  OnAfterGlobalCEFAppCreate := _AfterGlobalCEFAppCreate;

//finalization
//  FreeAndNil(_ChromeRender);


end.

