unit vr_android_utils;

{$mode delphi}{$H+}
{$I vrode.inc}
interface
{$IFDEF ANDROID}
uses
  Classes, SysUtils, Laz_And_Controls, AndroidWidget,
  vr_utils, vr_intfs, vr_transport, vr_app_utils;

type

  { TAndroidApp }

  TAndroidApp = class(jApp)
  protected
    procedure DoAfterInitialized; override;
  public
    procedure Finish(); override;
  end;

  { TjWebViewTransport }

  TjWebViewTransport = class(TCustomTransportClient)
  private
    FWB: jWebView;
    procedure OnWBPostMessage(Sender: TObject; msg: string);
  protected
    procedure DoSend(const AMsg: string; const AClient: Pointer = nil); override;
  public
    constructor Create(const ABrowser: jWebView); overload;
  end;

  { TAndroidBrowser }

  TAndroidBrowser = class(TComponent, IWebBrowser)
  private
    FWB: jWebView;
    FOnBeforeLoad: TOnBeforeLoad;
    FOnLoad: TNotifyEvent;
    procedure OnWBStatus(Sender: TObject; Status: TWebViewStatus; URL: String;
        var CanNavi: Boolean);
  private
    function GetEnabled: Boolean;
    function GetInitialized: Boolean;
    function GetOnBeforeLoad: TOnBeforeLoad;
    function GetOnFocused: TNotifyEvent;
    function GetOnKeyDown: TWBKeyEvent;
    function GetOnLoad: TNotifyEvent;
    function GetOnShowContextMenu: TOnShowContextMenu;
    procedure SetEnabled(AValue: Boolean);
    procedure SetOnBeforeLoad(AValue: TOnBeforeLoad);
    procedure SetOnFocused(AValue: TNotifyEvent);
    procedure SetOnKeyDown(AValue: TWBKeyEvent);
    procedure SetOnLoad(AValue: TNotifyEvent);
    procedure SetOnShowContextMenu(AValue: TOnShowContextMenu);
  protected
    { IWebBrowser }
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
    function IsVisible: Boolean;//check Parent
  public
    constructor Create(AOwner: TComponent; const AWebView: jWebView); overload;
  end;

  {$IFDEF TEST_MODE}

  { TTestAndroidThread }

  TTestAndroidThread = class(TThread)
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
  end;

  {$ENDIF}

function and_GetAssetsPath: string;
function and_GetInternalAppStoragePath: string;
function and_CopyFromAssetsToInternalAppStorage(const AssetFilename: string;
    out AResultFile: string; const ASkipIfExists: Boolean = True): Boolean;
function MainForm: jForm;{$IFNDEF TEST_MODE}inline;{$ENDIF}

function jctrl_IsEnabled(ACtrl: TAndroidWidget): Boolean;//check Parents
function jctrl_IsVisible(ACtrl: TAndroidWidget): Boolean;//check Parents

//var
//  MainForm: jForm;

implementation

function and_GetAssetsPath: string;
begin
  Result := 'file:///android_asset/';
end;

function and_GetInternalAppStoragePath: string;
begin
  if not (gApp.Form is jForm) then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(jForm(gApp.Form).GetInternalAppStoragePath);
end;

function and_CopyFromAssetsToInternalAppStorage(const AssetFilename: string;
  out AResultFile: string; const ASkipIfExists: Boolean): Boolean;
var
  sDestPath, sSrcFile: String;
begin
  Result := False;
  sDestPath := and_GetInternalAppStoragePath;
  if sDestPath = '' then Exit;
  if file_Starts(and_GetAssetsPath, AssetFilename) then
    sSrcFile := Copy(AssetFilename, Length(and_GetAssetsPath) + 1, MaxInt)
  else
    sSrcFile := ExcludeLeadingPathDelimiter(AssetFilename);

  AResultFile := sDestPath + sSrcFile;
  if (ASkipIfExists and file_Exists(AResultFile)) then
    Exit(True)
  else
    AResultFile := '';
  if not dir_Force(sDestPath + file_ExtractDir(sSrcFile)) then
    Exit;

  AResultFile := MainForm.CopyFromAssetsToInternalAppStorage(sSrcFile);
  Result := file_Exists(AResultFile);
end;

function MainForm: jForm;
begin
  Result := jForm(gApp.Form);
end;

function jctrl_IsEnabled(ACtrl: TAndroidWidget): Boolean;
begin
  while ACtrl <> nil do
    begin
      if ACtrl.Enabled then Exit(True);
      ACtrl := ACtrl.Parent;
    end;
  Result := False;
end;

function jctrl_IsVisible(ACtrl: TAndroidWidget): Boolean;
begin
  while ACtrl <> nil do
    begin
      if ACtrl.Visible then Exit(True);
      ACtrl := ACtrl.Parent;
    end;
  Result := False;
end;

{ TAndroidApp }

procedure TAndroidApp.DoAfterInitialized;
begin
  inherited DoAfterInitialized;
  //NotifyAppEvent(AET_AppInitialized, Self);
end;

procedure TAndroidApp.Finish();
begin
  NotifyAppEvent(AET_AppClose, Self);
  inherited Finish();
end;

{ TTestAndroidThread }

procedure TTestAndroidThread.Execute;
begin

end;

procedure TTestAndroidThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FreeOnTerminate := True;
end;

{ TAndroidBrowser }

function TAndroidBrowser.GetEnabled: Boolean;
begin
  Result := FWB.Enabled;
end;

function TAndroidBrowser.GetInitialized: Boolean;
begin
  Result := FWB.Initialized;
end;

function TAndroidBrowser.GetOnBeforeLoad: TOnBeforeLoad;
begin
  Result := FOnBeforeLoad;
end;

function TAndroidBrowser.GetOnFocused: TNotifyEvent;
begin
  Result := nil;
end;

function TAndroidBrowser.GetOnKeyDown: TWBKeyEvent;
begin
  Result := nil;
end;

function TAndroidBrowser.GetOnLoad: TNotifyEvent;
begin
  Result := FOnLoad;
end;

function TAndroidBrowser.GetOnShowContextMenu: TOnShowContextMenu;
begin
  Result := nil
end;

procedure TAndroidBrowser.OnWBStatus(Sender: TObject; Status: TWebViewStatus;
  URL: String; var CanNavi: Boolean);
begin
  case Status of
    wvOnBefore:
      if Assigned(FOnBeforeLoad) then
        FOnBeforeLoad(FWB, URL{%H-}, CanNavi);
    wvOnFinish:
      if Assigned(FOnLoad) then
        FOnLoad(FWB);
  end;
end;

procedure TAndroidBrowser.SetEnabled(AValue: Boolean);
begin
  FWB.Enabled := AValue;
end;

procedure TAndroidBrowser.SetOnBeforeLoad(AValue: TOnBeforeLoad);
begin
  FOnBeforeLoad := AValue;
end;

procedure TAndroidBrowser.SetOnFocused(AValue: TNotifyEvent);
begin

end;

procedure TAndroidBrowser.SetOnKeyDown(AValue: TWBKeyEvent);
begin

end;

procedure TAndroidBrowser.SetOnLoad(AValue: TNotifyEvent);
begin
  FOnLoad := AValue;
end;

procedure TAndroidBrowser.SetOnShowContextMenu(AValue: TOnShowContextMenu);
begin

end;

procedure TAndroidBrowser.Navigate(const AUrl: string);
begin
  FWB.Navigate(AUrl);
end;

procedure TAndroidBrowser.Refresh(const AIgnoreCache: Boolean);
begin
  FWB.Refresh;
end;

function TAndroidBrowser.LoadFromString(const S: string): Integer;
begin
  Result := 0;
  FWB.LoadFromHtmlString(S);
end;

function TAndroidBrowser.SaveToString: string;
begin

end;

procedure TAndroidBrowser.PrintToPDF(const AFileName: string);
begin

end;

function TAndroidBrowser.Focused: Boolean;
begin
  Result := False;
end;

function TAndroidBrowser.ClearSelection: Boolean;
begin
  Result := False;
end;

function TAndroidBrowser.SelectAll: Boolean;
begin
  Result := False;
end;

procedure TAndroidBrowser.ExecuteJavaScript(const ACode: string);
begin
  FWB.EvaluateJavascript(ACode);
end;

function TAndroidBrowser.IsEnabled: Boolean;
begin
  Result := jctrl_IsEnabled(FWB);
end;

function TAndroidBrowser.IsVisible: Boolean;
begin
  Result := jctrl_IsVisible(FWB);
end;

constructor TAndroidBrowser.Create(AOwner: TComponent; const AWebView: jWebView);
begin
  Create(AOwner);
  FWB := AWebView;
  FWB.OnStatus := OnWBStatus;
end;

{ TjWebViewTransport }

procedure TjWebViewTransport.OnWBPostMessage(Sender: TObject; msg: string);
begin
  DoReceive(msg, nil, nil);
end;

procedure TjWebViewTransport.DoSend(const AMsg: string; const AClient: Pointer);
begin
  FWB.EvaluateJavascript('window.__android_receive_msg(`' + AMsg + '`)');
end;

constructor TjWebViewTransport.Create(const ABrowser: jWebView);
begin
  Create(False);
  FWB := ABrowser;
  ABrowser.OnPostMessage := OnWBPostMessage;
end;

function _OnShowConfirm(const S: string; AIsYesNo: Boolean): Boolean;
begin

end;

procedure _OnShowInfo(const S: string);
begin
  MainForm.ShowMessage(S);
end;

procedure _OnShowError(const AError: string; ACaption: string = '');
begin
  gApp.ShowMessage(ACaption, AError, 'OK');
end;


initialization
  OnShowConfirm := _OnShowconfirm;
  OnShowInfo := _OnShowInfo;
  OnShowError := _OnShowError;

{$ELSE NOT ANDROID}implementation{$ENDIF}
end.

