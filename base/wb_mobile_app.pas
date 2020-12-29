unit wb_mobile_app;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  vr_utils, vr_JsonRpcClasses, vr_json_conf, vr_transport;

type

  { TCustomWBMobileApp }
  {$M+}
  TCustomWBMobileApp = class(TComponent)
  private
    FRpc: TJsonRpcClient;
    FWB: IWebBrowser;
    procedure OnWBShowContextMenu(Sender: TObject; constref {%H-}APoint: TPoint; var ADone: Boolean);
  protected
    function GetJsonConfigFile: string; virtual; abstract;
    procedure DoLoadConfig(const {%H-}ACfg: TJSONConfig); virtual;
    procedure DoSaveConfig(const {%H-}ACfg: TJSONConfig); virtual;
  public
    constructor {%H-}Create(const ATransport: TCustomTransport; const ABrowser: IWebBrowser;
        const AOwnTransport: Boolean = True); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure SaveConfig;

    property Rpc: TJsonRpcClient read FRpc;
    property WB: IWebBrowser read FWB;
  end;
  {$M-}

implementation

{ TCustomWBMobileApp }

procedure TCustomWBMobileApp.OnWBShowContextMenu(Sender: TObject; constref
  APoint: TPoint; var ADone: Boolean);
begin
  ADone := True;
end;

procedure TCustomWBMobileApp.DoLoadConfig(const ACfg: TJSONConfig);
begin

end;

procedure TCustomWBMobileApp.DoSaveConfig(const ACfg: TJSONConfig);
begin

end;

constructor TCustomWBMobileApp.Create(const ATransport: TCustomTransport;
  const ABrowser: IWebBrowser; const AOwnTransport: Boolean);
begin
  inherited Create(nil);
  FWB := ABrowser;
  FRpc := TJsonRpcClient.Create(ATransport, AOwnTransport, True);
  FRpc.AddPublishedMethods(Self);

  //FWb.ChromiumBrowser.OnConsoleMessage := OnWBConsoleMessage;
  FWb.OnShowContextMenu := OnWBShowContextMenu;
  //FWb.OnKeyDown := OnWbOnKeyDown;
  //FWb.OnBeforeLoad := OnWBBeforeLoad;
  //FWb.OnLoad := OnWBLoad;
end;

procedure TCustomWBMobileApp.AfterConstruction;
begin
  LoadConfig;
end;

procedure TCustomWBMobileApp.BeforeDestruction;
begin
  SaveConfig;
  inherited BeforeDestruction;
end;

destructor TCustomWBMobileApp.Destroy;
begin
  FreeAndNil(FRpc);
  inherited Destroy;
end;

procedure TCustomWBMobileApp.LoadConfig;
var
  cfg: TJSONConfig = nil;
begin
  inherited AfterConstruction;
  try
    cfg := TJSONConfig.Create(GetJsonConfigFile);
    DoLoadConfig(cfg);
  finally
    cfg.Free;
  end;
end;

procedure TCustomWBMobileApp.SaveConfig;
var
  cfg: TJSONConfig = nil;
begin
  try
    cfg := TJSONConfig.Create(GetJsonConfigFile);
    DoSaveConfig(cfg);
  finally
    cfg.Free;
  end;
end;

end.

