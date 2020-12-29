unit mobile_desktop_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  vr_utils, chrome_jsonrpc;

type

  { TCustomMobileDesktopForm }

  TCustomMobileDesktopForm = class(TForm)
    btnBack: TButton;
    btnHome: TButton;
    btnRefresh: TButton;
    pnlBrowser: TPanel;
    pnlBottom: TPanel;
    procedure btnRefreshClick(Sender: TObject);
  private
    FWB: TJsonRpcChromeBrowser;
  protected
    property WB: TJsonRpcChromeBrowser read FWB;
    procedure Loaded; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public

  end;

var
  CustomMobileDesktopForm: TCustomMobileDesktopForm;

implementation

{$R *.lfm}

{ TCustomMobileDesktopForm }

procedure TCustomMobileDesktopForm.btnRefreshClick(Sender: TObject);
begin
  FWB.Refresh(True);
end;

procedure TCustomMobileDesktopForm.Loaded;
begin
  inherited Loaded;
  FWB := TJsonRpcChromeBrowser.CreateWithParent(Self, pnlBrowser);
  FWB.Align := alClient;
end;

procedure TCustomMobileDesktopForm.DoClose(var CloseAction: TCloseAction);
begin
  Include(AppStates, apsClosing);
  inherited DoClose(CloseAction);
end;

end.

