unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, chrome_jsonrpc, vr_utils, vr_JsonRpcClasses, vr_variant, vr_promise,
  vr_JsonUtils, vr_fpclasses, vr_classes;

type
  TChromeJsonRpcClient = class;

  { TForm1 }

  TForm1 = class(TForm)
    btnRequest: TButton;
    btnRefresh: TButton;
    btnRequestWait: TButton;
    btnNotifyWB: TButton;
    cmbFuncs: TComboBox;
    pnlWB: TPanel;
    pnlCtrls: TPanel;
    procedure btnNotifyWBClick(Sender: TObject);
    procedure btnRequestWaitClick(Sender: TObject);
    procedure btnRequestClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWb: TJsonRpcChromeBrowser;
    FRpc: TChromeJsonRpcClient;
  protected
    procedure Loaded; override;
  public

  end;

  { TChromeJsonRpcClient }

  TChromeJsonRpcClient = class(TJsonRpcClient)
  published
    procedure ra_showMessage(const AParams: IJsonData; out {%H-}AResult: string;
        var {%H-}AErrorCode: Integer);
    procedure na_notification_1(const AParams: IJsonData; const {%H-}AClient: Pointer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function {%H-}OnTestFuncResult(const AValue: IVariant; const {%H-}AData: IVariant): IPromise;
var
  jd: IJsonData;
  i: Integer;
  frm: TForm1;
begin
  frm := TForm1(AData.Obj);
  if frm = nil then ;
  Result := TPromise.Resolve(AValue);
  if (AValue.Intf.QueryInterface(IJsonData, jd) = S_OK) and
      jd.GetInt(i)
      //and IsMainThread()
    then
      ShowMessage(IntToStr(i));
      //Beep;
end;

{ TChromeJsonRpcClient }

procedure TChromeJsonRpcClient.ra_showMessage(const AParams: IJsonData; out
  AResult: string; var AErrorCode: Integer);
var
  arr: IJsonArray;
  S, S1: string;
begin
  if AParams.GetArr(arr) and
    arr.GetStr(0, S) and
    arr.GetStr(1, S1) then
    Dialogs.ShowMessage(S + ' : ' + S1);
end;

procedure TChromeJsonRpcClient.na_notification_1(const AParams: IJsonData;
  const AClient: Pointer);
var
  arr: IJsonArray;
  S: string;
begin
  if AParams.GetArr(arr) and
    arr.GetStr(0, S) then
    Dialogs.ShowMessage(S);
end;

procedure TForm1.btnRequestClick(Sender: TObject);
var
  sParams, sFunc: string;
begin
  //FWbServer.PostJsonString(rpc_Request('1', 'test_func', Edit1.Text));
  NameValueOfString(cmbFuncs.Text, sFunc{%H-}, sParams{%H-}, ' ');
  FRpc.SendRequest(sFunc, sParams).ThenP(@OnTestFuncResult, Self, nil, False);
end;

procedure TForm1.btnRequestWaitClick(Sender: TObject);
var
  jd: IJsonData;
  i: Integer;
  sFunc, sParams: string;
begin
  NameValueOfString(cmbFuncs.Text, sFunc{%H-}, sParams{%H-}, ' ');
  jd := FRpc.SendRequestWait(sFunc, sParams);
  if jd.GetInt(i) then
    ShowMessage(IntToStr(i));
end;

procedure TForm1.btnNotifyWBClick(Sender: TObject);
begin
  FRpc.SendNotification('notification_1',
        //'["notification: ' + cmbFuncs.Text + '"]'
        'true'
    );
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  FWb.Refresh(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  tr: TChromeTransportClient;
begin
  FWb.Navigate(ExtractFilePath(Application.ExeName) + 'test_embed.html');
  tr := TChromeTransportClient.Create(FWb);
  FRpc := TChromeJsonRpcClient.Create(tr, True, True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FRpc.Free;
end;

procedure TForm1.Loaded;
begin
  inherited Loaded;
  FWb := TJsonRpcChromeBrowser.CreateWithParent(Self, pnlWB);
  FWb.Align := alClient;
end;

end.

