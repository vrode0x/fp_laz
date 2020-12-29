unit TransportWS;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, vr_transport, vr_utils, WebSocket2, CustomServer2;

type
  TWebSocketConnectionDataNotSync = function(ASender: TWebSocketCustomConnection;
      ACode: Integer; AData: TMemoryStream): Boolean of object;

  { TNotSyncWebSocketClientConnection }

  TNotSyncWebSocketClientConnection = class(TWebSocketClientConnection)
  private
    FOnReadNotSync: TWebSocketConnectionDataNotSync;
  protected
    procedure DoSyncRead; override;
  public
    property OnReadNotSync: TWebSocketConnectionDataNotSync
      read FOnReadNotSync write FOnReadNotSync;
  end;


  { TWSTransportClient }

  TWSTransportClient = class(TCustomTransportClient)
  private
    FHost: string;
    FPort: Word;
    FResourceName: string;
    FClient: TNotSyncWebSocketClientConnection;
    FOpened: PtrInt;
    procedure OnClose(aSender: TWebSocketCustomConnection; aCloseCode: integer;
        aCloseReason: string; aClosedByPeer: boolean);
    procedure OnOpen(aSender: TWebSocketCustomConnection);
    function OnReadNotSync(ASender: TWebSocketCustomConnection; ACode: Integer;
        AData: TMemoryStream): Boolean;
    procedure OnTerminateClient(Sender: TObject);
    procedure StopClient;
  protected
    procedure DoSend(const AMsg: string; const AClient: Pointer = nil); override;
    function DoConnect: Boolean; override;
    function DoDisConnect: Boolean; override;
  public
    constructor Create(APort: Word; const AHost: string = '';
        const AResourceName: string = ''); virtual; overload;
    destructor Destroy; override;
  end;

  { TNotSyncWebSocketServerConnection }

  TNotSyncWebSocketServerConnection = class(TWebSocketServerConnection)
  private
    FOnReadNotSync: TWebSocketConnectionDataNotSync;
  protected
    procedure DoSyncRead; override;
  public
    property OnReadNotSync: TWebSocketConnectionDataNotSync
      read FOnReadNotSync write FOnReadNotSync;
  end;

  { TWSTransportServer }

  TWSTransportServer = class(TCustomTransportServer)
  private
    FServer: TWebSocketServer;
    procedure OnAfterAddConnection(Server: TCustomServer; aConnection: TCustomConnection);
    procedure OnAfterRemoveConnection(Server: TCustomServer; aConnection: TCustomConnection);
    function OnClientReadNotSync(ASender: TWebSocketCustomConnection;
      ACode: Integer; AData: TMemoryStream): Boolean;
  protected
    function GetAlive: Boolean; override;
    procedure DoSend(const AMsg: string; const AClient: Pointer = nil); override;
  public
    constructor {%H-}Create(APort: Word = 0; const AHost: string = '';
        const AResource: string = ''); //overload;
    destructor Destroy; override;
  end;

implementation

type

  { TTransportWebSocketServer }

  TTransportWebSocketServer = class(TWebSocketServer)
  protected
    function GetWebSocketConnectionClass(Socket: TTCPCustomConnectionSocket;
      Header: TStringList; ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer; var Protocol, Extensions: string
      ): TWebSocketServerConnections; override;
  public
    property Terminated;
  end;

{ TTransportWebSocketServer }

function TTransportWebSocketServer.GetWebSocketConnectionClass(
  Socket: TTCPCustomConnectionSocket; Header: TStringList; ResourceName, Host,
  Port, Origin, Cookie: string; out HttpResult: integer; var Protocol,
  Extensions: string): TWebSocketServerConnections;
begin
  Result := TNotSyncWebSocketServerConnection;
end;

{ TNotSyncWebSocketServerConnection }

procedure TNotSyncWebSocketServerConnection.DoSyncRead;
begin
  if fReadFinal and Assigned(FOnReadNotSync) then
    begin
      fReadStream.Position := 0;
      if FOnReadNotSync(Self, fReadCode, fReadStream) then
        Exit;
    end;
  inherited DoSyncRead;
end;

{ TWSTransportServer }

procedure TWSTransportServer.OnAfterAddConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
begin
  if aConnection is TWebSocketCustomConnection then
    TNotSyncWebSocketServerConnection(aConnection).OnReadNotSync := OnClientReadNotSync;
  DoAddConnection(aConnection);
end;

procedure TWSTransportServer.OnAfterRemoveConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
begin
  if aConnection is TWebSocketCustomConnection then
    TNotSyncWebSocketServerConnection(aConnection).OnReadNotSync := nil;
  DoRemoveConnection(aConnection);
end;

function TWSTransportServer.OnClientReadNotSync(
  ASender: TWebSocketCustomConnection; ACode: Integer; AData: TMemoryStream): Boolean;
var
  S: string;
begin
  Result := True;
  SetLength(S, aData.Size);
  aData.Read(S[1], aData.Size);
  DoReceive(S, ASender, ASender);
end;

function TWSTransportServer.GetAlive: Boolean;
begin
  Result := (FServer <> nil) and not TTransportWebSocketServer(FServer).Terminated;
end;

procedure TWSTransportServer.DoSend(const AMsg: string; const AClient: Pointer);
begin
  if AClient = nil then
    FServer.BroadcastText(AMsg)
  else
    TWebSocketCustomConnection(AClient).SendText(AMsg);
end;

constructor TWSTransportServer.Create(APort: Word; const AHost: string;
  const AResource: string);
begin
  inherited Create(False);
  FServer := TTransportWebSocketServer.Create(IfThen(AHost = '', '0.0.0.0', AHost),
      IntToStr(APort));
  FServer.OnAfterAddConnection := OnAfterAddConnection;
  FServer.OnAfterRemoveConnection := OnAfterRemoveConnection;
  FServer.FreeOnTerminate := True;
  FServer.Start;
end;

destructor TWSTransportServer.Destroy;
begin
  FServer.TerminateThread;
  FServer := nil;
  inherited Destroy;
end;

{ TNotSyncWebSocketClientConnection }

procedure TNotSyncWebSocketClientConnection.DoSyncRead;
begin
  if fReadFinal and Assigned(FOnReadNotSync) then
    begin
      fReadStream.Position := 0;
      if FOnReadNotSync(Self, fReadCode, fReadStream) then
        Exit;
    end;
  inherited DoSyncRead;
end;

{ TWSTransportClient }

procedure TWSTransportClient.StopClient;
begin
  if FClient <> nil then
    begin
      FClient.OnTerminate := nil;
      FClient.OnClose := nil;
      FClient.OnReadNotSync := nil;
      FClient.TerminateThread;
      FClient := nil;
      DoDisconnect;
    end;
end;

procedure TWSTransportClient.DoSend(const AMsg: string; const AClient: Pointer);
begin
  if FClient <> nil then
    FClient.SendText(AMsg);
end;

procedure TWSTransportClient.OnTerminateClient(Sender: TObject);
begin
  StopClient;
end;

function TWSTransportClient.OnReadNotSync(ASender: TWebSocketCustomConnection;
  ACode: Integer; AData: TMemoryStream): Boolean;
var
  sMessage: string;
begin
  Result := True;
  if AData.Size = 0 then Exit;
  SetLength(sMessage, aData.Size);
  aData.Read(PChar(sMessage)^, aData.Size);
  DoReceive(sMessage, nil, ASender);
end;

procedure TWSTransportClient.OnClose(aSender: TWebSocketCustomConnection;
  aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  StopClient;
end;

procedure TWSTransportClient.OnOpen(aSender: TWebSocketCustomConnection);
begin
  FOpened := 1;
end;

function TWSTransportClient.DoConnect: Boolean;
begin
  Result := False;
  StopClient;
  FClient := TNotSyncWebSocketClientConnection.Create(FHost, IntToStr(FPort), FResourceName);
  FClient.FreeOnTerminate := True;
  FClient.OnTerminate := OnTerminateClient;
  FClient.OnReadNotSync := OnReadNotSync;
  FClient.OnClose := OnClose;
  FClient.OnOpen := OnOpen;
  FClient.Start;
  FOpened := 0;
  Result := Wait(10000, @FOpened, 0, True) = 1;
end;

function TWSTransportClient.DoDisConnect: Boolean;
begin
  FOpened := -1;
  StopClient;
  Result := True;
end;

constructor TWSTransportClient.Create(APort: Word; const AHost: string;
  const AResourceName: string);
begin
  Create;
  FHost := IfThen(AHost = '', LOCAL_HOST, AHost);
  FPort := APort;
  FResourceName := IfThen(AResourceName = '', '/', AResourceName);
end;

destructor TWSTransportClient.Destroy;
begin
  StopClient;
  inherited Destroy;
end;

end.

