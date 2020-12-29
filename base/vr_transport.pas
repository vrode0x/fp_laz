unit vr_transport;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, vr_utils;

type
  TTransportReadEvent = procedure(const AMsg: string; const AClient: Pointer;
      const AThread: TThread) of object;
  TTransportServerConnectionEvent = procedure(const AClient: Pointer) of object;


  { TCustomTransport }

  TCustomTransport = class
  private
    FNextId: PtrInt;
    FOnReceive: TTransportReadEvent;
    FUseContentLengthField: Boolean;
    FUnDoneMsg: string;
    function CheckSendedText(const AMsg: string): string;
    function GetNextReceivedMsg(var AReceivedText: string; out AMsg: string): Boolean;
  protected
    procedure DoReceive(const AMsg: string; const AClient: Pointer; const AThread: TThread);
    procedure DoSend(const AMsg: string; const AClient: Pointer = nil); virtual; abstract;
  public
    constructor Create(const AUseContentLengthField: Boolean = False); virtual;

    //if AClient=nil send all clients; if not Server then AClient ignore
    procedure Send(const AMsg: string; const AClient: Pointer = nil); virtual;

    function NextId: string;
    function NextIdAsInt: PtrInt;

    property UseContentLengthField: Boolean read FUseContentLengthField
        write FUseContentLengthField default False;
    property OnReceive: TTransportReadEvent read FOnReceive write FOnReceive;
  end;

  { TCustomTransportClient }

  TCustomTransportClient = class(TCustomTransport)
  private
    FOnDisconnect: TNotifyEvent;
  protected
    function DoConnect: Boolean; virtual;
    function DoDisconnect: Boolean; virtual;
  public
    function Connect: Boolean; //virtual;
    function DisConnect: Boolean; //virtual;

    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

  { TCustomTransportServer }

  TCustomTransportServer = class(TCustomTransport)
  private
    FOnConnect: TTransportServerConnectionEvent;
    FOnDisconnect: TTransportServerConnectionEvent;
  protected
    FClients: TThreadList;
    procedure DoAddConnection(const AClient: Pointer); virtual;
    procedure DoRemoveConnection(const AClient: Pointer); virtual;
    function GetAlive: Boolean; virtual;
    function GetClientCount: Integer; //virtual;
  public
    constructor Create(const AUseContentLengthField: Boolean = False); override;
    destructor Destroy; override;

    function GetClient(const AIndex: Integer; out AClient: Pointer): Boolean;
    property Alive: Boolean read GetAlive;
    property ClientCount: Integer read GetClientCount;
    property OnConnect: TTransportServerConnectionEvent
        read FOnConnect write FOnConnect;
    property OnDisconnect: TTransportServerConnectionEvent
        read FOnDisconnect write FOnDisconnect;
  end;

implementation

const
  SEP_HEADERS = #13#10#13#10;

{ TCustomTransportServer }

function TCustomTransportServer.GetClientCount: Integer;
var
  lst: TList;
begin
  lst := FClients.LockList;
  try
    Result := lst.Count;
  finally
    FClients.UnlockList;
  end;
end;

constructor TCustomTransportServer.Create(const AUseContentLengthField: Boolean);
begin
  inherited Create(AUseContentLengthField);
  FClients := TThreadList.Create;
end;

destructor TCustomTransportServer.Destroy;
begin
  FreeAndNil(FClients);
  inherited Destroy;
end;

function TCustomTransportServer.GetAlive: Boolean;
begin
  Result := False;
end;

procedure TCustomTransportServer.DoAddConnection(const AClient: Pointer);
begin
  FClients.Add(AClient);
  if Assigned(FOnConnect) then
    FOnConnect(AClient);
end;

procedure TCustomTransportServer.DoRemoveConnection(const AClient: Pointer);
begin
  FClients.Remove(AClient);
  if Assigned(FOnDisconnect) then
    FOnDisconnect(AClient);
end;

function TCustomTransportServer.GetClient(const AIndex: Integer; out
  AClient: Pointer): Boolean;
var
  lst: TList;
begin
  lst := FClients.LockList;
  try
    Result := (AIndex >= 0) and (AIndex < lst.Count);
    if Result then
      AClient := lst[AIndex]
    else
      AClient := nil;
  finally
    FClients.UnlockList;
  end;
end;

{ TCustomTransportClient }

function TCustomTransportClient.DoConnect: Boolean;
begin
  Result := False;
end;

function TCustomTransportClient.DoDisconnect: Boolean;
begin
  Result := True;
end;

function TCustomTransportClient.Connect: Boolean;
begin
  Result := DoConnect;
end;

function TCustomTransportClient.DisConnect: Boolean;
begin
  Result := DoDisconnect;
  if Result and Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

{ TCustomTransport }

function TCustomTransport.CheckSendedText(const AMsg: string): string;
begin
  if FUseContentLengthField then
    Result := 'Content-Length: ' + IntToStr(Length(AMsg) + 2) + SEP_HEADERS + AMsg + #13#10
  else
    Result := AMsg;
end;

function TCustomTransport.GetNextReceivedMsg(var AReceivedText: string; out AMsg: string): Boolean;
var
  S, sHeader: String;
  i: SizeInt;
  n: integer;
begin
  if AReceivedText = '' then Exit(False);
  try
    if UseContentLengthField then
      begin
        Result := False;
        if FUnDoneMsg <> '' then
          begin
            AReceivedText := FUnDoneMsg + AReceivedText;
            FUnDoneMsg := '';
          end;
        //else
        //  AReceivedText := AReceivedText;

        //repeat
          i := Pos(SEP_HEADERS, AReceivedText); //'Content-Length: 573'#13#10#13#10'{"seq":0,...}'
          if i > 0 then
            begin
              S := Copy(AReceivedText, 1, i - 1);
              Inc(i, Length(SEP_HEADERS));

              repeat
                NameValueOfString(S, sHeader{%H-}, S, #13#10);
                if sHeader = '' then Break;
                if (CompareText(NameOfString(sHeader, ':'), 'Content-Length') = 0) then
                  begin
                    //S := '';
                    if TryStrToInt(ValueOfString(sHeader, ':'), n) and
                        (n <= (Length(AReceivedText) - i + 1)) then
                      begin
                        AMsg := Copy(AReceivedText, i, n);
                        AReceivedText := Copy(AReceivedText, i + n, MaxInt);
                      end;
                    Break;
                  end;
              until (S = '');

              if AMsg <> '' then
                Exit(True);
              //if S <> '' then
              //  begin
              //    OnRead(S);
              //    Continue;
              //  end;
            end;

          FUnDoneMsg := AReceivedText;
          AReceivedText := '';
        //  Break;
        //until False;
      end
    else
      begin
        AMsg := AReceivedText;
        AReceivedText := '';
        Result := True;
      end;
  except
    //{$IFDEF TEST_MODE}Beep;{$ENDIF}
  end;
end;

procedure TCustomTransport.DoReceive(const AMsg: string;
  const AClient: Pointer; const AThread: TThread);
var
  sMsg, S: String;
begin
  if (AMsg = '') or not Assigned(FOnReceive) then Exit;
  sMsg := AMsg;
  while GetNextReceivedMsg(sMsg, S) do
    FOnReceive(S, AClient, AThread);
end;

constructor TCustomTransport.Create(const AUseContentLengthField: Boolean);
begin
  FNextId := 1;
  FUseContentLengthField := AUseContentLengthField;
end;

procedure TCustomTransport.Send(const AMsg: string; const AClient: Pointer);
begin
  DoSend(CheckSendedText(AMsg), AClient);
end;

function TCustomTransport.NextId: string;
begin
  Result := IntToStr(FNextId);
  Inc(FNextId);
end;

function TCustomTransport.NextIdAsInt: PtrInt;
begin
  Result := FNextId;
  Inc(FNextId);
end;

end.

