unit vr_JsonRpc;

{$mode delphi}{$H+}
{$I vrode.inc}

interface

uses
  Classes, SysUtils, strutils, vr_utils, vr_JsonUtils, fpjson;

type
  TJsonRpcMessageType = (jrmtInvalid, jrmtRequest, jrmtNotification,
      jrmtSuccess, jrmtError);

  IJsonRpcMessage = interface
    ['{47C573F9-32F0-4F88-80DA-141A69B3A320}']
    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentSize: Integer = DefaultIndentSize): string;
    function AsJsonObject: IJsonObject;
  end;

  { IJsonRpcNotification }

  IJsonRpcNotification = interface(IJsonRpcMessage)
    ['{749D28A6-9589-4DDB-98F1-B4AED7BFD0B1}']
    function GetMethod: string;
    function GetParams: IJsonData;

    property Method: string read GetMethod;
    property Params: IJsonData read GetParams;//IJsonObject or IJsonArray
  end;

  { IJsonRpcSuccess }

  IJsonRpcSuccess = interface(IJsonRpcMessage)
    ['{3D3170B2-491A-433F-8949-5D265DE477C0}']
    function GetID: string;
    function GetResult: IJsonData;

    property ID: string read GetID;
    property Result: IJsonData read GetResult;
  end;
  PIJsonRpcSuccess = ^IJsonRpcSuccess;

  { IJsonRpcError }

  IJsonRpcError = interface(IJsonRpcMessage)
    ['{E045C95A-F442-40DD-B22F-D8BF64CE7013}']
    function GetCode: Integer;
    function GetData: IJsonData;
    function GetID: string;
    function GetMessage: string;

    property ID: string read GetID;
    property Code: Integer read GetCode;
    property Message: string read GetMessage;
    property Data: IJsonData read GetData;
  end;

  { IJsonRpcRequest }

  IJsonRpcRequest = interface(IJsonRpcNotification)
    ['{D0163BD0-99A7-4B8E-94F6-342EEF71EA63}']
    function GetID: string;

    property ID: string read GetID;
  end;


  { TJsonRpcMessage }

  TJsonRpcMessage = class(TInterfacedObject, IJsonRpcMessage)
  protected
    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentSize: Integer = DefaultIndentSize): string;
    function AsJsonObject: IJsonObject; virtual;
  end;

  { TJsonRpcNotification }

  TJsonRpcNotification = class(TJsonRpcMessage, IJsonRpcNotification)
  private
    FMethod: string;
    FParams: IJsonData;//IJsonObject or IJsonArray
  protected
    function AsJsonObject: IJsonObject; override;
    { IJsonRpcNotification }
    function GetMethod: string;
    function GetParams: IJsonData;
  public
    constructor Create(const AMethod: string; const AParams: IJsonData = nil);
  end;

  { TJsonRpcSuccess }

  TJsonRpcSuccess = class(TJsonRpcMessage, IJsonRpcSuccess)
  private
    FId: string;
    FResult: IJsonData;
  protected
    function AsJsonObject: IJsonObject; override;
    { IJsonRpcSuccess }
    function GetID: string;
    function GetResult: IJsonData;
  public
    constructor Create(const AId: string; const AResult: IJsonData);
  end;

  { TJsonRpcError }

  TJsonRpcError = class(TJsonRpcMessage, IJsonRpcError)
  private
    FId: string;
    FCode: Integer;
    FMessage: string;
    FData: IJsonData;
  protected
    function AsJsonObject: IJsonObject; override;
    { IJsonRpcError }
    function GetCode: Integer;
    function GetData: IJsonData;
    function GetID: string;
    function GetMessage: string;
  public
    constructor Create(const ACode: Integer; const AMsg: string;
        const AData: IJsonData = nil; const AId: string = ''); overload;
  end;

  { TJsonRpcRequest }

  TJsonRpcRequest = class(TJsonRpcNotification, IJsonRpcRequest)
  public
    FId: string;
  protected
    function AsJsonObject: IJsonObject; override;
    { IJsonRpcRequest }
    function GetID: string;
  public
    constructor Create(const AId: string; const AMethod: string;
        const AParams: IJsonData = nil);
  end;

function rpc_Request(const AId: string; const AMethod: string;
      const AParams: string = ''): string;
function rpc_Notification(const AMethod: string; const AParams: string = ''): string;

function rpc_Success(const AId: string; const AResult: string = ''): string;
function rpc_Error(const AId: string; const ACode: Integer;
      const AMsg: string; const AData: string = ''): string;

function rpc_Parse(const AMsg: string; out AObject: IJsonRpcMessage): TJsonRpcMessageType;

const
  JSON_RPC_VERSION_2 = '2.0';

  FIELD_JSONRPC = 'jsonrpc';
  FIELD_ID = 'id';
  FIELD_METHOD = 'method';
  FIELD_PARAMS = 'params';
  FIELD_RESULT = 'result';
  FIELD_ERROR = 'error';
  FIELD_ERROR_CODE = 'code';
  FIELD_ERROR_MSG = 'message';
  FIELD_ERROR_DATA = 'data';

  ERROR_INVALID_JSONRPC_VER =
    'Invalid JSON-RPC Version. Supported JSON-RPC 2.0 only';
  ERROR_NO_FIELD = 'No ''%s'' field present';
  ERROR_INVALID_REQUEST_ID =
    'Invalid Request ''id'', MUST BE not empty string or integer';
  ERROR_INVALID_REQUEST_ID_TYPE =
    'Invalid Request ''id'' data type, it should be string or integer';
  ERROR_INVALID_ERROR_ID_TYPE =
    'Invalid Error ''id'' data type, it should be string or integer';
  ERROR_INVALID_ERROR_ID =
    'Invalid Error ''id'', MUST BE not empty string, integer or null';
  ERROR_INVALID_METHOD_NAME = 'Empty ''method'' field';
  ERROR_INVALID_ERROR_OBJ = 'Invalid ''error'' object';
  ERROR_INVALID_ERROR_CODE =
    'Invalid ''error.code'', it MUST BE in the range [-32768..-32000]';
  ERROR_INVALID_ERROR_MSG = 'Empty ''error.message''';

  PRC_ERR_INVALID_REQUEST = 'Invalid Request';
  PRC_ERR_METHOD_NOT_FOUND = 'Method Not Found';
  RPC_ERR_INVALID_PARAMS = 'Invalid Params';
  RPC_ERR_INTERNAL_ERROR = 'Internal Error';
  RPC_ERR_PARSE_ERROR = 'Parse Error';

  CODE_INVALID_REQUEST = -32600;
  CODE_METHOD_NOT_FOUND = -32601;
  CODE_INVALID_PARAMS = -32602;
  CODE_INTERNAL_ERROR = -32603;
  CODE_PARSE_ERROR = -32700;

implementation

function _JSON_RPC: string;
begin
  Result := '"jsonrpc": "2.0"';
end;

function _ValidateParams(const AParams: string; out AResult: string): Boolean;
begin
  if AParams = '' then
    AResult := '[]'
  else
    begin
      AResult := AParams;
      if AResult[1] in ['[', '{'] then
        //
      else if (AResult[1] <> '"') and
              (Pos(',', AResult) = 0) and
              not IsNumberWithDots(AResult) and
              (Pos(AResult + ',', 'null,true,false,') = 0) then
        begin
          AResult := '["' + AResult + '"]';
        end
      else
        AResult := '[' + AResult + ']';
    end;
  Result := json_Validate(AResult, True);
  if not Result then
    AResult := '';
end;

function rpc_Request(const AId: string; const AMethod: string;
  const AParams: string): string;
var
  sParams: string;
begin
  if _ValidateParams(AParams, sParams) then
    Result := Format('{"id": "%s","method": "%s","params":%s,%s}',
        [AId, AMethod, sParams, _JSON_RPC])
  else
    Result := '';
end;

function rpc_Notification(const AMethod: string; const AParams: string): string;
var
  sParams: string;
begin
  if _ValidateParams(AParams, sParams) then
    Result := Format('{"method": "%s","params":%s,%s}',
          [AMethod, sParams, _JSON_RPC])
  else
    Result := '';
end;

function _ValidateResult(const S: string; out AResult: string): Boolean;
begin
  if S = '' then
    AResult := '{}'
  else
    begin
      AResult := S;
      if not (AResult[1] in ['[', '{', '"']) and
            not IsNumberWithDots(AResult) and
            (Pos(AResult + ',', 'null,true,false,') = 0) then
        begin
          AResult := '"' + AResult + '"';
        end
      else
        AResult := '[' + AResult + ']';
    end;
  Result := json_Validate(AResult, True);
  if not Result then
    AResult := '';
end;

function rpc_Success(const AId: string; const AResult: string): string;
var
  sResult: string;
begin
  if _ValidateResult(AResult, sResult) then
    Result := Format('{"id": "%s","result": %s,%s}', [AId, sResult, _JSON_RPC])
  else
    Result := '';
end;

function rpc_Error(const AId: string; const ACode: Integer; const AMsg: string;
  const AData: string): string;
var
  sData: string;
begin
  if _ValidateResult(AData, sData) then
    Result := Format('{"id":"%s","error":{"code":%d,"message":"%s","data":%s},%s}',
          [AId, ACode, AMsg, sData, _JSON_RPC])
  else
    Result := '';
end;

function rpc_Parse(const AMsg: string; out AObject: IJsonRpcMessage): TJsonRpcMessageType;
var
  Root: IJsonObject;
  MsgType: TJsonRpcMessageType = jrmtInvalid;
  Id: string;

  function _CreateParseError(const AData: IJsonData): IJsonRpcError;
  begin
    Result := TJsonRpcError.Create(CODE_PARSE_ERROR, RPC_ERR_PARSE_ERROR, AData);
  end;

  function _CreateInvalidRequest(const AData: IJsonData): IJsonRpcError;
  begin
    Result := TJsonRpcError.Create(CODE_INVALID_REQUEST, PRC_ERR_INVALID_REQUEST, AData);
  end;

  function _CreateInvalidParams(const AData: IJsonData): IJsonRpcError;
  begin
    Result := TJsonRpcError.Create(CODE_INVALID_PARAMS, RPC_ERR_INVALID_PARAMS, AData);
  end;

  function _CheckRequestId(AIgnoreExists: Boolean): Boolean;
  var
    DataId: IJsonData;
  begin
    Result := False;
    if not Root.GetData(FIELD_ID, DataId) then
      begin
        if AIgnoreExists then
          Result := True
        else
          AObject := _CreateInvalidRequest(json(Format(ERROR_NO_FIELD, [FIELD_ID])));
        Exit;
      end;

    if not (DataId.JsonType in [jtNumber, jtString]) then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_REQUEST_ID_TYPE));
        Exit;
      end;

    Id := DataId.AsString;
    if (Trim(Id) = '') then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_REQUEST_ID));
        Exit;
      end;
    Result := True;
  end;

  function _CheckErrorId: Boolean;
  var
    DataId: IJsonData;
  begin
    Result := False;
    if not Root.GetData(FIELD_ID, DataId) then
      begin
        AObject := _CreateInvalidRequest(json(Format(ERROR_NO_FIELD, [FIELD_ID])));
        Exit;
      end;

    if not (DataId.JsonType in [jtNumber, jtString, jtNull]) then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_ERROR_ID_TYPE));
        Exit;
      end;

    Id := DataId.AsString;
    if (Trim(Id) = '') and not DataId.IsNull then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_ERROR_ID));
        Exit;
      end;
    Result := True;
  end;

  procedure _ParseSuccess(const AResult: IJsonData);
  begin
    if _CheckRequestId(False) then
      begin
        AObject := TJsonRpcSuccess.Create(Id, AResult);
        MsgType := jrmtSuccess;
      end;
  end;

  procedure _ParseError(const AError: IJsonData);
  var
    dataCode: IJsonData;
    dataMsg: IJsonData;
    objError: IJsonObject;
    Code: Integer;
    Msg: string;
  begin
    if not _CheckErrorId then
      Exit;

    if not (AError.GetObj(objError) and
        objError.GetData(FIELD_ERROR_CODE, dataCode) and (dataCode.JsonType = jtNumber) and
        objError.GetData(FIELD_ERROR_MSG, dataMsg) and (dataMsg.JsonType = jtString)) then
      begin
        AObject := _CreateInvalidParams(json(ERROR_INVALID_ERROR_OBJ));
        Exit;
      end;
    Msg := dataMsg.ToString;
    if (Trim(Msg) = '') then
      begin
        AObject := _CreateInvalidParams(json(ERROR_INVALID_ERROR_MSG));
        Exit;
      end;
    Code := dataCode.ToInteger;
    if (Code < -32768) or (Code > -32000) then
      begin
        AObject := _CreateInvalidParams(json(ERROR_INVALID_ERROR_CODE));
        Exit;
      end;

    AObject := TJsonRpcError.Create(Code, Msg, objError.GetAsData(FIELD_ERROR_DATA), Id);
    MsgType := jrmtError;
  end;

  procedure _ParseNotificationOrRequest(const AMethod: IJsonData);
  var
    sMethod: String;
    Data: IJsonData;
  begin
    sMethod := AMethod.AsString;
    if (AMethod.JsonType <> jtString) or (Trim(sMethod) = '') then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_METHOD_NAME));
        Exit;
      end;

    if not _CheckRequestId(True) then
      Exit;

    Data := Root.GetAsData(FIELD_PARAMS);
    if Id = '' then
      begin
        AObject := TJsonRpcNotification.Create(sMethod, Data);
        MsgType := jrmtNotification;
      end
    else
      begin
        AObject := TJsonRpcRequest.Create(Id, sMethod, Data);
        MsgType := jrmtRequest;
      end;
  end;

  procedure _Parse;
  var
    Data: IJsonData;
  begin
    if Root.GetData(FIELD_RESULT, Data) then
      _ParseSuccess(Data)
    else if Root.GetData(FIELD_ERROR, Data) then
      _ParseError(Data)
    else if Root.GetData(FIELD_METHOD, Data) then
      _ParseNotificationOrRequest(Data)
    else
      AObject := _CreateInvalidRequest(Root.AsData);
  end;

  function _CheckHeader: Boolean;
  begin
    Result := False;
    if not Root.KeyExists(FIELD_JSONRPC) then
      begin
        AObject := _CreateInvalidRequest(json(Format(ERROR_NO_FIELD, [FIELD_JSONRPC])));
        Exit;
      end;
    if Root.GetAsString(FIELD_JSONRPC) <> JSON_RPC_VERSION_2 then
      begin
        AObject := _CreateInvalidRequest(json(ERROR_INVALID_JSONRPC_VER));
        Exit;
      end;
    Result := True;
  end;

begin
  Result := jrmtInvalid;
  if not json_ParseAsObject(AMsg, Root) then
    begin
      AObject := _CreateParseError(json(AMsg));
      Exit;
    end;

  if not _CheckHeader then Exit;

  _Parse;
  Result := MsgType;
end;

{ TJsonRpcMessage }

function TJsonRpcMessage.AsJsonString(const AOptions: TFormatOptions;
  const AIndentSize: Integer): string;
begin
  Result := AsJsonObject.AsJsonString(AOptions, AIndentSize);
end;

function TJsonRpcMessage.AsJsonObject: IJsonObject;
begin
  Result := json_CreateObject;
  Result.SetString(FIELD_JSONRPC, JSON_RPC_VERSION_2);
end;

{ TJsonRpcRequest }

function TJsonRpcRequest.AsJsonObject: IJsonObject;
begin
  Result := inherited AsJsonObject;
  Result.SetString(FIELD_ID, FID);
end;

function TJsonRpcRequest.GetID: string;
begin
  Result := FId;
end;

constructor TJsonRpcRequest.Create(const AId: string; const AMethod: string;
  const AParams: IJsonData);
begin
  inherited Create(AMethod, AParams);
  FId := AId;
end;

{ TJsonRpcError }

function TJsonRpcError.AsJsonObject: IJsonObject;
var
  objError: IJsonObject;
begin
  Result := inherited AsJsonObject;
  Result.SetObjectAsText(FIELD_ID, FID);
  objError := Result.SetObjectAsText(FIELD_ERROR, '');
  objError.SetInteger(FIELD_ERROR_CODE, FCode);
  objError.SetString(FIELD_ERROR_MSG, FMessage);
  if Assigned(FData) then
    objError.SetData(FIELD_ERROR_DATA, FData.Clone);
end;

function TJsonRpcError.GetCode: Integer;
begin
  Result := FCode;
end;

function TJsonRpcError.GetData: IJsonData;
begin
  Result := FData;
end;

function TJsonRpcError.GetID: string;
begin
  Result := FId;
end;

function TJsonRpcError.GetMessage: string;
begin
  Result := FMessage;
end;

constructor TJsonRpcError.Create(const ACode: Integer; const AMsg: string;
  const AData: IJsonData; const AId: string);
begin
  FCode := ACode;
  FId := AId;
  FMessage := AMsg;
  if AData = nil then
    FData := json_CreateNull
  else
    FData := AData;
end;

{ TJsonRpcSuccess }

function TJsonRpcSuccess.AsJsonObject: IJsonObject;
begin
  Result := inherited AsJsonObject;
  Result.SetString(FIELD_ID, FId);
  Result.SetData(FIELD_RESULT, FResult.Clone);
end;

function TJsonRpcSuccess.GetID: string;
begin
  Result := FId;
end;

function TJsonRpcSuccess.GetResult: IJsonData;
begin
  Result := FResult;
end;

constructor TJsonRpcSuccess.Create(const AId: string; const AResult: IJsonData);
begin
  FId := AId;
  FResult := AResult;
end;

{ TJsonRpcNotification }

function TJsonRpcNotification.AsJsonObject: IJsonObject;
begin
  Result := inherited AsJsonObject;
  Result.SetString(FIELD_METHOD, FMethod);
  if Assigned(FParams) then
    Result.SetData(FIELD_PARAMS, FParams.Clone);
end;

function TJsonRpcNotification.GetMethod: string;
begin
  Result := FMethod;
end;

function TJsonRpcNotification.GetParams: IJsonData;
begin
  Result := FParams;
end;

constructor TJsonRpcNotification.Create(const AMethod: string;
  const AParams: IJsonData);
begin
  FMethod := AMethod;
  if AParams <> nil then
    FParams := AParams
  else
    FParams := json_CreateObject.AsData;
end;


end.

