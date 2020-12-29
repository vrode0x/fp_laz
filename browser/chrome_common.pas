unit chrome_common;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses
  Classes, SysUtils, variants,
  vr_utils, vr_types, //vrUtilsPlus,
  uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFValue,
  uCEFListValue;

type
  Pustring = ^ustring;

const
  MSG_CMD: ustring = 'cmd';
  MSG_EVENT: ustring = 'event';

  { Commands }
  CMD_FUNC_EXEC           = 1; //First param: function name
  CMD_POST_JSON_STRING    = 2;

  { Events }
  EVENT_JSON_RECEIVED     = 1;



function GetObjectKeyValueObj(const AObj: ICefv8Value; const AKey: ustring; out AResult: ICefv8Value): Boolean;
function GetObjectKeyValueStr(const AObj: ICefv8Value; const AKey: ustring; out S: ustring): Boolean;
function GetObjectKeyValueStrDef(const AObj: ICefv8Value; const AKey: ustring;
    const ADefault: ustring = ''): ustring;
function GetObjectKeyValueInt(const AObj: ICefv8Value; const AKey: ustring; out I: Integer): Boolean;
function GetObjectKeyValueIntDef(const AObj: ICefv8Value; const AKey: ustring;
    const ADefault: Integer = 0): Integer;

function FindFunction(const ACtx: ICefv8Context; const AName: ustring; out AFunc: ICefv8Value): Boolean;

function CefValueToCefv8Value(const AValue: ICefValue): ICefv8Value;
function Cefv8ValueToCefValue(const AValue: ICefv8Value): ICefValue;

function CefListValueToCefv8ValueArray(const AList: ICefListValue; const AFromIndex: Integer = 0): TCefv8ValueArray;
function CefListValueToStringArray(const AList: ICefListValue; const AFromIndex: Integer = 0): TStringArray;
function CefListValueToByteArray(const AList: ICefListValue; const AFromIndex: Integer = 0): TByteDynArray;

procedure AddCefv8ValueToCefListValue(const AValue: ICefv8Value; const AList: ICefListValue; const AIndex: Integer);

function VarsToCefListValue(const AVars: array of const): ICefListValue;
procedure AddVarToCefListValue(const AList: ICefListValue; AIndex: Integer; constref AVar: TVarRec);

var
  ChromeInitialized: Boolean = False;
  OnAfterGlobalCEFAppCreate: TProcedure = nil;

implementation

function GetObjectKeyValueObj(const AObj: ICefv8Value; const AKey: ustring; out
  AResult: ICefv8Value): Boolean;
begin
  Result := False;
  if not AObj.IsObject then Exit;
  AResult := AObj.GetValueByKey(AKey);
  Result := AResult.IsObject;
  if not Result then
    AResult := nil;
end;

function GetObjectKeyValueStr(const AObj: ICefv8Value; const AKey: ustring; out S: ustring): Boolean;
var
  val: ICefv8Value;
begin
  Result := False;
  if not AObj.IsObject then Exit;
  val := AObj.GetValueByKey(AKey);
  if (val = nil) or not val.IsString then Exit;
  Result := True;
  S := val.GetStringValue;
end;

function GetObjectKeyValueStrDef(const AObj: ICefv8Value; const AKey: ustring;
  const ADefault: ustring): ustring;
begin
  if not GetObjectKeyValueStr(AObj, AKey, Result) then
    Result := ADefault;
end;

function GetObjectKeyValueInt(const AObj: ICefv8Value; const AKey: ustring; out I: Integer): Boolean;
var
  val: ICefv8Value;
begin
  Result := False;
  if not AObj.IsObject then Exit;
  val := AObj.GetValueByKey(AKey);
  if (val = nil) or not val.IsInt then Exit;
  Result := True;
  I := val.GetIntValue;
end;

function GetObjectKeyValueIntDef(const AObj: ICefv8Value; const AKey: ustring;
  const ADefault: Integer): Integer;
begin
  if not GetObjectKeyValueInt(AObj, AKey, Result) then
    Result := ADefault;
end;

function FindFunction(const ACtx: ICefv8Context; const AName: ustring; out AFunc: ICefv8Value): Boolean;
var
  err: ICefV8Exception = nil;
begin
  Result := (ACtx <> nil) and ACtx.Eval(AName, '', 0, AFunc{%H-}, err) and
        AFunc.IsFunction;
  if not Result then
    AFunc := nil;
end;

function CefValueToCefv8Value(const AValue: ICefValue): ICefv8Value;
begin
  if (AValue = nil) or not AValue.IsValid then
    begin
      Result := TCefv8ValueRef.NewUndefined;
      Exit;
    end;
  try
    case AValue.GetType of
      VTYPE_NULL: Result := TCefv8ValueRef.NewNull;
      VTYPE_BOOL: Result := TCefv8ValueRef.NewBool(AValue.GetBool);
      VTYPE_INT: Result := TCefv8ValueRef.NewInt(AValue.GetInt);
      VTYPE_DOUBLE: Result := TCefv8ValueRef.NewDouble(AValue.GetDouble);
      VTYPE_STRING: Result := TCefv8ValueRef.NewString(AValue.GetString);
      //VTYPE_BINARY: Result := TCefv8ValueRef.NewBool(AValue.GetBool);
      //VTYPE_DICTIONARY: Result := TCefv8ValueRef.NewBool(AValue.GetBool);
      //VTYPE_LIST: Result := TCefv8ValueRef.NewBool(AValue.GetBool);
      //VTYPE_INVALID: ;
      else
        Result := TCefv8ValueRef.NewUndefined;
    end;
  except
    Result := TCefv8ValueRef.NewUndefined;
  end;
end;

function Cefv8ValueToCefValue(const AValue: ICefv8Value): ICefValue;
var
  lst: ICefListValue;
  i: Integer;
begin
  Result := TCefValueRef.New;
  if (AValue = nil) or not AValue.IsValid then
    Exit;
  if AValue.IsString then
    Result.SetString(AValue.GetStringValue)
  else if AValue.IsInt then
    Result.SetInt(AValue.GetIntValue)
  else if AValue.IsBool then
    Result.SetBool(Integer(AValue.GetBoolValue))
  else if AValue.IsNull then
    Result.SetNull
  else if AValue.IsDouble then
    Result.SetDouble(AValue.GetDoubleValue)
  else if AValue.IsArray then
    begin
      //if List,Dictionary in List may be ERROR on exit upper stack function
      //use AddCefv8ValueToCefListValue()
      lst := TCefListValueRef.New;
      Result.SetList(lst);
      for i := 0 to AValue.GetArrayLength - 1 do
        lst.SetValue(i, Cefv8ValueToCefValue(AValue.GetValueByIndex(i)));
    end;
  //else if AValue.IsObject then;  //ToDo lst.SetDictionary();
end;

function CefListValueToCefv8ValueArray(const AList: ICefListValue;
  const AFromIndex: Integer): TCefv8ValueArray;
var
  i: Integer;
begin
  if (AList = nil) or not AList.IsValid then
    begin
      SetLength(Result, 0);
      Exit;
    end;
  SetLength(Result, AList.GetSize - AFromIndex);
  for i := 0 to Length(Result) - 1 do
    Result[i] := CefValueToCefv8Value(AList.GetValue(i + AFromIndex));
end;

function CefListValueToStringArray(const AList: ICefListValue;
  const AFromIndex: Integer): TStringArray;
var
  i: Integer;
  LenArr: NativeUInt;
begin
  SetLength(Result , 0);
  if (AList = nil) or not AList.IsValid then
    Exit;

  LenArr := AList.GetSize - AFromIndex;
  if LenArr <= 0 then Exit;

  SetLength(Result, LenArr);
  for i := 0 to LenArr - 1 do
    Result[i] := utf_16To8(AList.GetString(AFromIndex + i));
end;

function CefListValueToByteArray(const AList: ICefListValue;
  const AFromIndex: Integer): TByteDynArray;
var
  i: Integer;
  LenArr: NativeUInt;
begin
  SetLength(Result , 0);
  if (AList = nil) or not AList.IsValid then
    Exit;

  LenArr := AList.GetSize - AFromIndex;
  if LenArr <= 0 then Exit;

  SetLength(Result, LenArr);
  for i := 0 to LenArr - 1 do
    Result[i] := AList.GetInt(AFromIndex + i);
end;

procedure AddCefv8ValueToCefListValue(const AValue: ICefv8Value;
  const AList: ICefListValue; const AIndex: Integer);
var
  i: Integer;
begin
  if (AValue = nil) or not AValue.IsValid or
      (AList = nil) or not AList.IsValid then
    Exit;

  if AValue.IsArray then
    begin
      for i := 0 to AValue.GetArrayLength - 1 do
        AList.SetValue(AIndex + i, Cefv8ValueToCefValue(AValue.GetValueByIndex(i)));
    end
  else if AValue.IsObject then
    begin
      //Todo
    end
  else
    AList.SetValue(AIndex, Cefv8ValueToCefValue(AValue));
end;

function VarsToCefListValue(const AVars: array of const): ICefListValue;
var
  i: Integer;
begin
  Result := TCefListValueRef.New;
  for i := 0 to Length(AVars) - 1 do
    AddVarToCefListValue(Result, i, AVars[i]);
end;

procedure AddVarToCefListValue(const AList: ICefListValue; AIndex: Integer;
  constref AVar: TVarRec);
begin
  case AVar.VType of
    vtInteger: AList.SetInt(AIndex, AVar.VInteger);
    vtBoolean: AList.SetBool(AIndex, AVar.VBoolean);
    vtAnsiString: AList.SetString(AIndex, UTF8Decode(AnsiString(AVar.VAnsiString)));
    vtUnicodeString: AList.SetString(AIndex, ustring(AVar.VUnicodeString));
    vtWideString: AList.SetString(AIndex, WideString(AVar.VWideString));
    else
      AList.SetNull(AIndex);
  end;
end;


end.

