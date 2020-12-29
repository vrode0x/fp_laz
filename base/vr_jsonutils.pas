unit vr_JsonUtils;

{$mode objfpc}{$H+}
{$I vrode.inc}

interface

uses
  Classes, SysUtils, fpjson, types, jsonparser, jsonscanner, math, vr_utils, vr_intfs;

const
{%Region fpjson enums aliases}
  jtUnknown = TJSONtype.jtUnknown;
  jtNumber = TJSONtype.jtNumber;
  jtString = TJSONtype.jtString;
  jtBoolean = TJSONtype.jtBoolean;
  jtNull = TJSONtype.jtNull;
  jtArray = TJSONtype.jtArray;
  jtObject = TJSONtype.jtObject;

  foSingleLineArray = TFormatOption.foSingleLineArray;
  foSingleLineObject = TFormatOption.foSingleLineArray;
  foDoNotQuoteMembers = TFormatOption.foSingleLineArray;
  foUseTabchar = TFormatOption.foSingleLineArray;

  JSON_TYPES_AS_STRING = [jtString, jtNumber, jtBoolean];
{%EndRegion}

type
  IJSONData = interface;
  IJsonArray = interface;
  IJsonObject = interface;

{%Region Functions}
function json_Escape(const S: string): string;
function json_Unescape(const S: string): string;

function json_GetPathStr(const AJsonText: string; const APath: string;
    out AValue: string): Boolean;
function json_GetPathInt(const AJsonText: string; const APath: string;
    out AValue: Integer): Boolean;
function json_GetPathBool(const AJsonText: string; const APath: string;
    out AValue: Boolean): Boolean;
function json_GetPathObj(const AJsonText: string; const APath: string;
    out AValue: IJsonObject): Boolean;
function json_GetPathArr(const AJsonText: string; const APath: string;
    out AValue: IJsonArray): Boolean;
function json_GetPathData(const AJsonText: string; const APath: string;
    out AValue: IJSONData): Boolean;

function json(const AJsonText: string): IJSONData;
function json_Parse(const AJsonText: string; out AJsonData: IJSONData;
    const AShowError: Boolean = False): Boolean;
function json_ParseAsArray(const AJsonText: string; out arr: IJsonArray;
    const AShowError: Boolean = False): Boolean;
function json_ParseAsObject(const AJsonText: string; out obj: IJSONObject;
    const AShowError: Boolean = False): Boolean;
function json_Validate(const AJsonText: string; const AShowError: Boolean = False): Boolean;

function json_CreateObject: IJsonObject;
function json_CreateArray: IJsonArray;
function json_CreateString(const AValue: string): IJSONData;
function json_CreateInteger(const AValue: Integer): IJSONData;
function json_CreateBoolean(const AValue: Boolean): IJSONData;
function json_CreateNull: IJSONData;
{$IFDEF DEBUG_MODE}
function json_ParseAsObjectDebug(const AJsonText: string; out AJsonObject: IJSONObject;
    const AShowError: Boolean = False): Boolean;
{$ENDIF}
//implementation only >>>
//function json_GetData(const jo: TJSONObject; const AName: string; out AData: TJSONData): Boolean;
//function json_GetObject(const AObject: TJSONObject; const AName: string;
//    out AResult: TJSONObject; const AIsClone: Boolean = False): Boolean;
//function json_GetPathAsData(const AObject: TJSONObject; const APath: string;
//    out AData: TJSONData): Boolean;

{%EndRegion}

type

  { IJsonData }

  IJsonData = interface(IVariantObject)
    ['{05B5044C-C935-49D2-8B27-BC11FD81374A}']
    function GetJsonData: TJSONData;

    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsString: Boolean;
    function IsInteger: Boolean;
    function IsBoolean: Boolean;
    function IsDouble: Boolean;
    function IsNull: Boolean;

    function ToInteger: Integer;
    function ToBoolean: Boolean;
    function ToString: string;
    function ToDouble: Double;
    function ToObject: IJsonObject;
    function ToArray: IJsonArray;

    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsString: string;
    function AsObject : IJsonObject;
    function AsArray: IJsonArray;

    function GetStrEx(out AType: TJSONtype): string;
    function GetStr(out AValue: string): Boolean;
    function GetInt(out AValue: Integer): Boolean;
    function GetBool(out AValue: Boolean): Boolean;
    function GetObj(out AObject: IJsonObject): Boolean;
    function GetArr(out AnArray: IJsonArray): Boolean;

    function GetData(const AName: string; out AValue: IJsonData): Boolean;
    function KeyExists(const AKey: string): Boolean;

    function JsonType: TJSONtype;
    function Clone: IJsonData;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;
  end;
  PIJsonData = ^IJsonData;

  { IJsonObject }

  IJsonObject = interface(IVariantObject)
    ['{0D1C47D7-1091-4321-8420-A3C9BBD988D6}']
  {%Region private}
    function GetItem(AIndex: Integer): IJsonData;
    function GetName(AIndex: Integer): string;
    function GetNewArr(const AName: string): IJsonArray;
    function GetNewObj(const AName: string): IJsonObject;
    procedure SetItem(AIndex: Integer; AValue: IJsonData);

    function GetPathAsArray(const APath: string): IJSONArray;
    function GetPathAsBoolean(const APath: string): Boolean;
    function GetPathAsData(const APath: string): IJSONData;
    function GetPathAsDouble(const APath: string): Double;
    function GetPathAsInteger(const APath: string): Integer;
    function GetPathAsObject(const APath: string): IJSONObject;
    function GetPathAsString(const APath: string): string;
    procedure SetPathArray(const APath: string; const AValue: IJSONArray);
    procedure SetPathBoolean(const APath: string; const AValue: Boolean);
    procedure SetPathData(const APath: string; const AValue: IJSONData);
    procedure SetPathDouble(const APath: string; const AValue: Double);
    procedure SetPathInteger(const APath: string; const AValue: Integer);
    procedure SetPathObject(const APath: string; const AValue: IJSONObject);
    procedure SetPathString(const APath: string; const AValue: string);

    function GetAsData(const AName: string): IJsonData;
    function GetAsInteger(const AName: string): Integer;
    function GetAsBoolean(const AName: string): Boolean;
    function GetAsDouble(const AName: string): Double;
    function GetAsNull(const AName: string): Boolean;
    function GetAsObject(const AName: string): IJsonObject;
    function GetAsArray(const AName: string): IJsonArray;
    function GetAsString(const AName: string): string;

    procedure SetData(const AName: string; const AValue: IJsonData);
    procedure SetInteger(const AName: string; const AValue: Integer);
    procedure SetBoolean(const AName: string; const AValue: Boolean);
    procedure SetDouble(const AName: string; const AValue: Double);
    procedure SetNull(const AName: string; const AValue: Boolean);
    procedure SetString(const AName: string; const AValue: string);
    procedure SetObject(const AName: string; const AObject: IJsonObject = nil);
    procedure SetArray(const AName: string; const Arr: IJsonArray = nil);
  {%EndRegion private}
  //public
    function GetJsonObject: TJSONObject;

    function GetIntDef(const AName: string; const ADefault: Integer = 0): Integer;
    function GetBoolDef(const AName: string; const ADefault: Boolean = False): Boolean;
    function GetStrDef(const AName: string; const ADefault: string): string;
    function GetDoubleDef(const AName: string; const ADefault: Double): Double;

    function GetPathIntDef(const APath: string; const ADefault: Integer): Integer;
    function GetPathBoolDef(const APath: string; const ADefault: Boolean): Boolean;
    function GetPathStrDef(const APath: string; const ADefault: string): string;
    function GetPathDoubleDef(const APath: string; const ADefault: Double): Double;

    function GetData(const AName: string; out AValue: IJsonData): Boolean;
    function GetInt(const AName: string; out AValue: Integer): Boolean;
    function GetBool(const AName: string; out AValue: Boolean): Boolean;
    function GetDouble(const AName: string; out AValue: Double): Boolean;
    function GetObj(const AName: string; out AValue: IJsonObject): Boolean;
    function GetArr(const AName: string; out AValue: IJsonArray): Boolean;
    function GetStr(const AName: string; out AValue: string): Boolean;
    function GetType(const AName: string): TJSONtype;

    function GetPathData(const APath: string; out AData: IJSONData): Boolean;
    function GetPathStr(const APath: string; out AResult: string): Boolean;
    function GetPathInt(const APath: string; out AResult: Integer): Boolean;
    function GetPathBool(const APath: string; out AResult: Boolean): Boolean;
    function GetPathDouble(const APath: string; out AResult: Double): Boolean;
    function GetPathObj(const APath: string; out AResult: IJSONObject): Boolean;
    function GetPathArr(const APath: string; out AResult: IJSONArray): Boolean;

    function GetAsStringEx(const AName: string; out AType: TJSONtype): string;
    function SetObjectAsText(const AName: string; const AJsonText: string =''): IJsonObject;
    function SetArrayAsText(const AName: string; const AJsonText: string =''): IJsonArray;

    function Delete(const AKey: string): Boolean;
    procedure Clear;
    function KeyExists(const AKey: string): Boolean;
    function Keys: TStringDynArray;
    function Clone: IJsonObject;
    function AsData: IJsonData;

    function Length: Integer;
    property Names[AIndex: Integer]: string read GetName;
    property Items[AIndex: Integer]: IJsonData read GetItem write SetItem; default;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;

    property PathData[const APath: string]: IJSONData read GetPathAsData write SetPathData;
    property PathStr[const APath: string]: string read GetPathAsString write SetPathString;
    property PathInt[const APath: string]: Integer read GetPathAsInteger write SetPathInteger;
    property PathBool[const APath: string]: Boolean read GetPathAsBoolean write SetPathBoolean;
    property PathDouble[const APath: string]: Double read GetPathAsDouble write SetPathDouble;
    property PathObj[const APath: string]: IJSONObject read GetPathAsObject write SetPathObject;
    property PathArr[const APath: string]: IJSONArray read GetPathAsArray write SetPathArray;

    property T[const AName: string]: TJSONtype read GetType;
    property Data[const AName: string]: IJsonData read GetAsData write SetData;
    property Int[const AName: string]: Integer read GetAsInteger write SetInteger;
    property Bool[const AName: string]: Boolean read GetAsBoolean write SetBoolean;
    property Double[const AName: string]: Double read GetAsDouble write SetDouble;
    property Null[const AName: string]: Boolean read GetAsNull write SetNull;
    property Str[const AName: string]: string read GetAsString write SetString;
    property Obj[const AName: string]: IJsonObject read GetAsObject write SetObject;
    property Arr[const AName: string]: IJsonArray read GetAsArray write SetArray;

    property NewObj[const AName: string]: IJsonObject read GetNewObj;
    property NewArr[const AName: string]: IJsonArray read GetNewArr;
  end;
  PIJsonObject = ^IJsonObject;

  { IJsonArray }

  IJsonArray = interface(IVariantObject)
    ['{83975B17-39F9-44B9-A261-BF753737E7A1}']
  //private
    function GetAsArray(const AIndex: Integer): IJsonArray;
    function GetAsBoolean(const AIndex: Integer): Boolean;
    function GetAsDouble(const AIndex: Integer): Double;
    function GetAsInteger(const AIndex: Integer): Integer;
    function GetAsNull(const AIndex: Integer): Boolean;
    function GetAsObject(const AIndex: Integer): IJsonObject;
    function GetAsString(const AIndex: Integer): string;
    function GetItem(const AIndex: Integer): IJsonData;
    function GetType(const AIndex: Integer): TJSONtype;
    procedure SetArray(const AIndex: Integer; const AValue: IJsonArray);
    procedure SetBoolean(const AIndex: Integer; const AValue: Boolean);
    procedure SetDouble(const AIndex: Integer; const AValue: Double);
    procedure SetInteger(const AIndex: Integer; const AValue: Integer);
    procedure SetItem(const AIndex: Integer; const AValue: IJsonData);
    procedure SetObject(const AIndex: Integer; const AValue: IJsonObject);
    procedure SetString(const AIndex: Integer; const AValue: string);
  //public
    function GetJsonArray: TJSONArray;
    function Length: Integer;

    function GetStr(const AIndex: Integer; out S: string): Boolean;
    function GetInt(const AIndex: Integer; out I: Integer): Boolean;
    function GetBool(const AIndex: Integer; out B: Boolean): Boolean;
    function GetObj(const AIndex: Integer; out AObject: IJsonObject): Boolean;
    function GetArr(const AIndex: Integer; out arr: IJsonArray): Boolean;
    function GetData(const AIndex: Integer; out AData: IJsonData): Boolean;

    //if AIndex < 0 then Add
    function AddStr(const AValue: string; AIndex: Integer = -1): IJsonData;
    function AddInt(const AValue: Integer; AIndex: Integer = -1): IJsonData;
    function AddBool(const AValue: Boolean; AIndex: Integer = -1): IJsonData;
    function AddDoulbe(const AValue: Double; AIndex: Integer = -1): IJsonData;
    function AddNull(const AIndex: Integer = -1): IJsonData;
    function AddData(const AData: IJsonData; AIndex: Integer = -1): IJsonData;
    function AddObj(const AObject: IJsonObject = nil; AIndex: Integer = -1): IJsonObject;
    function AddArr(const Arr: IJsonArray; AIndex: Integer = -1): IJsonArray;

    function AddArrAsText(const AJsonText: string = ''; const AIndex: Integer = -1): IJsonArray;
    function AddObjAsText(const AJsonText: string = ''; const AIndex: Integer = -1): IJsonObject;

    function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AStart: Integer; AEnd: Integer): Boolean; overload;
    procedure Clear;

    function Clone: IJsonArray;
    function AsData: IJsonData;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;

    property Items[const AIndex: Integer]: IJsonData read GetItem write SetItem; default;
    property T[const AIndex: Integer]: TJSONtype read GetType;
    property Int[const AIndex: Integer]: Integer read GetAsInteger write SetInteger;
    property Bool[const AIndex: Integer]: Boolean read GetAsBoolean write SetBoolean;
    property Double[const AIndex: Integer]: Double read GetAsDouble write SetDouble;
    property Null[const AIndex: Integer]: Boolean read GetAsNull;
    property Str[const AIndex: Integer]: string read GetAsString write SetString;
    property Obj[const AIndex: Integer]: IJsonObject read GetAsObject write SetObject;
    property Arr[const AIndex: Integer]: IJsonArray read GetAsArray write SetArray;
  end;
  PIJsonArray = ^IJsonArray;

{$IFDEF TEST_MODE}
function json_GetLastError: string;
{$ENDIF}

implementation

{$IFDEF TEST_MODE}
ThreadVar
  _LastError: string;

function json_GetLastError: string;
begin
  Result := _LastError;
end;
{$ENDIF}

type
  { TIJsonObject }

  TIJsonObject = class(TInterfacedObject, IJsonObject)
  private
    FJsonObject: TJSONObject;
    FRoot: IUnknown;
    function GetItem(AIndex: Integer): IJsonData;
    function GetName(AIndex: Integer): string;
    function GetPathAsDouble(const APath: string): Double;
    procedure SetItem(AIndex: Integer; AValue: IJsonData);

    procedure SetData(const AName: string; const AValue: IJsonData);
    procedure SetInteger(const AName: string; const AValue: Integer);
    procedure SetBoolean(const AName: string; const AValue: Boolean);
    procedure SetDouble(const AName: string; const AValue: Double);
    procedure SetNull(const AName: string; const AValue: Boolean);
    procedure SetString(const AName: string; const AValue: string);
    procedure SetObject(const AName: string; const AObject: IJsonObject = nil);
    procedure SetArray(const AName: string; const Arr: IJsonArray = nil);

    function GetAsData(const AName: string): IJsonData;
    function GetAsInteger(const AName: string): Integer;
    function GetAsBoolean(const AName: string): Boolean;
    function GetAsDouble(const AName: string): Double;
    function GetAsNull(const AName: string): Boolean;
    function GetAsObject(const AName: string): IJsonObject;
    function GetAsArray(const AName: string): IJsonArray;
    function GetAsString(const AName: string): string;

    function GetPathAsArray(const APath: string): IJSONArray;
    function GetPathAsBoolean(const APath: string): Boolean;
    function GetPathAsData(const APath: string): IJSONData;
    function GetPathAsInteger(const APath: string): Integer;
    function GetPathAsObject(const APath: string): IJSONObject;
    function GetPathAsString(const APath: string): string;

    procedure SetPathArray(const APath: string; const AValue: IJSONArray);
    procedure SetPathBoolean(const APath: string; const AValue: Boolean);
    procedure SetPathData(const APath: string; const AValue: IJSONData);
    procedure SetPathDouble(const APath: string; const AValue: Double);
    procedure SetPathInteger(const APath: string; const AValue: Integer);
    procedure SetPathObject(const APath: string; const AValue: IJSONObject);
    procedure SetPathString(const APath: string; const AValue: string);
  public
    { IJsonObject }
    function GetJsonObject: TJSONObject;
    function GetNewArr(const AName: string): IJsonArray;
    function GetNewObj(const AName: string): IJsonObject;

    function GetIntDef(const AName: string; const ADefault: Integer): Integer;
    function GetBoolDef(const AName: string; const ADefault: Boolean): Boolean;
    function GetStrDef(const AName: string; const ADefault: string): string;
    function GetDoubleDef(const AName: string; const ADefault: Double): Double;

    function GetData(const AName: string; out AValue: IJsonData): Boolean;
    function GetInt(const AName: string; out AValue: Integer): Boolean;
    function GetBool(const AName: string; out AValue: Boolean): Boolean;
    function GetDouble(const AName: string; out AValue: Double): Boolean;
    function GetNull(const AName: string): Boolean;
    function GetStr(const AName: string; out AValue: string): Boolean;
    function GetObj(const AName: string; out AValue: IJsonObject): Boolean;
    function GetArr(const AName: string; out AValue: IJsonArray): Boolean;
    function GetType(const AName: string): TJSONtype;

    function GetAsStringEx(const AName: string; out AType: TJSONtype): string;
    function SetObjectAsText(const AName: string; const AJsonText: string =''): IJsonObject;
    function SetArrayAsText(const AName: string; const AJsonText: string =''): IJsonArray;

    function Delete(const AKey: string): Boolean;
    procedure Clear;
    function KeyExists(const AKey: string): Boolean;
    function Keys: TStringDynArray;
    function Clone: IJsonObject;
    function AsData: IJsonData;

    function Length: Integer;
    property Names[AIndex: Integer]: string read GetName;
    property Items[AIndex: Integer]: IJsonData read GetItem write SetItem; default;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;

    function GetPathIntDef(const APath: string; const ADefault: Integer): Integer;
    function GetPathBoolDef(const APath: string; const ADefault: Boolean): Boolean;
    function GetPathStrDef(const APath: string; const ADefault: string): string;
    function GetPathDoubleDef(const APath: string; const ADefault: Double): Double;

    function GetPathData(const APath: string; out AData: IJSONData): Boolean;
    function GetPathStr(const APath: string; out AResult: string): Boolean;
    function GetPathInt(const APath: string; out AResult: Integer): Boolean;
    function GetPathBool(const APath: string; out AResult: Boolean): Boolean;
    function GetPathDouble(const APath: string; out AResult: Double): Boolean;
    function GetPathObj(const APath: string; out AResult: IJSONObject): Boolean;
    function GetPathArr(const APath: string; out AResult: IJSONArray): Boolean;

    property PathData[const APath: string]: IJSONData read GetPathAsData write SetPathData;
    property PathStr[const APath: string]: string read GetPathAsString write SetPathString;
    property PathInt[const APath: string]: Integer read GetPathAsInteger write SetPathInteger;
    property PathBool[const APath: string]: Boolean read GetPathAsBoolean write SetPathBoolean;
    property PathDouble[const APath: string]: Double read GetPathAsDouble write SetPathDouble;
    property PathObj[const APath: string]: IJSONObject read GetPathAsObject write SetPathObject;
    property PathArr[const APath: string]: IJSONArray read GetPathAsArray write SetPathArray;

    property T[const AName: string]: TJSONtype read GetType;
    property Data[const AName: string]: IJsonData read GetAsData write SetData;
    property Int[const AName: string]: Integer read GetAsInteger write SetInteger;
    property Bool[const AName: string]: Boolean read GetAsBoolean write SetBoolean;
    property Double[const AName: string]: Double read GetAsDouble write SetDouble;
    property Null[const AName: string]: Boolean read GetAsNull write SetNull;
    property Str[const AName: string]: string read GetAsString write SetString;
    property Obj[const AName: string]: IJsonObject read GetAsObject write SetObject;
    property Arr[const AName: string]: IJsonArray read GetAsArray write SetArray;
  public
    constructor Create(AJsonObject: TJSONObject; const ARoot: IUnknown); virtual;
    destructor Destroy; override;
  end;

  { TIJsonData }

  TIJsonData = class(TInterfacedObject, IJsonData)
  private
    FJsonData: TJsonData;
    FRoot: IUnknown;
  public
    { IJsonData }
    function GetJsonData: TJsonData;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsString: Boolean;
    function IsInteger: Boolean;
    function IsDouble: Boolean;
    function IsBoolean: Boolean;
    function IsNull: Boolean;

    function ToInteger: Integer;
    function ToBoolean: Boolean;
    function ToString_: string;
    function IJsonData.ToString = ToString_;
    function ToDouble: Double;
    function ToObject: IJsonObject;
    function ToArray: IJsonArray;

    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsString: string;
    function AsDouble: Double;
    function AsObject : IJsonObject;
    function AsArray: IJsonArray;

    function GetStrEx(out AType: TJSONtype): string;
    function GetStr(out AValue: string): Boolean;
    function GetInt(out AValue: Integer): Boolean;
    function GetBool(out AValue: Boolean): Boolean;
    function GetDouble(out AValue: Double): Boolean;
    function GetObj(out AObject: IJsonObject): Boolean;
    function GetArr(out AnArray: IJsonArray): Boolean;

    function GetData(const AName: string; out AValue: IJsonData): Boolean;
    function KeyExists(const AKey: string): Boolean;
    function JsonType: TJSONtype;
    function Clone: IJsonData;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;
  public
    constructor Create(AJsonData: TJsonData; const ARoot: IUnknown);
    destructor Destroy; override;
  end;

  { TIJsonArray }

  TIJsonArray = class(TInterfacedObject, IJsonArray)
  private
    FJsonArray: TJSONArray;
    FRoot: IUnknown;
    procedure ExpandArray(const AIndex: Integer);
    function GetAsArray(const AIndex: Integer): IJsonArray;
    function GetAsBoolean(const AIndex: Integer): Boolean;
    function GetAsDouble(const AIndex: Integer): Double;
    function GetAsInteger(const AIndex: Integer): Integer;
    function GetAsNull(const AIndex: Integer): Boolean;
    function GetAsObject(const AIndex: Integer): IJsonObject;
    function GetAsString(const AIndex: Integer): string;
    function GetType(const AIndex: Integer): TJSONtype;
    procedure SetArray(const AIndex: Integer; const AValue: IJsonArray);
    procedure SetBoolean(const AIndex: Integer; const AValue: Boolean);
    procedure SetDouble(const AIndex: Integer; const AValue: Double);
    procedure SetInteger(const AIndex: Integer; const AValue: Integer);
    procedure SetObject(const AIndex: Integer; const AValue: IJsonObject);
    procedure SetString(const AIndex: Integer; const AValue: string);
  public
    { IJsonArray }
    function GetJsonArray: TJSONArray;
    function Length: Integer;
    function GetItem(const AIndex: Integer): IJsonData;
    procedure SetItem(const AIndex: Integer; const AValue: IJsonData);

    function GetStr(const AIndex: Integer; out S: string): Boolean;
    function GetInt(const AIndex: Integer; out I: Integer): Boolean;
    function GetBool(const AIndex: Integer; out B: Boolean): Boolean;
    function GetDouble(const AIndex: Integer; out D: Double): Boolean;
    function GetObj(const AIndex: Integer; out AObject: IJsonObject): Boolean;
    function GetArr(const AIndex: Integer; out arr: IJsonArray): Boolean;
    function GetData(const AIndex: Integer; out AData: IJsonData): Boolean;

    //if AIndex < 0 then Add
    function AddStr(const AValue: string; AIndex: Integer = -1): IJsonData;
    function AddInt(const AValue: Integer; AIndex: Integer = -1): IJsonData;
    function AddBool(const AValue: Boolean; AIndex: Integer = -1): IJsonData;
    function AddDoulbe(const AValue: Double; AIndex: Integer = -1): IJsonData;
    function AddNull(const AIndex: Integer = -1): IJsonData;
    function AddData(const AData: IJsonData; AIndex: Integer = -1): IJsonData;
    function AddObj(const AObject: IJsonObject = nil; AIndex: Integer = -1): IJsonObject;
    function AddArr(const Arr: IJsonArray; AIndex: Integer = -1): IJsonArray;

    function AddArrAsText(const AJsonText: string = ''; const AIndex: Integer = -1): IJsonArray;
    function AddObjAsText(const AJsonText: string = ''; const AIndex: Integer = -1): IJsonObject;

    function Delete(AIndex: Integer): Boolean; overload;
    function Delete(AStart: Integer; AEnd: Integer): Boolean; overload;
    procedure Clear;

    function Clone: IJsonArray;
    function AsData: IJsonData;

    function AsJsonString(const AOptions: TFormatOptions = DefaultFormat;
        const AIndentsize: Integer = DefaultIndentSize): string;

    property Items[const AIndex: Integer]: IJsonData read GetItem write SetItem; default;
    property T[const AIndex: Integer]: TJSONtype read GetType;
    property Int[const AIndex: Integer]: Integer read GetAsInteger write SetInteger;
    property Bool[const AIndex: Integer]: Boolean read GetAsBoolean write SetBoolean;
    property Double[const AIndex: Integer]: Double read GetAsDouble write SetDouble;
    property Null[const AIndex: Integer]: Boolean read GetAsNull;
    property Str[const AIndex: Integer]: string read GetAsString write SetString;
    property Obj[const AIndex: Integer]: IJsonObject read GetAsObject write SetObject;
    property Arr[const AIndex: Integer]: IJsonArray read GetAsArray write SetArray;
  public
    constructor Create(AJsonArray: TJSONArray; const ARoot: IUnknown);
    destructor Destroy; override;

  end;


function json_GetData(const jo: TJSONObject; const AName: string; out
  AData: TJSONData): Boolean;
var
  i: Integer;
begin
  if jo <> nil then
    begin
      i := jo.IndexOfName(AName);
      if i <> -1 then
        begin
          AData := jo.Items[i];
          Exit(True);
        end;
    end;
  Result := False;
  AData := nil;
end;

function json_Escape(const S: string): string;
begin
  Result := StringToJSONString(S);
end;

function json_Unescape(const S: string): string;
begin
  Result := JSONStringToString(S);
end;

function json_GetObject(const AObject: TJSONObject; const AName: string; out
  AResult: TJSONObject; const AIsClone: Boolean = False): Boolean;
var
  jd: TJSONData;
begin
  if json_GetData(AObject, AName, jd) and
      (jd.JSONType = jtObject) then
    begin
      if AIsClone then
        AResult := TJSONObject(jd.Clone)
      else
        AResult := TJSONObject(jd);
      Exit(True);
    end;
  Result := False;
  AResult := nil;
end;

function json_GetPathAsData(const AObject: TJSONObject;
  const APath: string; out AData: TJSONData): Boolean;
var
  i, iLast: LongInt;
  arr: TStringDynArray;
  jo: TJSONObject;
begin
  Result := False;
  AData := nil;
  str_SplitByChar(APath, '/', arr);
  iLast := Length(arr) - 1;
  jo := AObject;
  for i := 0 to iLast - 1 do
    begin
      if not json_GetObject(jo, arr[i], jo) then Exit;
    end;
  Result := json_GetData(jo, arr[iLast], AData);
end;

function json_GetPathAsObjectAndProp(const AObject: TJSONObject;
  const APath: string; out AObj: TJSONObject; out AProp: string): Boolean;
var
  i, iLast: LongInt;
  arr: TStringDynArray;
  jo: TJSONObject;
begin
  Result := False;
  AObj := nil;
  str_SplitByChar(APath, '/', arr);
  iLast := Length(arr) - 1;
  jo := AObject;
  for i := 0 to iLast - 1 do
    begin
      if not json_GetObject(jo, arr[i], jo) then Exit;
    end;
  AObj := jo;
  AProp := arr[iLast];
  Result := True;
end;

function json_GetPathStr(const AJsonText: string; const APath: string; out
  AValue: string): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathStr(APath, AValue);
end;

function json_GetPathInt(const AJsonText: string; const APath: string; out
  AValue: Integer): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathInt(APath, AValue);
end;

function json_GetPathBool(const AJsonText: string; const APath: string; out
  AValue: Boolean): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathBool(APath, AValue);
end;

function json_GetPathObj(const AJsonText: string; const APath: string; out
  AValue: IJsonObject): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathObj(APath, AValue);
end;

function json_GetPathArr(const AJsonText: string; const APath: string; out
  AValue: IJsonArray): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathArr(APath, AValue);
end;

function json_GetPathData(const AJsonText: string; const APath: string; out
  AValue: IJSONData): Boolean;
var
  obj: IJSONObject;
begin
  Result := json_ParseAsObject(AJsonText, obj) and
      obj.GetPathData(APath, AValue);
end;

function _Parse(const AJsonText: string; out jd: TJSONData; const AShowError: Boolean = False): Boolean;
var
  pr: TJSONParser;
begin
  Result := False;
  jd := nil;
  if (AJsonText = '') or
        not (AJsonText[1] in ['{', '[']) then
    begin
      jd := CreateJSON(AJsonText);
      Exit(True);
    end;

  pr := TJSONParser.Create(AJsonText{$IFDEF VER3}, [joUTF8]{$ENDIF});
  try
    try
      jd := pr.Parse;
      Result := jd <> nil;
    except
      on E: Exception do
        begin
          jd := nil;
          {$IFDEF TEST_MODE}
          _LastError := E.Message;{$ENDIF}
          if AShowError then
            ShowError('json_Parse'#10 + E.Message);
        end;
    end;
  finally
    pr.Free;
  end;
end;

function json(const AJsonText: string): IJSONData;
begin
  json_Parse(AJsonText, Result, False);
end;

function json_Parse(const AJsonText: string; out AJsonData: IJSONData;
  const AShowError: Boolean): Boolean;
var
  jd: TJSONData;
begin
  Result := _Parse(AJsonText, jd, AShowError);
  if Result then
    AJsonData := TIJsonData.Create(jd, nil);
end;

function json_ParseAsArray(const AJsonText: string; out arr: IJsonArray;
  const AShowError: Boolean): Boolean;
var
  jd: TJSONData;
begin
  Result := _Parse(AJsonText, jd, AShowError) and (jd is TJSONArray);
  if Result then
    arr := TIJsonArray.Create(TJSONArray(jd), nil)
  else if (jd <> nil) then
    jd.Free;
end;

function json_ParseAsObject(const AJsonText: string; out obj: IJSONObject;
  const AShowError: Boolean): Boolean;
var
  jd: TJSONData;
begin
  Result := _Parse(AJsonText, jd, AShowError) and (jd is TJSONObject);
  if Result then
    obj := TIJsonObject.Create(TJSONObject(jd), nil)
  else if (jd <> nil) then
    jd.Free;
end;

function json_Validate(const AJsonText: string; const AShowError: Boolean): Boolean;
var
  Data: IJSONData;
begin
  Result := json_Parse(AJsonText, Data, AShowError);
end;

function json_CreateObject: IJsonObject;
begin
  Result := TIJsonObject.Create(CreateJSONObject([]), nil);
end;

function json_CreateArray: IJsonArray;
begin
  Result := TIJsonArray.Create(CreateJSONArray([]), nil);
end;

function json_CreateString(const AValue: string): IJSONData;
begin
  Result := TIJsonData.Create(CreateJSON(AValue), nil);
end;

function json_CreateInteger(const AValue: Integer): IJSONData;
begin
  Result := TIJsonData.Create(CreateJSON(AValue), nil);
end;

function json_CreateBoolean(const AValue: Boolean): IJSONData;
begin
  Result := TIJsonData.Create(CreateJSON(AValue), nil);
end;

function json_CreateNull: IJSONData;
begin
  Result := TIJsonData.Create(CreateJSON, nil);
end;

{$IFDEF DEBUG_MODE}
type

  { TIJsonObjectDebug }

  TIJsonObjectDebug = class(TIJsonObject)
  public
    constructor Create(AJsonObject: TJSONObject; const ARoot: IUnknown); override;
    destructor Destroy; override;
  end;

function json_ParseAsObjectDebug(const AJsonText: string; out
  AJsonObject: IJSONObject; const AShowError: Boolean): Boolean;
var
  APasObject: TJSONObject;
begin
  Result := _Parse(AJsonText, TJSONData(APasObject), AShowError) and
      (APasObject is TJSONObject);
  if Result then
    AJsonObject := TIJsonObjectDebug.Create(APasObject, nil)
  else if (APasObject <> nil) then
    FreeAndNil(APasObject);
end;

var
  _iDebugCount: Integer = 0;
{ TIJsonObjectDebug }

constructor TIJsonObjectDebug.Create(AJsonObject: TJSONObject;
  const ARoot: IUnknown);
begin
  inherited Create(AJsonObject, ARoot);
  Inc(_iDebugCount);
end;

destructor TIJsonObjectDebug.Destroy;
begin
  Dec(_iDebugCount);
  inherited Destroy;
end;
{$ENDIF}

{ TIJsonArray }

function TIJsonArray.GetJsonArray: TJSONArray;
begin
  Result := FJsonArray;
end;

function TIJsonArray.Length: Integer;
begin
  Result := FJsonArray.Count;
end;

function TIJsonArray.GetItem(const AIndex: Integer): IJsonData;
begin
  GetData(AIndex, Result);
  //Result := TIJsonData.Create(FJsonArray.Items[AIndex], Self);
end;

procedure TIJsonArray.SetItem(const AIndex: Integer; const AValue: IJsonData);
begin
  FJsonArray.Items[AIndex] := AValue.GetJsonData.Clone;
end;

function TIJsonArray.GetObj(const AIndex: Integer; out AObject: IJsonObject
  ): Boolean;
var
  data: TJSONData;
begin
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtObject);
  if Result then
    AObject := TIJsonObject.Create(TJSONObject(data), Self);
end;

function TIJsonArray.GetArr(const AIndex: Integer; out arr: IJsonArray
  ): Boolean;
var
  data: TJSONData;
begin
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtArray);
  if Result then
    arr := TIJsonArray.Create(TJSONArray(data), Self);
end;

function TIJsonArray.GetData(const AIndex: Integer; out AData: IJsonData
  ): Boolean;
var
  data: TJSONData;
begin
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil);
  if Result then
    AData := TIJsonData.Create(data, Self);
end;

function TIJsonArray.GetStr(const AIndex: Integer; out S: string): Boolean;
var
  data: TJSONData;
begin
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtString);
  if Result then
    S := data.AsString;
end;

function TIJsonArray.GetInt(const AIndex: Integer; out I: Integer): Boolean;
var
  data: TJSONData;
begin
  I := 0;
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtNumber);
  if Result then
    I := data.AsInteger;
end;

function TIJsonArray.GetBool(const AIndex: Integer; out B: Boolean): Boolean;
var
  data: TJSONData;
begin
  B := False;
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtBoolean);
  if Result then
    B := data.AsBoolean;
end;

function TIJsonArray.GetDouble(const AIndex: Integer; out D: Double): Boolean;
var
  data: TJSONData;
begin
  D := 0;
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit(False);
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtNumber);
  if Result then
    D := data.AsFloat;
end;

function TIJsonArray.AddStr(const AValue: string; AIndex: Integer): IJsonData;
begin
  if AIndex < 0 then
    Result := Items[FJsonArray.Add(AValue)]
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := CreateJSON(AValue);
      Result := Items[AIndex];
    end;
end;

function TIJsonArray.AddInt(const AValue: Integer; AIndex: Integer): IJsonData;
begin
  if AIndex < 0 then
    Result := Items[FJsonArray.Add(AValue)]
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := CreateJSON(AValue);
      Result := Items[AIndex];
    end;
end;

function TIJsonArray.AddBool(const AValue: Boolean; AIndex: Integer): IJsonData;
begin
  if AIndex < 0 then
    Result := Items[FJsonArray.Add(AValue)]
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := CreateJSON(AValue);
      Result := Items[AIndex];
    end;
end;

function TIJsonArray.AddDoulbe(const AValue: Double; AIndex: Integer): IJsonData;
begin
  if AIndex < 0 then
    Result := Items[FJsonArray.Add(AValue)]
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := CreateJSON(AValue);
      Result := Items[AIndex];
    end;
end;

function TIJsonArray.AddNull(const AIndex: Integer): IJsonData;
begin
  if AIndex < 0 then
    Result := Items[FJsonArray.Add(CreateJSON)]
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := CreateJSON;
      Result := Items[AIndex];
    end;
end;

function TIJsonArray.AddData(const AData: IJsonData; AIndex: Integer): IJsonData;
var
  data: TJSONData;
begin
  if AData = nil then
    data := CreateJSON
  else
    data := AData.GetJsonData.Clone;
  if AIndex < 0 then
    AIndex := FJsonArray.Add(data)
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := data;
    end;
  Result := Items[AIndex];
end;

function TIJsonArray.AddObj(const AObject: IJsonObject; AIndex: Integer): IJsonObject;
var
  data: TJSONData;
begin
  if AObject = nil then
    data := CreateJSONObject([])
  else
    data := AObject.GetJsonObject.Clone;
  if AIndex < 0 then
    AIndex := FJsonArray.Add(data)
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := data;
    end;
  GetObj(AIndex, Result);
end;

function TIJsonArray.AddArr(const Arr: IJsonArray; AIndex: Integer): IJsonArray;
var
  data: TJSONData;
begin
  if Arr = nil then
    data := CreateJSONArray([])
  else
    data := Arr.GetJsonArray.Clone;
  if AIndex < 0 then
    AIndex := FJsonArray.Add(data)
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := data;
    end;
  GetArr(AIndex, Result);
end;

function TIJsonArray.AddArrAsText(const AJsonText: string; const AIndex: Integer
  ): IJsonArray;
var
  jd: TJSONData;
begin
  Result := nil;
  if AJsonText = '' then
    jd := CreateJSONArray([])
  else if not _Parse(AJsonText, jd, False) or not (jd is TJSONArray) then
    Exit;
  if AIndex < 0 then
    FJsonArray.Add(jd)
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := jd;
    end;
  Result := TIJsonArray.Create(TJSONArray(jd), Self);
end;

function TIJsonArray.AddObjAsText(const AJsonText: string; const AIndex: Integer
  ): IJsonObject;
var
  jd: TJSONData;
begin
  Result := nil;
  if AJsonText = '' then
    jd := CreateJSONObject([])
  else if not _Parse(AJsonText, jd, False) or not (jd is TJSONObject) then
    Exit;
  if AIndex < 0 then
    FJsonArray.Add(jd)
  else
    begin
      ExpandArray(AIndex);
      FJsonArray.Items[AIndex] := jd;
    end;
  Result := TIJsonObject.Create(TJSONObject(jd), Self);
end;

procedure TIJsonArray.ExpandArray(const AIndex: Integer);
begin
  while FJsonArray.Count <= AIndex do
    FJsonArray.Add(TJSONNull.Create);
end;

function TIJsonArray.GetAsArray(const AIndex: Integer): IJsonArray;
begin
  GetArr(AIndex, Result);
end;

function TIJsonArray.GetAsBoolean(const AIndex: Integer): Boolean;
begin
  GetBool(AIndex, Result);
end;

function TIJsonArray.GetAsDouble(const AIndex: Integer): Double;
begin
  GetDouble(AIndex, Result);
end;

function TIJsonArray.GetAsInteger(const AIndex: Integer): Integer;
begin
  GetInt(AIndex, Result);
end;

function TIJsonArray.GetAsNull(const AIndex: Integer): Boolean;
var
  data: TJSONData;
begin
  Result := False;
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit;
  data := FJsonArray.Items[AIndex];
  Result := (data <> nil) and (data.JSONType = jtNull);
end;

function TIJsonArray.GetAsObject(const AIndex: Integer): IJsonObject;
begin
  GetObj(AIndex, Result);
end;

function TIJsonArray.GetAsString(const AIndex: Integer): string;
begin
  GetStr(AIndex, Result);
end;

function TIJsonArray.GetType(const AIndex: Integer): TJSONtype;
var
  data: TJSONData;
begin
  Result := jtUnknown;
  if (AIndex < 0) or (AIndex >= FJsonArray.Count) then Exit;
  data := FJsonArray.Items[AIndex];
  if (data <> nil) then
    Result := data.JSONType;
end;

procedure TIJsonArray.SetArray(const AIndex: Integer; const AValue: IJsonArray);
begin
  if AValue = nil then
    FJsonArray.Arrays[AIndex] := CreateJSONArray([])
  else
    FJsonArray.Items[AIndex] := AValue.GetJsonArray.Clone;
end;

procedure TIJsonArray.SetBoolean(const AIndex: Integer; const AValue: Boolean);
begin
  FJsonArray.Booleans[AIndex] := AValue;
end;

procedure TIJsonArray.SetDouble(const AIndex: Integer; const AValue: Double);
begin
  FJsonArray.Floats[AIndex] := AValue;
end;

procedure TIJsonArray.SetInteger(const AIndex: Integer; const AValue: Integer);
begin
  FJsonArray.Integers[AIndex] := AValue;
end;

procedure TIJsonArray.SetObject(const AIndex: Integer; const AValue: IJsonObject);
begin
  if AValue = nil then
    FJsonArray.Objects[AIndex] := CreateJSONObject([])
  else
    FJsonArray.Items[AIndex] := AValue.GetJsonObject.Clone;
end;

procedure TIJsonArray.SetString(const AIndex: Integer; const AValue: string);
begin
  FJsonArray.Strings[AIndex] := AValue;
end;

function TIJsonArray.Delete(AIndex: Integer): Boolean;
begin
  Result := True;
  FJsonArray.Delete(AIndex);
end;

function TIJsonArray.Delete(AStart: Integer; AEnd: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if AStart < 0 then
    AStart := 0;
  for i := Min(AEnd, FJsonArray.Count - 1) downto AStart do
    FJsonArray.Delete(i);
end;

procedure TIJsonArray.Clear;
begin
  FJsonArray.Clear;
end;

function TIJsonArray.Clone: IJsonArray;
begin
  Result := TIJsonArray.Create(TJSONArray(FJsonArray.Clone), nil);
end;

function TIJsonArray.AsData: IJsonData;
begin
  Result := TIJsonData.Create(FJsonArray, Self);
end;

function TIJsonArray.AsJsonString(const AOptions: TFormatOptions;
  const AIndentsize: Integer): string;
begin
  Result := FJsonArray.FormatJSON(AOptions, AIndentsize);
end;

constructor TIJsonArray.Create(AJsonArray: TJSONArray; const ARoot: IUnknown);
begin
  FJsonArray := AJsonArray;
  FRoot := ARoot;
end;

destructor TIJsonArray.Destroy;
begin
  if FRoot = nil then
    FJsonArray.Free;
  inherited Destroy;
end;

{ TIJsonData }

function TIJsonData.GetJsonData: TJsonData;
begin
  Result := FJsonData;
end;

function TIJsonData.IsObject: Boolean;
begin
  Result := FJsonData.JSONType = jtObject;
end;

function TIJsonData.IsArray: Boolean;
begin
  Result := FJsonData.JSONType = jtArray;
end;

function TIJsonData.IsString: Boolean;
begin
  Result := FJsonData.JSONType = jtString;
end;

function TIJsonData.IsInteger: Boolean;
begin
  Result := (FJsonData.JSONType = jtNumber) and not (FJsonData is TJSONFloatNumber);
end;

function TIJsonData.IsDouble: Boolean;
begin
  Result := FJsonData is TJSONFloatNumber;
end;

function TIJsonData.IsBoolean: Boolean;
begin
  Result := FJsonData.JSONType = jtBoolean;
end;

function TIJsonData.IsNull: Boolean;
begin
  Result := FJsonData.JSONType = jtNull;
end;

function TIJsonData.ToInteger: Integer;
begin
  Result := FJsonData.AsInteger;
end;

function TIJsonData.ToBoolean: Boolean;
begin
  Result := FJsonData.AsBoolean;
end;

function TIJsonData.ToString_: string;
begin
  Result := FJsonData.AsString;
end;

function TIJsonData.ToDouble: Double;
begin
  Result := FJsonData.AsFloat;
end;

function TIJsonData.ToObject: IJsonObject;
begin
  GetObj(Result);
end;

function TIJsonData.ToArray: IJsonArray;
begin
  GetArr(Result);
end;

function TIJsonData.AsInteger: Integer;
begin
  GetInt(Result);
end;

function TIJsonData.AsBoolean: Boolean;
begin
  GetBool(Result);
end;

function TIJsonData.AsString: string;
var
  t: TJSONtype;
begin
  Result := GetStrEx(t);
end;

function TIJsonData.AsDouble: Double;
begin
  GetDouble(Result);
end;

function TIJsonData.AsObject: IJsonObject;
begin
  GetObj(Result);
end;

function TIJsonData.AsArray: IJsonArray;
begin
  GetArr(Result);
end;

function TIJsonData.GetStrEx(out AType: TJSONtype): string;
begin
  AType := FJsonData.JSONType;
  if (AType in JSON_TYPES_AS_STRING) then
    Result := FJsonData.AsString
  else
    Result := '';
end;

function TIJsonData.GetStr(out AValue: string): Boolean;
var
  jt: TJSONtype;
begin
  AValue := GetStrEx(jt);
  Result := jt in JSON_TYPES_AS_STRING;
end;

function TIJsonData.GetInt(out AValue: Integer): Boolean;
begin
  Result := FJsonData.JSONType in [jtNumber, jtBoolean];
  if Result then
    begin
      AValue := FJsonData.AsInteger;
      Result := (FJsonData.JSONType = jtNumber) and not (FJsonData is TJSONFloatNumber);
    end
  else
    AValue := 0;
end;

function TIJsonData.GetBool(out AValue: Boolean): Boolean;
begin
  Result := FJsonData.JSONType in [jtNumber, jtBoolean];
  if Result then
    begin
      AValue := FJsonData.AsBoolean;
      Result := FJsonData.JSONType = jtBoolean;
    end
  else
    AValue := False;
end;

function TIJsonData.GetDouble(out AValue: Double): Boolean;
begin
  Result := FJsonData.JSONType in [jtNumber, jtBoolean];
  if Result then
    begin
      AValue := FJsonData.AsInteger;
      Result := FJsonData is TJSONFloatNumber;
    end
  else
    AValue := 0;
end;

function TIJsonData.GetObj(out AObject: IJsonObject): Boolean;
begin
  Result := (FJsonData.JSONType = jtObject);
  if Result then
    AObject := TIJsonObject.Create(TJSONObject(FJsonData), Self);
end;

function TIJsonData.GetArr(out AnArray: IJsonArray): Boolean;
begin
  Result := FJsonData.JSONType = jtArray;
  if Result then
    AnArray := TIJsonArray.Create(TJSONArray(FJsonData), Self);
end;

function TIJsonData.GetData(const AName: string; out AValue: IJsonData): Boolean;
var
  Data: TJSONData;
begin
  if (FJsonData.JSONType = jtObject) then
    begin
      Data := TJSONObject(FJsonData).Find(AName);
      Result := (Data <> nil);
      if Result then
        AValue := TIJsonData.Create(Data, Self);
    end
  else
    Result := False;
end;

function TIJsonData.KeyExists(const AKey: string): Boolean;
begin
  Result := (FJsonData.JSONType = jtObject) and
      (TJSONObject(FJsonData).Find(AKey) <> nil);
end;

function TIJsonData.JsonType: TJSONtype;
begin
  Result := FJsonData.JSONType;
end;

function TIJsonData.Clone: IJsonData;
var
  Data: TJSONData;
begin
  Data := FJsonData.Clone;
  Result := TIJsonData.Create(Data, nil);
end;

function TIJsonData.AsJsonString(const AOptions: TFormatOptions;
  const AIndentsize: Integer): string;
begin
  Result := FJsonData.FormatJSON(AOptions, AIndentsize);
end;

constructor TIJsonData.Create(AJsonData: TJsonData; const ARoot: IUnknown);
begin
  FJsonData := AJsonData;
  FRoot := ARoot;
end;

destructor TIJsonData.Destroy;
begin
  if FRoot = nil then
    FJsonData.Free;
  inherited Destroy;
end;

{ TIJsonObject }

function TIJsonObject.GetItem(AIndex: Integer): IJsonData;
begin
  Result := TIJsonData.Create(FJsonObject.Items[AIndex], Self);
end;

function TIJsonObject.GetName(AIndex: Integer): string;
begin
  Result := FJsonObject.Names[AIndex];
end;

function TIJsonObject.GetPathAsDouble(const APath: string): Double;
begin
  GetPathDouble(APath, Result);
end;

function TIJsonObject.GetPathAsArray(const APath: string): IJSONArray;
begin
  GetPathArr(APath, Result);
end;

function TIJsonObject.GetPathAsBoolean(const APath: string): Boolean;
begin
  GetPathBool(APath, Result);
end;

function TIJsonObject.GetPathAsData(const APath: string): IJSONData;
begin
  GetPathData(APath, Result);
end;

function TIJsonObject.GetPathAsInteger(const APath: string): Integer;
begin
  GetPathInt(APath, Result);
end;

function TIJsonObject.GetPathAsObject(const APath: string): IJSONObject;
begin
  GetPathObj(APath, Result);
end;

function TIJsonObject.GetPathAsString(const APath: string): string;
begin
  GetPathStr(APath, Result);
end;

procedure TIJsonObject.SetItem(AIndex: Integer; AValue: IJsonData);
begin
  FJsonObject.Items[AIndex] := AValue.GetJsonData.Clone;
end;

procedure TIJsonObject.SetPathArray(const APath: string;
  const AValue: IJSONArray);
var
  jo: TJSONObject;
  sProp: string;
  jd: TJSONData;
begin
  if not json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then Exit;
  if AValue = nil then
    jd := CreateJSONArray([])
  else
    jd := AValue.GetJsonArray.Clone;
  jo.Elements[sProp] := jd;
end;

procedure TIJsonObject.SetPathBoolean(const APath: string; const AValue: Boolean);
var
  jo: TJSONObject;
  sProp: string;
begin
  if json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then
    jo.Booleans[sProp] := AValue;
end;

procedure TIJsonObject.SetPathData(const APath: string; const AValue: IJSONData);
var
  jo: TJSONObject;
  sProp: string;
  jd: TJSONData;
begin
  if not json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then Exit;
  if AValue = nil then
    jd := CreateJSON
  else
    jd := AValue.GetJsonData.Clone;
  jo.Elements[sProp] := jd;
end;

procedure TIJsonObject.SetPathDouble(const APath: string; const AValue: Double);
var
  jo: TJSONObject;
  sProp: string;
begin
  if json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then
    jo.Floats[sProp] := AValue;
end;

procedure TIJsonObject.SetPathInteger(const APath: string; const AValue: Integer);
var
  jo: TJSONObject;
  sProp: string;
begin
  if json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then
    jo.Integers[sProp] := AValue;
end;

procedure TIJsonObject.SetPathObject(const APath: string; const AValue: IJSONObject);
var
  jo: TJSONObject;
  sProp: string;
  jd: TJSONData;
begin
  if not json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then Exit;
  if AValue = nil then
    jd := CreateJSONObject([])
  else
    jd := AValue.GetJsonObject.Clone;
  jo.Elements[sProp] := jd;
end;

procedure TIJsonObject.SetPathString(const APath: string; const AValue: string);
var
  jo: TJSONObject;
  sProp: string;
begin
  if json_GetPathAsObjectAndProp(FJsonObject, APath, jo, sProp) then
    jo.Strings[sProp] := AValue;
end;

function TIJsonObject.GetJsonObject: TJSONObject;
begin
  Result := FJsonObject;
end;

function TIJsonObject.GetNewArr(const AName: string): IJsonArray;
var
  a: TJSONArray;
begin
  a := CreateJSONArray([]);
  FJsonObject.Arrays[AName] := a;
  Result := TIJsonArray.Create(a, Self)
end;

function TIJsonObject.GetNewObj(const AName: string): IJsonObject;
var
  o: TJSONObject;
begin
  o := CreateJSONObject([]);
  FJsonObject.Objects[AName] := o;
  Result := TIJsonObject.Create(o, Self)
end;

function TIJsonObject.GetData(const AName: string; out AValue: IJsonData): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName);
  Result := jd <> nil;
  if Result then
    AValue := TIJsonData.Create(jd, Self)
  else
    AValue := nil;
end;

function TIJsonObject.GetInt(const AName: string; out AValue: Integer): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtNumber);
  Result := jd <> nil;
  if Result then
    AValue := jd.AsInteger
  else
    AValue := 0;
end;

function TIJsonObject.GetIntDef(const AName: string; const ADefault: Integer): Integer;
begin
  if not GetInt(AName, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetBoolDef(const AName: string; const ADefault: Boolean
  ): Boolean;
begin
  if not GetBool(AName, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetStrDef(const AName: string; const ADefault: string
  ): string;
begin
  if not GetStr(AName, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetDoubleDef(const AName: string; const ADefault: Double): Double;
begin
  if not GetDouble(AName, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetBool(const AName: string; out AValue: Boolean
  ): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtBoolean);
  Result := jd <> nil;
  if Result then
    AValue := jd.AsBoolean
  else
    AValue := False;
end;

function TIJsonObject.GetDouble(const AName: string; out AValue: Double): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtNumber);
  Result := jd <> nil;
  if Result then
    AValue := jd.AsFloat
  else
    AValue := 0;
end;

function TIJsonObject.GetNull(const AName: string): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtNull);
  Result := (jd <> nil) and jd.IsNull;
end;

function TIJsonObject.GetStr(const AName: string; out AValue: string): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtString);
  Result := jd <> nil;
  if Result then
    AValue := jd.AsString;
end;

function TIJsonObject.GetAsData(const AName: string): IJsonData;
begin
  GetData(AName, Result);
end;

function TIJsonObject.GetAsInteger(const AName: string): Integer;
begin
  GetInt(AName, Result);
end;

function TIJsonObject.GetAsBoolean(const AName: string): Boolean;
begin
  GetBool(AName, Result);
end;

function TIJsonObject.GetAsDouble(const AName: string): Double;
begin
  GetDouble(AName, Result);
end;

function TIJsonObject.GetAsNull(const AName: string): Boolean;
begin
  Result := GetNull(AName);
end;

function TIJsonObject.GetAsObject(const AName: string): IJsonObject;
begin
  GetObj(AName, Result);
end;

function TIJsonObject.GetAsArray(const AName: string): IJsonArray;
begin
  GetArr(AName, Result);
end;

function TIJsonObject.GetAsString(const AName: string): string;
var
  jt: TJSONtype;
begin
  Result := GetAsStringEx(AName, jt);
end;

function TIJsonObject.GetAsStringEx(const AName: string; out
  AType: TJSONtype): string;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName);
  if (jd <> nil) then
    AType := jd.JSONType
  else
    AType := jtUnknown;
  if (AType in JSON_TYPES_AS_STRING) then
    Result := jd.AsString;
end;

function TIJsonObject.GetObj(const AName: string; out AValue: IJsonObject): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtObject);
  Result := jd <> nil;
  if Result then
    AValue := TIJsonObject.Create(TJSONObject(jd), Self)
  else
    AValue := nil;
end;

function TIJsonObject.GetArr(const AName: string; out AValue: IJsonArray
  ): Boolean;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName, jtArray);
  Result := jd <> nil;
  if Result then
    AValue := TIJsonArray.Create(TJSONArray(jd), Self)
  else
    AValue := nil;
end;

function TIJsonObject.GetType(const AName: string): TJSONtype;
var
  jd: TJSONData;
begin
  jd := FJsonObject.Find(AName);
  if jd <> nil then
    Result := jd.JSONType
  else
    Result := jtUnknown;
end;

procedure TIJsonObject.SetData(const AName: string; const AValue: IJsonData);
begin
  FJsonObject.Elements[AName] := AValue.GetJsonData.Clone;
end;

procedure TIJsonObject.SetInteger(const AName: string; const AValue: Integer);
begin
  FJsonObject.Integers[AName] := AValue;
end;

procedure TIJsonObject.SetBoolean(const AName: string; const AValue: Boolean);
begin
  FJsonObject.Booleans[AName] := AValue;
end;

procedure TIJsonObject.SetDouble(const AName: string; const AValue: Double);
begin
  FJsonObject.Floats[AName] := AValue;
end;

procedure TIJsonObject.SetNull(const AName: string; const AValue: Boolean);
begin
  FJsonObject.Nulls[AName] := AValue;
end;

procedure TIJsonObject.SetString(const AName: string; const AValue: string);
begin
  FJsonObject.Strings[AName] := AValue;
end;

procedure TIJsonObject.SetObject(const AName: string; const AObject: IJsonObject);
begin
  if AObject = nil then
    FJsonObject.Nulls[AName] := True
  else
    begin
      FJsonObject.Objects[AName] := TJSONObject(AObject.GetJsonObject.Clone);
    end;
end;

procedure TIJsonObject.SetArray(const AName: string; const Arr: IJsonArray);
begin
  if Arr = nil then
    FJsonObject.Arrays[AName] := CreateJSONArray([])
  else
    FJsonObject.Arrays[AName] := TJSONArray(Arr.GetJsonArray.Clone);
end;

function TIJsonObject.SetObjectAsText(const AName: string;
  const AJsonText: string): IJsonObject;
var
  jd: TJSONData;
begin
  Result := nil;
  if AJsonText = '' then
    jd := CreateJSONObject([])
  else if not _Parse(AJsonText, jd, False) or not (jd is TJSONObject) then
    Exit;
  FJsonObject.Objects[AName] := TJSONObject(jd);
  Result := TIJsonObject.Create(TJSONObject(jd), Self);
end;

function TIJsonObject.SetArrayAsText(const AName: string;
  const AJsonText: string): IJsonArray;
var
  ja: TJSONData;
begin
  Result := nil;
  if AJsonText = '' then
    ja := CreateJSONArray([])
  else if not _Parse(AJsonText, ja, False) or not (ja is TJSONArray) then
    Exit;
  FJsonObject.Arrays[AName] := TJSONArray(ja);
  Result := TIJsonArray.Create(TJSONArray(ja), Self);
end;

function TIJsonObject.Delete(const AKey: string): Boolean;
begin
  Result := True;
  FJsonObject.Delete(AKey);
end;

procedure TIJsonObject.Clear;
begin
  FJsonObject.Clear;
end;

function TIJsonObject.KeyExists(const AKey: string): Boolean;
begin
  Result := FJsonObject.Find(AKey) <> nil;
end;

function TIJsonObject.Keys: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, FJsonObject.Count);
  for i := 0 to FJsonObject.Count - 1 do
    Result[i] := FJsonObject.Names[i];
end;

function TIJsonObject.Clone: IJsonObject;
begin
  Result := TIJsonObject.Create(TJSONObject(FJsonObject.Clone), nil);
end;

function TIJsonObject.AsData: IJsonData;
begin
  Result := TIJsonData.Create(FJsonObject, Self);
end;

function TIJsonObject.Length: Integer;
begin
  Result := FJsonObject.Count;
end;

function TIJsonObject.AsJsonString(const AOptions: TFormatOptions;
  const AIndentsize: Integer): string;
begin
  Result := FJsonObject.FormatJSON(AOptions, AIndentsize);
end;

function TIJsonObject.GetPathIntDef(const APath: string; const ADefault: Integer): Integer;
begin
  if not GetPathInt(APath, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetPathBoolDef(const APath: string;
  const ADefault: Boolean): Boolean;
begin
  if not GetPathBool(APath, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetPathStrDef(const APath: string; const ADefault: string): string;
begin
  if not GetPathStr(APath, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetPathDoubleDef(const APath: string;
  const ADefault: Double): Double;
begin
  if not GetPathDouble(APath, Result) then
    Result := ADefault;
end;

function TIJsonObject.GetPathData(const APath: string; out AData: IJSONData
  ): Boolean;
var
  d: TJSONData;
begin
  Result := json_GetPathAsData(FJsonObject, APath, d);
  if Result then
    AData := TIJsonData.Create(d, Self);
end;

function TIJsonObject.GetPathStr(const APath: string; out AResult: string
  ): Boolean;
var
  jd: TJSONData;
begin
  Result := json_GetPathAsData(FJsonObject, APath, jd) and
      (jd.JSONType in [jtNumber, jtString, jtBoolean]);
  if Result then
    AResult := jd.AsString;
  //else if AsJsonIfNoValue then
  //  AValue := jd.AsJSON;
end;

function TIJsonObject.GetPathInt(const APath: string; out AResult: Integer
  ): Boolean;
var
  jd: TJSONData;
begin
  Result := json_GetPathAsData(FJsonObject, APath, jd) and (jd.JSONType = jtNumber);
  if Result then
    AResult := jd.AsInteger;
end;

function TIJsonObject.GetPathBool(const APath: string; out AResult: Boolean): Boolean;
var
  jd: TJSONData;
begin
  Result := json_GetPathAsData(FJsonObject, APath, jd) and (jd.JSONType = jtBoolean);
  if Result then
    AResult := jd.AsBoolean;
end;

function TIJsonObject.GetPathDouble(const APath: string; out AResult: Double
  ): Boolean;
var
  jd: TJSONData;
begin
  Result := json_GetPathAsData(FJsonObject, APath, jd) and (jd.JSONType = jtNumber);
  if Result then
    AResult := jd.AsFloat;
end;

function TIJsonObject.GetPathObj(const APath: string; out AResult: IJSONObject): Boolean;
var
  jo: TJSONObject;
begin
  Result := json_GetPathAsData(FJsonObject, APath, TJSONData(jo)) and (jo is TJSONObject);
  if Result then
    AResult := TIJsonObject.Create(jo, Self);
end;

function TIJsonObject.GetPathArr(const APath: string; out AResult: IJSONArray): Boolean;
var
  ja: TJSONArray;
begin
  Result := json_GetPathAsData(FJsonObject, APath, TJSONData(ja)) and (ja is TJSONArray);
  if Result then
    AResult := TIJsonArray.Create(ja, Self);
end;

constructor TIJsonObject.Create(AJsonObject: TJSONObject; const ARoot: IUnknown);
begin
  FJsonObject := AJsonObject;
  FRoot := ARoot;
end;

destructor TIJsonObject.Destroy;
begin
  if FRoot = nil then
    FJsonObject.Free;
  inherited Destroy;
end;

{$IFDEF DEBUG_MODE}
finalization
  if _iDebugCount <> 0 then
    _iDebugCount := 0;
{$ENDIF}
end.

