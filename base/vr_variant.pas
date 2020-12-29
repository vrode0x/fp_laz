unit vr_variant;

{$IFDEF fpc}{$mode objfpc}{$ENDIF} {$H+}
{$I vrode.inc}
interface

uses
  SysUtils, Classes, vr_types, vr_intfs;

type
  IVariant = interface;
  TIVariantProc = procedure(const AVar: IVariant);
  TIVariantMethod = procedure(const AVar: IVariant) of object;

  TIVariantKind = (ivkEmpty,
      ivkInteger, ivkDouble, ivkString, ivkBoolean,
      ivkInterface,
      ivkObject, //ivar_Obj(obj, Own); overloading assignment operator -> Own=False
      ivkError,  //ivar_Err('Error', 0): IVariant.Str-Error;.Int=Code
      ivkArray,  //see TIArrayVariant<T>
      ivkCustom  //see TICustomVariant<T>,
      );

  IVariant = interface
    ['{6ACD0A3A-D173-4299-9349-56D17D19CF02}']
    function Int(const ADefault: Int64 = 0): Int64;
    function Float(const ADefault: Extended = 0): Extended;
    function Str(const ADefault: string = ''): string;
    function WStr(const ADefault: WideString = ''): WideString;
    function Bool(const ADefault: Boolean = False): Boolean;
    function Intf: IUnknown;
    function Obj: TObject;
    function Memory: Pointer;

    function Kind: TIVariantKind;
    function Copy: IVariant;
  end;

function ivar_Obj(const AObject: TObject; const AOwn: Boolean= False): IVariant; inline;
function ivar_Err(const AError: string; const ACode: Integer = 0): IVariant; inline;
function ivar_Intf(const AUnknown: IUnknown): IVariant; inline;

{%Region Operator Overloading}

operator := (v : Int64) z : IVariant;
operator := (v : Extended) z : IVariant;
operator := (v : string) z : IVariant;
operator := (v : WideString) z : IVariant;
operator := (v : Boolean) z : IVariant;
operator := (v : TObject) z : IVariant;
//operator := (v : IUnknown) z : IVariant; //TComponent assigned as IUnknown
operator := (v : IFPObject) z : IVariant;
operator := (v : IVariantObject) z : IVariant;
operator := (v : IInterfaceList) z : IVariant;
//operator := (v : Variant) z : IVariant; //Compiler Error

operator := (v : IVariant) z : Int64;
operator := (v : IVariant) z : Extended;
operator := (v : IVariant) z : string;
operator := (v : IVariant) z : WideString;
operator := (v : IVariant) z : Boolean;
operator := (v : IVariant) z : TObject;
//operator := (v : IVariant) z : IUnknown; //Compiler Error


{%EndRegion Operator Overloading}

type
  { TIVariant }

  TIVariant = class(TInterfacedObject, IVariant)
  public
    function Int(const ADefault: Int64 = 0): Int64; virtual;
    function Str(const ADefault: string = ''): string; virtual;
    function WStr(const ADefault: WideString = ''): WideString;
    function Bool(const ADefault: Boolean = False): Boolean; virtual;
    function Float(const ADefault: Extended = 0): Extended; virtual;
    function Intf: IUnknown; virtual;
    function Obj: TObject; virtual;
    function Memory: Pointer; virtual;

    function Kind: TIVariantKind; virtual;
    function Copy: IVariant; virtual;
  end;

  { TIIntegerVariant }

  TIIntegerVariant = class(TIVariant)
  private
    FInt: Int64;
  public
    constructor Create(const AInt: Int64);

    function Int(const {%H-}ADefault: Int64 = 0): Int64; override;
    function Str(const {%H-}ADefault: string = ''): string; override;
    function Bool(const {%H-}ADefault: Boolean = False): Boolean; override;
    function Float(const {%H-}ADefault: Extended = 0): Extended; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TIFloatVariant }

  TIFloatVariant = class(TIVariant)
  private
    FFloat: Extended;
  public
    constructor Create(const AFloat: Extended);

    function Bool(const {%H-}ADefault: Boolean = False): Boolean; override;
    function Int(const {%H-}ADefault: Int64 = 0): Int64; override;
    function Str(const {%H-}ADefault: string = ''): string; override;
    function Float(const {%H-}ADefault: Extended = 0): Extended; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TIStringVariant }

  TIStringVariant = class(TIVariant)
  private
    FStr: string;
  public
    constructor Create(const AStr: string);

    function Int(const ADefault: Int64 = 0): Int64; override;
    function Str(const {%H-}ADefault: string = ''): string; override;
    function Bool(const ADefault: Boolean = False): Boolean; override;
    function Float(const ADefault: Extended = 0): Extended; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TIBooleanVariant }

  TIBooleanVariant = class(TIVariant)
  private
    FBool: Boolean;
  public
    constructor Create(const ABool: Boolean);

    function Bool(const {%H-}ADefault: Boolean = False): Boolean; override;
    function Int(const {%H-}ADefault: Int64 = 0): Int64; override;
    function Str(const {%H-}ADefault: string = ''): string; override;
    function Float(const {%H-}ADefault: Extended = 0): Extended; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TIInterfaceVariant }

  TIInterfaceVariant = class(TIVariant)
  private
    FIntf: IUnknown;
  public
    constructor Create(const AIntf: IUnknown);

    function Intf: IUnknown; override;

    function Kind: TIVariantKind; override;
    //function Copy: IVariant; override;
  end;

  { TIObjectVariant }

  TIObjectVariant = class(TIVariant)
  private
    FObj: TObject;
    FOwn: Boolean;
  public
    constructor Create(const AObj: TObject; const AOwn: Boolean = False);
    destructor Destroy; override;

    function Obj: TObject; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TIErrorVariant }

  TIErrorVariant = class(TIVariant)
  private
    FErr: string;
    FCode: Integer;
  public
    constructor Create(const AErr: string; const ACode: Integer = 0);

    function Int(const {%H-}ADefault: Int64 = 0): Int64; override;
    function Str(const {%H-}ADefault: string = ''): string; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
  end;

  { TICustomVariant }
  {**
    type
      TMyDataVariant = TICustomVariant<TMyData>;
    var
      v: IVariant;
      obj: TMyDataVariant;
      rec: TMyData;
    begin
      //v := TMyDataVariant.Create(rec);
      if Supports(v, TMyDataVariant, obj) then  //safe
        rec := obj.Data;
      rec := TMyData(v.Memory^);                //unsafe
    end;   }
  generic TICustomVariant<T> = class(TIVariant)
  private
    FData: T;
  public
    constructor Create(constref AData: T);

    function Memory: Pointer; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
    property Data: T read FData;
  end;

  { TIArrayVariant }
  {**
    type
      TMyArrVariant = TIArrayVariant<TMyRec>;
      TMyRecArray = array of TMyRec;
    var
      v: IVariant;
      arr: TMyRecArray;
      obj: TMyArrVariant;
    begin
      //v := TMyArrVariant.Create(arr);
      if Supports(v, TMyArrVariant, obj) then  //safe
        arr := obj.Arr;
      arr := TMyRecArray(v.Memory^);           //unsafe
    end;   }
  generic TIArrayVariant<T> = class(TIVariant)
  type
    TArrayType = array of T;
  private
    FArr: TArrayType;
  public
    constructor Create(constref Arr: TArrayType);

    function Memory: Pointer; override;

    function Kind: TIVariantKind; override;
    function Copy: IVariant; override;
    property Arr: TArrayType read FArr;
  end;


implementation

operator := (v: Int64)z: IVariant;
begin
  Result := TIIntegerVariant.Create(v);
end;

operator := (v: Extended)z: IVariant;
begin
  Result := TIFloatVariant.Create(v);
end;

operator := (v : string) z : IVariant;
begin
  Result := TIStringVariant.Create(v);
end;

operator := (v: WideString)z: IVariant;
begin
  Result := TIStringVariant.Create(UTF8Encode(v));
end;

operator := (v: Boolean)z: IVariant;
begin
  Result := TIBooleanVariant.Create(v);
end;

operator := (v: TObject)z: IVariant;
begin
  Result := TIObjectVariant.Create(v, False);
end;

//operator := (v: IUnknown)z: IVariant;
//begin
//  Result := TIInterfaceVariant.Create(v);
//end;

operator := (v : IFPObject) z : IVariant;
begin
  Result := TIInterfaceVariant.Create(v);
end;

operator := (v : IVariantObject) z : IVariant;
begin
  Result := TIInterfaceVariant.Create(v);
end;

operator := (v : IInterfaceList) z : IVariant;
begin
  Result := TIInterfaceVariant.Create(v);
end;

operator := (v: IVariant)z: Int64;
begin
  Result := v.Int(0);
end;

operator := (v: IVariant)z: Extended;
begin
  Result := v.Float(0);
end;

operator := (v: IVariant)z: string;
begin
  Result := v.Str('');
end;

operator := (v : IVariant) z : WideString;
begin
  Result := v.WStr('');
end;

operator := (v: IVariant)z: Boolean;
begin
  Result := v.Bool(False);
end;

operator := (v: IVariant)z: TObject;
begin
  Result := v.Obj;
end;

//operator := (v : IVariant) z : IUnknown;
//begin
//  Result := v.Intf;
//end;

function ivar_Obj(const AObject: TObject; const AOwn: Boolean): IVariant;
begin
  Result := TIObjectVariant.Create(AObject, AOwn);
end;

function ivar_Err(const AError: string; const ACode: Integer): IVariant;
begin
  Result := TIErrorVariant.Create(AError, ACode);
end;

function ivar_Intf(const AUnknown: IUnknown): IVariant;
begin
  Result := TIInterfaceVariant.Create(AUnknown);
end;

{ TIErrorVariant }

constructor TIErrorVariant.Create(const AErr: string; const ACode: Integer);
begin
  FErr := AErr;
  FCode := ACode;
end;

function TIErrorVariant.Int(const ADefault: Int64): Int64;
begin
  Result := FCode;
end;

function TIErrorVariant.Str(const ADefault: string): string;
begin
  Result := FErr;
end;

function TIErrorVariant.Kind: TIVariantKind;
begin
  Result := ivkError;
end;

function TIErrorVariant.Copy: IVariant;
begin
  Result := TIErrorVariant.Create(FErr, FCode);
end;

{ TIArrayVariant }

constructor TIArrayVariant.Create(constref Arr: TArrayType);
begin
  FArr := Arr;
end;

function TIArrayVariant.Memory: Pointer;
begin
  Result := @FArr;
end;

function TIArrayVariant.Kind: TIVariantKind;
begin
  Result := ivkArray;
end;

function TIArrayVariant.Copy: IVariant;
var
  a: TArrayType;
  i: Integer;
begin
  SetLength(a, Length(FArr));
  for i := 0 to Length(a) - 1 do
    a[i] := FArr[i];
  Result := TIArrayVariant.Create(a);
end;

{ TICustomVariant }

constructor TICustomVariant.Create(constref AData: T);
begin
  FData := AData;
end;

function TICustomVariant.Memory: Pointer;
begin
  Result := @FData;
end;

function TICustomVariant.Kind: TIVariantKind;
begin
  Result := ivkCustom;
end;

function TICustomVariant.Copy: IVariant;
begin
  Result := TICustomVariant.Create(FData);
end;

{ TIObjectVariant }

constructor TIObjectVariant.Create(const AObj: TObject; const AOwn: Boolean);
begin
  if AObj <> nil then
    begin
      FObj := AObj;
      FOwn := AOwn;
    end
  else
    begin
      FObj := TObject.Create;
      FOwn := True;
    end;
end;

destructor TIObjectVariant.Destroy;
begin
  if FOwn then
    FObj.Free;
  inherited Destroy;
end;

function TIObjectVariant.Kind: TIVariantKind;
begin
  Result := ivkObject;
end;

function TIObjectVariant.Copy: IVariant;

  function _TryCopy(out AVar: IVariant): Boolean;
  var
    obj_: TObject = nil;
  begin
    Result := False;
    try
      if not (FObj is TPersistent) then Exit;
      obj_ := FObj.newinstance;
      TPersistent(obj_).Assign(TPersistent(FObj));
      AVar := TIObjectVariant.Create(obj_, True);
    except
      if obj_ <> nil then
        obj_.Free;
    end;
  end;

begin
  if not _TryCopy(Result) then
    Result := inherited Copy;
end;

function TIObjectVariant.Obj: TObject;
begin
  Result := FObj;
end;

{ TIFloatVariant }

constructor TIFloatVariant.Create(const AFloat: Extended);
begin
  FFloat := AFloat;
end;

function TIFloatVariant.Kind: TIVariantKind;
begin
  Result := ivkDouble;
end;

function TIFloatVariant.Bool(const ADefault: Boolean): Boolean;
begin
  Result := Boolean(Round(FFloat));
end;

function TIFloatVariant.Int(const ADefault: Int64): Int64;
begin
  Result := Round(FFloat);
end;

function TIFloatVariant.Str(const ADefault: string): string;
begin
  Result := FloatToStr(FFloat);
end;

function TIFloatVariant.Float(const ADefault: Extended): Extended;
begin
  Result := FFloat;
end;

function TIFloatVariant.Copy: IVariant;
begin
  Result := TIFloatVariant.Create(FFloat);
end;

{ TIInterfaceVariant }

constructor TIInterfaceVariant.Create(const AIntf: IUnknown);
begin
  if AIntf <> nil then
    FIntf := AIntf
  else
    FIntf := TInterfacedObject.Create;
end;

function TIInterfaceVariant.Kind: TIVariantKind;
begin
  Result := ivkInterface;
end;

function TIInterfaceVariant.Intf: IUnknown;
begin
  Result := FIntf;
end;

{ TIBooleanVariant }

constructor TIBooleanVariant.Create(const ABool: Boolean);
begin
  FBool := ABool;
end;

function TIBooleanVariant.Kind: TIVariantKind;
begin
  Result := ivkBoolean;
end;

function TIBooleanVariant.Bool(const ADefault: Boolean): Boolean;
begin
  Result := FBool;
end;

function TIBooleanVariant.Int(const ADefault: Int64): Int64;
begin
  Result := Ord(FBool);
end;

function TIBooleanVariant.Str(const ADefault: string): string;
begin
  Result := BoolToStr(FBool, True);
end;

function TIBooleanVariant.Float(const ADefault: Extended): Extended;
begin
  Result := Ord(FBool);
end;

function TIBooleanVariant.Copy: IVariant;
begin
  Result := TIBooleanVariant.Create(FBool);
end;

{ TIIntegerVariant }

constructor TIIntegerVariant.Create(const AInt: Int64);
begin
  FInt := AInt;
end;

function TIIntegerVariant.Kind: TIVariantKind;
begin
  Result := ivkInteger;
end;

function TIIntegerVariant.Bool(const ADefault: Boolean): Boolean;
begin
  Result := Boolean(FInt);
end;

function TIIntegerVariant.Float(const ADefault: Extended): Extended;
begin
  Result := FInt;
end;

function TIIntegerVariant.Int(const ADefault: Int64): Int64;
begin
  Result := FInt;
end;

function TIIntegerVariant.Str(const ADefault: string): string;
begin
  Result := IntToStr(FInt);
end;

function TIIntegerVariant.Copy: IVariant;
begin
  Result := TIIntegerVariant.Create(FInt);
end;

{ TIStringVariant }

constructor TIStringVariant.Create(const AStr: string);
begin
  FStr := AStr;
end;

function TIStringVariant.Kind: TIVariantKind;
begin
  Result := ivkString;
end;

function TIStringVariant.Int(const ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(FStr, ADefault);
end;

function TIStringVariant.Str(const ADefault: string): string;
begin
  Result := FStr;
end;

function TIStringVariant.Bool(const ADefault: Boolean): Boolean;
begin
  Result := StrToBoolDef(FStr, ADefault);
end;

function TIStringVariant.Float(const ADefault: Extended): Extended;
begin
  Result := StrToFloatDef(FStr, ADefault);
end;

function TIStringVariant.Copy: IVariant;
begin
  Result := TIStringVariant.Create(FStr);
end;

{ TIVariant }

function TIVariant.Kind: TIVariantKind;
begin
  Result := ivkEmpty;
end;

function TIVariant.Int(const ADefault: Int64): Int64;
begin
  Result := ADefault;
end;

function TIVariant.Str(const ADefault: string): string;
begin
  Result := ADefault;
end;

function TIVariant.WStr(const ADefault: WideString): WideString;
begin
  Result := UTF8Decode(Str(UTF8Encode(ADefault)));
end;

function TIVariant.Bool(const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TIVariant.Float(const ADefault: Extended): Extended;
begin
  Result := ADefault;
end;

function TIVariant.Intf: IUnknown;
begin
  Result := TInterfacedObject.Create;
end;

function TIVariant.Obj: TObject;
begin
  Result := nil;
end;

function TIVariant.Memory: Pointer;
begin
  Result := nil;{$IFDEF TEST_MODE}
  raise Exception.Create('IVariant.Memory(): not allowed for this type');{$ENDIF}
end;

function TIVariant.Copy: IVariant;
begin
  Result := nil;{$IFDEF TEST_MODE}
  raise Exception.Create('IVariant.Copy(): not allowed for this type');{$ENDIF}
end;

end.

