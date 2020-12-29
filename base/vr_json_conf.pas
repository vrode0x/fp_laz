{
    This file is part of the Free Component Library

    Implementation of TJSONConfig class
    Copyright (c) 2007 Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TJSONConfig enables applications to use JSON files for storing their
  configuration data
}

{
  Modified(by Yauheni Nazimau) version of \packages\fcl-json\src\jsonconf.pp
  TODO: TvrJsonConfig = class(TJsonConfig)
}

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ENDIF}

unit vr_json_conf;

interface

uses
  SysUtils, Classes, fpjson, jsonparser, contnrs, types,
  vr_utils, LazUTF8Classes;

//resourcestring
//  SWrongRootName = 'XML file has wrong root element name';

type
  EJSONConfigError = class(Exception);
  TPathFlags = set of (pfHasValue, pfWriteAccess);

(* ********************************************************************
   "APath" is the path and name of a value: A JSON configuration file
   is hierachical. "/" is the path delimiter, the part after the last
   "/" is the name of the value. The path components will be mapped
   to nested JSON objects, with the name equal to the part. In practice
   this means that "/my/path/value" will be written as:
   {
     "my" : {
       "path" : {
         "value" : Value
       }
     }
   }
   ******************************************************************** *)

  { TJSONConfig }

  TJSONConfig = class(TComponent)
  private
    FFilename: String;
    FKey: TJSONObject;
    FArrayKey: TJSONArray;
    FStartEmpty: Boolean;
    FKeyStack: TStack;
    FArrayKeyStack: TStack;
    procedure DoSetFilename(const AFilename: String; ForceReload: Boolean);
    procedure SetFilename(const AFilename: String);
    procedure SetStartEmpty(const AValue: Boolean);
    Function StripSlash(P : String) : String;
    function EnumSubKeys_(AKey: TJSONObject; AList : TStrings): Boolean;
    function EnumValues_(AKey: TJSONObject; AList : TStrings; WithValues: Boolean = False): Boolean;
  protected
    FJSON: TJSONObject;
    FModified: Boolean;
    procedure Loaded; override;
    function FindPath(Const APath: String; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: String; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: String; AllowCreate : Boolean;Out ElName : String) : TJSONObject;
    function FindElement(Const APath: String; CreateParent : Boolean) : TJSONData;
    function FindElement(Const APath: String; CreateParent : Boolean; Out AParent : TJSONObject; Out ElName : String) : TJSONData;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(const AFileName: string); overload;
    constructor CreateClean(const AFileName: string);
    destructor Destroy; override;
    procedure Clear;
    procedure Flush;    // Writes the JSON file
    function TryOpenKey(const aPath: String; const AIsPushKey: Boolean = False): Boolean;
    procedure OpenKey(const aPath: String; AllowCreate : Boolean);
    function OpenArrayKey(const aPath: String; AllowCreate : Boolean): Boolean;
    function OpenKeyInArray(AIndex: Integer; AllowCreate : Boolean): Boolean;
    function GetArrayValues(out AValues: TStringDynArray): Boolean; overload;
    function GetArrayValues(AValues: TStrings): Boolean; overload;
    procedure SetArrayValues(const AValues: TStringDynArray); overload;
    procedure SetArrayValues(const AValues: TStrings); overload;
    procedure AddValueToArray(const AValue: string);
    function KeyExists(const AKey: string): Boolean;
    function SubKeyExists(const AKey: string): Boolean;
    function SubKeysExists: Boolean;
    procedure PushKey;
    procedure PopKey;
    procedure CloseKey;
    procedure ResetKey;
    function EnumSubKeys(Const APath : String; List: TStrings): Boolean;
    function EnumValues(Const APath : String; List: TStrings; WithValues: Boolean = False): Boolean;
    function EnumValues(Const APath : String; out arr: TStringDynArray; WithValues: Boolean = False): Boolean;
    function EnumCurrSubKeys(AList : TStrings): Boolean;
    function EnumCurrSubKeys(out arr : TStringDynArray): Boolean;
    function EnumCurrValues(List : TStrings; WithValues: Boolean = False): Boolean;
    function EnumCurrValues(out arr : TStringDynArray; WithValues: Boolean = False): Boolean;

    function  GetValue(const APath: String; const ADefault: String): String; overload;
    function  GetValue(const APath: String; ADefault: Integer): Integer; overload;
    function  GetValue(const APath: String; ADefault: Int64): Int64; overload;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean; overload;
    function  GetValue(const APath: String; ADefault: Double): Double; overload;
    procedure SetValue(const APath: String; const AValue: String); overload;
    procedure SetValue(const APath: String; AValue: Integer); overload;
    procedure SetValue(const APath: String; AValue: Int64); overload;
    procedure SetValue(const APath: String; AValue: Boolean); overload;
    procedure SetValue(const APath: String; AValue: Double); overload;

    procedure SetDeleteValue(const APath: String; const AValue, DefValue: String); overload;
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Integer); overload;
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Int64); overload;
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Boolean); overload;

    procedure DeletePath(const APath: String);
    procedure DeleteValue(const APath: String);
    property Modified: Boolean read FModified;
  published
    property Filename: String read FFilename write SetFilename;
    property StartEmpty: Boolean read FStartEmpty write SetStartEmpty;
  end;


// ===================================================================

implementation

Const
  SErrInvalidJSONFile = '"%s" is not a valid JSON configuration file.';
  SErrCouldNotOpenKey = 'Could not open key "%s".';

constructor TJSONConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSON:=TJSONObject.Create;
  FKey:=FJSON;
  FKeyStack := TStack.Create;
  FArrayKeyStack := TStack.Create;
end;

constructor TJSONConfig.Create(const AFileName: string);
begin
  Create(nil);
  DoSetFilename(AFileName, True);
end;

constructor TJSONConfig.CreateClean(const AFileName: string);
begin
  Create(nil);
  StartEmpty := True;
  DoSetFilename(AFileName, False);
end;

destructor TJSONConfig.Destroy;
begin
  FKeyStack.Free;
  FArrayKeyStack.Free;
  if Assigned(FJSON) then
    begin
    Flush;
    FreeANdNil(FJSON);
    end;
  inherited Destroy;
end;

procedure TJSONConfig.Clear;
begin
  FJSON.Clear;
  FKey:=FJSON;
end;

procedure TJSONConfig.Flush;

//Var
//  F : Text;

begin
  if Modified then
    begin
    str_SaveToFile(FFilename, FJSON.AsJSON, True);
    //file_Assign(F,FFileName);
    //try
    //  Rewrite(F);
    //except
    //  Exit;
    //end;
    //Try
    //  Writeln(F,FJSON.AsJSON);
    //Finally
    //  CloseFile(F);
    //end;
    FModified := False;
    end;
end;

function TJSONConfig.FindObject(const APath: String; AllowCreate: Boolean
  ): TJSONObject;

Var
  Dummy : String;

begin
  Result:=FindObject(APath,AllowCreate,Dummy);
end;

function TJSONConfig.FindObject(const APath: String; AllowCreate: Boolean; out
  ElName: String): TJSONObject;

Var
  S,El : String;
  P,I : Integer;
  T : TJSonObject;

begin
//  Writeln('Looking for : ', APath);
  S:=APath;
  If Pos('/',S)=1 then
    Result:=FJSON
  else
    Result:=FKey;
  Repeat
    P:=Pos('/',S);
    If (P<>0) then
      begin
      // Only real paths, ignore double slash
      If (P<>1) then
        begin
        El:=Copy(S,1,P-1);
        If (Result.Count=0) then
          I:=-1
        else
          I:=Result.IndexOfName(El);
        If (I=-1) then
          // No element with this name.
          begin
          If AllowCreate then
            begin
            // Create new node.
            T:=Result;
            Result:=TJSonObject.Create;
            T.Add(El,Result);
            end
          else
            Result:=Nil
          end
        else
          // Node found, check if it is an object
          begin
          if (Result.Items[i].JSONtype=jtObject) then
            Result:=Result.Objects[el]
          else
            begin
//            Writeln(el,' type wrong');
            If AllowCreate then
              begin
//              Writeln('Creating ',el);
              Result.Delete(I);
              T:=Result;
              Result:=TJSonObject.Create;
              T.Add(El,Result);
              end
            else
              Result:=Nil
            end;
          end;
        end;
      Delete(S,1,P);
      end;
  Until (P=0) or (Result=Nil);
  ElName:=S;
end;

function TJSONConfig.FindElement(const APath: String; CreateParent: Boolean
  ): TJSONData;

Var
  O : TJSONObject;
  ElName : String;

begin
  Result:=FindElement(APath,CreateParent,O,ElName);
end;

function TJSONConfig.FindElement(const APath: String; CreateParent: Boolean;
  out AParent: TJSONObject; out ElName: String): TJSONData;

Var
  I : Integer;

begin
  Result:=Nil;
  Aparent:=FindObject(APath,CreateParent,ElName);
  If Assigned(Aparent) then
    begin
//    Writeln('Found parent, looking for element:',elName);
    I:=AParent.IndexOfName(ElName);
//    Writeln('Element index is',I);
    If (I<>-1) And (AParent.items[I].JSONType<>jtObject) then
      Result:=AParent.Items[i];
    end;
end;


function TJSONConfig.GetValue(const APath: String; const ADefault: String): String;

var
  El : TJSONData;

begin
  try
    El:=FindElement(StripSlash(APath),False);
    If Assigned(El) then
      Result:=El.AsString
    else
      Result:=ADefault;
  except
    Result:=ADefault;
  end;
end;

function TJSONConfig.GetValue(const APath: String; ADefault: Integer): Integer;
var
  El : TJSONData;

begin
  try
    El:=FindElement(StripSlash(APath),False);
    If Not Assigned(el) then
      Result:=ADefault
    else if (el is TJSONNumber) then
      Result:=El.AsInteger
    else
      Result:=StrToIntDef(El.AsString,ADefault);
  except
    Result:=ADefault;
  end;
end;

function TJSONConfig.GetValue(const APath: String; ADefault: Int64): Int64;
var
  El : TJSONData;

begin
  try
    El:=FindElement(StripSlash(APath),False);
    If Not Assigned(el) then
      Result:=ADefault
    else if (el is TJSONNumber) then
      Result:=El.AsInt64
    else
      Result:=StrToInt64Def(El.AsString,ADefault);
  except
    Result:=ADefault;
  end;
end;

function TJSONConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;

var
  El : TJSONData;

begin
  try
    El:=FindElement(StripSlash(APath),False);
    If Not Assigned(el) then
      Result:=ADefault
    else if (el is TJSONBoolean) then
      Result:=El.AsBoolean
    else
      Result:=StrToBoolDef(El.AsString,ADefault);
  except
    Result:=ADefault;
  end;
end;

function TJSONConfig.GetValue(const APath: String; ADefault: Double): Double;

var
  El : TJSONData;

begin
  try
    El:=FindElement(StripSlash(APath),False);
    If Not Assigned(el) then
      Result:=ADefault
    else if (el is TJSONNumber) then
      Result:=El.AsFloat
    else
      Result:=StrToFloatDef(El.AsString,ADefault);
  except
    Result:=ADefault;
  end;
end;


procedure TJSONConfig.SetValue(const APath: String; const AValue: String);

var
  El : TJSONData;
  ElName : String;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (El.JSONType<>jtString) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONString.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsString:=AVAlue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: String; const AValue, DefValue: String);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: String; AValue: Integer);

var
  El : TJSONData;
  ElName : String;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONIntegerNumber)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONIntegerNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInteger:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: String; AValue: Int64);

var
  El : TJSONData;
  ElName : String;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONInt64Number)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONInt64Number.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInt64:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: String; AValue,
  DefValue: Integer);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetDeleteValue(const APath: String; AValue,
  DefValue: Int64);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: String; AValue: Boolean);

var
  El : TJSONData;
  ElName : String;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (el.JSONType<>jtBoolean) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONBoolean.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsBoolean:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: String; AValue: Double);

var
  El : TJSONData;
  ElName : String;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONFloatNumber)) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONFloatNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsFloat:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: String; AValue,
  DefValue: Boolean);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TJSONConfig.DeletePath(const APath: String);

Var
  P : String;
  L : integer;
  Node : TJSONObject;
  ElName : String;

begin
  P:=StripSlash(APath);
  L:=Length(P);
  If (L>0) then
    begin
    Node := FindObject(P,False,ElName);
    If Assigned(Node) then
      begin
      L:=Node.IndexOfName(ElName);
      If (L<>-1) then
        begin
        Node.Delete(L);
        FModified := True;
        end;
      end;
    end;
end;

procedure TJSONConfig.DeleteValue(const APath: String);

begin
  DeletePath(APath);
end;

procedure TJSONConfig.Loaded;
begin
  inherited Loaded;
  if Length(Filename) > 0 then
    DoSetFilename(Filename,True);
end;

function TJSONConfig.FindPath(const APath: String; AllowCreate: Boolean
  ): TJSONObject;

Var
  P : String;
  L : Integer;

begin
  P:=APath;
  L:=Length(P);
  If (L=0) or (P[L]<>'/') then
    P:=P+'/';
  Result:=FindObject(P,AllowCreate);
end;

procedure TJSONConfig.DoSetFilename(const AFilename: String; ForceReload: Boolean);

Var
  P : TJSONParser;
  J : TJSONData;
  F : TFileStreamUTF8;

begin
  if (not ForceReload) and file_SameName(FFilename, AFilename) then
    exit;
  FFilename := AFilename;

  if csLoading in ComponentState then
    exit;

  Flush;
  if not file_Exists(AFileName) or FStartEmpty then
    Clear
  else
    begin
    F:=TFileStreamUTF8.Create(AFileName,fmopenRead);
    try
      P:=TJSONParser.Create(F);
      try
        J:=P.Parse;
        If (J is TJSONObject) then
          begin
          FreeAndNil(FJSON);
          FJSON:=J as TJSONObject;
          FKey:=FJSON;
          end
        else
          Raise EJSONConfigError.CreateFmt(SErrInvalidJSONFile,[AFileName]);
      finally
        P.Free;
      end;
    finally
      F.Free;
    end;
    end;
end;

procedure TJSONConfig.SetFilename(const AFilename: String);
begin
  DoSetFilename(AFilename, False);
end;

procedure TJSONConfig.SetStartEmpty(const AValue: Boolean);
begin
  if AValue <> StartEmpty then
    begin
      FStartEmpty := AValue;
      if (not AValue) and not Modified then
        DoSetFilename(Filename, True);
    end;
end;

function TJSONConfig.StripSlash(P: String): String;

Var
  L : Integer;

begin
  L:=Length(P);
  If (L>0) and (P[l]='/') then
    Result:=Copy(P,1,L-1)
  else
    Result:=P;
end;

function TJSONConfig.EnumSubKeys_(AKey: TJSONObject; AList: TStrings): Boolean;
Var
  I : Integer;
begin
  AList.Clear;
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If AKey.Items[i] is TJSONObject then
        AList.Add(AKey.Names[i]);
    end;
  Result := AList.Count > 0;
end;

function TJSONConfig.EnumValues_(AKey: TJSONObject; AList: TStrings;
  WithValues: Boolean): Boolean;
Var
  I : Integer;
begin
  AList.Clear;
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If Not (AKey.Items[i] is TJSONObject) then
        begin
          if WithValues then
            AList.Add(AKey.Names[i] + '=' + AKey.Items[I].AsString)
          else
            AList.Add(AKey.Names[i]);
        end;
    end;
  Result := AList.Count > 0;
end;


procedure TJSONConfig.CloseKey;
begin
  ResetKey;
end;

function TJSONConfig.TryOpenKey(const aPath: String; const AIsPushKey: Boolean): Boolean;
begin
  if AIsPushKey then PushKey;
  try
    OpenKey(aPath, False);
    Result := FKey <> nil;
  except
    Result := False;
  end;
  if not Result then
    if AIsPushKey then PopKey
    else FKey := FJSON;
end;

procedure TJSONConfig.OpenKey(const aPath: String; AllowCreate: Boolean);

Var
  P : String;
  L : Integer;
begin
  P:=APath;
  L:=Length(P);
  If (L=0) then
    FKey:=FJSON
  else
    begin
    if (P[L]<>'/') then
      P:=P+'/';
    FKey:=FindObject(P,AllowCreate);
    If (FKey=Nil) Then
      Raise EJSONConfigError.CreateFmt(SErrCouldNotOpenKey,[APath]);
    end;
end;

function TJSONConfig.OpenArrayKey(const aPath: String; AllowCreate: Boolean): Boolean;
Var
  P : String;
  L : Integer;
  Parent: TJSONObject;
  sArray: String;
  arr: TJSONData;
begin
  Result := False;
  FArrayKey := nil;
  P:=APath;
  L:=Length(P);
  If (L>0) then
    begin
      arr:=FindElement(P,AllowCreate, Parent, sArray);
      if (arr = nil) or not (arr is TJSONArray) then
        begin
          if (Parent = nil) or not AllowCreate then Exit;
          if (arr <> nil) then
            Parent.Remove(arr);
          arr := TJSONArray.Create;
          Parent.Add(sArray, arr);
        end;
      FArrayKey := TJSONArray(arr);
      Exit(True);
    end;
  //If (FArrayKey=Nil) Then
  //  Raise EJSONConfigError.CreateFmt(SErrCouldNotOpenKey,[APath]);
end;

function TJSONConfig.OpenKeyInArray(AIndex: Integer; AllowCreate: Boolean): Boolean;
var
  data: TJSONData;
begin
  Result := False;
  FKey:=FJSON;
  if FArrayKey = nil then Exit;
  if AIndex >= FArrayKey.Count then
    begin
      if not AllowCreate then Exit;
      for AIndex := FArrayKey.Count to AIndex do
        FArrayKey.Add(TJSONObject.Create);
    end;
  data := FArrayKey.Items[AIndex];
  if not (data is TJSONObject) then
    begin
      if not AllowCreate then Exit;
      FArrayKey.Remove(data);
      data := TJSONObject.Create;
      FArrayKey.Insert(AIndex, data);
    end;
  FKey := TJSONObject(data);
  Result := True;
end;

function TJSONConfig.GetArrayValues(out AValues: TStringDynArray): Boolean;
var
  I, Len: Integer;
  a: TStringDynArray;
begin
  Result := False;
  SetLength(AValues, 0);
  Len := FArrayKey.Count;
  if (FArrayKey = nil) or (Len = 0) then Exit;
  SetLength(a, Len);
  try
    for I := 0 to Len - 1 do
      begin
        a[I] := FArrayKey.Items[I].AsString;
      end;
    Result := True;
    AValues := a;
  except
  end;
end;

function TJSONConfig.GetArrayValues(AValues: TStrings): Boolean;
var
  arr: TStringDynArray;
begin
  Result := GetArrayValues(arr);
  if Result then
    arrS_SaveToStrs(arr, AValues)
  else
    AValues.Clear;
end;

procedure TJSONConfig.SetArrayValues(const AValues: TStringDynArray);
var
  I: Integer;
begin
  if FArrayKey <> nil then
    begin
      for I := 0 to Length(AValues) - 1 do
        FArrayKey.Add(AValues[I]);
      if Length(AValues) > 0 then
        FModified := True;
    end;
end;

procedure TJSONConfig.SetArrayValues(const AValues: TStrings);
var
  arr: TStringDynArray;
begin
  arrS_LoadFromStrs(arr, AValues);
  SetArrayValues(arr);
end;

procedure TJSONConfig.AddValueToArray(const AValue: string);
begin
  if FArrayKey <> nil then
    begin
      FArrayKey.Add(AValue);
      FModified := True;
    end;
end;

function TJSONConfig.KeyExists(const AKey: string): Boolean;{$IFDEF VER3}
var
  val: TJSONData;{$ENDIF}
begin
  {$IFDEF VER3}
  Result := (FKey <> nil) and FKey.Find(AKey, val);{$ELSE}
  Result := (FKey <> nil) and (FKey.Find(AKey) <> nil);{$ENDIF}
end;

function TJSONConfig.SubKeyExists(const AKey: string): Boolean;{$IFDEF VER3}
var
  val: TJSONData;{$ENDIF}
begin
  Result := (FKey <> nil) and
      {$IFDEF VER3}FKey.Find(AKey, val) and (val is TJSONObject){$ELSE}
      (FKey.Find(AKey) is TJSONObject){$ENDIF};
end;

function TJSONConfig.SubKeysExists: Boolean;
var
  i: Integer;
begin
  If Assigned(FKey) then
    For i:=0 to FKey.Count-1 do
      If FKey.Items[i] is TJSONObject then
        Exit(True);
  Result := False;
end;

procedure TJSONConfig.PushKey;
begin
  FKeyStack.Push(FKey);
  FArrayKeyStack.Push(FArrayKey);
end;

procedure TJSONConfig.PopKey;
begin
  FKey := TJSONObject(FKeyStack.Pop);
  FArrayKey := TJSONArray(FArrayKeyStack.Pop);
end;

procedure TJSONConfig.ResetKey;
begin
  FKey:=FJSON;
  FArrayKey:=nil;
end;

function TJSONConfig.EnumSubKeys(const APath: String; List: TStrings): Boolean;

Var
  AKey : TJSONObject;

begin
  AKey:=FindPath(APath,False);
  Result := EnumSubKeys_(AKey, List);
end;

function TJSONConfig.EnumValues(const APath: String; List: TStrings;
  WithValues: Boolean): Boolean;

Var
  AKey : TJSONObject;

begin
  AKey:=FindPath(APath,False);
  Result := EnumValues_(AKey, List, WithValues);
end;

function TJSONConfig.EnumValues(const APath: String; out arr: TStringDynArray;
  WithValues: Boolean): Boolean;
var
  strs: TStringListUTF8;
begin
  strs := TStringListUTF8.Create;
  try
    Result := EnumValues(APath, strs, WithValues);
    if Result then
      arrS_LoadFromStrs(arr, strs)
    else
      SetLength(arr, 0);
  finally
    strs.Free;
  end;
end;

function TJSONConfig.EnumCurrSubKeys(AList: TStrings): Boolean;
begin
  Result := EnumSubKeys_(FKey, AList);
end;

function TJSONConfig.EnumCurrSubKeys(out arr: TStringDynArray): Boolean;
var
  strs: TStringListUTF8;
begin
  strs := TStringListUTF8.Create;
  try
    Result := EnumSubKeys_(FKey, strs);
    if Result then
      arrS_LoadFromStrs(arr, strs)
    else
      SetLength(arr, 0);
  finally
    strs.Free;
  end;
end;

function TJSONConfig.EnumCurrValues(List: TStrings; WithValues: Boolean): Boolean;
begin
  Result := EnumValues_(FKey, List, WithValues);
end;

function TJSONConfig.EnumCurrValues(out arr: TStringDynArray;
  WithValues: Boolean): Boolean;
var
  strs: TStringListUTF8;
begin
  strs := TStringListUTF8.Create;
  try
    Result := EnumValues_(FKey, strs, WithValues);
    if Result then
      arrS_LoadFromStrs(arr, strs)
    else
      SetLength(arr, 0);
  finally
    strs.Free;
  end;
end;


end.
