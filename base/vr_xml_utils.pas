unit vr_xml_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, vr_utils, vr_classes, vr_intfs,
  RegExpr, laz2_XMLRead, Laz2_DOM, LConvEncoding;

type
  TDomElementArray = array of TDOMElement;

function TryReadXMLFile(out ADoc: TXMLDocument; const AFilename: String;
    AFlags: TXMLReaderFlags = []; AIsShowError: Boolean = False): Boolean;

function xml_GetChildrenByTagName(const AParent: TDOMElement; const AName: DOMString;
    out arr: TDomElementArray): Boolean;
function xml_GetElementByTagAndAttrValue(const AParent: TDOMElement;
    const ATag, Attr, AValue: DOMString; out AResult: TDOMElement;
    const ACheckChild: Boolean = False): Boolean;
function xml_GetElementByPathAndAttrValue(const AParent: TDOMElement;
    const APath, Attr, AValue: DOMString; out AResult: TDOMElement;
    const ACheckChild: Boolean = False): Boolean;


function xml_FindNode(const AParent: TDOMNode; const AName: string; out ANode: TDOMNode): Boolean;
function xml_HasElement(const AParent: TDOMNode; const AName: string): Boolean;
function xml_FindElement(const AParent: TDOMNode; const AName: string; out AElement: TDOMElement): Boolean;
function xml_GetAttr(const ANode: TDOMElement; const Atrr: string; out AValue: string): Boolean;
function xml_AttrDef(const ANode: TDOMElement; const Atrr: string; const ADefault: string = ''): string;
function xml_AttrAsInt(const ANode: TDOMElement; const Atrr: string; const ADefault: Integer = 0): Integer;
function xml_AttrAsDouble(const ANode: TDOMElement; const Atrr: string; const ADefault: Double = 0): Double;

function xml_GetAllAttrValues(const AName: string; out AValues: IFPStringList;
    const AParent: TDOMElement): Boolean;

function xml_ChildText(const AParent: TDOMNode; const AName: string; out AValue: string): Boolean;
function xml_ChildTextDef(const AParent: TDOMNode; const AName: string; const ADefault: string = ''): string;
function xml_ChildTextAsInt(const AParent: TDOMNode; const AName: string; const ADefault: Integer = 0): Integer;
function xml_ChildAttr(const AParent: TDOMElement; const AName, Attr: string; out AValue: string): Boolean;
function xml_ChildAttrDef(const AParent: TDOMElement; const AName, Attr: string; const ADefault: string = ''): string;
function xml_ChildAttrAsInt(const AParent: TDOMElement; const AName, Attr: string; const ADefault: Integer = 0): Integer;

function xml_PathNode(const AParent: TDOMNode; const APath: string; out ANode: TDOMNode): Boolean;
function xml_PathElement(const AParent: TDOMNode; const APath: string; out AElement: TDOMElement): Boolean;
function xml_PathText(const AParent: TDOMElement; const APath: string; out AValue: string): Boolean;
function xml_PathTextDef(const AParent: TDOMElement; const APath: string;
    const ADefault: string = ''): string;
function xml_PathInt(const AParent: TDOMElement; const APath: string; out AValue: Integer): Boolean;
function xml_PathTextAsInt(const AParent: TDOMElement; const APath: string;
    const ADefault: Integer = 0): Integer;

function xml_PathAttr(const AParent: TDOMElement; const APath, Attr: string; out AValue: string): Boolean;
function xml_PathAttrDef(const AParent: TDOMElement; const APath, Attr: string;
    const ADefault: string = ''): string;
function xml_PathAttrAsInt(const AParent: TDOMElement; const APath, Attr: string;
    const ADefault: Integer = 0): Integer;
function xml_PathAttrAsDouble(const AParent: TDOMElement; const APath, Attr: string;
    const ADefault: Double = 0): Double;


function xml_AddTag(const AParent: TDOMElement; const ATag: string;
    const Attrs: array of string; const AValues: array of string): TDOMElement;
procedure xml_Clear(const AParent: TDOMElement);

implementation

var
  XmlFormatSettings: TFormatSettings;

function TryReadXMLFile(out ADoc: TXMLDocument; const AFilename: String;
  AFlags: TXMLReaderFlags; AIsShowError: Boolean): Boolean;
var
  S, sEncoding: String;
  st: TStringStreamUTF8 = nil;

  procedure _ReplaceEncoding; //<?xml version="1.0" encoding="UTF-16"?>
  var
    I: SizeInt;
    S1: RegExprString;
  begin
    I := Pos('?>', S);
    if I = 0 then Exit;
    S1 := ReplaceRegExpr('encoding.*=.*".*"', Copy(S, 1, I - 1), 'encoding="utf-8"', False);
    S := S1 + Copy(S, I, MaxInt);
  end;

begin
  try
    try
      //ReadXMLFile(ADoc, AFilename, AFlags);
      sEncoding := '';
      S := str_LoadFromFileEx(sEncoding, AFilename, False);
      if sEncoding <> EncodingUTF8 then
        _ReplaceEncoding;
      st := TStringStreamUTF8.Create(S);
      ReadXMLFile(ADoc, st, AFlags);
      Result := ADoc <> nil;
    except
      on E: Exception do
        begin
          Result := False;
          if AIsShowError then
            ShowExceptionMsg(E);
        end;
    end;
  finally
    st.Free;
  end;
end;

function xml_GetChildrenByTagName(const AParent: TDOMElement;
  const AName: DOMString; out arr: TDomElementArray): Boolean;
var
  el: TDOMElement;
  Len: Integer;
begin
  SetLength(arr, AParent.ChildNodes.Count);
  Len := 0;
  el := TDOMElement(AParent.FirstChild);
  while el <> nil do
    begin
      if (el is TDOMElement) and
            (el.CompareName(AName) = 0) then
        begin
          arr[Len] := el;
          Inc(Len);
        end;
      el := TDOMElement(el.NextSibling);
    end;
  SetLength(arr, Len);
  Result := Len > 0;
end;

function xml_GetElementByTagAndAttrValue(const AParent: TDOMElement;
  const ATag, Attr, AValue: DOMString; out AResult: TDOMElement;
  const ACheckChild: Boolean): Boolean;
var
  el: TDOMElement;
begin
  AResult := TDOMElement(AParent.FirstChild);
  while AResult <> nil do
    begin
      if (AResult is TDOMElement) and
            (AResult.CompareName(ATag) = 0) and
            SameStr(AResult.GetAttribute(Attr), AValue) then
        begin
          Exit(True);
        end;
      if ACheckChild and (AResult.FirstChild is TDOMElement) then
        begin
          Result := xml_GetElementByTagAndAttrValue(AResult,
              ATag, Attr, AValue, el, ACheckChild);
          if Result then
            begin
              AResult := el;
              Exit(True);
            end;
        end;
      AResult := TDOMElement(AResult.NextSibling);
    end;
  Result := False;
  AResult := nil;
end;

function xml_GetElementByPathAndAttrValue(const AParent: TDOMElement;
  const APath, Attr, AValue: DOMString; out AResult: TDOMElement;
  const ACheckChild: Boolean): Boolean;
var
  sPath, sTag: string;
  Node: TDOMNode;
begin
  NameValueOfString(APath, sPath{%H-}, sTag{%H-}, '/', True);
  if sTag = '' then
    begin
      Node := AParent;
      sTag := sPath;
    end
  else if not xml_PathNode(AParent, sPath, Node) or not (Node is TDOMElement) then
    Exit(False);

  Result := xml_GetElementByTagAndAttrValue(TDOMElement(Node), sTag, Attr, AValue, AResult, ACheckChild);
end;

function xml_FindNode(const AParent: TDOMNode; const AName: string; out
  ANode: TDOMNode): Boolean;
begin
  if AParent = nil then
    begin
      ANode := nil;
      Exit(False);
    end
  else
    begin
      ANode := AParent.FindNode(AName);
      Result := ANode <> nil;
    end;
end;

function xml_HasElement(const AParent: TDOMNode; const AName: string): Boolean;
var
  el: TDOMElement;
begin
  Result := xml_FindElement(AParent, AName, el);
end;

function xml_FindElement(const AParent: TDOMNode; const AName: string; out
  AElement: TDOMElement): Boolean;
begin
  TDOMNode(AElement) := AParent.FindNode(AName);
  Result := (AElement is TDOMElement);
  if not Result then
    AElement := nil;
end;

function xml_GetAttr(const ANode: TDOMElement; const Atrr: string; out
  AValue: string): Boolean;
var
  AttrNode: TDOMAttr;
begin
  AttrNode := ANode.GetAttributeNode(Atrr);
  Result := Assigned(AttrNode);
  if Result then
    AValue := AttrNode.Value
  else
    AValue := '';
end;

function xml_AttrDef(const ANode: TDOMElement; const Atrr: string;
  const ADefault: string): string;
var
  AttrNode: TDOMAttr;
begin
  AttrNode := ANode.GetAttributeNode(Atrr);
  if Assigned(AttrNode) then
    Result := AttrNode.Value
  else
    Result := ADefault;
end;

function xml_AttrAsInt(const ANode: TDOMElement; const Atrr: string;
  const ADefault: Integer): Integer;
var
  AttrNode: TDOMAttr;
begin
  AttrNode := ANode.GetAttributeNode(Atrr);
  if Assigned(AttrNode) then
    Result := StrToIntDef(AttrNode.Value, ADefault)
  else
    Result := ADefault;
end;

function xml_AttrAsDouble(const ANode: TDOMElement; const Atrr: string;
  const ADefault: Double): Double;
var
  AttrNode: TDOMAttr;
begin
  AttrNode := ANode.GetAttributeNode(Atrr);
  if Assigned(AttrNode) then
    Result := StrToFloatDef(AttrNode.Value, ADefault, XmlFormatSettings)
  else
    Result := ADefault;
end;

function xml_GetAllAttrValues(const AName: string; out AValues: IFPStringList;
  const AParent: TDOMElement): Boolean;

  procedure _CheckAttr(const AElement: TDOMElement);
  var
    sValue: DOMString;
  begin
    sValue := AElement.GetAttribute(AName);
    if sValue <> '' then
      AValues.Add(sValue);
  end;

  procedure _Do(const AElement: TDOMElement);
  var
    el: TDOMElement;
  begin
    _CheckAttr(AElement);
    el := TDOMElement(AElement.FirstChild);
    while el <> nil do
      begin
        if (el is TDOMElement) then
          begin
            _CheckAttr(el);
            _Do(el);
          end;
        el := TDOMElement(el.NextSibling);
      end;
  end;

begin
  AValues := new_StringList(True);
  _Do(AParent);
  Result := AValues.Count > 0;
  if not Result then
    AValues := nil;
end;

function xml_ChildText(const AParent: TDOMNode; const AName: string; out
  AValue: string): Boolean;
var
  Node: TDOMNode;
begin
  Node := AParent.FindNode(AName);
  Result := (Node <> nil);
  if Result then
    AValue := Node.TextContent;
end;

function xml_ChildTextDef(const AParent: TDOMNode; const AName: string;
  const ADefault: string): string;
begin
  if not xml_ChildText(AParent, AName, Result) then
    Result := ADefault;
end;

function xml_ChildTextAsInt(const AParent: TDOMNode; const AName: string;
  const ADefault: Integer): Integer;
var
  S: string;
begin
  if xml_ChildText(AParent, AName, S) then
    Result := StrToIntDef(S, ADefault)
  else
    Result := ADefault;
end;

function xml_ChildAttr(const AParent: TDOMElement; const AName, Attr: string; out
  AValue: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := xml_FindNode(AParent, AName, Node) and (Node is TDOMElement);
  if Result then
    begin
      AValue := TDOMElement(Node).GetAttribute(Attr);
      Result := AValue <> '';
    end;
end;

function xml_ChildAttrDef(const AParent: TDOMElement; const AName, Attr: string;
  const ADefault: string): string;
begin
  if not xml_ChildAttr(AParent, AName, Attr, Result) then
    Result := ADefault;
end;

function xml_ChildAttrAsInt(const AParent: TDOMElement; const AName,
  Attr: string; const ADefault: Integer): Integer;
var
  S: string;
begin
  if xml_ChildAttr(AParent, AName, Attr, S) then
    Result := StrToIntDef(S, ADefault)
  else
    Result := ADefault;
end;

function xml_PathNode(const AParent: TDOMNode; const APath: string; out
  ANode: TDOMNode): Boolean;
var
  ns: INextDelimStringGetter;
  S: string;
begin
  Result := False;
  if APath = '' then Exit;
  ANode := AParent;
  ns := New_NextDelimStringGetter(APath, '/', []);
  while ns.Next(S) do
    begin
      ANode := ANode.FindNode(S);
      if ANode = nil then Exit;
    end;
  Result := True;
end;

function xml_PathElement(const AParent: TDOMNode; const APath: string; out
  AElement: TDOMElement): Boolean;
begin
  Result := xml_PathNode(AParent, APath, TDOMNode(AElement)) and (AElement is TDOMElement);
end;

function xml_PathText(const AParent: TDOMElement; const APath: string; out AValue: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := xml_PathNode(AParent, APath, Node) and (Node is TDOMElement);
  if Result then
    begin
      AValue := TDOMElement(Node).TextContent;
      Result := AValue <> '';
    end;
end;

function xml_PathTextDef(const AParent: TDOMElement; const APath: string;
  const ADefault: string): string;
begin
  if not xml_PathText(AParent, APath, Result) then
    Result := ADefault;
end;

function xml_PathInt(const AParent: TDOMElement; const APath: string; out
  AValue: Integer): Boolean;
var
  S: string;
begin
  Result := xml_PathText(AParent, APath, S) and TryStrToInt(S, AValue);
  if not Result then
    AValue := 0;
end;

function xml_PathTextAsInt(const AParent: TDOMElement; const APath: string;
  const ADefault: Integer): Integer;
begin
  Result := StrToIntDef(xml_PathTextDef(AParent, APath), ADefault);
end;

function xml_PathAttr(const AParent: TDOMElement; const APath, Attr: string;
  out AValue: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := xml_PathNode(AParent, APath, Node) and (Node is TDOMElement);
  if Result then
    begin
      AValue := TDOMElement(Node).GetAttribute(Attr);
      Result := AValue <> '';
    end;
end;

function xml_PathAttrDef(const AParent: TDOMElement; const APath, Attr: string;
  const ADefault: string): string;
begin
  if not xml_PathAttr(AParent, APath, Attr, Result) then
    Result := ADefault;
end;

function xml_PathAttrAsInt(const AParent: TDOMElement; const APath,
  Attr: string; const ADefault: Integer): Integer;
var
  S: string;
begin
  if xml_PathAttr(AParent, APath, Attr, S) then
    Result := StrToIntDef(S, ADefault)
  else
    Result := ADefault;
end;

function xml_PathAttrAsDouble(const AParent: TDOMElement; const APath,
  Attr: string; const ADefault: Double): Double;
var
  S: string;
begin
  if xml_PathAttr(AParent, APath, Attr, S) then
    Result := StrToFloatDef(S, ADefault, XmlFormatSettings)
  else
    Result := ADefault;
end;

function xml_AddTag(const AParent: TDOMElement; const ATag: string;
  const Attrs: array of string; const AValues: array of string): TDOMElement;
var
  i: Integer;
  sValue: String;
begin
  Result := AParent.OwnerDocument.CreateElement(ATag);
  AParent.AppendChild(Result);
  for i := 0 to Min(Length(Attrs), Length(AValues)) - 1 do
    begin
      sValue := AValues[i];
      if sValue <> '' then
        Result.SetAttribute(Attrs[i], sValue);
    end;
end;

procedure xml_Clear(const AParent: TDOMElement);
var
  NextNode, Node: TDOMNode;
begin
  NextNode := AParent.FirstChild;
  while NextNode <> nil do
    begin
      Node := NextNode;
      NextNode := NextNode.NextSibling;
      AParent.RemoveChild(Node);
    end;
end;

initialization
  XmlFormatSettings := DefaultFormatSettings;
  XmlFormatSettings.DecimalSeparator := '.';

end.

