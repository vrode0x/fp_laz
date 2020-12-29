unit vr_ctrl_utils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, vr_utils, vr_classes, StdCtrls;

procedure form_LoadFromIni(const AFileName: string; const AForm: TForm; ASect: string = ''); overload;
procedure form_LoadFromIni(const F: IFPIniFileUTF8; const AForm: TForm; ASect: string = ''); overload;
procedure form_LoadFromIni(const F: IFPThreadIniFileUTF8; const AForm: TForm; const ASect: string = ''); overload;
procedure form_SaveToIni(const AFileName: string; const AForm: TForm; ASect: string = ''); overload;
procedure form_SaveToIni(const F: IFPIniFileUTF8; const AForm: TForm; ASect: string = ''); overload;
procedure form_SaveToIni(const F: IFPThreadIniFileUTF8; const AForm: TForm; const ASect: string = ''); overload;

function form_ByName(const FormName: string): TCustomForm; inline;

function cmb_IndexOfNoCaseSens(const cmb: TComboBox; const AItem: string): Integer;
function cmb_SelectNoCaseSens(const cmb: TComboBox; const AItem: string): Integer;
function cmb_Select(const cmb: TComboBox; const AItem: string): Integer;
//Delete and Set ItemIndex
function cmb_Delete(const cmb: TComboBox; AIndex: Integer): Boolean;

implementation

procedure form_LoadFromIni(const F: IFPThreadIniFileUTF8; const AForm: TForm;
  const ASect: string);
var
  ini: IFPIniFileUTF8;
begin
  ini := F.Lock;
  try
    form_LoadFromIni(ini, AForm, ASect);
  finally
    F.Unlock;
  end;
end;

procedure form_SaveToIni(const AFileName: string; const AForm: TForm;
  ASect: string);
var
  ini: IFPIniCachedFile;
begin
  ini := ini_GetCacheFile(AFileName);
  form_SaveToIni(ini, AForm, ASect);
end;

procedure form_SaveToIni(const F: IFPIniFileUTF8; const AForm: TForm;
  ASect: string);
var
  i: Integer;
begin
  case AForm.WindowState of
    //wsMinimized: i := 0;
    wsNormal: i := 1;
    wsMaximized: i := 2;
    else i := 0;
  end;//case
  if ASect = '' then
    begin
      ASect := AForm.Name;
      {$IFDEF TEST_MODE}
      if ASect = '' then
        ShowError('form_SaveToIni: AForm.Name = '''){$ENDIF}
    end;
  F.WriteInteger(ASect, 'WindowState', i);
  if i = 1 then
    begin
      F.WriteInteger(ASect, 'Top', AForm.Top);
      F.WriteInteger(ASect, 'Left', AForm.Left);
      F.WriteInteger(ASect, 'Height', AForm.Height);
      F.WriteInteger(ASect, 'Width', AForm.Width);
    end
  else
    begin
      F.WriteInteger(ASect, 'Top', AForm.RestoredTop);
      F.WriteInteger(ASect, 'Left', AForm.RestoredLeft);
      F.WriteInteger(ASect, 'Height', AForm.RestoredHeight);
      F.WriteInteger(ASect, 'Width', AForm.RestoredWidth);
    end;
end;

procedure form_LoadFromIni(const AFileName: string; const AForm: TForm;
  ASect: string);
var
  ini: IFPIniCachedFile;
begin
  ini := ini_GetCacheFile(AFileName);
  form_LoadFromIni(ini, AForm, ASect);
end;

procedure form_LoadFromIni(const F: IFPIniFileUTF8; const AForm: TForm;
  ASect: string);

  function _ReadInt(const AIdent: string; const ADefault, AMin: Integer): Integer;
  begin
    Result := F.ReadInteger(ASect, AIdent, ADefault);
    if Result < AMin then
      Result := AMin;
  end;

var
  i: Integer;
begin
  if ASect = '' then
    begin
      ASect := AForm.Name;
      {$IFDEF TEST_MODE}
      if ASect = '' then
        ShowError('form_LoadFromIni: AForm.Name = '''){$ENDIF}
    end;
  AForm.Top := _ReadInt('Top', AForm.Top, 1);
  AForm.Left := _ReadInt('Left', AForm.Left, 1);
  AForm.Height := _ReadInt('Height', AForm.Height, 200);
  AForm.Width := _ReadInt('Width', AForm.Width, 200);

  i := F.ReadInteger(ASect, 'WindowState', 1);
  case i of
    0: AForm.WindowState := wsMinimized;
    2: AForm.WindowState := wsMaximized;
    else //1
      AForm.WindowState := wsNormal;
  end;//case
end;

procedure form_SaveToIni(const F: IFPThreadIniFileUTF8; const AForm: TForm;
  const ASect: string);
var
  ini: IFPIniFileUTF8;
begin
  ini := F.Lock;
  try
    form_SaveToIni(ini, AForm, ASect);
  finally
    F.Unlock;
  end;
end;

function form_ByName(const FormName: string): TCustomForm;
begin
  Result := Screen.FindForm(FormName);
end;

function cmb_IndexOfNoCaseSens(const cmb: TComboBox; const AItem: string
  ): Integer;
var
  i: Integer;
begin
  for i := 0 to cmb.Items.Count - 1 do
    if SameText(cmb.Items[i], AItem) then
      Exit(i);
  Result := -1;
end;

function cmb_SelectNoCaseSens(const cmb: TComboBox; const AItem: string
  ): Integer;
begin
  Result := cmb_IndexOfNoCaseSens(cmb, AItem);
  if Result <> -1 then
    cmb.ItemIndex := Result;
end;

function cmb_Select(const cmb: TComboBox; const AItem: string): Integer;
begin
  Result := cmb.Items.IndexOf(AItem);
  if Result <> -1 then
    cmb.ItemIndex := Result;
end;

function cmb_Delete(const cmb: TComboBox; AIndex: Integer): Boolean;
var
  Items: TStrings;
begin
  Items := cmb.Items;
  if (AIndex < 0) or (AIndex >= Items.Count) then Exit(False);
  Result := True;
  Items.Delete(AIndex);
  if AIndex = Items.Count then
    Dec(AIndex);
  cmb.ItemIndex := AIndex;
end;

end.

