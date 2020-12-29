unit vr_zipper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper,
  {$IFDEF UTILS_MIN}vrUtilsMini{$ELSE}vr_utils, vr_classes{$IFDEF VER3},
  LazUTF8{$ENDIF}, FileUtil{$ENDIF};

type

  { TvrZipper }

  TvrZipper = class(TZipper)
  private
    FBasePath: string;
    FCaseSensitive: Boolean;
    procedure SetBasePath(const AValue: string);
  protected
    procedure ZipOneFile(Item: TZipFileEntry); override;
  public
    procedure AfterConstruction; override;
    property BasePath: string read FBasePath write SetBasePath;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;
{$IFNDEF UTILS_MIN}
function zip_Dir(const ADir: string; const ADest: string = '';
    const ABasePath: string = ''): Boolean;{$ENDIF}

function zip_ExtractAll(const AFileName, AOutputPath: string): Boolean;

implementation

{$IFNDEF UTILS_MIN}
function zip_Dir(const ADir: string; const ADest: string;
    const ABasePath: string): Boolean;
var
  zip: TvrZipper;
  strs: TStringListUTF8;
  I: Integer;
begin
  Result := False;
  strs := TStringListUTF8.Create;
  zip := TvrZipper.Create;
  try
    zip.BasePath := UTF8ToSys(ABasePath);
    if ADest = '' then
      zip.FileName := UTF8ToSys(ADir) + '.zip'
    else
      zip.FileName := UTF8ToSys(ADest);
    EnumFilesToStrs(ADir, strs);
    for I := 0 to strs.Count - 1 do
      strs[I] := UTF8ToSys(strs[I]);
    zip.Entries.AddFileEntries(strs);
    zip.ZipAllFiles;
    Result := True;
  finally
    zip.Free;
    strs.Free;
  end;
end;
{$ENDIF}
function zip_ExtractAll(const AFileName, AOutputPath: string): Boolean;
var
  zip: TUnZipper;
begin
  Result := False;
  zip := TUnZipper.Create;
  try
    try
      if not dir_Force(AOutputPath) then Exit;
      zip.FileName := UTF8ToSys(AFileName);
      zip.OutputPath := UTF8ToSys(AOutputPath);
      zip.Examine;//new:6.9.2
      zip.UnZipAllFiles;
      Result := True;
    except
    end;
  finally
    zip.Free;
  end;
end;

{ TvrZipper }

procedure TvrZipper.SetBasePath(const AValue: string);
begin
  FBasePath := IncludeTrailingPathDelimiter(AValue);
end;

procedure TvrZipper.ZipOneFile(Item: TZipFileEntry);
var
  Len: Integer;

  function _CompareName(const S1, S2: string): Integer;
  begin
    if FCaseSensitive then
      {%H-}Result := AnsiCompareStr(S1, S2)
    else
      Result := AnsiCompareText(S1, S2);
  end;

begin
  if (FBasePath <> '') then
    begin
      Len := Length(FBasePath);
      if _CompareName(Copy(Item.DiskFileName, 1, Len), FBasePath) = 0 then
        Item.ArchiveFileName := Copy(Item.DiskFileName, Len + 1, MaxInt);
    end;
  inherited ZipOneFile(Item);
end;

procedure TvrZipper.AfterConstruction;
begin
  inherited AfterConstruction;
  FCaseSensitive := FilenamesCaseSensitive;
end;

end.

