unit vr_globals;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses sysutils, {LazUTF8,} vr_utils;

var
  AppName, AppNameLong: string;
  Company: string;

  AppVersion,
    AppDesc: string;
  AppID: Integer;
  AppVer: Double;
  AppVerMax, AppVerMin: Double;//Serial key correct
  ReleaseDate: Integer;
  FirstReleaseYear, ReleaseYear: string;
  gsHomeUrl, gsBuyUrl, gsMail, gsActivateUrl: string;
  gsLicense, gsLicensedName, gsUpgradeTo: string;
  gIsExpired: Boolean;

  MyName: function: string = nil;

function AppCopyRightYears: string;


implementation

function AppCopyRightYears: string;
begin
  Result := ReleaseYear;
  if ReleaseYear <> FirstReleaseYear then
    Result := FirstReleaseYear + '-' + Result;
end;

function _GetMyName: string;
begin
  Result := '';
end;

function _GetVendorName: string;
begin
  Result := Company;
end;

function _GetApplicationName: string;
begin
  Result := str_RemoveEmptyChars(AppNameLong);
end;

procedure _Init;
var
  S: String;
begin
  if AppName = '' then
    AppName := file_ExtractNameOnly(ParamStrUTF8(0));
  if AppNameLong = '' then
    AppNameLong := AppName;
  OnGetVendorName := _GetVendorName;
  OnGetApplicationName := _GetApplicationName;

  if @MyName = nil then
    MyName := _GetMyName;

  if Company = '' then
    Company := 'MyCompany';

  //ReleaseDate := trunc(StrToDate('07/06/2009', 'mm/dd/yyyy', '/')); //40000
  S := {$I %DATE%};
  ReleaseYear := NameOfString(S, '/');
  if FirstReleaseYear = '' then
    FirstReleaseYear := ReleaseYear;
  ReleaseDate := trunc(StrToDate(S, 'yyyy/mm/dd', '/'));
end;

initialization
  _Init;


end.

