{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vr_browser;

{$warn 5023 off : no warning about unused units}
interface

uses
  chrome_browser, chrome_common, chrome_init, chrome_jsonrpc, 
  mobile_desktop_form, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vr_browser', @Register);
end.
