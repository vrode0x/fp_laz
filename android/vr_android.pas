{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vr_android;

{$warn 5023 off : no warning about unused units}
interface

uses
  vr_android_utils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('vr_android', @Register);
end.
