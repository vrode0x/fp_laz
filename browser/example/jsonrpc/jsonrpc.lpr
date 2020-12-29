program jsonrpc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, chrome_init, chrome_jsonrpc, vrUtilsPlus;

{$R *.res}

begin
  if chrome_Start(TJsonRpcChromeRender) then
    begin
      RequireDerivedFormResource := True;{$IFDEF LCL_UP2}
      Application.Scaled := True;{$ENDIF}
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
      chrome_Stop;
    end;
end.

