unit windows_compat;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

const
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
  DRIVE_UNKNOWN = 0;
  DRIVE_NO_ROOT_DIR = 1;

type
  ULONG  = cardinal;

function HIBYTE(w : longint) : BYTE; inline;
function LOBYTE(w : longint) : BYTE; inline;

function MAKEWORD(a,b : longint) : WORD; inline;

implementation

function HIBYTE(w : longint) : BYTE;
begin
   HIBYTE:=BYTE(((WORD(w)) shr 8) and $FF);
end;

function LOBYTE(w : longint) : BYTE;
begin
   LOBYTE:=BYTE(w);
end;

function MAKEWORD(a,b : longint) : WORD;
begin
   MAKEWORD:=WORD((BYTE(a)) or ((WORD(BYTE(b))) shl 8));
end;

end.

