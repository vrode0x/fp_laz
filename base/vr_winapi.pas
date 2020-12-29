unit vr_WinAPI;

{$mode delphi}{$H+}
{$I vrode.inc}
interface
{$IFDEF WINDOWS}
uses
  Windows, SysUtils, Classes, vr_types, Math, vr_WideStrings, LazUTF8
  {$IFDEF IGNORE_SHELL_COMPAT} , ShellApi, shlobj{$ENDIF};

function UTF8ToOEM(const S: string): string;
function OEMToUTF8(const S: string): string;

const
  WIN_VER_10    =	10.0;
  WIN_VER_8_1  =	6.3;
  WIN_VER_8    =	6.2;
  WIN_VER_7    =	6.1;
  WIN_VER_Server_2008_R2 = 6.1;
  WIN_VER_Server_2008	= 6.0;
  WIN_VER_Vista =	6.0;
  WIN_VER_Server_2003_R2 =	5.2;
  WIN_VER_Server_2003	= 5.2;
  WIN_VER_XP_64 =	5.2;
  WIN_VER_XP	  = 5.1;
  WIN_VER_2000  = 5.0;

function GetWindowsVersion: string;
function GetWindowsVersionNumber: Double;//WIN_VER_*
function CompareWindowsVersion(AVersion: Double): Integer;
function IsWinNT: Boolean;
function IsWin64: Boolean;

function GetSysDirW: WideString;
function GetSysDir: string;
function GetWindowsDirW: WideString;
function GetWindowsDir: string;
function GetSpecialFolderDirW(CSIDL_: Integer): WideString;
function GetSpecialFolderDir(CSIDL_: Integer): string;
function GetProgramFilesDirW: WideString;
function GetProgramFilesDir: string;
function SearchFilePathW(const AFileName: WideString; out APath: WideString): Boolean;
function SearchFilePath(const AFileName: string; out APath: string): Boolean;

function WinToDosTime(var Wtime : TFileTime; var DTime:longint): longbool;
function DosToWinTime(DTime: longint; var Wtime: TFileTime): longbool;

function file_ExistsW(const AFileName: WideString): Boolean;
function file_ExtractDirW(const AFileName: WideString): WideString;

function DuplicateHandle(var AHandle: THandle): Boolean;

function ExecuteDocumentW(const APath, AParams: WideString;
    const ACurrentDirectory: WideString; const AShowError: Boolean = False): Boolean;
function ExecuteDocumentExW(const APath, AParams: WideString;
    const ACurrentDirectory: WideString; const AShowError: Boolean;
    out AProcessHandle: THandle): Boolean;

function ExecuteProgramExW(const AProg, AParams, ACurrentDirectory: WideString;
    const AFlags: TExecProgramFlags; const AMSecWait: Integer;
    const AProcessId: PProcessId = nil; const AJobHandle: PHandle = nil;
    const AProcessHandle: PHandle = nil): Integer;


{%Region Registry}

function reg_KeyOpenReadW(const ASubKey: WideString; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function reg_KeyOpenWriteW(const ASubKey: WideString; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function reg_KeyOpenCreateW(const ASubKey: WideString; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
procedure reg_KeyClose(AKey: HKey);
function reg_KeyDeleteW(AKey: HKey; const ASubKey: WideString): Boolean;
function reg_KeyDeleteValueW(AKey: HKey; const ASubKey: WideString): Boolean;
function reg_KeyExistsW(AKey: HKey; const ASubKey: WideString): Boolean;
function reg_KeyValueExistsW(AKey: HKey; const AName: WideString): Boolean;
{*
If the function succeeds, the return value return will be one of the following:
  REG_BINARY , REG_DWORD, REG_DWORD_LITTLE_ENDIAN,
  REG_DWORD_BIG_ENDIAN, REG_EXPAND_SZ, REG_LINK , REG_MULTI_SZ,
  REG_NONE, REG_RESOURCE_LIST, REG_SZ
}
function reg_KeyGetValueTypeW(const AKey: HKEY; const AName: WideString): DWORD;
function reg_KeyValueSizeW(AKey: HKey; const AName: WideString): Integer;

function reg_KeyGetStringW(AKey: HKey; const AName: WideString;
    const ADefault: WideString = ''): WideString;
function reg_KeySetStringW(AKey: HKEY; const AName, AValue: WideString;
    AsExpand: Boolean = False): Boolean;
function reg_KeyGetDWordW(AKey: HKey; const AName: WideString; ADefault: DWORD = 0): DWORD;
function reg_KeySetDWordW(AKey: HKey; const AName: WideString; AValue: DWORD): Boolean;
function reg_KeyGetBinaryW(AKey: HKey; const AName: WideString; out ABuffer; ACount: Integer): Integer;
function reg_KeySetBinaryW(AKey: HKey; const AName: WideString; const ABuffer; ACount: Integer): Boolean;
function reg_KeyGetDateTimeW(AKey: HKey; const AName: WideString): TDateTime;
function reg_KeySetDateTimeW(AKey: HKey; const AName: WideString; ADateTime: TDateTime): Boolean;

//ToDo TWideStringList -> TWideStrings -> vrTypes
//function reg_KeyGetSubKeysW(const AKey: HKEY; AList: TWideStringList; AIsClearList: Boolean = True): Boolean;
//function reg_KeyGetValueNamesW(const AKey: HKEY; AList: TWideStringList; AIsClearList: Boolean = True): Boolean;

function reg_KeyOpenRead(const ASubKey: string; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function reg_KeyOpenWrite(const ASubKey: string; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function reg_KeyOpenCreate(const ASubKey: string; out AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function reg_KeyDelete(AKey: HKey; const ASubKey: string): Boolean;
function reg_KeyDeleteValue(AKey: HKey; const ASubKey: string): Boolean;
function reg_KeyExists(AKey: HKey; const ASubKey: string): Boolean;
function reg_KeyValueExists(AKey: HKey; const AName: string): Boolean;
function reg_KeyGetValueType(const AKey: HKEY; const AName: string): DWORD;
function reg_KeyValueSize(AKey: HKey; const AName: string): Integer;

function reg_KeyGetString(AKey: HKey; const AName: string;
    const ADefault: string = ''): string;
function reg_KeySetString(AKey: HKEY; const AName, AValue: string;
    AsExpand: Boolean = False): Boolean;
function reg_KeyGetDWord(AKey: HKey; const AName: string; ADefault: DWORD = 0): DWORD;
function reg_KeySetDWord(AKey: HKey; const AName: string; AValue: DWORD): Boolean;
function reg_KeyGetBinary(AKey: HKey; const AName: string; var ABuffer; ACount: Integer): Integer;
function reg_KeySetBinary(AKey: HKey; const AName: string; const ABuffer; ACount: Integer): Boolean;
function reg_KeyGetDateTime(AKey: HKey; const AName: string): TDateTime;
function reg_KeySetDateTime(AKey: HKey; const AName: string; ADateTime: TDateTime): Boolean;

function reg_KeyGetSubKeys(const AKey: HKEY; AList: TStrings; AIsClearList: Boolean = True): Boolean;
function reg_KeyGetValueNames(const AKey: HKEY; AList: TStrings;
    AIsAddValues: Boolean = False; AIsClearList: Boolean = True): Boolean;

function RegKeyExistsW(const ASubKey, AName: WideString; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegKeyValueExistsW(const ASubKey, AName: WideString; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegReadStringW(const ASubKey, AName: WideString; const ADefault: WideString = '';
    AKey: HKey = HKEY_CURRENT_USER): WideString;
function RegWriteStringW(const ASubKey, AName, AValue: WideString;
    AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegReadIntegerW(const ASubKey, AName: WideString; const ADefault: Integer;
    AKey: Cardinal = HKEY_CURRENT_USER): Integer;
function RegWriteIntegerW(const ASubKey, AName: WideString; const AValue: Integer;
    AKey: Cardinal = HKEY_CURRENT_USER): Boolean;

function RegKeyExists(const ASubKey, AName: string; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegKeyValueExists(const ASubKey, AName: string; AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegReadString(const ASubKey, AName: string; const ADefault: string = '';
    AKey: HKey = HKEY_CURRENT_USER): string;
function RegWriteString(const ASubKey, AName, AValue: string;
    AKey: HKey = HKEY_CURRENT_USER): Boolean;
function RegReadInteger(const ASubKey, AName: string; const ADefault: Integer;
    AKey: Cardinal = HKEY_CURRENT_USER): Integer;
function RegWriteInteger(const ASubKey, AName: string; const AValue: Integer;
    AKey: Cardinal = HKEY_CURRENT_USER): Boolean;
function RegReadBool(const ASubKey, AName: string; const ADefault: Boolean = False;
    AKey: Cardinal = HKEY_CURRENT_USER): Boolean;
function RegWriteBool(const ASubKey, AName: string; const AValue: Boolean;
    AKey: Cardinal = HKEY_CURRENT_USER): Boolean;

function RegGetSubKeys(const ASubKey: string; AList: TStrings;
    AKey: HKey = HKEY_CURRENT_USER; AIsClearList: Boolean = True): Boolean;
function RegGetValueNames(const ASubKey: string; AList: TStrings;
    AIsAddValues: Boolean = False; AKey: HKey = HKEY_CURRENT_USER;
    AIsClearList: Boolean = True): Boolean;

{%EndRegion Registry}

function GetProcessHandleFromHwnd(wnd: HWND): HANDLE; stdcall;
function IsHwndBelongToPID(const AWnd: HWND; const pid: Cardinal): Boolean;
function HwndFromPID(pid: Cardinal; out wnd: HWND): Boolean;
const
  GetHwndFromPID: function(pid: Cardinal; out wnd: HWND): Boolean = HwndFromPID;

{$IFNDEF IGNORE_SHELL_COMPAT}//ShellApi,shlobj to reduce size
const
  shell32 =  'shell32';

  CSIDL_DESKTOP                   = $0000;        // <desktop>
  CSIDL_INTERNET                  = $0001;        // Internet Explorer (icon on desktop)
  CSIDL_PROGRAMS                  = $0002;        // Start Menu\Programs
  CSIDL_CONTROLS                  = $0003;        // My Computer\Control Panel
  CSIDL_PRINTERS                  = $0004;        // My Computer\Printers
  CSIDL_PERSONAL                  = $0005;        // My Documents
  CSIDL_FAVORITES                 = $0006;        // <user name>\Favorites
  CSIDL_STARTUP                   = $0007;        // Start Menu\Programs\Startup
  CSIDL_RECENT                    = $0008;        // <user name>\Recent
  CSIDL_SENDTO                    = $0009;        // <user name>\SendTo
  CSIDL_BITBUCKET                 = $000a;        // <desktop>\Recycle Bin
  CSIDL_STARTMENU                 = $000b;        // <user name>\Start Menu
  CSIDL_MYDOCUMENTS               = $000c;        // logical "My Documents" desktop icon
  CSIDL_MYMUSIC                   = $000d;        // "My Music" folder
  CSIDL_MYVIDEO                   = $000e;        // "My Videos" folder
  CSIDL_DESKTOPDIRECTORY          = $0010;        // <user name>\Desktop
  CSIDL_DRIVES                    = $0011;        // My Computer
  CSIDL_NETWORK                   = $0012;        // Network Neighborhood (My Network Places)
  CSIDL_NETHOOD                   = $0013;        // <user name>\nethood
  CSIDL_FONTS                     = $0014;        // windows\fonts
  CSIDL_TEMPLATES                 = $0015;
  CSIDL_COMMON_STARTMENU          = $0016;        // All Users\Start Menu
  CSIDL_COMMON_PROGRAMS           = $0017;        // All Users\Start Menu\Programs
  CSIDL_COMMON_STARTUP            = $0018;        // All Users\Startup
  CSIDL_COMMON_DESKTOPDIRECTORY   = $0019;        // All Users\Desktop
  CSIDL_APPDATA                   = $001a;        // <user name>\Application Data
  CSIDL_PRINTHOOD                 = $001b;        // <user name>\PrintHood
  CSIDL_LOCAL_APPDATA             = $001c;        // <user name>\Local Settings\Applicaiton Data (non roaming)
  CSIDL_ALTSTARTUP                = $001d;        // non localized startup
  CSIDL_COMMON_ALTSTARTUP         = $001e;        // non localized common startup
  CSIDL_COMMON_FAVORITES          = $001f;
  CSIDL_INTERNET_CACHE            = $0020;
  CSIDL_COOKIES                   = $0021;
  CSIDL_HISTORY                   = $0022;
  CSIDL_COMMON_APPDATA            = $0023;        // All Users\Application Data
  CSIDL_WINDOWS                   = $0024;        // GetWindowsDirectory()
  CSIDL_SYSTEM                    = $0025;        // GetSystemDirectory()
  CSIDL_PROGRAM_FILES             = $0026;        // C:\Program Files
  CSIDL_MYPICTURES                = $0027;        // C:\Program Files\My Pictures
  CSIDL_PROFILE                   = $0028;        // USERPROFILE
  CSIDL_SYSTEMX86                 = $0029;        // x86 system directory on RISC
  CSIDL_PROGRAM_FILESX86          = $002a;        // x86 C:\Program Files on RISC
  CSIDL_PROGRAM_FILES_COMMON      = $002b;        // C:\Program Files\Common
  CSIDL_PROGRAM_FILES_COMMONX86   = $002c;        // x86 Program Files\Common on RISC
  CSIDL_COMMON_TEMPLATES          = $002d;        // All Users\Templates
  CSIDL_COMMON_DOCUMENTS          = $002e;        // All Users\Documents
  CSIDL_COMMON_ADMINTOOLS         = $002f;        // All Users\Start Menu\Programs\Administrative Tools
  CSIDL_ADMINTOOLS                = $0030;        // <user name>\Start Menu\Programs\Administrative Tools
  CSIDL_CONNECTIONS               = $0031;        // Network and Dial-up Connections
  CSIDL_COMMON_MUSIC              = $0035;        // All Users\My Music
  CSIDL_COMMON_PICTURES           = $0036;        // All Users\My Pictures
  CSIDL_COMMON_VIDEO              = $0037;        // All Users\My Video
  CSIDL_RESOURCES                 = $0038;        // Resource Direcotry
  CSIDL_RESOURCES_LOCALIZED       = $0039;        // Localized Resource Direcotry
  CSIDL_COMMON_OEM_LINKS          = $003a;        // Links to All Users OEM specific apps
  CSIDL_CDBURN_AREA               = $003b;        // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
  CSIDL_COMPUTERSNEARME           = $003d;        // Computers Near Me (computered from Workgroup membership)
  CSIDL_FLAG_CREATE               = $8000;        // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
  CSIDL_FLAG_DONT_VERIFY          = $4000;        // combine with CSIDL_ value to return an unverified folder path
  CSIDL_FLAG_NO_ALIAS             = $1000;        // combine with CSIDL_ value to insure non-alias versions of the pidl
  CSIDL_FLAG_PER_USER_INIT        = $0800;        // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
  CSIDL_FLAG_MASK                 = $FF00;        // mask for all possible flag values

  SEE_MASK_CLASSNAME       = $00000001;
  SEE_MASK_CLASSKEY        = $00000003;
{ Note INVOKEIDLIST overrides IDLIST }
  SEE_MASK_IDLIST          = $00000004;
  SEE_MASK_INVOKEIDLIST    = $0000000c;
  SEE_MASK_ICON            = $00000010;
  SEE_MASK_HOTKEY          = $00000020;
  SEE_MASK_NOCLOSEPROCESS  = $00000040;
  SEE_MASK_CONNECTNETDRV   = $00000080;
  SEE_MASK_FLAG_DDEWAIT    = $00000100;
  SEE_MASK_DOENVSUBST      = $00000200;
  SEE_MASK_FLAG_NO_UI      = $00000400;
  SEE_MASK_UNICODE         = $00004000;
  SEE_MASK_NO_CONSOLE      = $00008000;
  SEE_MASK_ASYNCOK         = $00100000;
  SEE_MASK_HMONITOR        = $00200000;
//if (_WIN32_IE >= 0x0500)
  SEE_MASK_NOQUERYCLASSSTORE= $01000000;
  SEE_MASK_WAITFORINPUTIDLE= $02000000;
//endif  (_WIN32_IE >= 0x500)
//if (_WIN32_IE >= 0x0560)
  SEE_MASK_FLAG_LOG_USAGE  = $04000000;

type
  {$IFDEF VER3}
  STARTUPINFOW = windows.STARTUPINFOW;
  {$ELSE}
   STARTUPINFOW = record
      cb : DWORD;
      lpReserved : LPWSTR;
      lpDesktop : LPWSTR;
      lpTitle : LPWSTR;
      dwX : DWORD;
      dwY : DWORD;
      dwXSize : DWORD;
      dwYSize : DWORD;
      dwXCountChars : DWORD;
      dwYCountChars : DWORD;
      dwFillAttribute : DWORD;
      dwFlags : DWORD;
      wShowWindow : WORD;
      cbReserved2 : WORD;
      lpReserved2 : LPBYTE;
      hStdInput : HANDLE;
      hStdOutput : HANDLE;
      hStdError : HANDLE;
   end;
   LPSTARTUPINFOW = ^STARTUPINFOW;
   _STARTUPINFOW = STARTUPINFOW;
   TSTARTUPINFOW = STARTUPINFOW;
   PSTARTUPINFOW = ^STARTUPINFOW;
  {$ENDIF}

  _SHELLEXECUTEINFOW = record
    cbSize : DWORD;
    fMask : ULONG;
    wnd : HWND;
    lpVerb : lpcwstr;
    lpFile : lpcwstr;
    lpParameters : lpcwstr;
    lpDirectory : lpcwstr;
    nShow : longint;
    hInstApp : HINST;
    lpIDList : LPVOID;
    lpClass : LPCWSTR;
    hkeyClass : HKEY;
    dwHotKey : DWORD;
    DUMMYUNIONNAME : record
      case longint of
        0 : ( hIcon : HANDLE );
        1 : ( hMonitor : HANDLE );
      end;
    hProcess : HANDLE;
  end;
  SHELLEXECUTEINFOW        = _SHELLEXECUTEINFOW;
  TSHELLEXECUTEINFOW       = _SHELLEXECUTEINFOW;
  LPSHELLEXECUTEINFOW      = ^_SHELLEXECUTEINFOW;

function SHGetSpecialFolderLocation( hwnd:HWND; csidl:longint;out ppidl: LPITEMIDLIST):HResult;StdCall; external shell32 name 'SHGetSpecialFolderLocation';
function SHGetPathFromIDListW(pidl:LPCITEMIDLIST; pszPath:LPWStr):BOOL;StdCall;external shell32 name 'SHGetPathFromIDListW';

function ShellExecuteW(hwnd: HWND;lpOperation : LPCWSTR ; lpFile : LPCWSTR; lpParameters : LPCWSTR; lpDirectory:  LPCWSTR; nShowCmd:LONGINT):HInst; stdcall; external shell32 name 'ShellExecuteW';
Function ShellExecuteExW(lpExecInfo: LPSHELLEXECUTEINFOW):Bool; stdcall; external shell32 name 'ShellExecuteExW';
{$ENDIF}

const
  IID_IShellLinkDataList: TGUID =
      (D1:$45e2b4ae; D2:$b1c3; D3:$11d0; D4:($b9, $2f, $0, $a0, $c9, $3, $12, $e1));

type
  IShellLinkDataList = interface
    ['{45e2b4ae-b1c3-11d0-b92f-00a0c90312e1}']
    function AddDataBlock(pDataBlock: Pointer): HRESULT; stdcall;
    function CopyDataBlock(dwSig: DWORD; ppDataBlock: PPointer): HRESULT; stdcall;
    function GetFlags(pdwFlags: PDWORD): HRESULT; stdcall;
    function RemoveDataBlock(dwSig: DWORD): HRESULT; stdcall;
    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;//dwFlags: SLDF_* in ShellApi.pp
  end;

{$IFNDEF JEDI_WINAPI}
  {$I jedi.inc}
{$ENDIF}
{$ENDIF WINDOWS}
implementation
{$IFDEF WINDOWS}

uses vr_utils;

function UTF8ToOEM(const S: string): string;
var
  Dst: PChar;
  ws: WideString;
begin
  ws := UTF8Decode(S);
  Dst := AllocMem((Length(ws) + 1) * SizeOf(WideChar));
  if CharToOemW(PWideChar(ws), Dst) then
    Result := StrPas(Dst)
  else
    Result := S;
  FreeMem(Dst);
end;

function OEMToUTF8(const S: string): string;
var
  Dst: PWideChar;
  ws: WideString;
begin
  Dst := AllocMem((Length(S) + 1) * SizeOf(WideChar));
  if OemToCharW(PChar(s), Dst) then
    begin
      ws := PWideChar(Dst);
      Result := UTF8Encode(ws);
    end
  else
    Result := s;
  FreeMem(Dst);
end;

function _GetWindowsVersionInfo: OSVERSIONINFOW;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  Result.dwOSVersionInfoSize := SizeOf(Result);
  GetVersionExW(@Result);
end;

function GetWindowsVersion: string;
var
  OSVer: OSVERSIONINFOW;
begin
  OSVer := _GetWindowsVersionInfo;
  Result := IntToStr(OSVer.dwMajorVersion) + '.' + IntToStr(OSVer.dwMinorVersion) + '.' +
    IntToStr(OSVer.dwBuildNumber) + ' ' + UTF8Encode(WideString(OSVer.szCSDVersion));
  Result := TrimRight(Result);
end;

function GetWindowsVersionNumber: Double;
var
  OSVer: OSVERSIONINFOW;
begin
  OSVer := _GetWindowsVersionInfo;
  TryStrToFloat(IntToStr(OSVer.dwMajorVersion) +
      DefaultFormatSettings.DecimalSeparator + IntToStr(OSVer.dwMinorVersion), Result);
end;

function CompareWindowsVersion(AVersion: Double): Integer;
begin
  Result := CompareValue(GetWindowsVersionNumber, AVersion, 0.001);
end;

function IsWinNT: Boolean;
var
  OSVer: OSVERSIONINFOW;
begin
  OSVer := _GetWindowsVersionInfo;
  Result := OSVer.dwPlatformId = VER_PLATFORM_WIN32_NT;
end;

function IsWin64: Boolean;
var
  Attr: DWORD;
begin
  Attr := Windows.GetFileAttributesW(PWideChar(GetWindowsDirW + '\SysWOW64\regedit.exe'));
  if Attr <> INVALID_FILE_ATTRIBUTES then
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result := False;
end;

function GetSysDirW: WideString;
var
  I: windows.UINT;
begin
  SetLength(Result, MAX_PATH);
  I := GetSystemDirectoryW(PWideChar(Result), MAX_PATH);
  SetLength(Result, I);//Result := PWideChar(Result);
end;

function GetSysDir: string;
begin
  Result := UTF8Encode(GetSysDirW);
end;

function GetWindowsDirW: WideString;
var
  I: windows.UINT;
begin
  SetLength(Result, MAX_PATH);
  I := GetWindowsDirectoryW(PWideChar(Result), MAX_PATH);
  SetLength(Result, I);//Result := PWideChar(Result);
end;

function GetWindowsDir: string;
begin
  Result := UTF8Encode(GetWindowsDirW);
end;

function GetSpecialFolderDirW(CSIDL_: Integer): WideString;
var
  //IDL: ITEMIDLIST;
  PIDL: PItemIDList;
  //S: string;
  //Path: LPSTR;
begin
  //Path := StrAlloc(MAX_PATH);
  //CSIDL_RECENT = $0008;
  //Folder := CSIDL_HISTORY
  {SetLength(Result, MAX_PATH);
  if SHGetSpecialFolderPath(0, PChar(Result), Folder, True) then
    Result := PChar(Result)
  else
    Result := '';}
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_, PIDL)) then
    begin
      SetLength(Result, MAX_PATH);
      SHGetPathFromIDListW(PIDL, PWideChar(Result));
      Result := PWideChar(Result);
    end;
  //StrDispose(Path);
end;

function GetSpecialFolderDir(CSIDL_: Integer): string;
begin
  Result := UTF8Encode(GetSpecialFolderDirW(CSIDL_));
end;

function _GetDrive(const APath: WideString): WideString;
begin
  Result := UTF8Decode(ExtractFileDrive(UTF8Encode(APath)));
end;

function GetProgramFilesDirW: WideString;
begin
  Result := GetSpecialFolderDirW(CSIDL_PROGRAM_FILES);
  if Length(Result) = 0 then
    begin
      Result := GetWindowsDirW;
      Result := _GetDrive(Result) + '\Program Files';
    end;
  if Result[Length(Result)] = '\' then// WideEndsStr(PathDelim, Result) then
    System.Delete(Result, Length(Result), 1);
end;

function GetProgramFilesDir: string;
begin
  Result := UTF8Encode(GetProgramFilesDirW);
end;

function SearchFilePathW(const AFileName: WideString; out APath: WideString): Boolean;
begin
  SetLength(APath, MAX_PATH);
  Result := Windows.SearchPathW(nil, PWideChar(AFileName),
      nil, MAX_PATH, PWideChar(APath), nil) > 0;
  if Result then
    APath := PWideChar(APath)
  else
    APath := '';
end;

function SearchFilePath(const AFileName: string; out APath: string): Boolean;
var
  wsResult: WideString;
begin
  Result := SearchFilePathW(UTF8ToUTF16(AFileName), wsResult);
  APath := UTF16ToUTF8(wsResult);
end;

function WinToDosTime(var Wtime: TFileTime; var DTime: longint): longbool;
var
  lft : TFileTime;
begin
  Result := FileTimeToLocalFileTime(WTime,lft{%H-}) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
end;

function DosToWinTime(DTime: longint; var Wtime: TFileTime): longbool;
var
  lft : TFileTime;
begin
  Result := DosDateTimeToFileTime(longrec(dtime).hi,longrec(dtime).lo,@lft) and
                LocalFileTimeToFileTime(lft,Wtime);
end;

function file_ExistsW(const AFileName: WideString): Boolean;
var
  Attr:Dword;
begin
  Attr:=GetFileAttributesW(PWideChar(AFileName));
  if Attr <> $ffffffff then
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result := False;
end;

function file_ExtractDirW(const AFileName: WideString): WideString;
begin
  Result := UTF8Decode(ExtractFileDir(UTF8Encode(AFileName)));
end;

function DuplicateHandle(var AHandle: THandle): Boolean;
var
  oldHandle: THandle;
begin
  oldHandle := AHandle;
  Result := Windows.DuplicateHandle(GetCurrentProcess(), oldHandle,
      GetCurrentProcess(), @AHandle, 0, True, DUPLICATE_SAME_ACCESS);
  if Result then
    Result := CloseHandle(oldHandle);
end;

function ExecuteDocumentW(const APath, AParams: WideString;
  const ACurrentDirectory: WideString; const AShowError: Boolean): Boolean;
var
  h: THandle;
begin
  Result := ExecuteDocumentExW(APath, AParams, ACurrentDirectory, AShowError, h);
  if Result then
    CloseHandle(h);
end;

function ExecuteDocumentExW(const APath, AParams: WideString;
  const ACurrentDirectory: WideString; const AShowError: Boolean; out
  AProcessHandle: THandle): Boolean;
var
  CurrDir: WideString;
  Info: TSHELLEXECUTEINFOW;
begin
  AProcessHandle := 0;
  if ACurrentDirectory = '' then
    CurrDir := file_ExtractDirW(APath)
  else
    CurrDir := ACurrentDirectory;
  if AShowError then
    begin
      FillChar(Info{%H-}, SizeOf(Info), 0);
      Info.cbSize := SizeOf(Info);
      Info.fMask := SEE_MASK_NOCLOSEPROCESS;
      Info.lpVerb := 'open';
      Info.lpFile := PWideChar(APath);
      Info.lpParameters := PWideChar(AParams);
      Info.lpDirectory := PWideChar(CurrDir);
      Info.nShow := SW_NORMAL;
      Result := ShellExecuteExW(@Info);//show error
      if Result then
        AProcessHandle := Info.hProcess;
    end
  else
    begin
      Result := ShellExecuteW(0, 'open',
          PWideChar(APath),
          PWideChar(AParams),
          PWideChar(CurrDir),
          SW_NORMAL) > 32;
    end;
end;

function ExecuteProgramExW(const AProg, AParams, ACurrentDirectory: WideString;
  const AFlags: TExecProgramFlags; const AMSecWait: Integer;
  const AProcessId: PProcessId; const AJobHandle: PHandle;
  const AProcessHandle: PHandle): Integer;
var
  pi: TProcessInformation;
  //ARedir: TProcessRedirectThread;

  function _Execute: Integer;
  var
    SI: TStartupInfoW;
    hProcess: THandle;
    code: DWord;
    CommandLine, CurrDir : WideString;
    Flags: DWORD = 0;//NORMAL_PRIORITY_CLASS;
  begin
    Result := EXEC_FAIL;
    FillChar(SI{%H-}, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    if epfHidden in AFlags then
      begin
        SI.wShowWindow := SW_HIDE;
        SI.dwFlags := STARTF_USESHOWWINDOW;
      end
    else
      SI.wShowWindow := SW_NORMAL;
    //new:6.9.5 SI.wShowWindow := IfThen(epfHidden in AFlags, SW_HIDE, SW_NORMAL);
    //if ARedir <> nil then
    //  begin
    //    flag_Set(SI.dwFlags, STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW);
    //    SI.hStdInput := ARedir.Input;
    //    SI.hStdOutput := ARedir.Output;
    //    SI.hStdError := ARedir.Stderr;
    //  end;

    if ACurrentDirectory = '' then
      CurrDir := file_ExtractDirW(AProg)
    else
      CurrDir := ACurrentDirectory;

    CommandLine := AProg + ' ' + AParams + #0;

    if epfNewConsole in AFlags then
      flag_Set(Flags, CREATE_NEW_CONSOLE);
    //if epfSuspended in AFlags then
    //  flag_Set(Flags, CREATE_SUSPENDED);
    if epfNewGroup in AFlags then
      flag_Set(Flags, CREATE_NEW_PROCESS_GROUP);
    if AJobHandle <> nil then
      flag_Set(Flags, CREATE_SUSPENDED);

    if not CreateProcessW(nil, PWideChar(CommandLine),
      nil, nil, epfInheritHandles in AFlags, Flags, nil,
      PWideChar(CurrDir),
      @SI, @PI) then
        begin
          Exit;
        end;


    hProcess := PI.hProcess;
    if AJobHandle <> nil then
      begin
        AJobHandle^ := CreateJobObject(nil, nil);
        AssignProcessToJobObject(AJobHandle^, hProcess);
        ResumeThread(PI.hThread);
        //if ARedir <> nil then
        //  ARedir.JobHandle := AJobHandle^;
      end;
    if (epfWait in AFlags) and (AMSecWait > 0) and
          (WaitForSingleObject(hProcess, DWORD(AMSecWait)) <> WAIT_FAILED) then//dword($ffffffff)) <> $ffffffff) then
      begin
        GetExitCodeProcess(hProcess, code{%H-});
        Result := code;
      end
    else
      begin
        //e:=EOSError.CreateFmt(SExecuteProcessFailed,[ACommandLine,GetLastError]);
        //e.ErrorCode:=GetLastError;
        if epfKillIfTimeOut in AFlags then
          begin
            Result := EXEC_TIME_OUT;
            TerminateProcess(PI.hProcess, Cardinal(EXEC_TIME_OUT));
          end
        else
          Result := 0;
      end;
  end;

begin
  if AProcessId <> nil then
    AProcessId^ := 0;
  if AProcessHandle <> nil then
    AProcessHandle^ := 0;
  //if ARedirThread is TProcessRedirectThread then
  //  ARedir := TProcessRedirectThread(ARedirThread)
  //else
  //  ARedir := nil;

  Result := _Execute;//(AProg, AParams, ACurrentDirectory, AFlags, AMSecWait,
      //pi, AJobHandle, ARedir);
  if Result <> EXEC_FAIL then
    begin
      CloseHandle(PI.hThread);
      if AProcessId <> nil then
        AProcessId^ := PI.dwProcessId;
      if AProcessHandle <> nil then
        begin
          AProcessHandle^ := PI.hProcess;
          //if ARedir <> nil then
          //  ARedir.ProcessHandle := PI.hProcess;
        end
      else
        CloseHandle(PI.hProcess);
    end;
end;


var
  _GetProcessHandleFromHwnd: function(wnd: HWND): HANDLE; stdcall; //external 'Oleacc.dll';
  _hlibOleacc: windows.HINST;

function _GetProcessHandleFromHwndDummy({%H-}wnd: HWND): HANDLE; stdcall;
begin
  Result := HANDLE(0);
end;

function reg_KeyOpenReadW(const ASubKey: WideString; out
  AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
begin
  Result := RegOpenKeyExW(AKey, PWideChar(ASubKey), 0, KEY_READ, AResult{%H-}) = ERROR_SUCCESS;
end;

function reg_KeyOpenWriteW(const ASubKey: WideString; out
  AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
begin
  Result := RegOpenKeyExW(AKey, PWideChar(ASubKey), 0, KEY_READ or KEY_WRITE, AResult{%H-}) = ERROR_SUCCESS;
end;

function reg_KeyOpenCreateW(const ASubKey: WideString; out
  AResult: HKey; AKey: HKey = HKEY_CURRENT_USER): Boolean;
var
  dwDisp: DWord;
begin
  Result := RegCreateKeyExW(AKey, PWideChar(ASubKey), 0, nil, 0, KEY_ALL_ACCESS,
      nil, AResult{%H-}, @dwDisp) = ERROR_SUCCESS;
end;

procedure reg_KeyClose(AKey: HKey);
begin
  if AKey <> 0 then
    RegCloseKey(AKey);
end;

function reg_KeyDeleteW(AKey: HKey; const ASubKey: WideString): Boolean;
begin
  Result := FALSE;
  if AKey <> 0 then
    Result := RegDeleteKeyW(AKey, PWideChar(ASubKey)) = ERROR_SUCCESS;
end;

function reg_KeyDeleteValueW(AKey: HKey; const ASubKey: WideString): Boolean;
begin
  Result := FALSE;
  if AKey <> 0 then
    Result := RegDeleteValueW(AKey, PWideChar(ASubKey)) = ERROR_SUCCESS;
end;

function reg_KeyExistsW(AKey: HKey; const ASubKey: WideString): Boolean;
var
  K: HKey;
begin
  Result := (AKey <> 0) and reg_KeyOpenReadW(ASubKey, K, AKey);
  if Result then
    reg_KeyClose(K);
end;

function reg_KeyValueExistsW(AKey: HKey; const AName: WideString): Boolean;
var
  dwType, dwSize: DWORD;
begin
  Result := (AKey <> 0) and
            (RegQueryValueExW(AKey, PWideChar(AName), nil,
            @dwType, nil, @dwSize ) = ERROR_SUCCESS);
end;

function reg_KeyGetValueTypeW(const AKey: HKEY; const AName: WideString): DWORD;
begin
  Result := AKey;
  if (AKey <> 0) and (AName <> '') then
    RegQueryValueExW(AKey, @AName[1], nil, @Result, nil, nil)
end;

function reg_KeyValueSizeW(AKey: HKey; const AName: WideString): Integer;
begin
  Result := 0;
  if AKey = 0 then Exit;
  RegQueryValueExW(AKey, PWideChar(AName), nil, nil, nil, @DWORD(Result));
end;

function reg_KeyGetStringW(AKey: HKey; const AName: WideString;
    const ADefault: WideString = ''): WideString;
var
  ws: WideString;
  ok: Boolean;
  LenWS: DWORD;
  rType: DWORD;
begin
  Result := ADefault;
  ok := RegQueryValueExW(AKey, PWideChar(AName), nil, nil,
      nil, @LenWS) = ERROR_SUCCESS;
  if ok then
    begin
      SetLength(ws, LenWS);
      ok := RegQueryValueExW(AKey, PWideChar(AName), nil, @rType,
          @ws[1], @LenWS) = ERROR_SUCCESS;
      if ok and ((rType = REG_SZ) or (rType = REG_EXPAND_SZ)
            or (rType = REG_MULTI_SZ)) then
        begin
          Result := WideTrimChar(PWideChar(ws), '"');
        end;
    end;
end;

function reg_KeySetStringW(AKey: HKEY; const AName, AValue: WideString;
  AsExpand: Boolean): Boolean;
var
  dwType: DWORD;
begin
  if AsExpand then
    dwType := REG_EXPAND_SZ
  else
    dwType := REG_SZ;
  Result := (AKey <> 0) and (RegSetValueExW(AKey, PWideChar(AName), 0, dwType,
            PWideChar(AValue), (Length(AValue) + 1) * 2) = ERROR_SUCCESS);
end;

function reg_KeyGetDWordW(AKey: HKey; const AName: WideString; ADefault: DWORD): DWORD;
var
  dwType, dwSize: DWORD;
begin
  dwSize := Sizeof(DWORD);
  Result := ADefault;
  if (AKey = 0) or
     (RegQueryValueExW(AKey, PWideChar(AName), nil, @dwType, PByte(@Result), @dwSize) <> ERROR_SUCCESS)
     or (dwType <> REG_DWORD) then
    Result := ADefault;
end;

function reg_KeySetDWordW(AKey: HKey; const AName: WideString; AValue: DWORD): Boolean;
begin
  Result := (AKey <> 0) and (RegSetValueExW(AKey, PWideChar(AName), 0,
    REG_DWORD, @AValue, SizeOf(DWORD)) = ERROR_SUCCESS);
end;

function reg_KeyGetBinaryW(AKey: HKey; const AName: WideString; out ABuffer;
  ACount: Integer): Integer;
begin
  Result := 0;
  if AKey = 0 then Exit;
  Result := ACount;
  RegQueryValueExW(AKey, PWideChar(AName), nil, nil, @ABuffer, @Result);
end;

function reg_KeySetBinaryW(AKey: HKey; const AName: WideString; const ABuffer;
  ACount: Integer): Boolean;
begin
  Result := (AKey <> 0) and (RegSetValueExW(AKey, PWideChar(AName ), 0,
                    REG_BINARY, @ABuffer, ACount) = ERROR_SUCCESS);
end;

function reg_KeyGetDateTimeW(AKey: HKey; const AName: WideString): TDateTime;
begin
  reg_KeyGetBinaryW(AKey, AName, Result, Sizeof(Result));
end;

function reg_KeySetDateTimeW(AKey: HKey; const AName: WideString;
  ADateTime: TDateTime): Boolean;
begin
  Result := reg_KeySetBinaryW(AKey, AName, ADateTime, Sizeof(ADateTime));
end;

function reg_KeyGetSubKeysW(const AKey: HKEY; AList: TWideStringList; AIsClearList: Boolean = True): Boolean;
var
  i, MaxSubKeyLen, Size: DWORD;
  Buf: PWideChar;
begin
  Result := False;
  if AIsClearList then
    AList.Clear;

  if RegQueryInfoKeyW(AKey, nil, nil, nil, nil, @MaxSubKeyLen, nil, nil, nil, nil,
      nil, nil) = ERROR_SUCCESS then
    begin
      if MaxSubKeyLen > 0 then
        begin
          Size := MaxSubKeyLen + 1;
          GetMem(Buf, Size * Sizeof(WideChar));
          i := 0;

          while RegEnumKeyExW(AKey, i, buf, Size, nil, nil, nil, nil) <> ERROR_NO_MORE_ITEMS do
            begin
              AList.Add(WideString(Buf));
              Size := MaxSubKeyLen + 1;
              inc(i);
            end;

          FreeMem(Buf{,MaxSubKeyLen + 1});
        end;
      Result := True;
    end;
end;

function reg_KeyGetValueNamesW(const AKey: HKEY; AList: TWideStringList;
    AIsAddValues: Boolean = False; AIsClearList: Boolean = True): Boolean;
var
  i, MaxValueNameLen, Size: DWORD;
  Buf: PWideChar;
  dwType: DWord;
  sName, sValue: WideString;
begin
  Result := False;
  if AIsClearList then
    AList.Clear;

  if RegQueryInfoKeyW(AKey, nil, nil, nil, nil, nil, nil, nil, @MaxValueNameLen, nil,
       nil, nil) = ERROR_SUCCESS then
    begin
      if MaxValueNameLen > 0 then
        begin
          Size := MaxValueNameLen + 1;
          GetMem(Buf, Size * SizeOf(WideChar));
          i := 0;
          while RegEnumValueW(AKey, i, buf, Size, nil, @dwType, nil, nil) <> ERROR_NO_MORE_ITEMS do
            begin
              sName := WideString(Buf);
              if AIsAddValues then
                begin
                  case dwType of
                    REG_SZ, REG_EXPAND_SZ:
                      sValue := reg_KeyGetStringW(AKey, sName, '');
                    else
                      sValue := ''
                  end;
                  sName := sName + '=' + sValue;
                end;
              AList.Add(sName);
              Size := MaxValueNameLen+1;
              inc(i);
            end;

          FreeMem(Buf {,MaxValueNameLen + ... system always knows how long buffer is});
        end;
      Result := True;
    end;
end;

function RegKeyExistsW(const ASubKey, AName: WideString; AKey: HKey): Boolean;
var
  hRegKey: HKEY;
begin
  Result := False;
  if reg_KeyOpenReadW(ASubKey, hRegKey, AKey) then
    begin
      Result := reg_KeyExistsW(hRegKey, AName);
      RegCloseKey(hRegKey);
    end;
end;

function RegKeyValueExistsW(const ASubKey, AName: WideString; AKey: HKey
  ): Boolean;
var
  hRegKey: HKEY;
begin
  Result := False;
  if reg_KeyOpenReadW(ASubKey, hRegKey, AKey) then
    begin
      Result := reg_KeyValueExistsW(hRegKey, AName);
      RegCloseKey(hRegKey);
    end;
end;

function RegReadStringW(const ASubKey, AName: WideString; const ADefault: WideString;
  AKey: HKey): WideString;
var
  hRegKey: HKEY;
begin
  Result := ADefault;
  if reg_KeyOpenReadW(ASubKey, hRegKey, AKey) then
    begin
      Result := reg_KeyGetStringW(hRegKey, AName, ADefault);
      RegCloseKey(hRegKey);
    end;
end;

function reg_KeyGetSubKeys(const AKey: HKEY; AList: TStrings;
  AIsClearList: Boolean): Boolean;
var
  strs: TWideStringList;
begin
  strs := TWideStringList.Create;
  try
    Result := reg_KeyGetSubKeysW(AKey, strs, AIsClearList);
    WideStringsToUTF8(strs, AList, AIsClearList);
  finally
    strs.Free;
  end;
end;

function reg_KeyGetValueNames(const AKey: HKEY; AList: TStrings;
  AIsAddValues: Boolean; AIsClearList: Boolean): Boolean;
var
  strs: TWideStringList;
begin
  strs := TWideStringList.Create;
  try
    Result := reg_KeyGetValueNamesW(AKey, strs, AIsAddValues, AIsClearList);
    WideStringsToUTF8(strs, AList, AIsClearList);
  finally
    strs.Free;
  end;
end;


function RegKeyExists(const ASubKey, AName: string; AKey: HKey): Boolean;
begin
  Result := RegKeyExistsW(UTF8Decode(ASubKey), UTF8Decode(AName), AKey);
end;

function RegKeyValueExists(const ASubKey, AName: string; AKey: HKey): Boolean;
begin
  Result := RegKeyValueExistsW(UTF8Decode(ASubKey), UTF8Decode(AName), AKey);
end;

function RegReadString(const ASubKey, AName: string; const ADefault: string;
  AKey: HKey): string;
begin
  Result := UTF8Encode(RegReadStringW(UTF8Decode(ASubKey), UTF8Decode(AName),
      UTF8Decode(ADefault), AKey));
end;

function RegWriteStringW(const ASubKey, AName, AValue: WideString; AKey: HKey): Boolean;
var
  Key: HKey;
begin
  Result := False;
  if reg_KeyOpenCreateW(ASubKey, Key, AKey) then
    begin
      Result := reg_KeySetStringW(Key, AName, AValue, False);
      reg_KeyClose(Key);
    end;
end;

function RegReadIntegerW(const ASubKey, AName: WideString;
  const ADefault: Integer; AKey: Cardinal): Integer;
var
  Key: HKEY;
begin
  Result := ADefault;
  if reg_KeyOpenReadW(ASubKey, Key, AKey) then
    begin
      Result := reg_KeyGetDWordW(Key, AName, ADefault);
      RegCloseKey(Key);
    end;
end;

function RegWriteIntegerW(const ASubKey, AName: WideString;
  const AValue: Integer; AKey: Cardinal): Boolean;
var
  Key: HKey;
begin
  Result := False;
  if reg_KeyOpenCreateW(ASubKey, Key, AKey) then
    begin
      Result := reg_KeySetDWord(Key, UTF8Encode(AName), AValue);
      reg_KeyClose(Key);
    end;
end;

function RegWriteString(const ASubKey, AName, AValue: string; AKey: HKey): Boolean;
begin
  Result := RegWriteStringW(UTF8Decode(ASubKey), UTF8Decode(AName),
      UTF8Decode(AValue), AKey);
end;

function RegReadInteger(const ASubKey, AName: string; const ADefault: Integer;
  AKey: Cardinal): Integer;
begin
  Result := RegReadIntegerW(UTF8Decode(ASubKey), UTF8Decode(AName), ADefault, AKey);
end;

function RegWriteInteger(const ASubKey, AName: string; const AValue: Integer;
  AKey: Cardinal): Boolean;
begin
  Result := RegWriteIntegerW(UTF8Decode(ASubKey), UTF8Decode(AName), AValue, AKey);
end;

function RegReadBool(const ASubKey, AName: string; const ADefault: Boolean;
  AKey: Cardinal): Boolean;
begin
  Result := RegReadIntegerW(UTF8Decode(ASubKey), UTF8Decode(AName), Ord(ADefault), AKey) <> 0;
end;

function RegWriteBool(const ASubKey, AName: string; const AValue: Boolean;
  AKey: Cardinal): Boolean;
begin
  Result := RegWriteIntegerW(UTF8Decode(ASubKey), UTF8Decode(AName), Ord(AValue), AKey);
end;

function reg_KeyOpenRead(const ASubKey: string; out AResult: HKey; AKey: HKey
  ): Boolean;
begin
  Result := reg_KeyOpenReadW(UTF8ToUTF16(ASubKey), AResult, AKey);
end;

function reg_KeyOpenWrite(const ASubKey: string; out AResult: HKey; AKey: HKey
  ): Boolean;
begin
  Result := reg_KeyOpenWriteW(UTF8ToUTF16(ASubKey), AResult, AKey);
end;

function reg_KeyOpenCreate(const ASubKey: string; out AResult: HKey; AKey: HKey
  ): Boolean;
begin
  Result := reg_KeyOpenCreateW(UTF8ToUTF16(ASubKey), AResult, AKey);
end;

function reg_KeyDelete(AKey: HKey; const ASubKey: string): Boolean;
begin
  Result := reg_KeyDeleteW(AKey, UTF8ToUTF16(ASubKey));
end;

function reg_KeyDeleteValue(AKey: HKey; const ASubKey: string): Boolean;
begin
  Result := reg_KeyDeleteValueW(AKey, UTF8ToUTF16(ASubKey));
end;

function reg_KeyExists(AKey: HKey; const ASubKey: string): Boolean;
begin
  Result := reg_KeyExistsW(AKey, UTF8ToUTF16(ASubKey));
end;

function reg_KeyValueExists(AKey: HKey; const AName: string): Boolean;
begin
  Result := reg_KeyValueExistsW(AKey, UTF8ToUTF16(AName));
end;

function reg_KeyGetValueType(const AKey: HKEY; const AName: string): DWORD;
begin
  Result := reg_KeyGetValueTypeW(AKey, UTF8ToUTF16(AName));
end;

function reg_KeyValueSize(AKey: HKey; const AName: string): Integer;
begin
  Result := reg_KeyValueSizeW(AKey, UTF8ToUTF16(AName));
end;

function reg_KeyGetString(AKey: HKey; const AName: string;
  const ADefault: string): string;
begin
  Result := UTF16ToUTF8(reg_KeyGetStringW(AKey, UTF8ToUTF16(AName), UTF8ToUTF16(ADefault)));
end;

function reg_KeySetString(AKey: HKEY; const AName, AValue: string;
  AsExpand: Boolean): Boolean;
begin
  Result := reg_KeySetStringW(AKey, UTF8ToUTF16(AName), UTF8ToUTF16(AValue), AsExpand);
end;

function reg_KeyGetDWord(AKey: HKey; const AName: string; ADefault: DWORD
  ): DWORD;
begin
  Result := reg_KeyGetDWordW(AKey, UTF8ToUTF16(AName), ADefault);
end;

function reg_KeySetDWord(AKey: HKey; const AName: string; AValue: DWORD
  ): Boolean;
begin
  Result := reg_KeySetDWordW(AKey, UTF8ToUTF16(AName), AValue);
end;

function reg_KeyGetBinary(AKey: HKey; const AName: string; var ABuffer;
  ACount: Integer): Integer;
begin
  Result := reg_KeyGetBinaryW(AKey, UTF8ToUTF16(AName), ABuffer, ACount);
end;

function reg_KeySetBinary(AKey: HKey; const AName: string; const ABuffer;
  ACount: Integer): Boolean;
begin
  Result := reg_KeySetBinaryW(AKey, UTF8ToUTF16(AName), ABuffer, ACount);
end;

function reg_KeyGetDateTime(AKey: HKey; const AName: string): TDateTime;
begin
  Result := reg_KeyGetDateTimeW(AKey, UTF8ToUTF16(AName));
end;

function reg_KeySetDateTime(AKey: HKey; const AName: string;
  ADateTime: TDateTime): Boolean;
begin
  Result := reg_KeySetDateTimeW(AKey, UTF8ToUTF16(AName), ADateTime);
end;

function RegGetSubKeys(const ASubKey: string; AList: TStrings; AKey: HKey;
  AIsClearList: Boolean): Boolean;
var
  Key: HKey;
begin
  if reg_KeyOpenRead(ASubKey, Key, AKey) then
    begin
      Result := reg_KeyGetSubKeys(Key, AList, AIsClearList);
      reg_KeyClose(Key);
    end;
end;

function RegGetValueNames(const ASubKey: string; AList: TStrings;
  AIsAddValues: Boolean; AKey: HKey; AIsClearList: Boolean): Boolean;
var
  Key: HKey;
begin
  if reg_KeyOpenRead(ASubKey, Key, AKey) then
    begin
      Result := reg_KeyGetValueNames(Key, AList, AIsAddValues, AIsClearList);
      reg_KeyClose(Key);
    end;
end;

function GetProcessHandleFromHwnd(wnd: HWND): HANDLE; stdcall;
begin
  if not Assigned(_GetProcessHandleFromHwnd) then
    begin
      _hlibOleacc := LoadLibrary('Oleacc.dll');
      if _hlibOleacc <> 0 then
        begin
          _GetProcessHandleFromHwnd := GetProcAddress(_hlibOleacc, 'GetProcessHandleFromHwnd');
          if not Assigned(_GetProcessHandleFromHwnd) then
            _GetProcessHandleFromHwnd := _GetProcessHandleFromHwndDummy;
        end;
    end;
  Result := _GetProcessHandleFromHwnd(wnd);
end;

type
  PHWND = ^HWND;
  TEnumWinsForPID = record
    pid: DWORD;
    wnd: PHWND;
    wndParam: HWND;
  end;
  PEnumWinsForPID = ^TEnumWinsForPID;

function __OnEnumChildIsHwndBelongToPID(wnd: HWND; lp: LPARAM): WINBOOL; stdcall;
var
  r: PEnumWinsForPID;
begin
  Result := True;
  r := {%H-}PEnumWinsForPID(lp);
  if r^.wndParam = wnd then
    begin
      Result := False;
      r.wnd^ := r.wndParam;
    end
  else
    begin
      EnumChildWindows(wnd, __OnEnumChildIsHwndBelongToPID, lp);
      Result := r.wnd^ <> r.wndParam;
    end;
end;

function _OnEnumIsHwndBelongToPID(wnd: HWND; lp: LPARAM): WINBOOL; stdcall;
var
  r: PEnumWinsForPID;
  pid: DWORD;
begin
  Result := True;
  r := {%H-}PEnumWinsForPID(lp);
  GetWindowThreadProcessId(wnd, pid{%H-});
  if (r^.pid = pid) then
    if (r^.wndParam = wnd) then
      begin
        Result := False;
        r.wnd^ := r.wndParam;
      end
    else
      begin
        EnumChildWindows(wnd, @__OnEnumChildIsHwndBelongToPID, lp);
        Result := r.wnd^ <> r.wndParam;
      end;
end;

function IsHwndBelongToPID(const AWnd: HWND; const pid: Cardinal): Boolean;
var
  wnd: HWND;
  r: TEnumWinsForPID;
begin
  wnd := 0;
  r.pid := pid;
  r.wnd := @wnd;
  r.wndParam := AWnd;
  EnumWindows(_OnEnumIsHwndBelongToPID, {%H-}LPARAM(@r));
  Result := wnd = AWnd;

  //Result := HwndFromPID_1(wnd, pid);
  //if not Result then Exit;
  //Result := wnd = AWnd;
  //if Result then Exit;
  //r.pid := 0;
  //r.wnd := @AWnd;
  //EnumChildWindows(wnd, @_OnEnumIsHwndBelongToPID, LPARAM(@r));
  //Result := (r.pid = 1);
end;

function _OnEnumWinsForPID(wnd: HWND; lp: LPARAM): WINBOOL; stdcall;
var
  r: PEnumWinsForPID;
  pid: DWORD;
begin
  Result := True;
  r := {%H-}PEnumWinsForPID(lp);
  GetWindowThreadProcessId(wnd, pid{%H-});
  if r^.pid = pid then
    begin
      Result := False;
      r.wnd^ := wnd;
    end;
end;

function HwndFromPID(pid: Cardinal; out wnd: HWND): Boolean;
var
  r: TEnumWinsForPID;
begin
  wnd := 0;
  r.pid := pid;
  r.wnd := @wnd;
  r.wndParam := 0;
  EnumWindows(_OnEnumWinsForPID, {%H-}LPARAM(@r));
  Result := wnd <> 0;
end;

{$IFNDEF JEDI_WINAPI}
//JwaWinIOctl
function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;
{$ENDIF}

finalization
  if _hlibOleacc <> 0 then
    FreeLibrary(_hlibOleacc);

{$ENDIF WINDOWS}
end.
