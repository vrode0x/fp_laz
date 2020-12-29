unit chrome_init;

{$mode objfpc}{$H+}
{$I vrode.inc}
interface
{.$DEFINE SELECT_CHROME_BIN}
uses {$IFNDEF VER3}LazUTF8,{$ENDIF}
  Classes, SysUtils, LCLIntf,
  vr_types, vr_utils, vr_classes,
  {$IFDEF WINDOWS}vr_WinAPI,{$ENDIF}
  uCEFApplication, chrome_common, chrome_browser;

function chrome_Start(ARenderClass: TCustomChromeRenderClass = nil): Boolean;
procedure chrome_Stop;

var
  UrlChromeEmbedDownload: string;

implementation

var
  _ChromeRender: TCustomChromeRender = nil;

function chrome_Start(ARenderClass: TCustomChromeRenderClass): Boolean;
var
  sChromeBinDir: UnicodeString;
  sChromeVer: string = '3.3538.1852';
  sCurrDir: String;
  {$IFDEF WINDOWS}
  sChromePlatform: string = 'win32';{$ENDIF}

  //Chromium called app with param   --type=gpu-process
  function _ValidParams: Boolean;
  begin
    Result := (ParamCount = 0) or
        not sys_ParamExists('--type');
  end;

  function _IsValidChromeBinDir(const ADir: string): Boolean;
  begin
    Result := dir_Exists(ADir) and file_Exists(IncludeTrailingPathDelimiter(ADir) + 'libcef.dll');
  end;

  {$IFDEF SELECT_CHROME_BIN}
  procedure _DownloadChromeBinDir(ASilent: Boolean);
  var
    sUrl: string;
  begin
    IniDeleteKey(MainIni, GS_COMMON, 'ChromeBinDir');
    sUrl := 'http://vrodesoft.com/download';
    if ASilent then
      OpenURL(sUrl)
    else
      PromptDownload(Format('Chromium Embedded %s', [sChromeVer]), sUrl);
  end;

  function _GetChromeBinDir: Boolean;
  var
    ini: IFPIniCachedFile;
    S, sInit, sSelect: String;
  begin
    Result := False;
    ini := ini_GetCacheFile(MainIni);
    sInit := {$IFDEF WINDOWS}'libcef.dll'{$ELSE}'libcef.so'{$ENDIF};
    S := ini.ReadString(GS_COMMON, 'ChromeBinDir');
    if not _IsValidChromeBinDir(S) then
      begin
        {$IFDEF WINDOWS}
        S := GetProgramFilesPath + Format('VrodeSoft\Chromium Embedded\%s\%s', [sChromeVer, sChromePlatform]);
        if not _IsValidChromeBinDir(S) then{$ENDIF}
          begin
            sSelect := Format('Select Chromium Embedded %s Folder (contain %s)', [sChromeVer, sInit]);
            case MessageDlg('Chromium Embedded Not Installed',
                'OK: Download from vrodesoft.com' + sLineBreak +
                'No: ' + sSelect,
                mtConfirmation, [mbOK, mbNo, mbCancel], 0) of
              mrOk:
                begin
                  _DownloadChromeBinDir(True);
                  Exit;
                end;
              mrNo:
                begin
                  if not SelectFile(S, False, sInit, '', '', [], sSelect) then
                    Exit;
                  S := file_ExtractDir(S);
                end;
              else
                Exit;
            end;//case
          end;
        if not _IsValidChromeBinDir(S) then Exit;
        ini.WriteString(GS_COMMON, 'ChromeBinDir', S);
        ini.UpdateFile;
      end;

    sChromeBinDir := utf_8To16(S);
    Result := True;
  end;
  {$ELSE}
  procedure _ShowInvalidChrome(const ASilent: Boolean = False);
  var
    S: String;
  begin
    //ShowError('Invalid Chromium Embedded. '+sLineBreak+'Reinstall Vrode Sheet Music.');
    //IniDeleteKey(MainIni, GS_COMMON, 'ChromeBinDir');
    //sUrl := 'http://vrodesoft.com/download';
    S := 'Invalid Chromium Embedded.' + sLineBreak;
    if UrlChromeEmbedDownload = '' then
      begin
        ShowErrorFmt(S + 'Reinstall "%s".', [ApplicationName]);
      end
    else if ASilent or
        ShowConfirmFmt(S + RS_QUEST_DOWNLOAD, [UrlChromeEmbedDownload]) then
      OpenURL(UrlChromeEmbedDownload);
    //PromptDownload(Format('Chromium Embedded %s', [sChromeVer]), UrlChromeEmbedDownload);
  end;

  function _GetChromeBinDir: Boolean;
  var
    S: String;
  begin
    Result := False;
    {$IFDEF WINDOWS}
    S := GetProgramFilesPath + Format('VrodeSoft\Chromium Embedded\%s\%s', [sChromeVer, sChromePlatform]);
    {$ELSE}
    S := AppPath + Format('Chromium Embedded\%s\%s', [sChromeVer, sChromePlatform]);
    {$ENDIF}
    if not _IsValidChromeBinDir(S) then Exit;
    sChromeBinDir := utf_8To16(S);
    Result := True;
  end;
  {$ENDIF}

  function _WaitChrome: Boolean;
  var
    iTicks: QWord;
    //sWaitFile: String;
  begin
    Result := False;
    iTicks := GetTickCount;
    //if _ValidParams then
    //  ShowInfo(ParamStrUTF8(1));
    //sWaitFile := CommonAppsDataAppPath + 'vrsheetwait.d';
    if sys_ParamExists('/wait-chrome') {or file_Exists(sWaitFile)} then
      begin
        //file_Delete(sWaitFile);
        iTicks += 60000;
      end;
    repeat
      Result := _GetChromeBinDir;
      if Result then Exit;
      Sleep(1000);
    until (GetTickCount > iTicks);
  end;

begin
  Result := False;
  if CompareWindowsVersion(WIN_VER_7) < 0 then
    begin
      ShowError('Windows 7 or higher is only supported');
      Exit;
    end;

  ParamValueDelim := '=';
  sCurrDir := file_ExtractDir(ParamStrUTF8(0));
  if not dir_SameName(dir_GetCurrent, sCurrDir) then
    dir_SetCurrent(sCurrDir); //Chrome create blob_storage folder in current dir

  //{$IFDEF TEST_MODE}
  //sChromeBinDir := 'D:\pf\ChromiumEmbed\3.3440.1806\win32';{$ELSE}
  //.{//ToDo}{$ENDIF}
  if not _WaitChrome then
    begin
      {$IFNDEF SELECT_CHROME_BIN}
      _ShowInvalidChrome;{$ENDIF}
      //_DownloadChromeBinDir;
      Exit;
    end;

  try
    GlobalCEFApp := TCefApplication.Create;
    if Assigned(OnAfterGlobalCEFAppCreate) then
      OnAfterGlobalCEFAppCreate;
    if ARenderClass = nil then
      ARenderClass := TCustomChromeRender;
    _ChromeRender := ARenderClass.Create;

    //{$IFDEF TEST_MODE}GlobalCEFApp.SingleProcess := True;{$ENDIF} //only for debug render process
    GlobalCEFApp.FrameworkDirPath := sChromeBinDir{%H-};
    GlobalCEFApp.ResourcesDirPath := sChromeBinDir;
    GlobalCEFApp.LocalesDirPath := sChromeBinDir + '\locales';

    if GlobalCEFApp.StartMainProcess then
      begin
        Result := True;
        ChromeInitialized := True;
      end
    else if _ValidParams then{$IFDEF SELECT_CHROME_BIN}
      _DownloadChromeBinDir(False){$ELSE}
      _ShowInvalidChrome
      {$ENDIF};

  finally
    //if not Result then
    //  chrome_Stop;
  end;
end;

procedure chrome_Stop;
begin
  //? Remove
end;

procedure _chrome_Stop;
begin
  FreeAndNil(_ChromeRender);
  ChromeInitialized := False;
  DestroyGlobalCEFApp;
end;


finalization
  _chrome_Stop;//FreeAndNil(_ChromeRender);

end.

