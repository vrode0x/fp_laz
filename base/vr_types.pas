unit vr_types;

{$mode delphi}{$H+}
{$I vrode.inc}
interface

uses Types, SysUtils,{$IFDEF WINDOWS}Windows{$ELSE}LMessages{$ENDIF};

resourcestring
  RS_Create       = 'Create';
  RS_Delete       = 'Delete';
  RS_Remove       =  'Remove';
  RS_Overwrite    = 'Overwrite';
  RS_Continue     = 'Continue';

  RS_COPY        = 'Copy';
  RS_CUT         = 'Cut';
  RS_PASTE       = 'Paste';

  RS_FileNotExists = 'File or folder "%s" is not exists';
  RS_InvalidPath   = 'Invalid Path';
  RS_InvalidFile   = 'Invalid File "%s"';
  //RS_FileNotExistsCreate = 'File or folder "%s" is not exists. Create?';
  RS_SaveChanges = 'Save changes in "%s"';
  RS_SaveAllChanges = 'Save all changes?';
  RS_FileAlreadyExists = 'File or folder "%s" is already exists.';
  RS_FileModifiedOnDisk = 'File "%s" has changed on disk. Reload?';
  RS_FilesModifiedOnDisk = 'Some files have changed on disk. Reload?';
  RS_ErrCreateFile     =  'Cannot create file "%s"';
  RS_ErrCopyFile     =  'Cannot copy file "%s"';
  RS_ErrRenameFile     =  'Cannot rename file "%s"';
  RS_ErrCreateDir  = 'Cannot create directory "%s"';
  RS_ErrSaveFile   = 'Cannot save file "%s"';
  RS_ErrFileWrite  = '"%s" write access denied';
  RS_ErrFileRead  = 'Cannot read file "%s"';
  //RS_ErrReadFile = 'Read file "%s" error';
  RS_ErrDeleteFile  = 'Unable to delete file or folder "%s"';
  RS_ErrFileExecute = 'Cannot exectute file "%s"';
  RS_NewDocCaption = 'Untitle';
  RS_TextNotFound = 'Text not found';
  RS_AskReplace = 'Replace this occurence of "%s"?';
  //RS_ActiveDoc    = 'Active Document';
  //RS_ActiveDocFolder = 'Active Document Folder';
  //RS_SiteRoot    = 50142;      //Site Root';
  RS_DeleteItem      = 'Delete "%s"?';
  RS_DeleteAll       =  'Delete all?';
  RS_DeleteSelected  = 'Delete Selected Items?';
  RS_DeleteOldFile   = 'Delete old file "%s"?';
  RS_DirNotEmpty     = 'Directory "%s" is not empty.';

  //RS_UpdatingDocTemp = 'Updating documents that use template';
  //RS_FileOutOfSite = 'The file can not be saved outside of a site';
  //RS_DefDbgNotInstalled = 5019; //Default Debugger is not installed';
  RS_ProgVerOrHigher = '%s %s or higher version is necessary';
  RS_WSHVerOrHigher  = 'Windows Script Host version "%s" or higher is necessary';

  RS_ErrFileNotFound  = 'File "%s" not found';
  RS_NotFound         = 'Not Found';
  RS_NotFoundSearch   = 'Search string "%s" not found!';
  RS_NoFilesFound     = 'No files found';

  RS_DS_FieldIsEmpty = 'Field "%s" is empty';

  RS_DEFAULT = 'Default';
  RS_CURRENT = 'Current';
  RS_ACTIVE = 'Active';
  RS_SET_DEFAULT = 'Set Default';
  RS_CUSTOMIZE   =  'Customize';
  RS_CHECK_UNCECK_ALL = 'Check/Uncheck All';
  RS_EMailIsNotValid = 'EMail "%s" is not valid';

  RS_ErrInternetNeed = 'No Internet Connection';
  RS_InstallingWWW = 'Installing %s (need internet connection)';
  RS_NeedInternet = ' (need internet connection)';

  RS_AllRight = 'OK';
  RS_Error = 'ERROR';

  RS_NOT_INSTALLED = '"%s" is not installed';
  RS_QUEST_DOWNLOAD = 'Download from %s ?';
  RS_QUEST_INSTALL = 'Install?';

  S_NotImplemented = 'Not Implemented';

function RS_PROMPT_DOWNLOAD: string;
function RS_PROMPT_INSTALL: string;
function S_FileOwerriteConfirm(const AFileName: string): string;
function S_ResultOfAction(const AnAction: string; AResult: Boolean): string;

const
  SIZE_SMALLINT = SizeOf(Smallint);
  SIZE_INT = SizeOf(Integer);
  SIZE_INT64 = SizeOf(Int64);
  SIZE_WIDECHAR = 2;

  SDefaultFilter = 'All files (*.*)|*.*';
  LETTERS_COMMON = '0123456789QWERTYUIOPASDFGHJKLZXCVBNM';

  GS_EXT_VR_FILES = '.vrfls';

  S_DEFAULT: string = 'Default';

  EXEC_OK       = 0;
  EXEC_FAIL     = -1;
  EXEC_TIME_OUT = -2;
type

  TExecProgramFlag = (epfNone, epfWait, epfNewGroup,
      epfInheritHandles, epfNewConsole, epfHidden,
      //epfSuspended,
      epfKillIfTimeOut);
  TExecProgramFlags = set of TExecProgramFlag;

  TOpenDocRec = record
    FileName: string;
    IsUpdatePopup: Boolean;
    AsText: Boolean;
    IsSilent: Boolean;
    IsSetActive: Boolean;
  end;
  POpenDocRec = ^TOpenDocRec;

  PPoint = types.PPoint;

{$IFDEF WINDOWS}
  TMessage = Windows.TMessage;
  {$ELSE}{$IFNDEF ANDROID}
  TMessage = TLMessage;
  TWndMethod = procedure(var msg : TMessage) of object;
  LONG = longint;
  TSize = Types.TSize;{$ENDIF}
const
  WM_USER = LMessages.WM_USER;
{$ENDIF}

type
  {$IFDEF WINDOWS}
  TOSEvent = PMsg;{$ELSE}
  TOSEvent = Pointer;{$ENDIF}

  TDocEditType = (etDesign, etCode, etBrowse);
  TStringFormatHow = (sfhBoth, sfhLeft, sfhRight);
  TProcessId = PtrInt;
  PProcessId = ^TProcessId;

  //{$IFNDEF WINDOWS}{$IFNDEF ANDROID}//Redefined in LCLType
  //THandle = LCLType.THandle;{$ENDIF}{$ENDIF}
  THandle = System.THandle;
  PHandle = ^System.THandle;

  TIntArray = array of PtrInt;
  PIntArray = ^TIntArray;
  TStringArray = TStringDynArray;//array of string;
  PStringArray = ^TStringDynArray;
  TCharsSet = TSysCharSet;
  TWideStringArray = TWideStringDynArray;//array of WideString;
  PWideStringArray = TWideStringArray;
  TInterfaceArray = array of IInterface;
  THandleArray = array of THandle;
  TFileNameArray = TStringArray;
  TWideFileNameArray = TWideStringArray;
  TPIntArray = array of PPtrInt;
  TObjectMethod = procedure of object;

  TDataProc = procedure(AData: PtrInt);
  TDataMethod = procedure(AData: PtrInt) of object;
  TPointerProc = procedure(const P: Pointer);
  TPointerMethod = procedure(const P: Pointer) of object;
  THandleProc = procedure(AHandle: THandle);
  THandleMethod = procedure(AHandle: THandle) of object;
  TInterfaceProc = procedure(const AIntf: IUnknown);
  TInterfaceMethod = procedure(const AIntf: IUnknown) of object;

  TNotifyProcedure = procedure(Sender: TObject);
  TNotifyStringProc = procedure(const AString: string; AData: Pointer);
  TNotifyStringMethod = procedure(const AString: string; AData: Pointer) of object;

  TNotifyProc = procedure(const Sender: TObject; const AData: Pointer);
  TNotifyMethod = procedure(const Sender: TObject; const AData: Pointer) of object;

  TNewLineMethod = function(const ALine: string; AData: Pointer;
        AIsAddToLastLine: Boolean): Integer of object;

  TGetBooleanObjectFunc = function: Boolean of object;
  TGetStringEvent = function: string of object;
  TGetStringByIdObjectFunc = function(AId: Integer): string of object;
  TProcWithVarString = procedure(var AStr: string);

  TBooleanDynArray = Types.TBooleanDynArray;
  TByteDynArray = Types.TByteDynArray;
  PByteDynArray = ^TByteDynArray;
  TIntegerDynArray = Types.TIntegerDynArray;
  TWordDynArray = Types.TWordDynArray;

  TEnvironmentScope = (esApp, esUser, esSystem);

  TGArray<T> = array of T;


function IntArray(arr: array of Integer): TIntArray;

procedure EmptyNotifyPoc(Sender: TObject);


implementation

function IntArray(arr: array of Integer): TIntArray;
var
  i: Integer;
begin
  SetLength(Result{%H-}, Length(arr));
  for i := 0 to Length(arr) - 1 do
    begin
      Result[i] := arr[i];
    end;
end;

procedure EmptyNotifyPoc(Sender: TObject);
begin

end;

function RS_PROMPT_DOWNLOAD: string;
begin
  Result := Format('%s.%s%s', [RS_NOT_INSTALLED, sLineBreak, RS_QUEST_DOWNLOAD]);
end;

function RS_PROMPT_INSTALL: string;
begin
  Result := Format('%s.%s%s', [RS_NOT_INSTALLED, sLineBreak, RS_QUEST_INSTALL]);
end;

function S_FileOwerriteConfirm(const AFileName: string): string;
begin
  Result := Format(RS_FileAlreadyExists, [AFileName]) + sLineBreak +
              RS_Overwrite + '?';
end;

function S_ResultOfAction(const AnAction: string; AResult: Boolean): string;
begin
  Result := AnAction + ': ';
  if AResult then Result += RS_AllRight else Result += RS_Error;
end;


end.

