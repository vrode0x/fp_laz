//const
//  kernel32 = 'kernel32.dll';
//  advapi32 = 'advapi32.dll';


{%Region JwaWinType}
type
  PWSTR = PWideChar;

{%EndRegion}

{%Region JwaWinNT}
const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

  JOB_OBJECT_ASSIGN_PROCESS          = $0001;
  JOB_OBJECT_SET_ATTRIBUTES          = $0002;
  JOB_OBJECT_QUERY                   = $0004;
  JOB_OBJECT_TERMINATE               = $0008;
  JOB_OBJECT_SET_SECURITY_ATTRIBUTES = $0010;
  JOB_OBJECT_ALL_ACCESS              = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1F ;

type
  _JOBOBJECTINFOCLASS = (
    JobObjectInfoClassPadding0,
    JobObjectBasicAccountingInformation,
    JobObjectBasicLimitInformation,
    JobObjectBasicProcessIdList,
    JobObjectBasicUIRestrictions,
    JobObjectSecurityLimitInformation,
    JobObjectEndOfJobTimeInformation,
    JobObjectAssociateCompletionPortInformation,
    JobObjectBasicAndIoAccountingInformation,
    JobObjectExtendedLimitInformation,
    JobObjectJobSetInformation,
    MaxJobObjectInfoClass);
  JOBOBJECTINFOCLASS = _JOBOBJECTINFOCLASS;
  TJobObjectInfoClass = JOBOBJECTINFOCLASS;

  _JOB_SET_ARRAY = record
    JobHandle: HANDLE;   // Handle to job object to insert
    MemberLevel: DWORD;  // Level of this job in the set. Must be > 0. Can be sparse.
    Flags: DWORD;        // Unused. Must be zero
  end;
  JOB_SET_ARRAY = _JOB_SET_ARRAY;
  PJOB_SET_ARRAY = ^JOB_SET_ARRAY;
  TJobSetArray = JOB_SET_ARRAY;
  PJobSetArray = PJOB_SET_ARRAY;

  PIO_COUNTERS = ^IO_COUNTERS;
  _IO_COUNTERS = record
    ReadOperationCount: Int64;
    WriteOperationCount: Int64;
    OtherOperationCount: Int64;
    ReadTransferCount: Int64;
    WriteTransferCount: Int64;
    OtherTransferCount: Int64;
  end;
  IO_COUNTERS = _IO_COUNTERS;
  TIoCounters = IO_COUNTERS;
  PIoCounters = PIO_COUNTERS;

  PJOBOBJECT_BASIC_ACCOUNTING_INFORMATION = ^JOBOBJECT_BASIC_ACCOUNTING_INFORMATION;
  _JOBOBJECT_BASIC_ACCOUNTING_INFORMATION = record
    TotalUserTime: LARGE_INTEGER;
    TotalKernelTime: LARGE_INTEGER;
    ThisPeriodTotalUserTime: LARGE_INTEGER;
    ThisPeriodTotalKernelTime: LARGE_INTEGER;
    TotalPageFaultCount: DWORD;
    TotalProcesses: DWORD;
    ActiveProcesses: DWORD;
    TotalTerminatedProcesses: DWORD;
  end;
  JOBOBJECT_BASIC_ACCOUNTING_INFORMATION = _JOBOBJECT_BASIC_ACCOUNTING_INFORMATION;
  TJobObjectBasicAccountingInformation = JOBOBJECT_BASIC_ACCOUNTING_INFORMATION;
  PJobObjectBasicAccountingInformation = PJOBOBJECT_BASIC_ACCOUNTING_INFORMATION;

  PJOBOBJECT_BASIC_LIMIT_INFORMATION = ^JOBOBJECT_BASIC_LIMIT_INFORMATION;
  _JOBOBJECT_BASIC_LIMIT_INFORMATION = record
    PerProcessUserTimeLimit: LARGE_INTEGER;
    PerJobUserTimeLimit: LARGE_INTEGER;
    LimitFlags: DWORD;
    MinimumWorkingSetSize: SIZE_T;
    MaximumWorkingSetSize: SIZE_T;
    ActiveProcessLimit: DWORD;
    Affinity: ULONG_PTR;
    PriorityClass: DWORD;
    SchedulingClass: DWORD;
  end;
  JOBOBJECT_BASIC_LIMIT_INFORMATION = _JOBOBJECT_BASIC_LIMIT_INFORMATION;
  TJobObjectBasicLimitInformation = JOBOBJECT_BASIC_LIMIT_INFORMATION;
  PJobObjectBasicLimitInformation = PJOBOBJECT_BASIC_LIMIT_INFORMATION;

  PJOBOBJECT_EXTENDED_LIMIT_INFORMATION = ^JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
  _JOBOBJECT_EXTENDED_LIMIT_INFORMATION = record
    BasicLimitInformation: JOBOBJECT_BASIC_LIMIT_INFORMATION;
    IoInfo: IO_COUNTERS;
    ProcessMemoryLimit: SIZE_T;
    JobMemoryLimit: SIZE_T;
    PeakProcessMemoryUsed: SIZE_T;
    PeakJobMemoryUsed: SIZE_T;
  end;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION = _JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
  TJobObjectExtendedLimitInformation = JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
  PJobObjectExtendedLimitInformation = PJOBOBJECT_EXTENDED_LIMIT_INFORMATION;

  PJOBOBJECT_BASIC_PROCESS_ID_LIST = ^JOBOBJECT_BASIC_PROCESS_ID_LIST;
  _JOBOBJECT_BASIC_PROCESS_ID_LIST = record
    NumberOfAssignedProcesses: DWORD;
    NumberOfProcessIdsInList: DWORD;
    ProcessIdList: array [0..0] of ULONG_PTR;
  end;
  JOBOBJECT_BASIC_PROCESS_ID_LIST = _JOBOBJECT_BASIC_PROCESS_ID_LIST;
  TJobObjectBasicProcessIdList = JOBOBJECT_BASIC_PROCESS_ID_LIST;
  PJobObjectBasicProcessIdList = PJOBOBJECT_BASIC_PROCESS_ID_LIST;

  PJOBOBJECT_BASIC_UI_RESTRICTIONS = ^JOBOBJECT_BASIC_UI_RESTRICTIONS;
  _JOBOBJECT_BASIC_UI_RESTRICTIONS = record
    UIRestrictionsClass: DWORD;
  end;
  JOBOBJECT_BASIC_UI_RESTRICTIONS = _JOBOBJECT_BASIC_UI_RESTRICTIONS;
  TJobObjectBasicUiRestrictions = JOBOBJECT_BASIC_UI_RESTRICTIONS;
  PJobObjectBasicUiRestrictions = PJOBOBJECT_BASIC_UI_RESTRICTIONS;

  PJOBOBJECT_SECURITY_LIMIT_INFORMATION = ^JOBOBJECT_SECURITY_LIMIT_INFORMATION;
  _JOBOBJECT_SECURITY_LIMIT_INFORMATION = record
    SecurityLimitFlags : DWORD;
    JobToken : THandle;
    SidsToDisable : PTOKEN_GROUPS;
    PrivilegesToDelete : PTOKEN_PRIVILEGES;
    RestrictedSids : PTOKEN_GROUPS;
  end;
  JOBOBJECT_SECURITY_LIMIT_INFORMATION = _JOBOBJECT_SECURITY_LIMIT_INFORMATION;
  TJobObjectSecurityLimitInformation = JOBOBJECT_SECURITY_LIMIT_INFORMATION;
  PJobObjectSecurityLimitInformation = PJOBOBJECT_SECURITY_LIMIT_INFORMATION;

  PJOBOBJECT_END_OF_JOB_TIME_INFORMATION = ^JOBOBJECT_END_OF_JOB_TIME_INFORMATION;
  _JOBOBJECT_END_OF_JOB_TIME_INFORMATION = record
    EndOfJobTimeAction: DWORD;
  end;
  JOBOBJECT_END_OF_JOB_TIME_INFORMATION = _JOBOBJECT_END_OF_JOB_TIME_INFORMATION;
  TJobObjectEndOfJobTimeInformation = JOBOBJECT_END_OF_JOB_TIME_INFORMATION;
  PJobObjectEndOfJobTimeInformation = PJOBOBJECT_END_OF_JOB_TIME_INFORMATION;

  PJOBOBJECT_ASSOCIATE_COMPLETION_PORT = ^JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
  _JOBOBJECT_ASSOCIATE_COMPLETION_PORT = record
    CompletionKey: Pointer;
    CompletionPort: THandle;
  end;
  JOBOBJECT_ASSOCIATE_COMPLETION_PORT = _JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
  TJobObjectAssociateCompletionPort = JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
  PJobObjectAssociateCompletionPort = PJOBOBJECT_ASSOCIATE_COMPLETION_PORT;

  PJOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION = ^JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION;
  _JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION = record
    BasicInfo: JOBOBJECT_BASIC_ACCOUNTING_INFORMATION;
    IoInfo: IO_COUNTERS;
  end;
  JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION = _JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION;
  TJobObjectBasicAndIoAccountingInformation = JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION;
  PJobObjectBasicAndIoAccountingInformation = PJOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION;

  _JOBOBJECT_JOBSET_INFORMATION = record
    MemberLevel: DWORD;
  end;
  JOBOBJECT_JOBSET_INFORMATION = _JOBOBJECT_JOBSET_INFORMATION;
  PJOBOBJECT_JOBSET_INFORMATION = ^JOBOBJECT_JOBSET_INFORMATION;
  TJobObjectSetInformation = JOBOBJECT_JOBSET_INFORMATION;
  PJobObjectSetInformation = PJOBOBJECT_JOBSET_INFORMATION;

const
  JOB_OBJECT_TERMINATE_AT_END_OF_JOB = 0;
  JOB_OBJECT_POST_AT_END_OF_JOB      = 1;

type
  PFILE_NOTIFY_INFORMATION = ^FILE_NOTIFY_INFORMATION;
  _FILE_NOTIFY_INFORMATION = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array [0..0] of WCHAR;
  end;
  FILE_NOTIFY_INFORMATION = _FILE_NOTIFY_INFORMATION;
  TFileNotifyInformation = FILE_NOTIFY_INFORMATION;
  PFileNotifyInformation = PFILE_NOTIFY_INFORMATION;

const
  FILE_ACTION_ADDED                  = $00000001;
  FILE_ACTION_REMOVED                = $00000002;
  FILE_ACTION_MODIFIED               = $00000003;
  FILE_ACTION_RENAMED_OLD_NAME       = $00000004;
  FILE_ACTION_RENAMED_NEW_NAME       = $00000005;

{%EndRegion}

{%Region jwaWinBase}
const
  LOGON_WITH_PROFILE         = $00000001;
  LOGON_NETCREDENTIALS_ONLY  = $00000002;
  LOGON_ZERO_PASSWORD_BUFFER = DWORD($80000000);

  INVALID_SET_FILE_POINTER = DWORD(-1);
  INVALID_FILE_ATTRIBUTES  = DWORD(-1);


function CreateProcessWithLogonW(lpUsername, lpDomain, lpPassword: LPCWSTR;
  dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR;
  lpStartupInfo: PSTARTUPINFOW; lpProcessInformation: PPROCESSINFORMATION): BOOL; external advapi32 name 'CreateProcessWithLogonW';

function CreateJobObject(lpJobAttributes: LPSECURITY_ATTRIBUTES;
  lpName: LPCTSTR): HANDLE; stdcall; external kernel32 name 'CreateJobObjectA';
function CreateJobObjectW(lpJobAttributes: LPSECURITY_ATTRIBUTES;
  lpName: LPCWSTR): HANDLE; stdcall; external kernel32 name 'CreateJobObjectW';

function OpenJobObject(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): HANDLE; stdcall; external kernel32 name 'OpenJobObjectA';
function OpenJobObjectW(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): HANDLE; stdcall; external kernel32 name 'OpenJobObjectW';

function AssignProcessToJobObject(hJob, hProcess: HANDLE):
    BOOL; stdcall; external kernel32 name 'AssignProcessToJobObject';

function TerminateJobObject(hJob: HANDLE; uExitCode: UINT):
    BOOL; stdcall; external kernel32 name 'TerminateJobObject';

function QueryInformationJobObject(hJob: HANDLE; JobObjectInformationClass: JOBOBJECTINFOCLASS;
  lpJobObjectInformation: LPVOID; cbJobObjectInformationLength: DWORD;
  lpReturnLength: LPDWORD): BOOL; stdcall; external kernel32 name 'QueryInformationJobObject';

function SetInformationJobObject(hJob: HANDLE; JobObjectInformationClass: JOBOBJECTINFOCLASS;
    lpJobObjectInformation: LPVOID; cbJobObjectInformationLength: DWORD):
    BOOL; stdcall; external kernel32 name 'SetInformationJobObject';

function IsProcessInJob(ProcessHandle, JobHandle: HANDLE; var Result_: BOOL):
    BOOL; stdcall; external kernel32 name 'IsProcessInJob';

function CreateJobSet(NumJob: ULONG; UserJobSet: PJOB_SET_ARRAY; Flags: ULONG):
    BOOL; stdcall; external kernel32 name 'CreateJobSet';

function ReadDirectoryChangesW(hDirectory: HANDLE; lpBuffer: LPVOID;
    nBufferLength: DWORD; bWatchSubtree: BOOL; dwNotifyFilter: DWORD;
    lpBytesReturned: LPDWORD; lpOverlapped: LPOVERLAPPED;
    lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE):
    BOOL; stdcall; external kernel32 name 'ReadDirectoryChangesW';

function GetProcessIdByProcessHandle(Process: HANDLE): DWORD; stdcall; external kernel32 name 'GetProcessId';

{%EndRegion}

{%Region JwaPsApi}
const
  PsapiLib = 'psapi.dll';
function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL; stdcall; external PsapiLib;
{%EndRegion}

{%Region JwaWinIOctl}
const
  FILE_DEVICE_BEEP                = $00000001;
  FILE_DEVICE_CD_ROM              = $00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
  FILE_DEVICE_CONTROLLER          = $00000004;
  FILE_DEVICE_DATALINK            = $00000005;
  FILE_DEVICE_DFS                 = $00000006;
  FILE_DEVICE_DISK                = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  FILE_DEVICE_INPORT_PORT         = $0000000a;
  FILE_DEVICE_KEYBOARD            = $0000000b;
  FILE_DEVICE_MAILSLOT            = $0000000c;
  FILE_DEVICE_MIDI_IN             = $0000000d;
  FILE_DEVICE_MIDI_OUT            = $0000000e;
  FILE_DEVICE_MOUSE               = $0000000f;
  FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
  FILE_DEVICE_NAMED_PIPE          = $00000011;
  FILE_DEVICE_NETWORK             = $00000012;
  FILE_DEVICE_NETWORK_BROWSER     = $00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  FILE_DEVICE_NULL                = $00000015;
  FILE_DEVICE_PARALLEL_PORT       = $00000016;
  FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
  FILE_DEVICE_PRINTER             = $00000018;
  FILE_DEVICE_SCANNER             = $00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
  FILE_DEVICE_SERIAL_PORT         = $0000001b;
  FILE_DEVICE_SCREEN              = $0000001c;
  FILE_DEVICE_SOUND               = $0000001d;
  FILE_DEVICE_STREAMS             = $0000001e;
  FILE_DEVICE_TAPE                = $0000001f;
  FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
  FILE_DEVICE_TRANSPORT           = $00000021;
  FILE_DEVICE_UNKNOWN             = $00000022;
  FILE_DEVICE_VIDEO               = $00000023;
  FILE_DEVICE_VIRTUAL_DISK        = $00000024;
  FILE_DEVICE_WAVE_IN             = $00000025;
  FILE_DEVICE_WAVE_OUT            = $00000026;
  FILE_DEVICE_8042_PORT           = $00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
  FILE_DEVICE_BATTERY             = $00000029;
  FILE_DEVICE_BUS_EXTENDER        = $0000002a;
  FILE_DEVICE_MODEM               = $0000002b;
  FILE_DEVICE_VDM                 = $0000002c;
  FILE_DEVICE_MASS_STORAGE        = $0000002d;
  FILE_DEVICE_SMB                 = $0000002e;
  FILE_DEVICE_KS                  = $0000002f;
  FILE_DEVICE_CHANGER             = $00000030;
  FILE_DEVICE_SMARTCARD           = $00000031;
  FILE_DEVICE_ACPI                = $00000032;
  FILE_DEVICE_DVD                 = $00000033;
  FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
  FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
  FILE_DEVICE_DFS_VOLUME          = $00000036;
  FILE_DEVICE_SERENUM             = $00000037;
  FILE_DEVICE_TERMSRV             = $00000038;
  FILE_DEVICE_KSEC                = $00000039;
  FILE_DEVICE_FIPS                = $0000003A;
  FILE_DEVICE_INFINIBAND          = $0000003B;

  FILE_FLAG_WRITE_THROUGH      = DWORD($80000000);
  FILE_FLAG_OVERLAPPED         = $40000000;
  FILE_FLAG_NO_BUFFERING       = $20000000;
  FILE_FLAG_RANDOM_ACCESS      = $10000000;
  FILE_FLAG_SEQUENTIAL_SCAN    = $08000000;
  FILE_FLAG_DELETE_ON_CLOSE    = $04000000;
  FILE_FLAG_BACKUP_SEMANTICS   = $02000000;
  FILE_FLAG_POSIX_SEMANTICS    = $01000000;
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
  FILE_FLAG_OPEN_NO_RECALL     = $00100000;
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;


  FILE_ANY_ACCESS     = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS    = $0001;           // file & pipe
  FILE_WRITE_ACCESS   = $0002;           // file & pipe

  METHOD_BUFFERED   = 0;
  METHOD_IN_DIRECT  = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER    = 3;
  METHOD_DIRECT_TO_HARDWARE     = METHOD_IN_DIRECT;
  METHOD_DIRECT_FROM_HARDWARE   = METHOD_OUT_DIRECT;

  IOCTL_VOLUME_BASE = DWORD('V');

  IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS = (
    (IOCTL_VOLUME_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    (0 shl 2) or METHOD_BUFFERED);


  IOCTL_DISK_BASE = FILE_DEVICE_DISK;

  IOCTL_DISK_GET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0012 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0013 shl 2) or METHOD_BUFFERED);


  FSCTL_GET_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (42 shl 2) or METHOD_BUFFERED);

  MAXIMUM_REPARSE_DATA_BUFFER_SIZE    = 16 * 1024;


type
  REPARSE_DATA_BUFFER = record
    ReparseTag: ULONG;
    ReparseDataLength: USHORT;
    Reserved: USHORT;
    case ReparseBuffer: (SymbolicLinkReparseBuffer,
            MountPointReparseBuffer, GenericReparseBuffer) of
      SymbolicLinkReparseBuffer: (
          SubstituteNameOffset: USHORT;
          SubstituteNameLength: USHORT;
          PrintNameOffset: USHORT;
          PrintNameLength: USHORT;
          Flags: ULONG;
          PathBuffer: array [1..1] of WCHAR;
        );
      MountPointReparseBuffer: (
        SubstituteNameOffset1: USHORT;
        SubstituteNameLength1: USHORT;
        PrintNameOffset1: USHORT;
        PrintNameLength1: USHORT;
        PathBuffer1: array [1..1] of WCHAR;
      );
      GenericReparseBuffer: (
        DataBuffer: Pointer);//array [1..1] of UCHAR);
  end;
  TReparse_Data_Buffer = REPARSE_DATA_BUFFER;
  PREPARSE_DATA_BUFFER = ^REPARSE_DATA_BUFFER;

  PDISK_EXTENT = ^DISK_EXTENT;
  _DISK_EXTENT = record
    DiskNumber: DWORD;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;
  DISK_EXTENT = _DISK_EXTENT;
  TDiskExtent = DISK_EXTENT;
  PDiskExtent = PDISK_EXTENT;

  PVOLUME_DISK_EXTENTS = ^VOLUME_DISK_EXTENTS;
  _VOLUME_DISK_EXTENTS = record
    NumberOfDiskExtents: DWORD;
    Extents: array [0..0] of DISK_EXTENT;
  end;
  VOLUME_DISK_EXTENTS = _VOLUME_DISK_EXTENTS;
  TVolumeDiskExtents = VOLUME_DISK_EXTENTS;
  PVolumeDiskExtents = PVOLUME_DISK_EXTENTS;

function FindFirstVolumeA(lpszVolumeName: LPSTR; cchBufferLength: DWORD): HANDLE; stdcall; external kernel32;
function FindFirstVolumeW(lpszVolumeName: LPWSTR; cchBufferLength: DWORD): HANDLE; stdcall; external kernel32;
function FindNextVolumeA(hFindVolume: HANDLE; lpszVolumeName: LPSTR;
  cchBufferLength: DWORD): BOOL; stdcall; external kernel32;
function FindNextVolumeW(hFindVolume: HANDLE; lpszVolumeName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall; external kernel32;
function FindVolumeClose(hFindVolume: HANDLE): BOOL; stdcall; external kernel32;

function GetVolumePathNamesForVolumeNameA(lpszVolumeName, lpszVolumePathNames: LPCSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall; external kernel32;
function GetVolumePathNamesForVolumeNameW(lpszVolumeName, lpszVolumePathNames: LPCWSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall; external kernel32;

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;

{%EndRegion JwaWinIOctl}

{%Region JwaWinCon}

function GetConsoleWindow: HWND; stdcall; external kernel32;
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32;
function AllocConsole: BOOL; stdcall; external kernel32;
function FreeConsole: BOOL; stdcall; external kernel32;

function ReadConsoleOutputCharacterW(hConsoleOutput: HANDLE; lpCharacter: LPWSTR;
    nLength: DWORD; dwReadCoord: COORD; out lpNumberOfCharsRead: DWORD): BOOL; stdcall; external kernel32;
function ReadConsoleOutputCharacterA(hConsoleOutput: HANDLE; lpCharacter: LPSTR;
    nLength: DWORD; dwReadCoord: COORD; out lpNumberOfCharsRead: DWORD): BOOL; stdcall; external kernel32;
function GetConsoleScreenBufferInfo(hConsoleOutput: HANDLE;
  var lpConsoleScreenBufferInfo: CONSOLE_SCREEN_BUFFER_INFO): BOOL; stdcall; external kernel32;


{%EndRegion}

{%Region JwaHtmlHelp}
const
  hhctrl = 'hhctrl.ocx';

  HH_DISPLAY_TOPIC           = $0000;
  HH_DISPLAY_TOC             = $0001;
  HH_DISPLAY_INDEX           = $0002;
  HH_DISPLAY_SEARCH          = $0003;

  HH_CLOSE_ALL               = $0012;  // close all windows opened directly or indirectly by the caller

  HH_INITIALIZE              = $001C;  // Initializes the help system.
  HH_UNINITIALIZE            = $001D;  // Uninitializes the help system.


function HtmlHelpW(hwndCaller: HWND; pszFile: LPCWSTR; uCommand: UINT;
    dwData: DWORD_PTR): HWND; stdcall; external hhctrl name 'HtmlHelpW';
{%EndRegion}

{%Region jwatlhelp32}

const
  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPMODULE32 = $00000010;
  TH32CS_INHERIT      = $80000000;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
                        TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;

type
  PPROCESSENTRY32W = ^PROCESSENTRY32W;
  tagPROCESSENTRY32W = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;          // this process
    th32DefaultHeapID: ULONG_PTR;
    th32ModuleID:DWORD;            // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD;    // this process's parent process
    pcPriClassBase: LONG;          // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array [0..MAX_PATH - 1] of WCHAR;   // Path
  end;
  PROCESSENTRY32W = tagPROCESSENTRY32W;
  LPPROCESSENTRY32W = ^PROCESSENTRY32W;
  TProcessEntry32W = PROCESSENTRY32W;

  PPROCESSENTRY32 = ^PROCESSENTRY32;
  tagPROCESSENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;          // this process
    th32DefaultHeapID: ULONG_PTR;
    th32ModuleID: DWORD;           // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD;    // this process's parent process
    pcPriClassBase: LONG;          // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array [0..MAX_PATH - 1] of Char;    // Path
  end;
  PROCESSENTRY32 = tagPROCESSENTRY32;
  LPPROCESSENTRY32 = ^PROCESSENTRY32;
  TProcessEntry32 = PROCESSENTRY32;

function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE; stdcall; external kernel32;

function Process32FirstW(hSnapshot: HANDLE; var lppe: TProcessEntry32W): BOOL; stdcall; external kernel32;
function Process32NextW(hSnapshot: HANDLE; var lppe: TProcessEntry32W): BOOL; stdcall; external kernel32;

function Process32First(hSnapshot: HANDLE; var lppe: TProcessEntry32): BOOL; stdcall; external kernel32;//always ANSI!
function Process32Next(hSnapshot: HANDLE; var lppe: TProcessEntry32): BOOL; stdcall; external kernel32; //always ANSI!

{%EndRegion jwatlhelp32}
