{.$DEFINE RELEASE_MODE} //!!! not delete this line
{$IFDEF RELEASE_MODE}
  {$DEFINE VR_RELEASE}
{$ELSE}
  {$DEFINE VR_DEBUG}
  {$DEFINE TEST_MODE}
  {$DEFINE DEBUG_AS_RELEASE}
  {$DEFINE DEBUG_MODE}
  {.$DEFINE USE_HEAPTRC}
{$ENDIF}

{$IFDEF TEST_MODE}
  {.$DEFINE IGNORE_NOT_IMPLEMENTED_ERR}//{$IFNDEF IGNORE_NOT_IMPLEMENTED_ERR}./ToDo{$ENDIF}
{$ENDIF}

{$IFDEF DEBUG_MODE}
  {$DEFINE USE_HEAPTRC}
{$ENDIF}

{$IFDEF VR_RELEASE}
  {$UNDEF USE_HEAPTRC}
  {$IFDEF TEST_MODE}
  Error: Uncomment first line
  {$ENDIF}
{$ENDIF}
{$IFDEF VR_DEBUG}
  {$IFNDEF TEST_MODE}
  Error: Comment first line
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  {$DEFINE MOBILE}
  {$DEFINE MOBILE_DEVICE}
{$ENDIF}
{$IFDEF MOBILE_DESKTOP}
  {$DEFINE MOBILE}
  {$UNDEF MOBILE_DEVICE}
{$ELSE}
  {$if defined(Windows) or defined(darwin)}
    {$UNDEF MOBILE}
    {$UNDEF MOBILE_DEVICE}
  {$endif}
{$ENDIF}

{$if defined(Windows) or defined(darwin)}

{$else}
  {$DEFINE FILENAME_CASESENSETIVE}
{$endif}

{$if declared(useHeapTrace)}
  {$UNDEF USE_HEAPTRC}
{$endIf}

