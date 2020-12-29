unit vr_timer;

{$mode delphi}{$H+}

interface
//ToD0 ? use TFPTimer(fptimer.pp) -> default NOT WINDOW
//{$IFNDEF WINDOWS}{$DEFINE LCL_TIMER}{$ENDIF}

uses {$IFDEF WINDOWS}Windows,{$ENDIF}Classes, SysUtils, vr_fpclasses, vr_SysUtils
    {$IFDEF LCL_TIMER}, CustomTimer{$ENDIF};

type
{$IFDEF LCL_TIMER}
  TCustomTimer = CustomTimer.TCustomTimer;
{$ELSE}
  TTimerHandle = {$IFDEF WINDOWS}UINT_PTR{$ELSE}THandle{$ENDIF};

  { TCustomTimer }

  TCustomTimer = class(TComponent)
  private
    FInterval     : Cardinal;
    //FOnStartTimer: TNotifyEvent;
    //FOnStopTimer: TNotifyEvent;
    FHandle  : TTimerHandle;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;
    class var FList: TThreadList;
    procedure Timer;
    class procedure DoOnTimer(const AHandle: TTimerHandle);
  protected
    procedure SetEnabled(AValue: Boolean); //virtual;
    procedure SetInterval(AValue: Cardinal); //virtual;
    procedure SetOnTimer(AValue: TNotifyEvent); //virtual;
  public
    //constructor Create(AOwner: TComponent); override;
    constructor CreateAlt(AOwner: TComponent; const AOnTimer: TNotifyEvent;
        const AEnabled: Boolean = True; const AInterval: Integer = 1000);
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    //property OnStartTimer: TNotifyEvent read FOnStartTimer write FOnStartTimer;
    //property OnStopTimer: TNotifyEvent read FOnStopTimer write FOnStopTimer;
  end;
{$ENDIF}

implementation

{$IFNDEF LCL_TIMER}
{$IFDEF WINDOWS}

procedure _WinTimerProc({%H-}AWnd: HWnd; {%H-}AMsg: UINT; AIdEvent: UINT_PTR; {%H-}AInterval: DWord); stdcall;
begin
  //if AMsg <> WM_TIMER then Exit;
  TCustomTimer.DoOnTimer(AIdEvent);
end;

{$ENDIF}

{ TCustomTimer }

procedure TCustomTimer.Timer;
begin

end;

procedure TCustomTimer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  {$IFDEF WINDOWS}
  if AValue then
    begin
      FHandle := SetTimer(0, UINT_PTR(Self), FInterval, _WinTimerProc);
      TThreadList.LockedCreateIfNil(FList, Self);
    end
  else
    begin
      TThreadList.LockedFreeAndNilIfEmpty(FList, Self);
      if FHandle <> 0 then
        begin
          KillTimer(0, UINT_PTR(FHandle));
          //if hWnd<>0 in SetTimer() then KillTimer(0, UINT_PTR(Self));
          FHandle := 0;
        end;
    end;
  {$ELSE}
  raise Exception.Create('<vr_timer.pas>TCustomTimer.SetEnabled(): define global LCL_TIMER (not implemented)');
  {$ENDIF}
end;

procedure TCustomTimer.SetInterval(AValue: Cardinal);
begin
  if FInterval = AValue then Exit;
  FInterval := AValue;
  if Enabled then
    begin
      Enabled := False;
      if AValue > 0 then
        Enabled := True;
    end;
end;

procedure TCustomTimer.SetOnTimer(AValue: TNotifyEvent);
begin
  if (TMethod(AValue).Data = TMethod(FOnTimer).Data) and
        (TMethod(AValue).Code = TMethod(FOnTimer).Code) then Exit;
  FOnTimer := AValue;
  if Enabled then
    begin
      Enabled := False;
      if Assigned(AValue) then
        Enabled := True;
    end;
end;

class procedure TCustomTimer.DoOnTimer(const AHandle: TTimerHandle);
var
  lst: TList;
  i: Integer;
  t: TCustomTimer;
begin
  if TThreadList.LockedGet(FList, lst) then
    try
      for i := 0 to lst.Count - 1 do
        begin
          t := TCustomTimer(lst[i]);
          if t.FHandle = AHandle then
            begin
              if Assigned(t.FOnTimer) then
                t.FOnTimer(t);
              Exit;
            end;
        end;
    finally
      FList.UnlockList;
    end;
end;

constructor TCustomTimer.CreateAlt(AOwner: TComponent;
  const AOnTimer: TNotifyEvent; const AEnabled: Boolean;
  const AInterval: Integer);
begin
  Create(AOwner);
  FOnTimer := AOnTimer;
  FInterval := AInterval;
  Enabled := AEnabled;
end;

destructor TCustomTimer.Destroy;
begin
  Enabled := False;
  TThreadList.LockedFreeAndNilIfEmpty(FList, Self);
  inherited Destroy;
end;
{$ENDIF NOT LCL_TIMER}

end.

