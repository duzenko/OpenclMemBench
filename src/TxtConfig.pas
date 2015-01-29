unit TxtConfig;

interface uses
  SysUtils;

var
  SIZE: Int64 = 128*1024*1024;
  REPEATS: Integer = 10;
  COUNT, PlatformNo, DeviceNo: Integer;
  StopAfterAllocate, AlternativeTimer, DisplayFirstResult: Boolean;

implementation

procedure ReadConfig;
var
  I: Integer;              
begin
  I := 1;
  while i <= ParamCount do begin
    if ParamStr(i) = 'p' then begin
      PlatformNo := StrToIntDef(ParamStr(i+1), 1);
      Inc(i);
    end;
    if ParamStr(i) = 'd' then begin
      DeviceNo := StrToIntDef(ParamStr(i+1), 1);
      Inc(i);
    end;
    if ParamStr(i) = 's' then begin
      SIZE := StrToIntDef(ParamStr(i+1), 128) * 1024 * 1024;
      Inc(i);
    end;
    if ParamStr(i) = 'r' then begin
      REPEATS := StrToIntDef(ParamStr(i+1), 10);
      Inc(i);
    end;
    if ParamStr(i) = 'stop' then 
      StopAfterAllocate := true;      
    if ParamStr(i) = 'a' then 
      AlternativeTimer := true;      
    if ParamStr(i) = 'f' then 
      DisplayFirstResult := true;      
    Inc(i);
  end;
end;

initialization
  ReadConfig;

end.
