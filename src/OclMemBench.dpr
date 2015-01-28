program OclMemBench;

{$APPTYPE CONSOLE}

{$R *.res}{$R 'cl.res' 'cl.rc'}

{$INCLUDE Libs\OpenCL\OpenCL.inc}

uses
  CL_platform in 'Libs\OpenCL\CL_platform.pas',
  CL in 'Libs\OpenCL\CL.pas',
  DelphiCL in 'Libs\OpenCL\DelphiCL.pas',
  SysUtils;

var
  SIZE, COUNT, REPEATS: Int64;

type
  TTestDevice = class(TDCLDevice)
    procedure Test;
  end;

function ResourceInitialize(Name, ResType: PChar): PAnsiChar;

  procedure Error;
  var
    S: string;
  begin
    S := Name;
    raise Exception.CreateFmt('Resource not found: %s', [S]);
  end;

var
  HResInfo, HGlobal: THandle;
begin
  HResInfo := FindResource(HInstance, Name, ResType);
  if HResInfo = 0 then Error;
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal = 0 then Error;
  Result := LockResource(HGlobal);
end;

{ TTestDevice }

procedure TTestDevice.Test;
var
  i, j, r: Integer;
  CommandQueue: TDCLCommandQueue;
  SimpleProgram: TDCLProgram;
  Kernel : TDCLKernel;
  Buffers: array of TDCLBuffer;
  tick: Single;
  ClSrc: PAnsiChar;
begin
  CommandQueue := CreateCommandQueue();

  for i := 0 to GlobalMemSize div SIZE-1 do begin
    Write(#13'Allocating chunk ', i+1);
    SetLength(Buffers, Length(Buffers)+1);
    Buffers[i] := CreateBuffer(SIZE, nil, []); //If dynamical array @Input[0]
    if Buffers[i].FMem = nil then begin
      Buffers[i].Free;
      SetLength(Buffers, Length(Buffers)-1);
      Break;
    end;
  end;
  WriteLn;
  if ParamStr(5) = 'stop' then begin
    Writeln('User stop after memory alloc. Press ENTER to continue...');
    Readln;
  end;

  ClSrc := ResourceInitialize('membench', 'CL');
  SimpleProgram := CreateProgram(@ClSrc);
  Kernel := SimpleProgram.CreateKernel('somekernel');

  for I := 0 to High(buffers) do begin
    Kernel.SetArg(0, Buffers[i]);
    tick := 0;
    CommandQueue.Execute(Kernel, COUNT);
    for j := 1 to REPEATS do begin
      CommandQueue.Execute(Kernel, COUNT);
      tick := tick + CommandQueue.ExecuteTime/1000000.;
    end;
    tick := tick / REPEATS;
    CommandQueue.ReadBuffer(buffers[i], 4, @r);//If dynamical array @Output[0]
    Writeln('Chunk', i+1:7, i*SIZE div 1024 div 1024:7, ' MB',
      tick:7:1, ' ms ',
      SIZE/tick/1024/1024:7:1, ' GB/s', (r=123):7);
  end;

  Kernel.Free();
  SimpleProgram.Free();
  for I := 0 to High(buffers) do
    buffers[i].Free();
  CommandQueue.Free();
end;

var
  i: Integer;
  Platforms: TDCLPlatforms;
  Platform: TDCLPlatform;
  Device: TDCLDevice;

begin
  Writeln('OpenCL memory test. Build 15.01.28. https://github.com/duzenko/OpenclMemBench');
  Writeln('Init OpenCL...');
  InitOpenCL();
  Writeln('Create platforms...');
  Platforms := TDCLPlatforms.Create();

  for i := 0 to Platforms.PlatformCount-1 do
    WriteLn('  [', i+1, '] ' , Platforms.Platforms[i].Name);
  if Platforms.PlatformCount = 1 then
    i := 1
  else
    if ParamCount >= 1 then
      i := StrToIntDef(ParamStr(1), 1)
    else begin
      Writeln('Select platform (enter number 1-', Platforms.PlatformCount, '):');
      ReadLn(i);
    end;
  Platform := Platforms.Platforms[i-1];
  Writeln('Using platform ', Platform.Name);

  for i := 0 to Platform.DeviceCount-1 do
    with Platform.Devices[i] do
      WriteLn('  [', i+1, '] ' , Trim(string(Name)), ' (',
        {MaxClockFrequency, 'MHz, ',} MaxComputeUnits, ' compute units)');
  if Platform.DeviceCount = 1 then
    i := 1
  else
    if ParamCount >= 2 then
      i := StrToIntDef(ParamStr(2), 0)
    else begin
      Writeln('Select device (enter number 1-', Platforms.PlatformCount, '):');
      ReadLn(i);
    end;
  Device := Platform.Devices[i-1];
  Writeln('Using device ', Trim(Device.Name), '. Memory available ',
    Device.MaxMemAllocSize div 1024 div 1024, ' MB of ', Device.GlobalMemSize div 1024 div 1024, 'MB');

  try
    SIZE := StrToIntDef(ParamStr(3), 128) * 1024 * 1024;
    REPEATS := StrToIntDef(ParamStr(4), 10);
    COUNT := SIZE div SizeOf(TCL_int4);
    Writeln('Chunk size: ', SIZE div 1024 div 1024, ' MB. Repeats: ', REPEATS, '.');
    TTestDevice(Device).Test;
    Writeln('All done. Press ENTER...');
    Readln;
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
end.
