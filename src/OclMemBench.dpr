program OclMemBench;

{$APPTYPE CONSOLE}

{$R *.res}{$R 'cl.res' 'cl.rc'}

{$INCLUDE Libs\OpenCL\OpenCL.inc}

uses
  CL_platform in 'Libs\OpenCL\CL_platform.pas',
  CL in 'Libs\OpenCL\CL.pas',
  DelphiCL in 'Libs\OpenCL\DelphiCL.pas',
  SysUtils;

const
  SIZE = 128*1024*1024;
  COUNT = SIZE div SizeOf(TCL_int4);

var
  CommandQueue: TDCLCommandQueue;
  SimpleProgram: TDCLProgram;
  Kernel : TDCLKernel;
  i, j, r: Integer;
  Platforms: TDCLPlatforms;
  Platform: TDCLPlatform;
  Device: TDCLDevice;
  Buffers: array of TDCLBuffer;
  tick: Single;
begin
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
      WriteLn('  [', i+1, '] ' , Trim(string(Name)), ' ',
        {MaxClockFrequency, 'MHz, ',} MaxComputeUnits, ' compute units');
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
  Writeln('Using device ', Device.Name, '. Memory available ',
    Device.MaxMemAllocSize div 1024 div 1024, ' MB of ', Device.GlobalMemSize div 1024 div 1024, 'MB');

  with Device do begin
    CommandQueue := CreateCommandQueue();
    for i := 0 to Device.GlobalMemSize div SIZE-1 do begin
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

    SimpleProgram := CreateProgram(ExtractFilePath(ParamStr(0))+'membench.cl');
    Kernel := SimpleProgram.CreateKernel('somekernel');
    for I := 0 to High(buffers) do begin
      Kernel.SetArg(0, Buffers[i]);
      tick := 0;
      CommandQueue.Execute(Kernel, COUNT);
      for j := 1 to 10 do begin
        CommandQueue.Execute(Kernel, COUNT);
        tick := tick + CommandQueue.ExecuteTime/1000000.;
      end;
      tick := tick / 10;
      CommandQueue.ReadBuffer(buffers[i], 4, @r);//If dynamical array @Output[0]
      Writeln('Chunk', i+1:7, i*128:7, ' MB',
        tick:7:1, ' ms ',
        128/tick:7:1, ' GB/s', (r=123):7);
    end;

    Kernel.Free();
    SimpleProgram.Free();
//    OutputBuffer.Free();
    for I := 0 to High(buffers) do begin
      buffers[i].Free();
    end;
    CommandQueue.Free();
    Free();//Free device
  end;

//  for i:=0 to COUNT-1 do Writeln(Output[i],' ');

  try
    Writeln('All done. Press ENTER...');
    Readln;
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
end.
