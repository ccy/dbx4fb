unit vcl.dbx.main;

interface

procedure StartApp;

implementation

uses SysUtils, Windows, Forms,
     TestFramework, GUITestRunner, TextTestRunner, vcl.dbx.cmdlines;

procedure StartApp;
var P: array[0..2] of HMODULE;
    H: HMODULE;
    iExitCode: integer;
begin
  iExitCode := 0;
  P[0] := LoadPackage('SQL.patch.rtl.bpl');
  P[1] := LoadPackage('SQL.patch.vcl.bpl');
  P[2] := LoadPackage('SQL.patch.dbx.bpl');
  try
    if TCmdLineParams_App.RunAsConsole then begin
      AllocConsole;
      try
        if TCmdLineParams_App.HasTestName then
          SetConsoleTitle(PChar(TCmdLineParams_App.GetTestName));
        with TextTestRunner.RunRegisteredTests do begin
          try
            if not WasSuccessful then Readln;
            iExitCode := ErrorCount + FailureCount;
          finally
            Free;
          end;
        end;
      finally
        FreeConsole;
      end;
    end else begin
      Application.Initialize;
      GUITestRunner.RunRegisteredTests;
    end;
  finally
    for H in P do
      UnloadPackage(H);
  end;

  TestFramework.ClearRegistry;
  ExitProcess(iExitCode);
end;

end.
