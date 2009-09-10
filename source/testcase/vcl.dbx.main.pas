unit vcl.dbx.main;

interface

procedure StartApp;

implementation

uses SysUtils, Windows, Forms,
     TestFramework, GUITestRunner, TextTestRunner, vcl.dbx.cmdlines;

procedure StartApp;
var P: array of HMODULE;
    H: HMODULE;
    iExitCode: integer;
    i: integer;
begin
  iExitCode := 0;

  i := 2;
  {$ifndef Unicode}Inc(i);{$endif}
  SetLength(P, i);

  P[0] := LoadPackage('SQL.patch.rtl.bpl');
  P[1] := LoadPackage('SQL.patch.vcl.bpl');
  {$ifndef Unicode}P[2] := LoadPackage('SQL.patch.dbx.bpl');{$endif}

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
