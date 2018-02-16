unit vcl.dbx.main;

interface

function StartApp: Cardinal;

implementation

uses
  Winapi.Windows, System.SysUtils, Vcl.Forms,
  GUITestRunner, TestFramework, TextTestRunner,
  vcl.dbx.cmdlines;

function StartApp: Cardinal;
var P: array of HMODULE;
    H: HMODULE;
    i: integer;
begin
  Result := 0;

  i := 1;
  {$ifndef Unicode}Inc(i);{$endif}
  SetLength(P, i);

  P[0] := LoadPackage('SQL.patch.rtl.bpl');
  {$ifndef Unicode}P[1] := LoadPackage('SQL.patch.dbx.bpl');{$endif}

  try
    if TCmdLineParams_App.RunAsConsole then begin
      AllocConsole;
      try
        if TCmdLineParams_App.HasTestName then
          SetConsoleTitle(PChar(TCmdLineParams_App.GetTestName));
        with TextTestRunner.RunRegisteredTests do begin
          try
            if not WasSuccessful then Readln;
            Result := ErrorCount + FailureCount;
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
end;

end.
