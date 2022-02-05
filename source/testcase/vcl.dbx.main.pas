unit vcl.dbx.main;

interface

function StartApp: Cardinal;

implementation

uses
  Winapi.Windows, System.SysUtils, Vcl.Forms,
  GUITestRunner, TestFramework, TextTestRunner,
  vcl.dbx.cmdlines;

function StartApp: Cardinal;
begin
  Result := 0;

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

  TestFramework.ClearRegistry;
end;

end.
