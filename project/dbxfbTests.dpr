program dbxfbTests;

uses
  FastMM4,
  SysUtils,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas';

{$R *.RES}

var P: array[0..2] of HMODULE;
    H: HMODULE;
begin
  P[0] := LoadPackage('SQL.patch.rtl.bpl');
  P[1] := LoadPackage('SQL.patch.vcl.bpl');
  P[2] := LoadPackage('SQL.patch.dbx.bpl');
  try
    Application.Initialize;
    if IsConsole then
      TextTestRunner.RunRegisteredTests
    else
      GUITestRunner.RunRegisteredTests;
  finally
    for H in P do
      UnloadPackage(H);
  end;
end.

