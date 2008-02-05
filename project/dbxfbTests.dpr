(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
          Search Path: ..\..\core\source\rtl
*)

program dbxfbTests;

uses
  SysUtils,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  MidasLib,
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas';

{$R *.RES}

var P: array[0..1] of HMODULE;
    H: HMODULE;
begin
  P[0] := LoadPackage('SQL.patch.vcl.bpl');
  P[1] := LoadPackage('SQL.patch.dbx.bpl');
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

