(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
*)

program dbxfbTests;

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas';

{$R *.RES}

begin
  if not IsConsole then
    ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

