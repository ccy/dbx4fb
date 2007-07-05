(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
          Search Path: ..\..\core\source\rtl
*)

program dbxfbTests;

uses
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas';

{$R *.RES}

begin
  {$if CompilerVersion>=18}
  if not IsConsole then
    ReportMemoryLeaksOnShutdown := True;
  {$ifend}
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

