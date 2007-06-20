(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
*)

program dbxfbTests;

uses
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  vcl.patch in '..\..\base\source\vcl.patch.pas',
  FastCodePatch in '..\..\base\source\FastCodePatch.pas',
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

