program dbxfbTests;

uses
  Winapi.Windows,
  firebird.utils in '..\source\testcase\firebird.utils.pas',
  vcl.dbx.cmdlines in '..\source\testcase\vcl.dbx.cmdlines.pas',
  vcl.dbx.main in '..\source\testcase\vcl.dbx.main.pas',
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas',
  firebird.api.testcase in '..\source\testcase\firebird.api.testcase.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  {$ifdef release}ExitProcess({$endif}
  StartApp
  {$ifdef release}){$endif}
  ;
end.
