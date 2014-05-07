program dbxfbTests;

uses
  FastMM4,
  Winapi.Windows,
  vcl.dbx.testcase in '..\source\testcase\vcl.dbx.testcase.pas',
  vcl.dbx.main in '..\source\testcase\vcl.dbx.main.pas',
  vcl.dbx.cmdlines in '..\source\testcase\vcl.dbx.cmdlines.pas';

{$R *.RES}

begin
  {$ifdef release}ExitProcess({$endif}
  StartApp
  {$ifdef release}){$endif}
  ;
end.
