(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
     Host Application: $(ActiveHostApplication)
*)

library dbxfb;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Windows,
  DBXpress,
  IB_Header in '..\..\core\source\rtl\IB_Header.pas',
  firebird.client in '..\..\core\source\rtl\firebird.client.pas',
  firebird.client.debug in '..\..\core\source\rtl\firebird.client.debug.pas',
  firebird.dsql in '..\..\core\source\rtl\firebird.dsql.pas',
  dbx.common in '..\source\dbx.common.pas',
  dbx.firebird in '..\source\dbx.firebird.pas',
  dbx.firebird.driver30 in '..\source\dbx.firebird.driver30.pas',
  dbx.firebird.metadata30 in '..\source\dbx.firebird.metadata30.pas',
  dbx.firebird.cursor30 in '..\source\dbx.firebird.cursor30.pas',
  dbx.firebird.command30 in '..\source\dbx.firebird.command30.pas',
  dbx.firebird.connection30 in '..\source\dbx.firebird.connection30.pas',
  dbx.firebird.factory30 in '..\source\dbx.firebird.factory30.pas',
  dbx.firebird.metadata25 in '..\source\dbx.firebird.metadata25.pas',
  dbx.firebird.connection25 in '..\source\dbx.firebird.connection25.pas',
  dbx.firebird.factory25 in '..\source\dbx.firebird.factory25.pas',
  dbx.firebird.command25 in '..\source\dbx.firebird.command25.pas',
  dbx.firebird.cursor25 in '..\source\dbx.firebird.cursor25.pas';

{$R *.res}

{$LIBPREFIX 'dbxby'}
{$LIBSUFFIX '30'}

function getSQLDriverFIREBIRD(SVendorLib, SResourceFile: PChar; out Obj): SQLResult; stdcall;
var H: THandle;
    sDir: string;
begin
  sDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(SVendorLib));

    H := LoadLibrary(SVendorLib);
    if H = 0 then
      Result := DBXERR_DRIVERINITFAILED
    else begin
      ISQLDriver(Obj) := TSQLDriver_Firebird.Create(H);
      Result := DBXERR_NONE;
    end;
  finally
    SetCurrentDir(sDir);
  end;
end;

exports
  getSQLDriverFIREBIRD;

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
