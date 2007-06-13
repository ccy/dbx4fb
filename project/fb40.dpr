(* Build Instruction:

     Output Directory: $(OutputDir)\$(ActiveProjectModule)
Unit Output Directory: $(UnitOutputDir)\$(ActiveProjectModule)
     Host Application: $(ActiveHostApplication)
*)

library fb40;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R 'rc\library.res' 'rc\library.rc'}

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
  DBXpress40 in '..\source\DBXpress40.pas',
  dbx.firebird.driver40 in '..\source\dbx.firebird.driver40.pas',
  dbx.firebird.metadata40 in '..\source\dbx.firebird.metadata40.pas',
  dbx.firebird.cursor40 in '..\source\dbx.firebird.cursor40.pas',
  dbx.firebird.command40 in '..\source\dbx.firebird.command40.pas',
  dbx.firebird.connection40 in '..\source\dbx.firebird.connection40.pas',
  dbx.firebird.factory40 in '..\source\dbx.firebird.factory40.pas';

{$LIBPREFIX 'dbx'}

function getSQLDriverFIREBIRD(SVendorLib, SResourceFile: PChar; out Obj): SQLResult40; stdcall;
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
      Pointer(Obj) := nil;

      ISQLDriver(Obj) := TSQLDriver_Firebird_40.Create(TSQLDriver_Firebird.Create(H));
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
