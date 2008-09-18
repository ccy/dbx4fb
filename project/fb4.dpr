library fb4;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }



{$R '..\..\build\rc\library.res' '..\..\build\rc\library.rc'}

uses
  SysUtils,
  Windows,
  IB_Header in '..\..\core\source\rtl\IB_Header.pas',
  firebird.charsets in '..\..\core\source\rtl\firebird.charsets.pas',
  firebird.client in '..\..\core\source\rtl\firebird.client.pas',
  firebird.client.debug in '..\..\core\source\rtl\firebird.client.debug.pas',
  firebird.dsql in '..\..\core\source\rtl\firebird.dsql.pas',
  dbx4.base in '..\source\dbx4.base.pas',
  dbx4.firebird.dll in '..\source\dbx4.firebird.dll.pas',
  dbx4.firebird.driver in '..\source\dbx4.firebird.driver.pas',
  dbx4.firebird.connection in '..\source\dbx4.firebird.connection.pas',
  dbx4.firebird.command in '..\source\dbx4.firebird.command.pas',
  dbx4.firebird.reader in '..\source\dbx4.firebird.reader.pas',
  dbx4.firebird.row in '..\source\dbx4.firebird.row.pas',
  dbx4.firebird.metadata in '..\source\dbx4.firebird.metadata.pas',
  dbx4.firebird.base in '..\source\dbx4.firebird.base.pas',
  CodeRedirect in '..\..\core\source\patch\CodeRedirect.pas',
  FmtBcd.QC50427 in '..\..\core\source\patch\FmtBcd.QC50427.pas',
  FmtBcd.QC55176 in '..\..\core\source\patch\FmtBcd.QC55176.pas',
  FmtBcd.QC55550 in '..\..\core\source\patch\FmtBcd.QC55550.pas';

{$LIBPREFIX 'dbx'}
{$LIBSUFFIX '0'}

begin
end.
