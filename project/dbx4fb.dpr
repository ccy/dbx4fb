library dbx4fb;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }



{$R 'library.res' '..\..\build\rc\library.rc'}

uses
  FastMM4,
  firebird.ibase.h in '..\..\core\source\firebird\firebird.ibase.h.pas',
  firebird.types_pub.h in '..\..\core\source\firebird\firebird.types_pub.h.pas',
  firebird.sqlda_pub.h in '..\..\core\source\firebird\firebird.sqlda_pub.h.pas',
  firebird.time.h in '..\..\core\source\firebird\firebird.time.h.pas',
  firebird.consts_pub.h in '..\..\core\source\firebird\firebird.consts_pub.h.pas',
  firebird.iberror.h in '..\..\core\source\firebird\firebird.iberror.h.pas',
  firebird.inf_pub.h in '..\..\core\source\firebird\firebird.inf_pub.h.pas',
  dbx4.base in '..\source\dbx4.base.pas',
  dbx4.firebird.dll in '..\source\dbx4.firebird.dll.pas',
  dbx4.firebird.driver in '..\source\dbx4.firebird.driver.pas',
  dbx4.firebird.connection in '..\source\dbx4.firebird.connection.pas',
  dbx4.firebird.command in '..\source\dbx4.firebird.command.pas',
  dbx4.firebird.reader in '..\source\dbx4.firebird.reader.pas',
  dbx4.firebird.row in '..\source\dbx4.firebird.row.pas',
  dbx4.firebird.metadata in '..\source\dbx4.firebird.metadata.pas',
  dbx4.firebird.base in '..\source\dbx4.firebird.base.pas',
  firebird.client.debug in '..\..\core\source\firebird\firebird.client.debug.pas',
  firebird.client in '..\..\core\source\firebird\firebird.client.pas',
  firebird.dsql in '..\..\core\source\firebird\firebird.dsql.pas',
  firebird.charsets in '..\..\core\source\firebird\firebird.charsets.pas',
  firebird.blr.h in '..\..\core\source\firebird\firebird.blr.h.pas',
  SysUtilsEx in '..\..\core\source\rtl\SysUtilsEx.pas',
  dbx4.firebird.dll.d14 in '..\source\dbx4.firebird.dll.d14.pas',
  Windows.PEB in '..\..\core\source\rtl\Windows.PEB.pas',
  Windows.ntdll in '..\..\core\source\rtl\Windows.ntdll.pas',
  WindowsEx in '..\..\core\source\rtl\WindowsEx.pas';

begin
end.
