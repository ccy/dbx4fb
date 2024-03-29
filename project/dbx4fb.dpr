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

{$R *.res}

uses
  dbx4.base in '..\source\dbx4.base.pas',
  dbx4.firebird.dll in '..\source\dbx4.firebird.dll.pas',
  dbx4.firebird.driver in '..\source\dbx4.firebird.driver.pas',
  dbx4.firebird.connection in '..\source\dbx4.firebird.connection.pas',
  dbx4.firebird.command in '..\source\dbx4.firebird.command.pas',
  dbx4.firebird.reader in '..\source\dbx4.firebird.reader.pas',
  dbx4.firebird.row in '..\source\dbx4.firebird.row.pas',
  dbx4.firebird.metadata in '..\source\dbx4.firebird.metadata.pas',
  dbx4.firebird.base in '..\source\dbx4.firebird.base.pas';

begin
end.
