unit dbx.firebird.factory40;

interface

uses DBXpress, firebird.client, dbx.common, dbx.firebird;

type
  TDBX_Firebird_Factory_40 = class(TInterfacedObject, IDBX_Firebird_Factory)
  protected
    function NewCommand(const aCommand: ISQLCommand): ISQLCommand;
    function NewConnection(const aConnection: ISQLConnection): ISQLConnection;
    function NewCursor(const aCursor: ISQLCursor): ISQLCursor;
    function NewDebuggerListener(const aDBXOptions: TDBXOptions): IFirebirdLibraryDebuggerListener;
    function NewMetaData(const aMetaData: ISQLMetaData): ISQLMetaData;
  end;

implementation

uses WideStrUtils, dbx.firebird.factory30, dbx.firebird.connection40, dbx.firebird.metadata40,
     dbx.firebird.command40, dbx.firebird.cursor40;

function TDBX_Firebird_Factory_40.NewCommand(
  const aCommand: ISQLCommand): ISQLCommand;
begin
  Result := TSQLCommand_Firebird_40.Create(aCommand);
end;

function TDBX_Firebird_Factory_40.NewConnection(const aConnection:
    ISQLConnection): ISQLConnection;
begin
  Result := TSQLConnection_Firebird_40.Create(aConnection);
end;

function TDBX_Firebird_Factory_40.NewCursor(
  const aCursor: ISQLCursor): ISQLCursor;
begin
  Result := TSQLCursor_Firebird_40.Create(aCursor);
end;

function TDBX_Firebird_Factory_40.NewDebuggerListener(
  const aDBXOptions: TDBXOptions): IFirebirdLibraryDebuggerListener;
begin
  Result := TFirebirdClientDebuggerListener_DBXCallBack_30.Create(aDBXOptions);
end;

function TDBX_Firebird_Factory_40.NewMetaData(
  const aMetaData: ISQLMetaData): ISQLMetaData;
begin
  Result := TSQLMetaData_Firebird_40.Create(aMetaData);
end;

initialization
  TDBX_Firebird.Register('4.0', TDBX_Firebird_Factory_40);
end.
