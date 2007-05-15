unit dbx.firebird.factory25;

interface

uses DBXpress, firebird.client, dbx.common, dbx.firebird;

type
  SQLTRACEDesc25 = packed record
    pszTrace        : array [0..1023] of AnsiChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  TFirebirdClientDebuggerListener_DBXCallBack_25 = class(TInterfacedObject, IFirebirdClientDebuggerListener)
  private
    FDBXOptions: TDBXOptions;
  protected
    procedure Update(const aDebugStr: string);
  public
    constructor Create(const aDBXOptions: TDBXOptions);
  end;

  TDBX_Firebird_Factory_25 = class(TInterfacedObject, IDBX_Firebird_Factory)
  protected
    function NewCommand(const aCommand: ISQLCommand): ISQLCommand;
    function NewConnection(const aConnection: ISQLConnection): ISQLConnection;
    function NewCursor(const aCursor: ISQLCursor): ISQLCursor;
    function NewDebuggerListener(const aDBXOptions: TDBXOptions): IFirebirdClientDebuggerListener;
    function NewMetaData(const aMetaData: ISQLMetaData): ISQLMetaData;
  end;

implementation

uses SysUtils, dbx.firebird.connection25, dbx.firebird.metadata25,
  dbx.firebird.command25, dbx.firebird.cursor25;

function TDBX_Firebird_Factory_25.NewCommand(
  const aCommand: ISQLCommand): ISQLCommand;
begin
  Result := TSQLCommand_Firebird_25.Create(aCommand);
end;

function TDBX_Firebird_Factory_25.NewConnection(const aConnection:
    ISQLConnection): ISQLConnection;
begin
  Result := TSQLConnection_Firebird_25.Create(aConnection);
end;

function TDBX_Firebird_Factory_25.NewCursor(
  const aCursor: ISQLCursor): ISQLCursor;
begin
  Result := TSQLCursor_Firebird_25.Create(aCursor);
end;

function TDBX_Firebird_Factory_25.NewDebuggerListener(
  const aDBXOptions: TDBXOptions): IFirebirdClientDebuggerListener;
begin
  Result := TFirebirdClientDebuggerListener_DBXCallBack_25.Create(aDBXOptions);
end;

function TDBX_Firebird_Factory_25.NewMetaData(
  const aMetaData: ISQLMetaData): ISQLMetaData;
begin
  Result := TSQLMetaData_Firebird_25.Create(aMetaData);
end;

constructor TFirebirdClientDebuggerListener_DBXCallBack_25.Create(
  const aDBXOptions: TDBXOptions);
begin
  inherited Create;
  FDBXOptions := aDBXOptions;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack_25.Update(
  const aDebugStr: string);
var D: SQLTRACEDesc25;
begin
  if Assigned(FDBXOptions.DBXCallBackEvent) then begin
    StrPLCopy(D.pszTrace, aDebugStr, Length(aDebugStr));
    D.eTraceCat := 0; {$Message 'Should find a way to specify the trace category'}
    D.ClientData := FDBXOptions.DBXCallBackInfo;
    D.uTotalMsgLen := Length(D.pszTrace);
    FDBXOptions.DBXCallBackEvent(integer(traceTRANSACT), @D);
  end;
end;

initialization
  TDBX_Firebird.Register('2.5', TDBX_Firebird_Factory_25);
end.
