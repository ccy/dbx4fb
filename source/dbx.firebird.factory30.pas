unit dbx.firebird.factory30;

interface

uses DBXpress, firebird.client, dbx.common, dbx.firebird;

type
  SQLTRACEDesc30 = packed record
    pszTrace        : array [0..1023] of WideChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  TFirebirdClientDebuggerListener_DBXCallBack_30 = class(TInterfacedObject, IFirebirdLibraryDebuggerListener)
  private
    FDBXOptions: TDBXOptions;
  protected
    procedure Update(const aDebugStr: string);
  public
    constructor Create(const aDBXOptions: TDBXOptions);
  end;

  TDBX_Firebird_Factory_30 = class(TInterfacedObject, IDBX_Firebird_Factory)
  protected
    function NewCommand(const aCommand: ISQLCommand): ISQLCommand;
    function NewConnection(const aConnection: ISQLConnection): ISQLConnection;
    function NewCursor(const aCursor: ISQLCursor): ISQLCursor;
    function NewDebuggerListener(const aDBXOptions: TDBXOptions): IFirebirdLibraryDebuggerListener;
    function NewMetaData(const aMetaData: ISQLMetaData): ISQLMetaData;
  end;

implementation

uses WideStrUtils;

function TDBX_Firebird_Factory_30.NewCommand(
  const aCommand: ISQLCommand): ISQLCommand;
begin
  Result := aCommand;
end;

function TDBX_Firebird_Factory_30.NewConnection(const aConnection:
    ISQLConnection): ISQLConnection;
begin
  Result := aConnection;
end;

function TDBX_Firebird_Factory_30.NewCursor(
  const aCursor: ISQLCursor): ISQLCursor;
begin
  Result := aCursor;
end;

function TDBX_Firebird_Factory_30.NewDebuggerListener(
  const aDBXOptions: TDBXOptions): IFirebirdLibraryDebuggerListener;
begin
  Result := TFirebirdClientDebuggerListener_DBXCallBack_30.Create(aDBXOptions);
end;

function TDBX_Firebird_Factory_30.NewMetaData(
  const aMetaData: ISQLMetaData): ISQLMetaData;
begin
  Result := aMetaData;
end;

constructor TFirebirdClientDebuggerListener_DBXCallBack_30.Create(
  const aDBXOptions: TDBXOptions);
begin
  inherited Create;
  FDBXOptions := aDBXOptions;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack_30.Update(const aDebugStr: string);
var D: SQLTRACEDesc30;
    W: WideString;
begin
  if Assigned(FDBXOptions.DBXCallBackEvent) then begin
    W := aDebugStr;
    WStrPLCopy(D.pszTrace, W, Length(D.pszTrace));
    D.eTraceCat := 0; {$Message 'Should find a way to specify the trace category'}
    D.ClientData := FDBXOptions.DBXCallBackInfo;
    D.uTotalMsgLen := WStrLen(D.pszTrace);
    FDBXOptions.DBXCallBackEvent(integer(traceTRANSACT), @D);
  end;
end;

initialization
  TDBX_Firebird.Register(DBXDRIVERVERSION30, TDBX_Firebird_Factory_30);
end.
