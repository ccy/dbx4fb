unit dbx.firebird.connection30;

interface

uses IB_Header, DBXpress, firebird.client, dbx.common;

type
  IDBXCallBack = interface(IInterface)
  ['{C4C44DCD-8440-4066-80B1-3B508A5772A2}']
    procedure SetDBXCallBack(const aEvent: TSQLCallBackEvent);
    procedure SetDBXCallBackInfo(const aInfo: integer);
  end;

  TFirebirdClientDebuggerListener_DBXCallBack = class(TInterfacedObject, IFirebirdClientDebuggerListener, IDBXCallBack)
  private
    FDBXCallBack: TSQLCallBackEvent;
    FDBXCallBackInfo: integer;
  protected
    procedure Update(const aDebugStr: string);
    procedure SetDBXCallBack(const aEvent: TSQLCallBackEvent);
    procedure SetDBXCallBackInfo(const aInfo: integer);
  end;

  TSqlConnection30_Firebird = class(TInterfacedObject, ISQLConnection,
      ISQLConnection30)
  strict private
    FDBHandle: isc_db_handle;
  private
    FConnQualifiedName: WideString;
    FDebuggerListener: IFirebirdClientDebuggerListener;
    FDBXOptions: TDBXOptions;
  private
    FClient: IFirebirdClient;
    FStatusVector: IStatusVector;
    FTransHandle: isc_tr_handle;
  protected
    function GetDBHandle: pisc_db_handle;
    function StatusVector: IStatusVector;
  protected
    function beginTransaction(TranID: LongWord): SQLResult; stdcall;
    function commit(TranID: LongWord): SQLResult; stdcall;
    function connect(): SQLResult; overload; stdcall;
    function connect(ServerName: PWideChar; UserName: PWideChar; Password:
        PWideChar): SQLResult; overload; stdcall;
    function disconnect: SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getSQLCommand(out pComm: ISQLCommand30): SQLResult; stdcall;
    function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult; stdcall;
    function rollback(TranID: LongWord): SQLResult; stdcall;
    function SetOption(eConnectOption: TSQLConnectionOption; lValue: LongInt):
        SQLResult; stdcall;
  public
    constructor Create(const aClient: IFirebirdClient);
    procedure BeforeDestruction; override;
  end;

implementation

uses Windows, dbx.firebird.command30, dbx.firebird.metadata30,
     WideStrUtils;

constructor TSqlConnection30_Firebird.Create(const aClient: IFirebirdClient);
begin
  inherited Create;
  FDBXOptions := TDBXOptions.Create;

  FClient := aClient;
  FDebuggerListener := TFirebirdClientDebuggerListener_DBXCallBack.Create;
  (FClient as IFirebirdClientDebugger).Add(FDebuggerListener);
end;

procedure TSqlConnection30_Firebird.BeforeDestruction;
begin
  inherited;
  FDBXOptions.Free;
end;

function TSqlConnection30_Firebird.beginTransaction(TranID: LongWord): SQLResult;
var T: TTransactionDesc;
    tpb: AnsiString;
    teb: isc_teb;
begin
  {$Message 'Unable to find isc_start_transaction header translation in pascal'}
  {$Message 'Implement various transaction behavior via Transaction Parameter Block (tpb)'}
  {$Message 'Study the support of multiple transaction'}
  T := pTTransactionDesc(TranID)^;

  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  FTransHandle := 0;
  teb.db_ptr := GetDBHandle;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);
  FClient.isc_start_multiple(StatusVector.pValue, @FTransHandle, 1, @teb);

  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);
end;

function TSqlConnection30_Firebird.commit(TranID: LongWord): SQLResult;
begin
  FClient.isc_commit_transaction(StatusVector.pValue, @FTransHandle);
  StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
end;

function TSqlConnection30_Firebird.connect: SQLResult;
begin
  Assert(False);
end;

function TSqlConnection30_Firebird.connect(ServerName: PWideChar; UserName:
    PWideChar; Password: PWideChar): SQLResult;
var DPB, sServerName: AnsiString;
begin
  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(UserName)) + UserName +
         char(isc_dpb_password) + char(Length(Password)) + Password;

  sServerName := ServerName;

  FDBHandle := 0;
  FClient.isc_attach_database(StatusVector.pValue, Length(sServerName), PAnsiChar(sServerName), GetDBHandle, Length(DPB), PAnsiChar(DPB));
  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);
end;

function TSqlConnection30_Firebird.disconnect: SQLResult;
begin
  FClient.isc_detach_database(StatusVector.pValue, GetDBHandle);
  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);
end;

function TSqlConnection30_Firebird.GetDBHandle: pisc_db_handle;
begin
  Result := @FDBHandle;
end;

function TSqlConnection30_Firebird.getErrorMessage(Error: PWideChar): SQLResult;
begin
  StatusVector.GetLastError.GetMessage(Error);
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  ErrorLen := StatusVector.GetError(FClient).GetLength;
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.GetOption(eDOption: TSQLConnectionOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  case eDOption of
    eConnAutoCommit: Assert(False);
    eConnBlockingMode: Assert(False);
    eConnBlobSize: Assert(False);
    eConnRoleName: Assert(False);
    eConnWaitOnLocks: Assert(False);
    eConnCommitRetain: Assert(False);
    eConnTxnIsoLevel: Assert(False);
    eConnNativeHandle: Assert(False);
    eConnServerVersion: Assert(False);
    eConnCallBack: Assert(False);
    eConnHostName: Assert(False);
    eConnDatabaseName: Assert(False);
    eConnCallBackInfo: Assert(False);
    eConnObjectMode: Assert(False);
    eConnMaxActiveComm: ;  {$Message 'Do not sure what to do here'}
    eConnServerCharSet: Assert(False);
    eConnSqlDialect: begin
      longint(PropValue^) := FDBXOptions.SQLDialect;
      Length := MaxLength;
    end;
    eConnRollbackRetain: Assert(False);
    eConnObjectQuoteChar: Assert(False);
    eConnConnectionName: Assert(False);
    eConnOSAuthentication: Assert(False);
    eConnSupportsTransaction: Assert(False);
    eConnMultipleTransaction: Assert(False);
    eConnServerPort: Assert(False);
    eConnOnLine: Assert(False);
    eConnTrimChar: begin
      Boolean(PropValue^) := FDBXOptions.TrimChar;
      Length := MaxLength;
    end;
    eConnQualifiedName: Assert(False);
    eConnCatalogName: begin
      PWideChar(PropValue)^ := #0;
      Length := 0;
    end;
    eConnSchemaName: begin
      PWideChar(PropValue)^ := #0;
      Length := 0;
    end;
    eConnObjectName: begin
      WStrPLCopy(PWideChar(PropValue), FConnQualifiedName, MaxLength);
      Length := System.Length(FConnQualifiedName) * SizeOf(WideChar);
    end;
    eConnQuotedObjectName: Assert(False);
    eConnCustomInfo: Assert(False);
    eConnTimeOut: Assert(False);
    eConnConnectionString: Assert(False);
    eConnTDSPacketSize: Assert(False);
    eConnClientHostName: Assert(False);
    eConnClientAppName: Assert(False);
    eConnCompressed: Assert(False);
    eConnEncrypted: Assert(False);
    eConnPrepareSQL: Assert(False);
    eConnDecimalSeparator: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.getSQLCommand(out pComm: ISQLCommand30):
    SQLResult;
begin
  pComm := TSQLCommand30_Firebird.Create(FClient, GetDBHandle, FDBXOptions);
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.getSQLMetaData(out pMetaData: ISQLMetaData30):
    SQLResult;
begin
  pMetaData := TSQLMetaData30_Firebird.Create(FClient, GetDBHandle, FDBXOptions);
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.rollback(TranID: LongWord): SQLResult;
begin
  FClient.isc_rollback_transaction(StatusVector.pValue, @FTransHandle);
  StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
end;

function TSqlConnection30_Firebird.SetOption(eConnectOption:
    TSQLConnectionOption; lValue: LongInt): SQLResult;
begin
  case eConnectOption of
    eConnAutoCommit: Assert(False);
    eConnBlockingMode: Assert(False);
    eConnBlobSize: ; {$Message 'Do not sure what to do here'}
    eConnRoleName: Assert(False);
    eConnWaitOnLocks: ; {$Message 'Do not sure what to do here'}
    eConnCommitRetain: ; {$Message 'Do not sure what to do here'}
    eConnTxnIsoLevel: Assert(False);
    eConnNativeHandle: Assert(False);
    eConnServerVersion: Assert(False);
    eConnCallBack: (FDebuggerListener as IDBXCallBack).SetDBXCallBack(TSQLCallBackEvent(lValue));
    eConnHostName: Assert(False);
    eConnDatabaseName: Assert(False);
    eConnCallBackInfo: (FDebuggerListener as IDBXCallBack).SetDBXCallBackInfo(lValue);
    eConnObjectMode: Assert(False);
    eConnMaxActiveComm: Assert(False);
    eConnServerCharSet: Assert(False);
    eConnSqlDialect: FDBXOptions.SQLDialect := lValue;
    eConnRollbackRetain: Assert(False);
    eConnObjectQuoteChar: Assert(False);
    eConnConnectionName: Assert(False);
    eConnOSAuthentication: Assert(False);
    eConnSupportsTransaction: Assert(False);
    eConnMultipleTransaction: Assert(False);
    eConnServerPort: Assert(False);
    eConnOnLine: Assert(False);
    eConnTrimChar: FDBXOptions.TrimChar := Boolean(lValue);
    eConnQualifiedName: begin
      FConnQualifiedName := WideString(PWideChar(lValue)); {$Message 'Do not sure what to do here'}
    end;
    eConnCatalogName: Assert(False);
    eConnSchemaName: Assert(False);
    eConnObjectName: Assert(False);
    eConnQuotedObjectName: Assert(False);
    eConnCustomInfo: Assert(False);
    eConnTimeOut: Assert(False);
    eConnConnectionString: Assert(False);
    eConnTDSPacketSize: Assert(False);
    eConnClientHostName: Assert(False);
    eConnClientAppName: Assert(False);
    eConnCompressed: Assert(False);
    eConnEncrypted: Assert(False);
    eConnPrepareSQL: Assert(False);
    eConnDecimalSeparator: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSqlConnection30_Firebird.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack.SetDBXCallBack(
  const aEvent: TSQLCallBackEvent);
begin
  FDBXCallBack := aEvent;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack.SetDBXCallBackInfo(
  const aInfo: integer);
begin
  FDBXCallBackInfo := aInfo;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack.Update(const aDebugStr: string);
type
  SQLTRACEDesc30 = packed record
    pszTrace        : array [0..1023] of WideChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;
var D: SQLTRACEDesc30;
    W: WideString;
begin
  if Assigned(FDBXCallBack) then begin 
    W := aDebugStr;
    WStrPLCopy(D.pszTrace, W, Length(D.pszTrace));
    D.eTraceCat := 0; {$Message 'Should find a way to specify the trace category'}
    D.ClientData := FDBXCallBackInfo;
    D.uTotalMsgLen := WStrLen(D.pszTrace);
    FDBXCallBack(integer(traceTRANSACT), @D);
  end;
end;

end.
