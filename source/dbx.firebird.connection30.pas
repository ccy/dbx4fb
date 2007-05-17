unit dbx.firebird.connection30;

interface

uses IB_Header, DBXpress, firebird.client, dbx.common;

type
  TSQLConnection_Firebird_30 = class(TInterfacedObject, ISQLConnection, ISQLConnection30)
  strict private
    FDBHandle: isc_db_handle;
  private
    FDebuggerListener: IFirebirdClientDebuggerListener;
    FDBXOptions: TDBXOptions;
  private
    FClient: IFirebirdClient;
    FStatusVector: IStatusVector;
    FTransactionPool: TFirebirdTransactionPool;
    procedure CheckDebugger;
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

uses SysUtils, Windows, dbx.firebird.command30, dbx.firebird.metadata30,
     WideStrUtils, dbx.firebird;

constructor TSQLConnection_Firebird_30.Create(const aClient: IFirebirdClient);
begin
  inherited Create;
  FDBXOptions := TDBXOptions.Create;
  FClient := aClient;
end;

procedure TSQLConnection_Firebird_30.BeforeDestruction;
begin
  inherited;
  FDBXOptions.Free;
end;

function TSQLConnection_Firebird_30.beginTransaction(TranID: LongWord): SQLResult;
var T: TTransactionDesc;
    N: IFirebirdTransaction;
begin
  {$Message 'Unable to find isc_start_transaction header translation in pascal'}

  T := pTTransactionDesc(TranID)^;
  if T.TransactionID = 0 then begin
    Result := DBXERR_INVALIDTXNID;
    Exit;
  end;

  try
    N := FTransactionPool.Add(T);
  except
    Result := DBXERR_DUPLICATETXNID;
    Exit;
  end;

  N.Start(StatusVector);
  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);
end;

procedure TSQLConnection_Firebird_30.CheckDebugger;
begin
  if Assigned(FDebuggerListener) then
    (FClient as IFirebirdClientDebugger).Remove(FDebuggerListener);
  FDebuggerListener := nil;
  if Assigned(FDBXOptions.DBXCallBackEvent) and (FDBXOptions.DBXCallBackInfo <> 0) then begin
    FDebuggerListener := TDBX_Firebird.Factory.NewDebuggerListener(FDBXOptions);
    (FClient as IFirebirdClientDebugger).Add(FDebuggerListener);
  end;
end;

function TSQLConnection_Firebird_30.commit(TranID: LongWord): SQLResult;
begin
  FTransactionPool.Commit(StatusVector, pTTransactionDesc(TranID)^.TransactionID);
  StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
end;

function TSQLConnection_Firebird_30.connect: SQLResult;
begin
  Assert(False);
end;

function TSQLConnection_Firebird_30.connect(ServerName: PWideChar; UserName:
    PWideChar; Password: PWideChar): SQLResult;
var DPB, sServerName: AnsiString;
begin
  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(UserName)) + UserName +
         char(isc_dpb_password) + char(Length(Password)) + Password;

  sServerName := ServerName;
  if FDBXOptions.HostName <> '' then
    sServerName := FDBXOptions.HostName + ':' + sServerName; 

  FDBHandle := 0;
  FClient.isc_attach_database(StatusVector.pValue, Length(sServerName), PAnsiChar(sServerName), GetDBHandle, Length(DPB), PAnsiChar(DPB));
  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);

  FTransactionPool.Free;
  FTransactionPool := TFirebirdTransactionPool.Create(FClient, GetDBHandle);
end;

function TSQLConnection_Firebird_30.disconnect: SQLResult;
begin
  FTransactionPool.Free;

  FClient.isc_detach_database(StatusVector.pValue, GetDBHandle);
  StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED);
end;

function TSQLConnection_Firebird_30.GetDBHandle: pisc_db_handle;
begin
  Result := @FDBHandle;
end;

function TSQLConnection_Firebird_30.getErrorMessage(Error: PWideChar): SQLResult;
begin
  StatusVector.GetLastError.GetMessage(Error);
  Result := DBXERR_NONE;
end;

function TSQLConnection_Firebird_30.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  ErrorLen := StatusVector.GetError(FClient).GetLength;
  Result := DBXERR_NONE;
end;

function TSQLConnection_Firebird_30.GetOption(eDOption: TSQLConnectionOption;
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
      WStrPCopy(PWideChar(PropValue), FDBXOptions.ConnQualifiedName);
      Length := System.Length(FDBXOptions.ConnQualifiedName);
    end;
    eConnQuotedObjectName: begin
      WStrPCopy(PWideChar(PropValue), FDBXOptions.ConnQuotedObjectName);
      Length := System.Length(FDBXOptions.ConnQuotedObjectName);
    end;
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

function TSQLConnection_Firebird_30.getSQLCommand(out pComm: ISQLCommand30):
    SQLResult;
var C: ISQLCommand;
begin
  C := TSQLCommand_Firebird_30.Create(FClient, GetDBHandle, FTransactionPool, FDBXOptions);
  ISQLCommand(pComm) := TDBX_Firebird.Factory.NewCommand(C);
  Result := DBXERR_NONE;
end;

function TSQLConnection_Firebird_30.getSQLMetaData(out pMetaData: ISQLMetaData30):
    SQLResult;
var M: ISQLMetaData;
begin
  M := TSQLMetaData_Firebird_30.Create(FClient, GetDBHandle, FTransactionPool, FDBXOptions);
  ISQLMetaData(pMetaData) := TDBX_Firebird.Factory.NewMetaData(M);
  Result := DBXERR_NONE;
end;

function TSQLConnection_Firebird_30.rollback(TranID: LongWord): SQLResult;
begin
  FTransactionPool.RollBack(StatusVector, pTTransactionDesc(TranID)^.TransactionID);
  StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
end;

function TSQLConnection_Firebird_30.SetOption(eConnectOption:
    TSQLConnectionOption; lValue: LongInt): SQLResult;
begin
  case eConnectOption of
    eConnAutoCommit: Assert(False);
    eConnBlockingMode: Assert(False);
    eConnBlobSize: FDBXOptions.BlobSize := lValue; 
    eConnRoleName: FDBXOptions.RoleName := PWideChar(lValue);
    eConnWaitOnLocks: ; {$Message 'Do not sure what to do here'}
    eConnCommitRetain: ; {$Message 'Do not sure what to do here'}
    eConnTxnIsoLevel: FDBXOptions.TransIsolationLevel := TTransIsolationLevel(lValue);  {$Message 'Transaction not sensitive to Isolation yet'} 
    eConnNativeHandle: Assert(False);
    eConnServerVersion: Assert(False);
    eConnCallBack: begin
      FDBXOptions.DBXCallBackEvent := TSQLCallBackEvent(lValue);
      CheckDebugger;
    end;
    eConnHostName: FDBXOptions.HostName := PWideChar(lValue);
    eConnDatabaseName: Assert(False);
    eConnCallBackInfo: begin
      FDBXOptions.DBXCallBackInfo := lValue;
      CheckDebugger;
    end;
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
    eConnQualifiedName: FDBXOptions.ConnQualifiedName := PWideChar(lValue);
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

function TSQLConnection_Firebird_30.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;


end.
