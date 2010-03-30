unit dbx4.firebird.connection;

interface

uses DBXCommon, DBXPlatform, DBXDynalink, firebird.client, dbx4.base, dbx4.firebird.base,
     firebird.ibase.h, firebird.consts_pub.h, firebird.types_pub.h;

type
  TFirebirdClientDebuggerListener_DBXCallBack = class(TInterfacedObject, IFirebirdLibraryDebuggerListener)
  private
    FCallbackEvent: DBXTraceCallback;
    FCallbackHandle: DBXCallbackHandle;
  protected
    procedure Update(const aDebugStr: string);
  public
    constructor Create(const aCallbackEvent: DBXTraceCallback; const
        aCallbackHandle: DBXCallbackHandle);
  end;

  TDBXConnection_Firebird = class(TDBXBase_Firebird, IDBXConnection, IDBXConnection_Firebird)
  strict private
    FDBHandle: isc_db_handle;
  private
    FCallbackEvent: DBXTraceCallback;
    FCallbackHandle: DBXCallbackHandle;
    FDebuggerListener: IFirebirdLibraryDebuggerListener;
    FDatabase: WideString;
    FFirebirdLibrary: IFirebirdLibrary;
    FHostName: WideString;
    FIsolationLevel: TInt32;
    FPassword: WideString;
    FSQLDialect: integer;
    FTransactionPool: TFirebirdTransactionPool;
    FTrimChar: boolean;
    FUserName: WideString;
    FServerCharSet: WideString;
    procedure CheckDebugger;
  protected
    function BeginTransaction(out TransactionHandle: TDBXTransactionHandle;
        IsolationLevel: TInt32): TDBXErrorCode;
    function Close: TDBXErrorCode; override;
    function Commit(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function Connect(Count: TInt32; Names, Values: TWideStringArray): TDBXErrorCode;
    function GetDBHandle: pisc_db_handle;
    function GetFirebirdLibrary: IFirebirdLibrary; override;
    function GetServerCharSet: WideString;
    function GetSQLDialect: integer;
    function GetTransactionPool: TFirebirdTransactionPool;
    function GetTrimChar: Boolean;
    function IsolationLevel: TInt32;
    function Rollback(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function SetCallbackEvent(CallbackHandle: DBXCallbackHandle; CallbackEvent:
        DBXTraceCallback): TDBXErrorCode;
  public
    constructor Create(const aDriver: IDBXDriver);
  end;

implementation

uses SysUtils, Windows, SqlConst, SysUtilsEx;

constructor TDBXConnection_Firebird.Create(const aDriver: IDBXDriver);
begin
  inherited Create;
  FFirebirdLibrary := (aDriver as IDBXDriver_Firebird).NewLibrary;
end;

function TDBXConnection_Firebird.BeginTransaction(
  out TransactionHandle: TDBXTransactionHandle;
  IsolationLevel: TInt32): TDBXErrorCode;
var O: TTransactionInfo;
    N: IFirebirdTransaction;
begin
  {$Message 'Unable to find isc_start_transaction header translation'}
  try
    case IsolationLevel of
      TDBXIsolations.RepeatableRead: O.Isolation := isoRepeatableRead;
      else
        O.Isolation := isoReadCommitted;
    end;
    N := FTransactionPool.Add(O);
  except
    Result := TDBXErrorCodes.VendorError;
    Exit;
  end;

  N.Start(StatusVector);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  TransactionHandle := nil;
  IFirebirdTransaction(TransactionHandle) := N;
  Result := TDBXErrorCodes.None;
end;

procedure TDBXConnection_Firebird.CheckDebugger;
begin
  if Assigned(FDebuggerListener) then
    (FFirebirdLibrary as IFirebirdLibraryDebugger).Remove(FDebuggerListener);
  FDebuggerListener := nil;
  if Assigned(FCallBackEvent) and Assigned(FCallbackHandle) then begin
    FDebuggerListener := TFirebirdClientDebuggerListener_DBXCallBack.Create(FCallbackEvent, FCallbackHandle);
    (FFirebirdLibrary as IFirebirdLibraryDebugger).Add(FDebuggerListener);
  end;
end;

function TDBXConnection_Firebird.Close: TDBXErrorCode;
begin
  FTransactionPool.Free;
  if FDBHandle <> nil then begin
    FFirebirdLibrary.isc_detach_database(StatusVector.pValue, GetDBHandle);
    StatusVector.CheckResult(Result, TDBXErrorCodes.ConnectionFailed);
  end else
    Result := TDBXErrorCodes.None;
end;

function TDBXConnection_Firebird.Commit(
  TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
begin
  FTransactionPool.Commit(StatusVector, IFirebirdTransaction(TransactionHandle));
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXConnection_Firebird.Connect(Count: TInt32; Names, Values:
    TWideStringArray): TDBXErrorCode;
var i: integer;
    DPB, sServerName: AnsiString;
begin
  FServerCharSet := 'None';
  for i := 0 to Count - 1 do begin
    if Names[i] = TDBXPropertyNames.Database then
      FDatabase := ExpandFileNameString(Values[i])
    else if Names[i] = TDBXPropertyNames.HostName then
      FHostName := Values[i]
    else if Names[i] = TDBXPropertyNames.UserName then
      FUserName := Values[i]
    else if Names[i] = TDBXPropertyNames.Password then
      FPassword := Values[i]
    else if SameText(Names[i], SQLSERVER_CHARSET_KEY) then
      FServerCharSet := Values[i]
    else if SameText(Names[i], SQLDIALECT_KEY) then begin
      if not TryStrToInt(Values[i], FSQLDialect) then
        FSQLDialect := 3;
    end else if SameText(Names[i], TRIMCHAR) then begin
      if not TryStrToBool(Values[i], FTrimChar) then
        FTrimChar := True;
    end else if SameText(Names[i], 'Interbase TransIsolation') then begin
      if Values[i] = 'ReadCommitted' then
        FIsolationLevel := TDBXIsolations.ReadCommitted
      else if Values[i] = 'RepeatableRead' then
        FIsolationLevel := TDBXIsolations.RepeatableRead
      else if Values[i] = 'DirtyRead' then
        FIsolationLevel := TDBXIsolations.DirtyRead
      else if Values[i] = 'Serializable' then
        FIsolationLevel := TDBXIsolations.Serializable
      else if Values[i] = 'SnapShot' then
        FIsolationLevel := TDBXIsolations.SnapShot
      else
        FIsolationLevel := TDBXIsolations.ReadCommitted
    end;
  end;

  DPB := AnsiChar(isc_dpb_version1) +
         AnsiChar(isc_dpb_lc_ctype) + AnsiChar(Length(FServerCharSet)) + AnsiString(FServerCharSet) +
         AnsiChar(isc_dpb_user_name) + AnsiChar(Length(FUserName)) + AnsiString(FUserName) +
         AnsiChar(isc_dpb_password) + AnsiChar(Length(FPassword)) + AnsiString(FPassword);

  sServerName := AnsiString(FDatabase);
  if FHostName <> '' then
    sServerName := AnsiString(FHostName) + ':' + AnsiString(sServerName);

  FDBHandle := nil;
  FFirebirdLibrary.isc_attach_database(StatusVector.pValue, Length(sServerName), PISC_SCHAR(sServerName), GetDBHandle, Length(DPB), PISC_SCHAR(DPB));
  StatusVector.CheckResult(Result, TDBXErrorCodes.ConnectionFailed);

  FTransactionPool.Free;
  FTransactionPool := TFirebirdTransactionPool.Create(FFirebirdLibrary, GetDBHandle);
end;

function TDBXConnection_Firebird.GetDBHandle: pisc_db_handle;
begin
  Result := @FDBHandle;
end;

function TDBXConnection_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := FFirebirdLibrary;
end;

function TDBXConnection_Firebird.GetServerCharSet: WideString;
begin
  Result := FServerCharSet;
end;

function TDBXConnection_Firebird.GetSQLDialect: integer;
begin
  Result := FSQLDialect;
end;

function TDBXConnection_Firebird.GetTransactionPool: TFirebirdTransactionPool;
begin
  Result := FTransactionPool;
end;

function TDBXConnection_Firebird.GetTrimChar: Boolean;
begin
  Result := FTrimChar;
end;

function TDBXConnection_Firebird.IsolationLevel: TInt32;
begin
  Result := FIsolationLevel;
end;

function TDBXConnection_Firebird.Rollback(
  TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
begin
  FTransactionPool.RollBack(StatusVector, IFirebirdTransaction(TransactionHandle));
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXConnection_Firebird.SetCallbackEvent(CallbackHandle:
    DBXCallbackHandle; CallbackEvent: DBXTraceCallback): TDBXErrorCode;
begin
  FCallbackHandle := CallbackHandle;
  FCallbackEvent := CallbackEvent;
  Result := TDBXErrorCodes.None;
  CheckDebugger;
end;

{ TFirebirdClientDebuggerListener_DBXCallBack }

constructor TFirebirdClientDebuggerListener_DBXCallBack.Create(const
    aCallbackEvent: DBXTraceCallback; const aCallbackHandle: DBXCallbackHandle);
begin
  inherited Create;
  FCallbackEvent := aCallbackEvent;
  FCallbackHandle := aCallbackHandle;
end;

procedure TFirebirdClientDebuggerListener_DBXCallBack.Update(
  const aDebugStr: string);
var W: WideString;
begin
  if Assigned(FCallbackEvent) then begin
    W := aDebugStr;
    FCallbackEvent(FCallbackHandle, TDBXTraceFlags.Transact, PWideChar(W));
  end;
end;

end.
