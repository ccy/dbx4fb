unit dbx4.firebird.connection;

interface

uses
  System.SysUtils, Data.DBXCommon, Data.DBXDynalink, Data.DBXPlatform,
  dbx4.base, dbx4.firebird.base, firebird.client, firebird.consts_pub.h,
  firebird.ibase.h, firebird.types_pub.h,
  firebird.delphi;

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
    FDatabase: WideString;
    FFirebirdLibrary: IFirebirdLibrary;
    FHostName: WideString;
    FIsDelphi2007Connection: boolean;
    FIsolationLevel: TInt32;
    FPassword: WideString;
    FRoleName: WideString;
    FSQLDialect: integer;
    FTransactionPool: TFirebirdTransactionPool;
    FTrimChar: boolean;
    FUserName: WideString;
    FServerCharSet: WideString;
    FWaitOnLocks: Boolean;
    FWaitOnLocksTimeOut: Integer;
    procedure SetupTimeZones(AddTimeZone: TAddTimeZone);
  protected
    function BeginTransaction(out TransactionHandle: TDBXTransactionHandle;
        IsolationLevel: TInt32): TDBXErrorCode;
    function Close: TDBXErrorCode; override;
    function Commit(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function Connect(Count: TInt32; Names, Values: TWideStringArray):
        TDBXErrorCode;
    function GetDBHandle: pisc_db_handle;
    function GetFirebirdLibrary: IFirebirdLibrary; override;
    function GetIsDelphi2007Connection: boolean;
    function GetServerCharSet: WideString;
    function GetSQLDialect: integer;
    function GetTransactionPool: TFirebirdTransactionPool;
    function GetTrimChar: Boolean;
    function GetVendorProperty(Name: TDBXWideString; Value: TDBXWideStringBuilder;
        MaxLength: Longint): TDBXErrorCode;
    function GetWaitOnLocks: Boolean;
    function IsolationLevel: TInt32;
    function Rollback(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function SetCallbackEvent(CallbackHandle: DBXCallbackHandle; CallbackEvent:
        DBXTraceCallback): TDBXErrorCode;
  public
    constructor Create(const aDriver: IDBXDriver);
  end;

implementation

uses
  Winapi.Windows, Data.SqlConst,
  firebird.dsql;

constructor TDBXConnection_Firebird.Create(const aDriver: IDBXDriver);
begin
  inherited Create;
  FFirebirdLibrary := (aDriver as IDBXDriver_Firebird).NewLibrary;
end;

function TDBXConnection_Firebird.BeginTransaction(
  out TransactionHandle: TDBXTransactionHandle;
  IsolationLevel: TInt32): TDBXErrorCode;
var O: TTransactionInfo;
    N: TFirebirdTransaction;
begin
  O.Init;
  O.WaitOnLocksTimeOut := FWaitOnLocksTimeOut;
  if IsolationLevel and FirebirdTransaction_WaitOnLocks = 0 then
    O.WaitOnLocks := FWaitOnLocks
  else
    O.WaitOnLocks := IsolationLevel and FirebirdTransaction_WaitOnLocks = FirebirdTransaction_WaitOnLocks;

  IsolationLevel := IsolationLevel and $00FF;
  try
    if IsolationLevel = TDBXIsolations.RepeatableRead then
      O.Isolation := isoRepeatableRead
    else
      O.Isolation := isoReadCommitted;
    N := FTransactionPool.Add(O);
  except
    Result := TDBXErrorCodes.VendorError;
    Exit;
  end;

  N.Start(StatusVector);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  TransactionHandle := nil;
  TFirebirdTransaction(TransactionHandle) := N;
  Result := TDBXErrorCodes.None;
end;

function TDBXConnection_Firebird.Close: TDBXErrorCode;
begin
  FreeAndNil(FTransactionPool);
  if FDBHandle <> nil then begin
    FFirebirdLibrary.isc_detach_database(StatusVector.pValue, GetDBHandle);
    StatusVector.CheckResult(Result, TDBXErrorCodes.ConnectionFailed);
  end else
    Result := TDBXErrorCodes.None;

  FFirebirdLibrary.SetupTimeZoneHandler(nil);
end;

function TDBXConnection_Firebird.Commit(
  TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
begin
  FTransactionPool.Commit(StatusVector, TFirebirdTransaction(TransactionHandle));
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXConnection_Firebird.Connect(Count: TInt32; Names, Values:
    TWideStringArray): TDBXErrorCode;
var i: integer;
    DPB, sServerName: AnsiString;
    T: TTransactionInfo;
begin
  FServerCharSet := 'None';
  FRoleName := '';
  FWaitOnLocks := False;
  FWaitOnLocksTimeOut := -1;
  for i := 0 to Count - 1 do begin
    if Names[i] = TDBXPropertyNames.Database then
      FDatabase := ExpandFileNameString(Values[i])
    else if Names[i] = TDBXPropertyNames.HostName then
      FHostName := Values[i]
    else if Names[i] = TDBXPropertyNames.UserName then
      FUserName := Values[i]
    else if Names[i] = TDBXPropertyNames.Password then
      FPassword := Values[i]
    else if Names[i] = ROLENAME_KEY then
      FRoleName := Values[i]
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
    end else if SameText(Names[i], WAITONLOCKS_KEY) then begin
      TryStrToBool(Values[i], FWaitOnLocks);
    end else if SameText(Names[i], 'WaitOnLocksTimeOut') then begin
      TryStrToInt(Values[i], FWaitOnLocksTimeOut);
    end else if SameText(Names[i], 'Delphi2007Connection') then begin
      if not TryStrToBool(Values[i], FIsDelphi2007Connection) then
        FIsDelphi2007Connection := False;
    end;
  end;

  DPB := AnsiChar(isc_dpb_version1) +
         AnsiChar(isc_dpb_lc_ctype) + AnsiChar(Length(FServerCharSet)) + AnsiString(FServerCharSet) +
         AnsiChar(isc_dpb_user_name) + AnsiChar(Length(FUserName)) + AnsiString(FUserName) +
         AnsiChar(isc_dpb_password) + AnsiChar(Length(FPassword)) + AnsiString(FPassword) +
         AnsiChar(isc_dpb_sql_role_name) + AnsiChar(Length(FRoleName)) + AnsiString(FRoleName);

  sServerName := AnsiString(FDatabase);
  if FHostName <> '' then
    sServerName := AnsiString(FHostName) + ':' + AnsiString(sServerName);

  FDBHandle := nil;
  FFirebirdLibrary.isc_attach_database(StatusVector.pValue, Length(sServerName), PISC_SCHAR(sServerName), GetDBHandle, Length(DPB), PISC_SCHAR(DPB));
  StatusVector.CheckResult(Result, TDBXErrorCodes.ConnectionFailed);

  Assert(FTransactionPool = nil);
  T.Init;
  T.ID := 0;
  if FIsolationLevel = TDBXIsolations.RepeatableRead then
    T.Isolation := isoRepeatableRead
  else
    T.Isolation := isoReadCommitted;
  T.WaitOnLocks := FWaitOnLocks;
  T.WaitOnLocksTimeOut := FWaitOnLocksTimeOut;
  FTransactionPool := TFirebirdTransactionPool.Create(FFirebirdLibrary, GetDBHandle, T);

  FFirebirdLibrary.SetupTimeZoneHandler(SetupTimeZones);
end;

function TDBXConnection_Firebird.GetDBHandle: pisc_db_handle;
begin
  Result := @FDBHandle;
end;

function TDBXConnection_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := FFirebirdLibrary;
end;

function TDBXConnection_Firebird.GetIsDelphi2007Connection: boolean;
begin
  Result := FIsDelphi2007Connection;
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

function TDBXConnection_Firebird.GetVendorProperty(Name: TDBXWideString; Value:
    TDBXWideStringBuilder; MaxLength: Longint): TDBXErrorCode;
begin
  var v: string;
  if Name = 'UnicodeEncoding' then
    v := 'false'
  else if Name = 'QuoteCharEnabled' then
    v := 'false'
  else if Name = 'ProductVersion' then
    v := '4.1'
  else if Name = 'ProductName' then
    v := 'dbExpress driver for Firebird'
  else
    Exit(TDBXErrorCodes.NotImplemented);

  TDBXPlatform.CopyWideStringToBuilder(v, MaxLength, Value);
  Result := TDBXErrorCodes.None;
end;

function TDBXConnection_Firebird.GetWaitOnLocks: Boolean;
begin
  Result := FWaitOnLocks;
end;

function TDBXConnection_Firebird.IsolationLevel: TInt32;
begin
  Result := FIsolationLevel;
end;

function TDBXConnection_Firebird.Rollback(
  TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
begin
  FTransactionPool.RollBack(StatusVector, TFirebirdTransaction(TransactionHandle));
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXConnection_Firebird.SetCallbackEvent(CallbackHandle:
    DBXCallbackHandle; CallbackEvent: DBXTraceCallback): TDBXErrorCode;
var D: IFirebirdLibraryDebuggerListener;
begin
  D := nil;

  if Assigned(CallBackEvent) and Assigned(CallbackHandle) then
    D := TFirebirdClientDebuggerListener_DBXCallBack.Create(CallbackEvent, CallbackHandle);

  (FFirebirdLibrary as IFirebirdLibraryDebugger).SetListener(D);

  Result := TDBXErrorCodes.None;
end;

procedure TDBXConnection_Firebird.SetupTimeZones(AddTimeZone: TAddTimeZone);
begin
  var Q := 'SELECT a.rdb$time_zone_id, (SELECT RDB$EFFECTIVE_OFFSET FROM rdb$time_zone_util.transitions(a.RDB$TIME_ZONE_NAME, current_timestamp, current_timestamp)) ' +
             'FROM rdb$time_zones a';

  var L := TFirebird_DSQL.Create(FFirebirdLibrary, FTransactionPool) as IFirebird_DSQL;
  L.Open(StatusVector, GetDBHandle, nil);
  try
    L.Prepare(StatusVector, Q, FSQLDialect);
    L.Execute(StatusVector);
    StatusVector.CheckAndRaiseError(FFirebirdLibrary);

    while L.Fetch(StatusVector) <> 100 do begin
      StatusVector.CheckAndRaiseError(FFirebirdLibrary);
      AddTimeZone(L.o_SQLDA.Vars[0].AsInt32, L.o_SQLDA.Vars[1].AsInt16);
    end;
  finally
    L.Close(StatusVector);
    StatusVector.CheckAndRaiseError(FFirebirdLibrary);
  end;
end;

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
