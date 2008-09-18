unit dbx4.firebird.dll;

interface

implementation

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform, DBXDynalink,
  dbx4.firebird.driver, dbx4.firebird.connection, dbx4.firebird.command,
  dbx4.firebird.reader, dbx4.base;

{$region ''}
//type
//  TDBXWideStringBuilder  = PWideChar;
//  TInt32                 = Longint;
//  TDBXWideString         = PWideChar;
//  TDBXAnsiString         = PChar;
//  TDBXAnsiStringBuilder  = PChar;
//
//  TDBXHandle             = pointer;
//  TDBXCommonHandle       = TDBXHandle;
//  TDBXCommandHandle      = TDBXHandle;
//  TDBXRowHandle          = TDBXHandle;
//  TDBXReaderHandle       = TDBXHandle;
//  TDBXConnectionHandle   = TDBXHandle;
//  TDBXTransactionHandle  = TDBXHandle;
//  TDBXDriverHandle       = TDBXHandle;
//  TDBXWritableRowHandle  = TDBXHandle;
//  DBXCallbackHandle      = TDBXHandle;
//
//  CBRType = (                           { Call-back return type }
//    cbrUSEDEF,                          { Take default action }
//    cbrCONTINUE,                        { Continue }
//    cbrABORT,                           { Abort the operation }
//    cbrCHKINPUT,                        { Input given }
//    cbrYES,                             { Take requested action }
//    cbrNO,                              { Do not take requested action }
//    cbrPARTIALASSIST,                   { Assist in completing the job }
//    cbrSKIP,                            { Skip this operation }
//    cbrRETRY                            { Retry this operation }
//  );
//
//  DBXTraceCallback      = function(Handle: DBXCallbackHandle; TraceCategory: TInt32; TraceMessage: TDBXWideString): CBRType; stdcall;
{$endregion}

function DBXBase_Close(Handle: TDBXCommonHandle): TDBXErrorCode; stdcall;
begin
  Result := IDBXBase(Handle).Close;
  IDBXBase(Handle) := nil;
end;

function DBXBase_GetErrorMessage(Handle: TDBXCommonHandle; LastErrorCode:
    TDBXErrorCode; ErrorMessage: TDBXWideStringBuilder): TDBXErrorCode; stdcall;
begin
  Result := IDBXBase(Handle).GetErrorMessage(LastErrorCode, ErrorMessage);
end;

function DBXBase_GetErrorMessageLength(Handle: TDBXCommonHandle; LastErrorCode:
    TDBXErrorCode; out ErrorLen: TInt32): TDBXErrorCode; stdcall;
begin
  Result := IDBXBase(Handle).GetErrorMessageLength(LastErrorCode, ErrorLen);
end;

function DBXCommand_CreateParameterRow(Handle: TDBXCommandHandle; out
    Parameters: TDBXRowHandle): TDBXErrorCode; stdcall;
var o: IDBXWritableRow;
begin
  Result := IDBXCommand(Handle).CreateParameterRow(o);
  Parameters := nil;
  IDBXWritableRow(Parameters) := o;
end;

function DBXCommand_Execute(Handle: TDBXCommandHandle; out Reader:
    TDBXReaderHandle): TDBXErrorCode; stdcall;
var o: IDBXReader;
begin
  Result := IDBXCommand(Handle).Execute(o);
  IDBXReader(Reader) := o;
end;

function DBXCommand_ExecuteImmediate(Handle: TDBXCommandHandle; const SQL:
    TDBXWideString; out Reader: TDBXReaderHandle): TDBXErrorCode; stdcall;
var R: IDBXReader;
begin
  Result := IDBXCommand(Handle).ExecuteImmediate(SQL, R);
  IDBXReader(Reader) := R;
end;

function DBXCommand_GetNextReader(Handle: TDBXCommandHandle; out Reader:
    TDBXReaderHandle): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXCommand_GetRowsAffected(Handle: TDBXCommandHandle; out Rows:
    Int64): TDBXErrorCode; stdcall;
begin
  Result := IDBXCommand(Handle).GetRowsAffected(Rows);
end;

function DBXCommand_Prepare(Handle: TDBXCommandHandle; const SQL:
    TDBXWideString; Count: TInt32): TDBXErrorCode; stdcall;
begin
  Result := IDBXCommand(Handle).Prepare(SQL, Count);
end;

function DBXCommand_SetMaxBlobSize(Handle: TDBXCommandHandle; MaxBlobSize:
    Int64): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXCommand_SetRowSetSize(Handle: TDBXCommandHandle; RowSetSize:
    Int64): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXConnection_BeginTransaction(Handle: TDBXConnectionHandle; out
    TransactionHandle: TDBXTransactionHandle; IsolationLevel: TInt32):
    TDBXErrorCode; stdcall;
begin
  Result := IDBXConnection(Handle).BeginTransaction(TransactionHandle, IsolationLevel);
end;

function DBXConnection_Commit(Handle: TDBXConnectionHandle; TransactionHandle:
    TDBXTransactionHandle): TDBXErrorCode; stdcall;
begin
  Result := IDBXConnection(Handle).Commit(TransactionHandle);
  IInterface(TransactionHandle) := nil;
end;

function DBXConnection_Connect(Handle: TDBXConnectionHandle; Count: TInt32;
    Names, Values: TWideStringArray): TDBXErrorCode; stdcall;
begin
  Result := IDBXConnection(Handle).Connect(Count, Names, Values);
end;

function DBXConnection_CreateCommand(Handle: TDBXConnectionHandle; const
    CommandType: TDBXWideString; out pCommand: TDBXCommandHandle):
    TDBXErrorCode; stdcall;
begin
  IDBXCommand(pCommand) := TDBXCommand_Firebird.Create(IDBXConnection(Handle), CommandType) as IDBXCommand;
  Result := TDBXErrorCodes.None;
end;

function DBXConnection_Disconnect(Handle: TDBXConnectionHandle): TDBXErrorCode;
    stdcall;
begin
  Assert(False);
end;

function DBXConnection_GetIsolation(Handle: TDBXConnectionHandle; out
    IsolationLevel: TInt32): TDBXErrorCode; stdcall;
begin
  IsolationLevel := IDBXConnection(Handle).IsolationLevel;
  Result := TDBXErrorCodes.None;
end;

function DBXConnection_Rollback(Handle: TDBXConnectionHandle;
    TransactionHandle: TDBXTransactionHandle): TDBXErrorCode; stdcall;
begin
  Result := IDBXConnection(Handle).Rollback(TransactionHandle);
  IInterface(TransactionHandle) := nil;
end;

function DBXConnection_SetCallbackEvent(Handle: TDBXConnectionHandle;
    CallbackHandle: DBXCallbackHandle; CallbackEvent: DBXTraceCallback):
    TDBXErrorCode; stdcall;
begin
  Result := IDBXConnection(Handle).SetCallbackEvent(CallbackHandle, CallbackEvent);
end;

function DBXDriver_CreateConnection(Handle: TDBXDriverHandle; out pConn:
    TDBXConnectionHandle): TDBXErrorCode; stdcall;
begin
  pConn := nil;
  IDBXConnection(pConn) := TDBXConnection_Firebird.Create(IDBXDriver(Handle)) as IDBXConnection;
  Result := TDBXErrorCodes.None;
end;

function DBXDriver_GetVersion(Handle: TDBXDriverHandle; Version:
    TDBXWideStringBuilder; MaxLength: TInt32): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXLoader_GetDriver(Count: TInt32; Names, Values: TWideStringArray;
    ErrorMessage: TDBXWideStringBuilder; out pDriver: TDBXDriverHandle):
    TDBXErrorCode; stdcall;
var o: IDBXDriver;
begin
  o := TDBXDriver_Firebird.Create(Count, Names, Values);
  if o.Loaded then begin
    pDriver := nil;
    IDBXDriver(pDriver) := o;
    Result := TDBXErrorCodes.None;
  end else begin
    pDriver := nil;
    Result := TDBXErrorCodes.DriverInitFailed;
  end;
end;

function DBXParameterRow_SetParameterType(Handle: TDBXRowHandle;Ordinal:
    TInt32; const Name: TDBXWideString; ChildPosition: TInt32; ParamDirection:
    TDBXParameterDirection; DBXType: TInt32; DBXSubType: TInt32; Size: Int64;
    Precision: Int64; Scale: TInt32): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetParameterType(Ordinal, Name, ChildPosition, ParamDirection, DBXType, DBXSubType, Size, Precision, Scale);
end;

function DBXReader_GetColumnCount(Handle: TDBXReaderHandle; out ColumnCount:
    TInt32): TDBXErrorCode; stdcall;
begin
  ColumnCount := IDBXReader(Handle).ColumnCount;
  Result := TDBXErrorCodes.None;
end;

function DBXReader_GetColumnMetadata(Handle: TDBXReaderHandle; Ordinal: TInt32;
    Name: TDBXWideStringBuilder; out ColumnType: TInt32; out ColumnSubType:
    TInt32; out Length: TInt32; out precision: TInt32; out scale: TInt32; out
    flags: TInt32): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetColumnMetadata(Ordinal, Name, ColumnType, ColumnSubType, Length, Precision, scale, flags);
end;

function DBXReader_Next(Handle: TDBXReaderHandle): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).Next;
end;

function DBXRow_GetBcd(Handle: TDBXRowHandle; Ordinal: TInt32; out Value: TBcd;
    out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetBoolean(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    LongBool; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetBoolean(Ordinal, Value, IsNull);
end;

function DBXRow_GetByteLength(Handle: TDBXRowHandle; Ordinal: TInt32; out
    Length: Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetByteLength(Ordinal, Length, IsNull);
end;

function DBXRow_GetBytes(Handle: TDBXRowHandle; Ordinal: TInt32; Offset: Int64;
    Value: TBytes; const LastIndex: TInt32;
    {dummy to simulate native "open array"} ValueOffset, Length: Int64; out
    ReturnLength: Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetBytes(Ordinal, Offset, Value, LastIndex, ValueOffset, Length, ReturnLength, IsNull);
end;

function DBXRow_GetDate(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TDBXDate; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetDouble(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    double; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetFixedBytes(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TBytes; const LastIndex: TInt32;{dummy to simulate native "open array"}
    ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetFixedBytes(Ordinal, Value, LastIndex, ValueOffset, IsNull);
end;

function DBXRow_GetInt16(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    SmallInt; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetInt32(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    LongInt; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetInt32(Ordinal, Value, IsNull);
end;

function DBXRow_GetInt64(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetObjectTypeName(Handle: TDBXRowHandle; Ordinal: TInt32;
    Value: TDBXWideStringBuilder; MaxLength: Integer): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetString(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetTime(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TDBXTime; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetTimeStamp(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXRow_GetWideString(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := IDBXReader(Handle).GetWideString(Ordinal, Value, IsNull);
end;

function DBXWritableRow_SetBcd(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TBcd): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetBcd(Ordinal, Value);
end;

function DBXWritableRow_SetBoolean(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: LongBool): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXWritableRow_SetBytes(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; BlobOffset: Int64; Value: TBytes; LastIndex: TInt32;
    {dummy to simulate native "open array"} ValueOffset: Int64; Length: Int64):
    TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetBytes(Ordinal, BlobOffset, Value, LastIndex, ValueOffset, Length);
end;

function DBXWritableRow_SetDate(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TDBXDate): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetDate(Ordinal, Value);
end;

function DBXWritableRow_SetDouble(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: double): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetDouble(Ordinal, Value);
end;

function DBXWritableRow_SetInt16(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: SmallInt): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetInt16(Ordinal, Value);
end;

function DBXWritableRow_SetInt32(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: LongInt): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetInt32(Ordinal, Value);
end;

function DBXWritableRow_SetInt64(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: Int64): TDBXErrorCode; stdcall;
begin
  Assert(False);
end;

function DBXWritableRow_SetNull(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetNull(Ordinal);
end;

function DBXWritableRow_SetString(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; const Value: TDBXAnsiString; Length: Int64): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetAnsiString(Ordinal, Value, Length);
end;

function DBXWritableRow_SetTime(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TDBXTime): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetTime(Ordinal, Value);
end;

function DBXWritableRow_SetTimeStamp(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; var Value: TSQLTimeStamp): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetTimeStamp(Ordinal, Value);
end;

function DBXWritableRow_SetWideString(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; const Value: TDBXWideString; Length: Int64): TDBXErrorCode; stdcall;
begin
  Result := IDBXWritableRow(Handle).SetWideString(Ordinal, Value, Length);
end;

exports
  DBXBase_Close,
  DBXBase_GetErrorMessage,
  DBXBase_GetErrorMessageLength,
  DBXCommand_CreateParameterRow,
  DBXCommand_Execute,
  DBXCommand_ExecuteImmediate,
  DBXCommand_GetNextReader,
  DBXCommand_GetRowsAffected,
  DBXCommand_Prepare,
  DBXCommand_SetMaxBlobSize,
  DBXCommand_SetRowSetSize,
  DBXConnection_BeginTransaction,
  DBXConnection_Commit,
  DBXConnection_Connect,
  DBXConnection_CreateCommand,
  DBXConnection_Disconnect,
  DBXConnection_GetIsolation,
  DBXConnection_Rollback,
  DBXConnection_SetCallbackEvent,
  DBXDriver_CreateConnection,
  DBXDriver_GetVersion,
  DBXLoader_GetDriver,
  DBXParameterRow_SetParameterType,
  DBXReader_GetColumnCount,
  DBXReader_GetColumnMetadata,
  DBXReader_Next,
  DBXRow_GetBcd,
  DBXRow_GetBoolean,
  DBXRow_GetByteLength,
  DBXRow_GetBytes,
  DBXRow_GetDate,
  DBXRow_GetDouble,
  DBXRow_GetFixedBytes,
  DBXRow_GetInt16,
  DBXRow_GetInt32,
  DBXRow_GetInt64,
  DBXRow_GetObjectTypeName,
  DBXRow_GetString,
  DBXRow_GetTime,
  DBXRow_GetTimeStamp,
  DBXRow_GetWideString,
  DBXWritableRow_SetBcd,
  DBXWritableRow_SetBoolean,
  DBXWritableRow_SetBytes,
  DBXWritableRow_SetDate,
  DBXWritableRow_SetDouble,
  DBXWritableRow_SetInt16,
  DBXWritableRow_SetInt32,
  DBXWritableRow_SetInt64,
  DBXWritableRow_SetNull,
  DBXWritableRow_SetString,
  DBXWritableRow_SetTime,
  DBXWritableRow_SetTimeStamp,
  DBXWritableRow_SetWideString;

end.
