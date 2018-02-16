unit dbx4.firebird.dll;

interface

implementation

uses System.SysUtils, Data.FmtBcd, Data.SqlTimSt, Winapi.Windows,
     {$if CompilerVersion=18.5}WideStrUtils,{$ifend}
     {$if CompilerVersion>=21}Data.DBXCommonResStrs,{$ifend}
     Data.DBXCommon, Data.DBXPlatform, Data.DBXDynalink,
     dbx4.firebird.driver, dbx4.firebird.connection, dbx4.firebird.command,
     dbx4.firebird.reader, dbx4.base;

// Test if it is WideString from Delphi 2007 DBX
function IsUnicodeStringArray(Arr: TRawByteStringArray): boolean;
begin
  Result := False;
  if Length(Arr) > 0 then
    Result := (StringElementSize(Arr[0]) = 2) and (StringCodePage(Arr[0]) = 1200{System.CP_UTF16}) and (StringRefCount(Arr[0]) < 1000);
end;

function ToWideStringArray(Arr: TRawByteStringArray): TWideStringArray;
var i, iArrayLen: integer;
    Convert: TFunc<RawByteString, string>;
begin
  iArrayLen := Length(Arr);
  SetLength(Result, iArrayLen);

  if IsUnicodeStringArray(Arr) then
    Convert := function (Source: RawByteString): string
               var R: RawByteString;
               begin
                 R := Source;
                 SetCodePage(R, CP_ACP, True);
                 Result := string(R);
               end
  else
    Convert := function (Source: RawByteString): string
               var W: WideString;
               begin
                 SetLength(W, Length(Source) div 2);
                 Move(Source[1], W[1], Length(Source));
                 Result := W;
               end;

  for i := Low(Arr) to High(Arr) do
    Result[i] := Convert(Arr[i]);
end;

function DBXBase_Close(Handle: TDBXCommonHandle): TDBXErrorCode; stdcall;
begin
  Result := IDBXBase(Handle).Close;
  if Result = TDBXErrorCodes.None then
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
begin
  Parameters := nil;
  Result := IDBXCommand(Handle).CreateParameterRow(Parameters);
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
  Result := TDBXErrorCodes.None;
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
  Result := TDBXErrorCodes.None;
end;

function DBXCommand_SetRowSetSize(Handle: TDBXCommandHandle; RowSetSize:
    Int64): TDBXErrorCode; stdcall;
begin
  Assert(False);
  Result := TDBXErrorCodes.None;
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
end;

function DBXConnection_Connect(Handle: TDBXConnectionHandle; Count: TInt32;
    Names, Values: TRawByteStringArray): TDBXErrorCode; stdcall;
var N, V: TWideStringArray;
    i: integer;
begin
  N := ToWideStringArray(Names);
  V := ToWideStringArray(Values);

  i := Length(N);
  SetLength(N, i+ 1);
  N[i] := 'Delphi2007Connection';

  i := Length(V);
  SetLength(V, i + 1);
  V[i] := BoolToStr(not IsUnicodeStringArray(Names), True);

  Result := IDBXConnection(Handle).Connect(Count + 1, N, V);
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
  Result := TDBXErrorCodes.None;
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
  Result := TDBXErrorCodes.None;
end;

function DBXLoader_GetDriver(Count: TInt32; Names, Values: TRawByteStringArray;
    ErrorMessage: TDBXWideStringBuilder; out pDriver: TDBXDriverHandle):
    TDBXErrorCode; stdcall;
var o: IDBXDriver;
    i: integer;
    N, V: TWideStringArray;
begin
  N := ToWideStringArray(Names);
  V := ToWideStringArray(Values);
  o := TDBXDriver_Firebird.Create(Count, N, V);
  if o.Loaded then begin
    pDriver := nil;
    IDBXDriver(pDriver) := o;
    Result := TDBXErrorCodes.None;
  end else begin
    pDriver := nil;
    Result := TDBXErrorCodes.DriverInitFailed;

    for i := 0 to Count - 1 do begin
      if N[i] = TDBXPropertyNames.VendorLib then begin
        {$if CompilerVersion=18.5}WStrPCopy(ErrorMessage, Format(sDLLLoadError, [V[i], Result]));{$ifend}
        {$if CompilerVersion>=20}StrPCopy(ErrorMessage, Format(sDLLLoadError, [V[i], Result]));{$ifend}
        Break;
      end;
    end;
  end;
end;

function DBXParameterRow_SetParameterType(Handle: TDBXRowHandle;Ordinal:
    TInt32; const Name: TDBXWideString; ChildPosition: TInt32; ParamDirection:
    TDBXParameterDirection; DBXType: TInt32; DBXSubType: TInt32; Size: Int64;
    Precision: Int64; Scale: TInt32): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetParameterType(Ordinal, Name, ChildPosition, ParamDirection, DBXType, DBXSubType, Size, Precision, Scale);
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
  Result := (IDBXBase(Handle) as IDBXRow).GetBcd(Ordinal, Value, IsNull);
end;

function DBXRow_GetBoolean(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    LongBool; out IsNull: LongBool): TDBXErrorCode; stdcall;
var B: WordBool;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetBoolean(Ordinal, B, IsNull);
  Value := B;
end;

function DBXRow_GetByteLength(Handle: TDBXRowHandle; Ordinal: TInt32; out
    Length: Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetByteLength(Ordinal, Length, IsNull);
end;

function DBXRow_GetBytes(Handle: TDBXRowHandle; Ordinal: TInt32; Offset: Int64;
    Value: TBytes; const LastIndex: TInt32;
    {dummy to simulate native "open array"} ValueOffset, Length: Int64; out
    ReturnLength: Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetBytes(Ordinal, Offset, Value, LastIndex, ValueOffset, Length, ReturnLength, IsNull);
end;

function DBXRow_GetDate(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TDBXDate; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).getDate(Ordinal, Value, IsNull);
end;

function DBXRow_GetDouble(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    double; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetDouble(Ordinal, Value, IsNull);
end;

function DBXRow_GetFixedBytes(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TBytes; const LastIndex: TInt32;{dummy to simulate native "open array"}
    ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetFixedBytes(Ordinal, Value, LastIndex, ValueOffset, IsNull);
end;

function DBXRow_GetInt16(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    SmallInt; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetInt16(Ordinal, Value, IsNull);
end;

function DBXRow_GetInt32(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    LongInt; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetInt32(Ordinal, Value, IsNull);
end;

function DBXRow_GetInt64(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    Int64; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetInt64(Ordinal, Value, IsNull);
end;

function DBXRow_GetObjectTypeName(Handle: TDBXRowHandle; Ordinal: TInt32;
    Value: TDBXWideStringBuilder; MaxLength: Integer): TDBXErrorCode; stdcall;
begin
  Assert(False);
  Result := TDBXErrorCodes.None;
end;

function DBXRow_GetString(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetAnsiString(Ordinal, Value, IsNull);
end;

function DBXRow_GetTime(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TDBXTime; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetTime(Ordinal, Value, IsNull);
end;

function DBXRow_GetTimeStamp(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    TSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetTimeStamp(Ordinal, Value, IsNull);
end;

function DBXRow_GetWideString(Handle: TDBXRowHandle; Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXBase(Handle) as IDBXRow).GetWideString(Ordinal, Value, IsNull);
end;

function DBXWritableRow_SetBcd(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TBcd): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetBcd(Ordinal, Value);
end;

function DBXWritableRow_SetBoolean(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: LongBool): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetBoolean(Ordinal, Value);
end;

function DBXWritableRow_SetBytes(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; BlobOffset: Int64; Value: TBytes; LastIndex: TInt32;
    {dummy to simulate native "open array"} ValueOffset: Int64; Length: Int64):
    TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetBytes(Ordinal, BlobOffset, Value, LastIndex, ValueOffset, Length);
end;

function DBXWritableRow_SetDate(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TDBXDate): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetDate(Ordinal, Value);
end;

function DBXWritableRow_SetDouble(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: double): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetDouble(Ordinal, Value);
end;

function DBXWritableRow_SetInt16(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: SmallInt): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetInt16(Ordinal, Value);
end;

function DBXWritableRow_SetInt32(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: LongInt): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetInt32(Ordinal, Value);
end;

function DBXWritableRow_SetInt64(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: Int64): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetInt64(Ordinal, Value);
end;

function DBXWritableRow_SetNull(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetNull(Ordinal);
end;

function DBXWritableRow_SetString(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; const Value: TDBXAnsiString; Length: Int64): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetAnsiString(Ordinal, Value, Length);
end;

function DBXWritableRow_SetTime(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: TDBXTime): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetTime(Ordinal, Value);
end;

function DBXWritableRow_SetTimeStamp(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; var Value: TSQLTimeStamp): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetTimeStamp(Ordinal, Value);
end;

function DBXWritableRow_SetWideString(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; const Value: TDBXWideString; Length: Int64): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetWideString(Ordinal, Value, Length);
end;

function DBX_SupportWaitOnLocksInIsolationLevel: Boolean; stdcall;
begin
  Result := True;
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
  DBXWritableRow_SetWideString,
  DBX_SupportWaitOnLocksInIsolationLevel;

end.
