unit dbx4.firebird.dll.d14;

interface

implementation

uses SqlTimSt, DBXCommon, DBXDynalink, dbx4.base;

function DBXRow_GetInt8(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    ShortInt; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := TDBXErrorCodes.NotImplemented;
end;

function DBXRow_GetSingle(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    single; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := TDBXErrorCodes.NotImplemented;
end;

function DBXRow_GetTimeStampOffset(Handle: TDBXRowHandle; Ordinal: TInt32; out
    Value: TSQLTimeStampOffset; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := TDBXErrorCodes.NotImplemented;
end;

function DBXRow_GetUInt8(Handle: TDBXRowHandle; Ordinal: TInt32; out Value:
    Byte; out IsNull: LongBool): TDBXErrorCode; stdcall;
begin
  Result := TDBXErrorCodes.NotImplemented;
end;

function DBXWritableRow_SetInt8(Handle: TDBXWritableRowHandle; Ordinal: TInt32;
    Value: ShortInt): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetInt8(Ordinal, Value);
end;

function DBXWritableRow_SetSingle(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: single): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetSingle(Ordinal, Value);
end;

function DBXWritableRow_SetTimeStampOffset(Handle: TDBXWritableRowHandle;
    Ordinal: TInt32; var Value: TSQLTimeStampOffset): TDBXErrorCode; stdcall;
begin
  Result := TDBXErrorCodes.NotImplemented;
end;

function DBXWritableRow_SetUInt8(Handle: TDBXWritableRowHandle; Ordinal:
    TInt32; Value: Byte): TDBXErrorCode; stdcall;
begin
  Result := (IDBXRow(Handle) as IDBXWritableRow).SetByte(Ordinal, Value);
end;

exports
  DBXRow_GetInt8,
  DBXRow_GetSingle,
  DBXRow_GetTimeStampOffset,
  DBXRow_GetUInt8,
  DBXWritableRow_SetInt8,
  DBXWritableRow_SetSingle,
  DBXWritableRow_SetTimeStampOffset,
  DBXWritableRow_SetUInt8;

end.
