unit dbx4.firebird.row;

interface

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform, dbx4.base,
  dbx4.firebird.base, firebird.dsql, firebird.client,
  firebird.ibase.h;

type
  TDBXRow_Firebird = class(TDBXBase_Firebird, IDBXRow, IDBXWritableRow)
  private
    FConnection: IDBXConnection;
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
    FMetaData: IMetaDataProvider;
    FTrimChar: boolean;
    function GetBcd(Ordinal: TInt32; out Value: PBcd; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetBoolean(Ordinal: TInt32; out Value: PWordBool; out IsNull:
        LongBool): TDBXErrorCode; overload;
    function GetDate(Ordinal: TInt32; out Value: PDate; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetDouble(Ordinal: TInt32; out Value: PDouble; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt16(Ordinal: TInt32; out Value: PSmallInt; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt32(Ordinal: TInt32; out Value: LongInt; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt64(Ordinal: TInt32; out Value: PInt64; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetTime(Ordinal: TInt32; Value: PInteger; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetTimeStamp(Ordinal: TInt32; out Value: PSQLTimeStamp; out IsNull:
        LongBool): TDBXErrorCode; overload;
    function Get_oVar(const Ordinal: TInt32): TXSQLVAR;
  protected  // IDBXRow
    function GetAnsiString(Ordinal: TInt32; Value: TDBXAnsiStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
    function GetBcd(Ordinal: TInt32; out Value: TBcd; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetBoolean(Ordinal: TInt32; out Value: WordBool; out IsNull:
        LongBool): TDBXErrorCode; overload;
    function GetByteLength(Ordinal: TInt32; out Length: Int64; out IsNull:
        LongBool): TDBXErrorCode;
    function GetBytes(Ordinal: TInt32; Offset: Int64; Value: TBytes; const
        LastIndex: TInt32; ValueOffset, Length: Int64; out ReturnLength: Int64; out
        IsNull: LongBool): TDBXErrorCode;
    function GetDate(Ordinal: TInt32; out Value: TDBXDate; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetDouble(Ordinal: TInt32; out Value: Double; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetFixedBytes(Ordinal: TInt32; Value: TBytes; const LastIndex: TInt32;
        ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode;
    function GetInt16(Ordinal: TInt32; out Value: SmallInt; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt32(Ordinal: TInt32; out Value: PLongInt; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt64(Ordinal: TInt32; out Value: Int64; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetTime(Ordinal: TInt32; out Value: TDBXTime; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetTimeStamp(Ordinal: TInt32; out Value: TSQLTimeStamp; out IsNull:
        LongBool): TDBXErrorCode; overload;
    function GetWideString(Ordinal: TInt32; Value: TDBXWideStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
  protected // IDBXWritableRow
    function SetAnsiString(Ordinal: TInt32; const Value: TDBXAnsiString; Length:
        Int64): TDBXErrorCode;
    function SetBcd(Ordinal: TInt32; Value: TBcd): TDBXErrorCode;
    function SetBoolean(Ordinal: TInt32; Value: WordBool): TDBXErrorCode;
    function SetByte(Ordinal: TInt32; Value: Byte): TDBXErrorCode;
    function SetBytes(Ordinal: TInt32; BlobOffset: Int64; Value: TBytes; LastIndex:
        TInt32; ValueOffset, Length: Int64): TDBXErrorCode;
    function SetDate(Ordinal: TInt32; Value: TDBXDate): TDBXErrorCode;
    function SetDouble(Ordinal: TInt32; Value: Double): TDBXErrorCode;
    function SetInt8(Ordinal: TInt32; Value: ShortInt): TDBXErrorCode;
    function SetInt16(Ordinal: TInt32; Value: SmallInt): TDBXErrorCode;
    function SetInt32(Ordinal: TInt32; Value: LongInt): TDBXErrorCode;
    function SetInt64(Ordinal: TInt32; Value: Int64): TDBXErrorCode;
    function SetNull(Ordinal: TInt32): TDBXErrorCode;
    function SetParameterType(Ordinal: TInt32; const Name: TDBXWideString;
        ChildPosition: TInt32; ParamDirection: TDBXParameterDirection; DBXType,
        DBXSubType: TInt32; Size, Precision: Int64; Scale: TInt32): TDBXErrorCode;
    function SetSingle(Ordinal: TInt32; Value: Single): TDBXErrorCode;
    function SetTime(Ordinal: TInt32; Value: TDBXTime): TDBXErrorCode;
    function SetTimeStamp(Ordinal: TInt32; var Value: TSQLTimeStamp): TDBXErrorCode;
    function SetWideString(Ordinal: TInt32; const Value: TDBXWideString; Length:
        Int64): TDBXErrorCode;
  protected
    function Close: TDBXErrorCode; override;
    function GetFirebirdLibrary: IFirebirdLibrary; override;
  public
    constructor Create(const aConnection: IDBXConnection; const aDBHandle:
        pisc_db_handle; const aMetaData: IMetaDataProvider; const aDSQL:
        IFirebird_DSQL; const aTrimChar: boolean);
  end;

implementation

constructor TDBXRow_Firebird.Create(const aConnection: IDBXConnection; const
    aDBHandle: pisc_db_handle; const aMetaData: IMetaDataProvider; const aDSQL:
    IFirebird_DSQL; const aTrimChar: boolean);
begin
  inherited Create;
  FConnection := aConnection;
  FDBHandle := aDBHandle;
  FMetaData := aMetaData;
  FDSQL := aDSQL;

  FTrimChar := aTrimChar;
end;

function TDBXRow_Firebird.Close: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetAnsiString(Ordinal: TInt32; Value:
    TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode;
var c: PAnsiChar;
    B: Boolean;
    V: TXSQLVAR;
begin
  V := Get_oVar(Ordinal);
  V.GetAnsiString(Value, B);
  IsNull := B;
  if FTrimChar then begin
    c := Value + V.sqllen - 1;
    while c^ = ' ' do begin
      c^ := #0;
      c := c - 1;
    end;
  end;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetBcd(Ordinal: TInt32; out Value: PBcd; out IsNull:
    LongBool): TDBXErrorCode;
var B: boolean;
begin
  Get_oVar(Ordinal).GetBCD(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetBcd(Ordinal: TInt32; out Value: TBcd;
  out IsNull: LongBool): TDBXErrorCode;
var p: PBcd;
begin
  p := @Value;
  Result := GetBcd(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetBoolean(Ordinal: TInt32; out Value: PWordBool; out
    IsNull: LongBool): TDBXErrorCode;
var V, B: boolean;
begin
  Get_oVar(Ordinal).GetBoolean(@V, B);
  Value^ := V;
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetBoolean(Ordinal: TInt32; out Value: WordBool; out
    IsNull: LongBool): TDBXErrorCode;
var p: PWordBool;
begin
  p := @Value;
  Result := GetBoolean(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetByteLength(Ordinal: TInt32; out Length: Int64; out
    IsNull: LongBool): TDBXErrorCode;
var i: Cardinal;
    B: Boolean;
begin
  Get_oVar(Ordinal).GetBlobSize(StatusVector, FDBHandle, FDSQL.Transaction, i, B);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  IsNull := B;
  Length := i;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetBytes(Ordinal: TInt32; Offset: Int64; Value:
    TBytes; const LastIndex: TInt32; ValueOffset, Length: Int64; out
    ReturnLength: Int64; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Assert(Offset = 0);
  Assert(ValueOffSet = 0);
  Get_oVar(Ordinal).GetBlob(StatusVector, FDBHandle, FDSQL.Transaction, Value, B, Length);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  ReturnLength := Length;
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetDate(Ordinal: TInt32; out Value: PDate; out
    IsNull: LongBool): TDBXErrorCode;
var B: boolean;
begin
  Get_oVar(Ordinal).GetDate(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetDate(Ordinal: TInt32; out Value: TDBXDate;
  out IsNull: LongBool): TDBXErrorCode;
var p: PDate;
begin
  p := @Value;
  Result := GetDate(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetDouble(Ordinal: TInt32; out Value: Double;
  out IsNull: LongBool): TDBXErrorCode;
var p: PDouble;
begin
  p := @Value;
  Result := GetDouble(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetDouble(Ordinal: TInt32; out Value: PDouble; out
    IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetDouble(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := (FConnection as IDBXBase_Firebird).FirebirdLibrary;
end;

function TDBXRow_Firebird.GetFixedBytes(Ordinal: TInt32; Value: TBytes; const
    LastIndex: TInt32; ValueOffset: TInt32; out IsNull: LongBool):
    TDBXErrorCode;
begin
  case FMetaData.GetColumnType(Ordinal) of
    TDBXDataTypes.AnsiStringType: Result := GetAnsiString(Ordinal, Pointer(Value), IsNull);
    TDBXDataTypes.WideStringType: Result := GetWideString(Ordinal, Pointer(Value), IsNull);
    TDBXDataTypes.BcdType:        Result := GetBcd(Ordinal, PBcd(Value), IsNull);
    TDBXDataTypes.BooleanType:    Result := GetBoolean(Ordinal, PWordBool(Value), IsNull);
    TDBXDataTypes.DateType:       Result := GetDate(Ordinal, PDate(Value), IsNull);
    TDBXDataTypes.Int16Type:      Result := GetInt16(Ordinal, PSmallInt(Value), IsNull);
    TDBXDataTypes.Int32Type:      Result := GetInt32(Ordinal, PLongint(Value), IsNull);
    TDBXDataTypes.Int64Type:      Result := GetInt64(Ordinal, PInt64(Value), IsNull);
    TDBXDataTypes.TimeStampType:  Result := GetTimeStamp(Ordinal, PSQLTimeStamp(Value), IsNull);
    TDBXDataTypes.TimeType:       Result := GetTime(Ordinal, PInteger(Value), IsNull);
    TDBXDataTypes.DoubleType:     Result := GetDouble(Ordinal, PDouble(Value), IsNull);
    else
      Result := NotSupported;
  end;
end;

function TDBXRow_Firebird.GetInt16(Ordinal: TInt32; out Value: SmallInt; out
    IsNull: LongBool): TDBXErrorCode;
var p: PSmallInt;
begin
  p := @Value;
  Result := GetInt16(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetInt16(Ordinal: TInt32; out Value: PSmallInt; out
    IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetShort(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetInt32(Ordinal: TInt32; out Value: LongInt;
  out IsNull: LongBool): TDBXErrorCode;
var p: PLongInt;
begin
  p := @Value;
  Result := GetInt32(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetInt32(Ordinal: TInt32; out Value: PLongInt; out
    IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetInteger(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetInt64(Ordinal: TInt32; out Value: Int64;
  out IsNull: LongBool): TDBXErrorCode;
var p: PInt64;
begin
  p := @Value;
  Result := GetInt64(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetInt64(Ordinal: TInt32; out Value: PInt64;
  out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetInt64(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetTime(Ordinal: TInt32; Value: PInteger; out IsNull:
    LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetTime(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetTime(Ordinal: TInt32; out Value: TDBXTime;
  out IsNull: LongBool): TDBXErrorCode;
var p: PInteger;
begin
  p := @Value;
  Result := GetTime(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetTimeStamp(Ordinal: TInt32;
  out Value: TSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode;
var p: PSQLTimeStamp;
begin
  p := @Value;
  Result := GetTimeStamp(Ordinal, p, IsNull);
end;

function TDBXRow_Firebird.GetTimeStamp(Ordinal: TInt32; out Value:
    PSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Get_oVar(Ordinal).GetTimeStamp(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.GetWideString(Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode;
var c: PWideChar;
    B: Boolean;
    V: TXSQLVAR;
begin
  V := Get_oVar(Ordinal);
  V.GetWideString(Value, B);
  IsNull := B;
  if FTrimChar then begin
    c := Value + V.GetTextLen - 1;
    while c^ = ' ' do begin
      c^ := #0;
      c := c - 1;
    end;
  end;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.Get_oVar(const Ordinal: TInt32): TXSQLVAR;
var i: integer;
begin
  i := Ordinal;
  if FDSQL.IsStoredProc and Assigned(FDSQL.i_SQLDA) then
    i := i - FDSQL.i_SQLDA.sqld;

  Result := FDSQL.o_SQLDA[i];
end;

function TDBXRow_Firebird.SetAnsiString(Ordinal: TInt32; const Value:
    TDBXAnsiString; Length: Int64): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetAnsiString(Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetBcd(Ordinal: TInt32; Value: TBcd): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetBCD(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetBoolean(Ordinal: TInt32; Value: WordBool):
    TDBXErrorCode;
var B: Boolean;
begin
  B := Value;
  FDSQL.i_SQLDA[Ordinal].SetBoolean(@B, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetByte(Ordinal: TInt32; Value: Byte): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetUInt8(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetBytes(Ordinal: TInt32; BlobOffset: Int64;
  Value: TBytes; LastIndex: TInt32; ValueOffset,
  Length: Int64): TDBXErrorCode;
begin
  Assert(BlobOffset = 0);
  Assert(ValueOffSet = 0);
  FDSQL.i_SQLDA[Ordinal].SetBlob(StatusVector, FDBHandle, FDSQL.Transaction, Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetDate(Ordinal: TInt32; Value: TDBXDate): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetDate(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetDouble(Ordinal: TInt32; Value: Double): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetDouble(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetInt16(Ordinal: TInt32; Value: SmallInt): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetShort(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetInt32(Ordinal: TInt32; Value: Integer): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetInteger(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetInt64(Ordinal: TInt32;
  Value: Int64): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetInt64(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetInt8(Ordinal: TInt32;
  Value: ShortInt): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetInt8(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetNull(Ordinal: TInt32): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].IsNull := True;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetParameterType(Ordinal: TInt32;
  const Name: TDBXWideString; ChildPosition: TInt32;
  ParamDirection: TDBXParameterDirection; DBXType, DBXSubType: TInt32;
  Size, Precision: Int64; Scale: TInt32): TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetSingle(Ordinal: TInt32;
  Value: Single): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetInteger(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetTime(Ordinal: TInt32; Value: TDBXTime): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetTime(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetTimeStamp(Ordinal: TInt32;
  var Value: TSQLTimeStamp): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetTimeStamp(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird.SetWideString(Ordinal: TInt32;
  const Value: TDBXWideString; Length: Int64): TDBXErrorCode;
begin
  FDSQL.i_SQLDA[Ordinal].SetWideString(Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

end.
