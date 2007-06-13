unit dbx4.firebird.reader;

interface

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform,
     IB_Header, dbx4.base, dbx4.firebird.base, firebird.dsql, firebird.client;

type
  TDBXReader_Firebird = class(TDBXBase, IDBXReader)
  private
    FMetaData: IMetaDataProvider;
  protected
    function Close: TDBXErrorCode; override;
    function ColumnCount: TInt32;
    function GetBoolean(Ordinal: TInt32; out Value, IsNull: LongBool):
        TDBXErrorCode;
    function GetBytes(Ordinal: TInt32; Offset: Int64; Value: TBytes; const
        LastIndex: TInt32; ValueOffset, Length: Int64; out ReturnLength: Int64; out
        IsNull: LongBool): TDBXErrorCode;
    function GetByteLength(Ordinal: TInt32; out Length: Int64; out IsNull:
        LongBool): TDBXErrorCode;
    function GetColumnMetadata(Ordinal: TInt32; Name: TDBXWideStringBuilder; out
        ColumnType, ColumnSubType, Length, precision, scale, flags: TInt32):
        TDBXErrorCode;
    function GetFixedBytes(Ordinal: TInt32; Value: TBytes; const LastIndex: TInt32;
        ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode;
    function GetInt32(Ordinal: TInt32; out Value: LongInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetString(Ordinal: TInt32; Value: TDBXAnsiStringBuilder; out IsNull:
        LongBool): TDBXErrorCode;
    function GetWideString(Ordinal: TInt32; Value: TDBXWideStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
    function Next: TDBXErrorCode;
  public
    constructor Create(const aMetaData: IMetaDataProvider);
  end;

  TDBXReader_Firebird1 = class(TDBXBase_Firebird, IDBXReader)
  private
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
    FMetaData: IMetaDataProvider;
    FTrimChar: boolean;
  protected
    function Close: TDBXErrorCode; override;
    function ColumnCount: TInt32;
    function GetBcd(Ordinal: TInt32; out Value: PBcd; out IsNull: LongBool):
        TDBXErrorCode;
    function GetBoolean(Ordinal: TInt32; out Value, IsNull: LongBool):
        TDBXErrorCode;
    function GetBytes(Ordinal: TInt32; Offset: Int64; Value: TBytes; const
        LastIndex: TInt32; ValueOffset, Length: Int64; out ReturnLength: Int64; out
        IsNull: LongBool): TDBXErrorCode;
    function GetByteLength(Ordinal: TInt32; out Length: Int64; out IsNull:
        LongBool): TDBXErrorCode;
    function GetColumnMetadata(Ordinal: TInt32; Name: TDBXWideStringBuilder; out
        ColumnType, ColumnSubType, Length, precision, scale, flags: TInt32):
        TDBXErrorCode;
    function GetDate(Ordinal: TInt32; out Value: PDate; out IsNull: LongBool):
        TDBXErrorCode;
    function GetDouble(Ordinal: TInt32; out Value: PDouble; out IsNull: LongBool):
        TDBXErrorCode;
    function GetFixedBytes(Ordinal: TInt32; Value: TBytes; const LastIndex: TInt32;
        ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode;
    function GetInt16(Ordinal: TInt32; out Value: PShortInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetInt32(Ordinal: TInt32; out Value: LongInt; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetInt32(Ordinal: TInt32; out Value: PLongint; out IsNull: LongBool):
        TDBXErrorCode; overload;
    function GetString(Ordinal: TInt32; Value: TDBXAnsiStringBuilder; out IsNull:
        LongBool): TDBXErrorCode;
    function GetTime(Ordinal: TInt32; Value: PInteger; out IsNull: LongBool):
        TDBXErrorCode;
    function GetTimeStamp(Ordinal: TInt32; out Value: PSQLTimeStamp; out IsNull:
        LongBool): TDBXErrorCode; overload;
    function GetWideString(Ordinal: TInt32; Value: TDBXWideStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
    function Next: TDBXErrorCode;
  public
    constructor Create(const aDBHandle: pisc_db_handle; const aMetaData:
        IMetaDataProvider; const aDSQL: IFirebird_DSQL; const aTrimChar: boolean);
  end;

implementation

uses Windows;

constructor TDBXReader_Firebird.Create(const aMetaData: IMetaDataProvider);
begin
  inherited Create;
  FMetaData := aMetaData;
end;

function TDBXReader_Firebird.Close: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.ColumnCount: TInt32;
begin
  Result := FMetaData.GetColumnCount;
end;

function TDBXReader_Firebird.GetBoolean(Ordinal: TInt32; out Value, IsNull:
    LongBool): TDBXErrorCode;
begin
  if (Ordinal = 3) or (Ordinal = 4) then
    Value := True
  else
    Value := False;
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.GetByteLength(Ordinal: TInt32;
  out Length: Int64; out IsNull: LongBool): TDBXErrorCode;
begin
  Assert(False);
end;

function TDBXReader_Firebird.GetBytes(Ordinal: TInt32; Offset: Int64;
  Value: TBytes; const LastIndex: TInt32; ValueOffset, Length: Int64;
  out ReturnLength: Int64; out IsNull: LongBool): TDBXErrorCode;
begin
  Assert(False);
end;

function TDBXReader_Firebird.GetColumnMetadata(Ordinal: TInt32; Name:
    TDBXWideStringBuilder; out ColumnType, ColumnSubType, Length, precision,
    scale, flags: TInt32): TDBXErrorCode;
begin
  lstrcpyW(Name, PWideChar(FMetaData.GetColumnName(Ordinal)));
  ColumnType := FMetaData.GetColumnType(Ordinal);
  ColumnSubType := FMetaData.GetColumnSubType(Ordinal);
  Length := FMetaData.GetColumnLength(Ordinal);
  precision := FMetaData.GetColumnPrecision(Ordinal);
  scale := FMetaData.GetColumnScale(Ordinal);
  flags := TDBXValueTypeFlags.ReadOnly + TDBXValueTypeFlags.Searchable;

  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.GetFixedBytes(Ordinal: TInt32; Value: TBytes;
  const LastIndex: TInt32; ValueOffset: TInt32;
  out IsNull: LongBool): TDBXErrorCode;
begin
  Assert(False);
end;

function TDBXReader_Firebird.GetInt32(Ordinal: TInt32; out Value: LongInt; out
    IsNull: LongBool): TDBXErrorCode;
begin
  Value := 0;
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.GetString(Ordinal: TInt32;
  Value: TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode;
begin
  Assert(False);
end;

function TDBXReader_Firebird.GetWideString(Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode;
var W: WideString;
begin
  W := '"';
  lstrcpyW(Value, PWideChar(W));
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.Next: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

constructor TDBXReader_Firebird1.Create(const aDBHandle: pisc_db_handle; const
    aMetaData: IMetaDataProvider; const aDSQL: IFirebird_DSQL; const aTrimChar:
    boolean);
begin
  inherited Create;
  FDBHandle := aDBHandle;
  FMetaData := aMetaData;
  FDSQL := aDSQL;
  FTrimChar := aTrimChar;
end;

function TDBXReader_Firebird1.Close: TDBXErrorCode;
begin
  FDSQL.Close(StatusVector);
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXReader_Firebird1.ColumnCount: TInt32;
begin
  Result := FMetaData.GetColumnCount;
end;

function TDBXReader_Firebird1.GetBcd(Ordinal: TInt32; out Value: PBcd; out
    IsNull: LongBool): TDBXErrorCode;
var B: boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetBCD(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetBoolean(Ordinal: TInt32; out Value, IsNull:
    LongBool): TDBXErrorCode;
begin
  Assert(False);
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetByteLength(Ordinal: TInt32;
  out Length: Int64; out IsNull: LongBool): TDBXErrorCode;
var i: Cardinal;
    B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetBlobSize(StatusVector, FDBHandle, FDSQL.Transaction, i, B);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  IsNull := B;
  Length := i;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetBytes(Ordinal: TInt32; Offset: Int64;
  Value: TBytes; const LastIndex: TInt32; ValueOffset, Length: Int64;
  out ReturnLength: Int64; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  Assert(Offset = 0);
  Assert(ValueOffSet = 0);
  FDSQL.o_SQLDA[Ordinal].GetBlob(StatusVector, FDBHandle, FDSQL.Transaction, Value, B, Length);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
  
  ReturnLength := Length;
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetColumnMetadata(Ordinal: TInt32; Name:
    TDBXWideStringBuilder; out ColumnType, ColumnSubType, Length, precision,
    scale, flags: TInt32): TDBXErrorCode;
begin
  lstrcpyW(Name, PWideChar(FMetaData.GetColumnName(Ordinal)));
  ColumnType := FMetaData.GetColumnType(Ordinal);
  ColumnSubType := FMetaData.GetColumnSubType(Ordinal);
  Length := FMetaData.GetColumnLength(Ordinal);
  precision := FMetaData.GetColumnPrecision(Ordinal);
  scale := FMetaData.GetColumnScale(Ordinal);
  flags := TDBXValueTypeFlags.Searchable;

  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetDate(Ordinal: TInt32; out Value: PDate; out
    IsNull: LongBool): TDBXErrorCode;
var B: boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetDate(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetDouble(Ordinal: TInt32; out Value: PDouble;
    out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetDouble(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetFixedBytes(Ordinal: TInt32; Value: TBytes;
  const LastIndex: TInt32; ValueOffset: TInt32;
  out IsNull: LongBool): TDBXErrorCode;
begin
  case FMetaData.GetColumnType(Ordinal) of
    TDBXDataTypes.AnsiStringType: Result := GetString(Ordinal, Pointer(Value), IsNull);
    TDBXDataTypes.WideStringType: Result := GetWideString(Ordinal, Pointer(Value), IsNull);
    TDBXDataTypes.BcdType:        Result := GetBcd(Ordinal, PBcd(Value), IsNull);
    TDBXDataTypes.DateType:       Result := GetDate(Ordinal, PDate(Value), IsNull);
    TDBXDataTypes.Int16Type:      Result := GetInt16(Ordinal, PShortInt(Value), IsNull);
    TDBXDataTypes.Int32Type:      Result := GetInt32(Ordinal, PLongint(Value), IsNull);
    TDBXDataTypes.TimeStampType:  Result := GetTimeStamp(Ordinal, PSQLTimeStamp(Value), IsNull);
    TDBXDataTypes.TimeType:       Result := GetTime(Ordinal, PInteger(Value), IsNull);
    TDBXDataTypes.DoubleType:     Result := GetDouble(Ordinal, PDouble(Value), IsNull);
    else
      Assert(False);
  end;
end;

function TDBXReader_Firebird1.GetInt16(Ordinal: TInt32;
  out Value: PShortInt; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetShort(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetInt32(Ordinal: TInt32;
  out Value: PLongint; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetInteger(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetInt32(Ordinal: TInt32; out Value: LongInt; out
    IsNull: LongBool): TDBXErrorCode;
begin
  Assert(False);
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetString(Ordinal: TInt32; Value:
    TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode;
var c: PChar;
    B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetString(Value, B);
  IsNull := B;
  if FTrimChar then begin
    c := Value + FDSQL.o_SQLDA[Ordinal].sqllen - 1;
    while c^ = ' ' do begin
      c^ := #0;
      c := c - 1;
    end;
  end;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetTime(Ordinal: TInt32; Value: PInteger; out
    IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetTime(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetTimeStamp(Ordinal: TInt32; out Value:
    PSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode;
var B: Boolean;
begin
  FDSQL.o_SQLDA[Ordinal].GetTimeStamp(Value, B);
  IsNull := B;
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.GetWideString(Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode;
//var B: boolean;
//    S: AnsiString;
//    W: WideString;
//    i, iLen: Cardinal;
begin
  Assert(False);
//  iLen := FMetaData.GetColumnLength(Ordinal);
//  SetLength(S, iLen);
//  FDSQL.o_SQLDA[Ordinal].GetString(PAnsiChar(S), B);
//  IsNull := B;
//  if FTrimChar then begin
//    i := iLen - 1;
//    while S[i] = ' ' do begin
//      S[i] := #0;
//      i := i - 1;
//    end;
//  end;
//  W := S;
//  lstrcpyW(Value, PWideChar(W));
//  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird1.Next: TDBXErrorCode;
var R: integer;
begin
  R := FDSQL.Fetch(StatusVector);
  if (R <> 0) and (R <> 100) then
    StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError)
  else if R = 100 then
    Result := TDBXErrorCodes.EOF
  else
   Result := TDBXErrorCodes.None;
end;

end.
