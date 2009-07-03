unit dbx4.firebird.reader;

interface

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform,
     dbx4.base, dbx4.firebird.base,
     firebird.dsql, firebird.client, firebird.ibase.h;

type
  TDBXReader_Firebird = class abstract(TDBXBase_Firebird)
  private
    FConnection: IDBXConnection;
    FMetaData: IMetaDataProvider;
  protected  // IDBXBase_Firebird
    function GetFirebirdLibrary: IFirebirdLibrary; override;
  protected  // IDBXReader2
    function ColumnCount: TInt32;
    function GetColumnMetadata(Ordinal: TInt32; Name: TDBXWideStringBuilder; out
        ColumnType, ColumnSubType, Length, precision, scale, flags: TInt32):
        TDBXErrorCode;
  public
    constructor Create(const aConnection: IDBXConnection; const aMetaData:
        IMetaDataProvider);
  end;

  TDBXReader_Firebird_GetDatabase = class(TDBXReader_Firebird, IDBXReader)
  private
    FRow: IDBXRow;
  protected
    function Close: TDBXErrorCode; override;
    function Next: TDBXErrorCode;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    procedure AfterConstruction; override;
  end;

  TDBXRow_Firebird_GetDatabase = class(TDBXBase_Firebird, IDBXRow)
  protected
    function GetAnsiString(Ordinal: TInt32; Value: TDBXAnsiStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
    function GetBoolean(Ordinal: TInt32; out Value, IsNull: LongBool):
        TDBXErrorCode;
    function GetByteLength(Ordinal: TInt32; out Length: Int64; out IsNull:
        LongBool): TDBXErrorCode;
    function GetBytes(Ordinal: TInt32; Offset: Int64; Value: TBytes; const
        LastIndex: TInt32; ValueOffset, Length: Int64; out ReturnLength: Int64; out
        IsNull: LongBool): TDBXErrorCode;
    function GetBcd(Ordinal: TInt32; out Value: TBcd; out IsNull: LongBool):
        TDBXErrorCode;
    function GetDate(Ordinal: TInt32; out Value: TDBXDate; out IsNull: LongBool):
        TDBXErrorCode;
    function GetDouble(Ordinal: TInt32; out Value: Double; out IsNull: LongBool):
        TDBXErrorCode;
    function GetFixedBytes(Ordinal: TInt32; Value: TBytes; const LastIndex: TInt32;
        ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode;
    function GetInt16(Ordinal: TInt32; out Value: SmallInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetInt32(Ordinal: TInt32; out Value: LongInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetInt64(Ordinal: TInt32; out Value: Int64; out IsNull: LongBool):
        TDBXErrorCode;
    function GetTime(Ordinal: TInt32; out Value: TDBXTime; out IsNull: LongBool):
        TDBXErrorCode;
    function GetTimeStamp(Ordinal: TInt32; out Value: TSQLTimeStamp; out IsNull:
        LongBool): TDBXErrorCode;
    function GetWideString(Ordinal: TInt32; Value: TDBXWideStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
  end;

  TDBXReader_Firebird_DSQL = class(TDBXReader_Firebird, IDBXReader)
  private
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
    FRow: IDBXRow;
    FTrimChar: boolean;
  protected
    function Close: TDBXErrorCode; override;
    function Next: TDBXErrorCode;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(const aConnection: IDBXConnection; const aDBHandle:
        pisc_db_handle; const aMetaData: IMetaDataProvider; const aDSQL:
        IFirebird_DSQL; const aTrimChar: boolean);
  end;

implementation

uses Windows, dbx4.firebird.row;

constructor TDBXReader_Firebird.Create(const aConnection: IDBXConnection;
    const aMetaData: IMetaDataProvider);
begin
  inherited Create;
  FConnection := aConnection;
  FMetaData := aMetaData;
end;

function TDBXReader_Firebird.ColumnCount: TInt32;
begin
  Result := FMetaData.GetColumnCount;
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
  if FMetaData.GetIsNullable(Ordinal) then
    flags := TDBXValueTypeFlags.Nullable
  else
    flags := 0;

  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := (FConnection as IDBXBase_Firebird).FirebirdLibrary;
end;

procedure TDBXReader_Firebird_GetDatabase.AfterConstruction;
begin
  inherited;
  FRow := TDBXRow_Firebird_GetDatabase.Create;
end;

function TDBXReader_Firebird_GetDatabase.Close: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird_GetDatabase.Next: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXReader_Firebird_GetDatabase.QueryInterface(const IID: TGUID; out
    Obj): HResult;
begin
  if IsEqualGUID(IID, IDBXRow) then begin
    IDBXRow(Obj) := FRow;
    Result := S_OK;
  end else
    Result := inherited QueryInterface(IID, Obj);
end;

constructor TDBXReader_Firebird_DSQL.Create(const aConnection:
    IDBXConnection; const aDBHandle: pisc_db_handle; const aMetaData:
    IMetaDataProvider; const aDSQL: IFirebird_DSQL; const aTrimChar: boolean);
begin
  inherited Create(aConnection, aMetaData);
  FConnection := aConnection;
  FDBHandle := aDBHandle;
  FDSQL := aDSQL;
  FTrimChar := aTrimChar;

  FRow := TDBXRow_Firebird.Create(FConnection, FDBHandle, FMetaData, FDSQL, FTrimChar);
end;

function TDBXReader_Firebird_DSQL.Close: TDBXErrorCode;
begin
  FDSQL.Close(StatusVector);
  StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError);
end;

function TDBXReader_Firebird_DSQL.Next: TDBXErrorCode;
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

function TDBXReader_Firebird_DSQL.QueryInterface(const IID: TGUID; out Obj):
    HResult;
begin
  if IsEqualGUID(IID, IDBXRow) then begin
    IDBXRow(Obj) := FRow;
    Result := S_OK;
  end else
    Result := inherited QueryInterface(IID, Obj);
end;

function TDBXRow_Firebird_GetDatabase.GetAnsiString(Ordinal: TInt32;
  Value: TDBXAnsiStringBuilder; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetBcd(Ordinal: TInt32;
  out Value: TBcd; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetBoolean(Ordinal: TInt32; out Value,
    IsNull: LongBool): TDBXErrorCode;
begin
  if (Ordinal = 3) or (Ordinal = 4) then
    Value := True
  else
    Value := False;
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird_GetDatabase.GetByteLength(Ordinal: TInt32;
  out Length: Int64; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetBytes(Ordinal: TInt32;
  Offset: Int64; Value: TBytes; const LastIndex: TInt32; ValueOffset,
  Length: Int64; out ReturnLength: Int64;
  out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetDate(Ordinal: TInt32;
  out Value: TDBXDate; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetDouble(Ordinal: TInt32;
  out Value: Double; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetFixedBytes(Ordinal: TInt32;
  Value: TBytes; const LastIndex: TInt32; ValueOffset: TInt32;
  out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetInt16(Ordinal: TInt32; out Value:
    SmallInt; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetInt32(Ordinal: TInt32; out Value:
    LongInt; out IsNull: LongBool): TDBXErrorCode;
begin
  Value := 0;
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

function TDBXRow_Firebird_GetDatabase.GetInt64(Ordinal: TInt32;
  out Value: Int64; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetTime(Ordinal: TInt32;
  out Value: TDBXTime; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetTimeStamp(Ordinal: TInt32;
  out Value: TSQLTimeStamp; out IsNull: LongBool): TDBXErrorCode;
begin
  Result := NotSupported;
end;

function TDBXRow_Firebird_GetDatabase.GetWideString(Ordinal: TInt32; Value:
    TDBXWideStringBuilder; out IsNull: LongBool): TDBXErrorCode;
var W: WideString;
begin
  W := '"';
  lstrcpyW(Value, PWideChar(W));
  IsNull := False;
  Result := TDBXErrorCodes.None;
end;

end.
