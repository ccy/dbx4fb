unit dbx4.firebird.reader;

interface

uses
  System.SysUtils, Data.DBXCommon, Data.DBXPlatform,
  dbx4.base, dbx4.firebird.base, firebird.client, firebird.dsql, firebird.ibase.h;

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

uses
  Winapi.Windows,
  dbx4.firebird.row;

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
  Result := FDSQL.Close(StatusVector);
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

end.
