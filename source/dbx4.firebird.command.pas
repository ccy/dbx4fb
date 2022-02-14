unit dbx4.firebird.command;

interface

uses
  System.Classes, Data.DBXCommon, Data.DBXDynalink, Data.DBXPlatform,
  dbx4.base, dbx4.firebird.base, dbx4.firebird.connection, dbx4.firebird.reader,
  firebird.client, firebird.dsql, firebird.ibase.h;

type
  TMetaDataProvider_Firebird = class(TInterfacedObject, IMetaDataProvider)
  private
    FSQLDA: TXSQLDA;
    procedure Unsupported;
  protected
    function GetColumnCount: TInt32;
    function GetColumnLength(const aColNo: TInt32): LongWord; virtual;
    function GetColumnName(const aColNo: TInt32): WideString;
    function GetColumnNameLength(const aColNo: TInt32): TInt32;
    function GetColumnPrecision(const aColNo: TInt32): TInt32;
    function GetColumnScale(const aColNo: TInt32): TInt32;
    function GetColumnType(const aColNo: TInt32): TInt32; virtual;
    function GetColumnSubType(const aColNo: TInt32): TInt32;
    function GetIsNullable(const aColNo: TInt32): boolean;
  public
    constructor Create(const aSQLDA: TXSQLDA);
  end;

  TMetaDataProvider_Firebird_D2007 = class(TMetaDataProvider_Firebird)
  protected
    function GetColumnLength(const aColNo: TInt32): LongWord; override;
    function GetColumnType(const aColNo: TInt32): TInt32; override;
  end;

  TDBXCommand_Firebird = class(TDBXBase_Firebird, IDBXCommand)
  private
    FCommandType: WideString;
    FConnection: IDBXConnection;
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
    FServerCharSet: WideString;
    FSQLDialect: integer;
    FTransactionPool: TFirebirdTransactionPool;
    FTrimChar: Boolean;
    FParameterRows: TList;
    function GetParameterRows: TList;
    function NewMetaDataProvider(const aSQLDA: TXSQLDA): IMetaDataProvider;
  protected
    function Close: TDBXErrorCode; override;
    function CreateParameterRow(out aRow: TDBXRowHandle): TDBXErrorCode;
    function Execute(out Reader: IDBXReader): TDBXErrorCode;
    function GetFirebirdLibrary: IFirebirdLibrary; override;
    function GetRowsAffected(out Rows: Int64): TDBXErrorCode;
    function Prepare(const SQL: TDBXWideString; Count: TInt32): TDBXErrorCode;
  public
    constructor Create(const aConnection: IDBXConnection; const aCommandType:
        TDBXWideString);
    procedure BeforeDestruction; override;
  end;

implementation

uses
  System.StrUtils, System.SysUtils, System.WideStrings, Data.FMTBcd,
  Data.SqlTimSt,
  dbx4.firebird.metadata, dbx4.firebird.row, firebird.blr.h, firebird.charsets,
  firebird.consts_pub.h, firebird.iberror.h, firebird.ods.h, firebird.sqlda_pub.h, firebird.dsc.h;

constructor TMetaDataProvider_Firebird.Create(const aSQLDA: TXSQLDA);
begin
  inherited Create;
  FSQLDA := aSQLDA;
end;

function TMetaDataProvider_Firebird.GetColumnCount: TInt32;
begin
  Result := FSQLDA.Count;
end;

function TMetaDataProvider_Firebird.GetColumnLength(const aColNo: TInt32):
    LongWord;
var V: TXSQLVAR;
begin
  V := FSQLDA.Vars[aColNo];
  if V.CheckType(SQL_INT64) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_FLOAT) then
    Result := SizeOf(Double)
  else if V.CheckType(SQL_TIMESTAMP) then
    Result := SizeOf(TSQLTimeStamp)
  else if V.CheckType(SQL_LONG) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_SHORT) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_BOOLEAN) then
    Result := SizeOf(WordBool)
  else if V.CheckType(SQL_INT128) then
    Result := SizeOf(TBcd)
  else
    Result := V.Size;
end;

function TMetaDataProvider_Firebird.GetColumnName(const aColNo: TInt32):
    WideString;
begin
  Result := Copy(FSQLDA.Vars[aColNo].aliasname, 1, FSQLDA.Vars[aColNo].aliasname_length);
end;

function TMetaDataProvider_Firebird.GetColumnNameLength(const aColNo: TInt32):
    TInt32;
begin
  Result := FSQLDA.Vars[aColNo].aliasname_length;
end;

function TMetaDataProvider_Firebird.GetColumnPrecision(const aColNo: TInt32):
    TInt32;
var V: TXSQLVAR;
begin
  V := FSQLDA.Vars[aColNo];
  if V.CheckType(SQL_TEXT) then
    Result := V.GetTextLen
  else if V.CheckType(SQL_VARYING) then
    Result := V.GetTextLen
  else if V.CheckType(SQL_INT64) then //{$if CompilerVersion > 18.5} and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) {$ifend} then
    Result := 18
  else if V.CheckType(SQL_LONG) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := 9
  else if V.CheckType(SQL_SHORT) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := 4
  else if V.CheckType(SQL_INT128) then
    Result := 38
  else
    Result := v.sqllen;
end;

function TMetaDataProvider_Firebird.GetColumnScale(const aColNo: TInt32):
    TInt32;
begin
  Result := FSQLDA.Vars[aColNo].sqlscale;
end;

function TMetaDataProvider_Firebird.GetColumnSubType(const aColNo: TInt32):
    TInt32;
var iType: SmallInt;
begin
  Result := FSQLDA.Vars[aColNo].sqlsubtype;

  iType := FSQLDA.Vars[aColNo].sqltype and $7FFE;
  if iType = SQL_BLOB then begin
    if Result = isc_blob_text then begin
      if FSQLDA.Vars[aColNo].sqlscale = CS_UTF8 then
        // http://tracker.firebirdsql.org/browse/CORE-977 (Put blob charset in XSQLVAR::sqlscale)
        Result := {$ifdef Unicode}TDBXSubDataTypes{$else}TDBXDataTypes{$endif}.WideMemoSubType
      else
        Result := {$ifdef Unicode}TDBXSubDataTypes{$else}TDBXDataTypes{$endif}.MemoSubType;
    end else
      Result := 0;
  end;
end;

function TMetaDataProvider_Firebird.GetColumnType(const aColNo: TInt32): TInt32;
var iType, iSubType, iScale: Smallint;
begin
  Result := TDBXDataTypes.UnknownType;
  iType := FSQLDA.Vars[aColNo].sqltype and $7FFE;
  iSubType := FSQLDA.Vars[aColNo].sqlsubtype;
  iScale := FSQLDA.Vars[aColNo].sqlscale;
  case iType of
    SQL_SHORT: begin
      if iSubType = dsc_num_type_none then
        Result := TDBXDataTypes.Int16Type
      else
        Result := TDBXDataTypes.BcdType
    end;
    SQL_TEXT,
    SQL_VARYING: begin
      if FSQLDA.Vars[aColNo].CheckCharSet(CS_UTF8) or FSQLDA.Vars[aColNo].CheckCharSet(CS_UNICODE_FSS) then
        Result := TDBXDataTypes.WideStringType
      else
        Result := TDBXDataTypes.AnsiStringType;
    end;
    SQL_LONG: begin
      if iSubType = dsc_num_type_none then
        Result := TDBXDataTypes.Int32Type
      else
        Result := TDBXDataTypes.BcdType
    end;
    SQL_BLOB: Result := TDBXDataTypes.BlobType;
    SQL_BOOLEAN: Result := TDBXDataTypes.BooleanType;
    SQL_INT64: begin
      if (iSubType = dsc_num_type_none) and (iScale = 0) then
        Result := TDBXDataTypes.Int64Type
      else
        Result := TDBXDataTypes.BcdType;
    end;
    SQL_FLOAT: Result := TDBXDataTypes.DoubleType;
    SQL_DOUBLE: Result := TDBXDataTypes.DoubleType;
    SQL_TYPE_DATE: Result := TDBXDataTypes.DateType;
    SQL_TYPE_TIME: Result := TDBXDataTypes.TimeType;
    SQL_TIMESTAMP: Result := TDBXDataTypes.TimeStampType;
    SQL_INT128: Result := TDBXDataTypes.BcdType;
    else
      Unsupported;
  end;
end;

function TMetaDataProvider_Firebird.GetIsNullable(const aColNo: TInt32):
    boolean;
begin
  Result := FSQLDA.Vars[aColNo].IsNullable;
end;

procedure TMetaDataProvider_Firebird.Unsupported;
begin
  Assert(False, 'Unsupported');
end;

function TMetaDataProvider_Firebird_D2007.GetColumnLength(const aColNo:
    TInt32): LongWord;
var V: TXSQLVAR;
begin
  V := FSQLDA.Vars[aColNo];
  if V.CheckType(SQL_INT64) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_FLOAT) then
    Result := SizeOf(Double)
  else if V.CheckType(SQL_TIMESTAMP) then
    Result := SizeOf(TSQLTimeStamp)
  else if V.CheckType(SQL_LONG) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_SHORT) and (V.sqlsubtype <> dsc_num_type_none) then
    Result := SizeOf(TBcd)
  else
    Result := V.Size;
end;

function TMetaDataProvider_Firebird_D2007.GetColumnType(const aColNo: TInt32):
    TInt32;
var iType, iSubType, iScale: Smallint;
begin
  Result := TDBXDataTypes.UnknownType;
  iType := FSQLDA.Vars[aColNo].sqltype and $7FFE;
  iSubType := FSQLDA.Vars[aColNo].sqlsubtype;
  iScale := FSQLDA.Vars[aColNo].sqlscale;
  case iType of
    SQL_SHORT: begin
      if iSubType = dsc_num_type_none then
        Result := TDBXDataTypes.Int16Type
      else
        Result := TDBXDataTypes.BcdType;
    end;
    SQL_TEXT,
    SQL_VARYING: begin
      if FSQLDA.Vars[aColNo].CheckCharSet(CS_UTF8) or FSQLDA.Vars[aColNo].CheckCharSet(CS_UNICODE_FSS) then
        Result := TDBXDataTypes.WideStringType
      else
        Result := TDBXDataTypes.AnsiStringType;
    end;
    SQL_LONG: begin
      if iSubType = dsc_num_type_none then
        Result := TDBXDataTypes.Int32Type
      else
        Result := TDBXDataTypes.BcdType;
    end;
    SQL_BLOB: Result := TDBXDataTypes.BlobType;
    SQL_INT64: begin
      if (iSubType = dsc_num_type_none) and (iScale = 0) then
        Result := TDBXDataTypes.BcdType
      else
        Result := TDBXDataTypes.BcdType;
    end;
    SQL_FLOAT: Result := TDBXDataTypes.DoubleType;
    SQL_DOUBLE: Result := TDBXDataTypes.DoubleType;
    SQL_TYPE_DATE: Result := TDBXDataTypes.DateType;
    SQL_TYPE_TIME: Result := TDBXDataTypes.TimeType;
    SQL_TIMESTAMP: Result := TDBXDataTypes.TimeStampType;
    else
      Unsupported;
  end;
end;

constructor TDBXCommand_Firebird.Create(const aConnection: IDBXConnection;
    const aCommandType: TDBXWideString);
begin
  inherited Create;
  FConnection := aConnection;
  FCommandType := aCommandType;

  with (FConnection as IDBXConnection_Firebird) do begin
    FSQLDialect := SQLDialect;
    FTransactionPool := TransactionPool;
    FDBHandle := DBHandle;
    FTrimChar := TrimChar;
    FServerCharSet := ServerCharSet;
  end;
end;

procedure TDBXCommand_Firebird.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FParameterRows);
end;

function TDBXCommand_Firebird.Close: TDBXErrorCode;
var i: integer;
    e: TFBIntType;
    o: TDBXRowHandle;
begin
  if Assigned(FDSQL) then begin
    FDSQL.Close(StatusVector);
    if StatusVector.CheckFirebirdError(e) then begin
      if e <> isc_network_error then
        if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then
          Exit;
    end;
  end;
  for i := 0 to GetParameterRows.Count - 1 do begin
    o := GetParameterRows[i];
    IDBXBase(o).Close;
    IDBXBase(o) := nil;
  end;

  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.CreateParameterRow(out aRow: TDBXRowHandle):
    TDBXErrorCode;
begin
  var M: IMetaDataProvider := nil;
  if Assigned(FDSQL) then
    M := NewMetaDataProvider(FDSQL.o_SQLDA);
  var o: IDBXBase := TDBXRow_Firebird.Create(FConnection, FDBHandle, M, FDSQL, (FConnection as IDBXConnection_Firebird).TrimChar);

  IDBXBase(aRow) := o;
  GetParameterRows.Add(aRow);

  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.Execute(out Reader: IDBXReader): TDBXErrorCode;
var M: IMetaDataProvider;
begin
  Assert(Assigned(FDSQL));

  FDSQL.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  M := NewMetaDataProvider(FDSQL.o_SQLDA);
  Reader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, FTrimChar);
  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := (FConnection as IDBXBase_Firebird).FirebirdLibrary;
end;

function TDBXCommand_Firebird.GetParameterRows: TList;
begin
  if FParameterRows = nil then
    FParameterRows := TList.Create;
  Result := FParameterRows;
end;

function TDBXCommand_Firebird.GetRowsAffected(
  out Rows: Int64): TDBXErrorCode;
var R: Cardinal;
begin
  if Assigned(FDSQL) then begin
    FDSQL.GetRowsAffected(StatusVector, R);
    Rows := R;
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
  end else begin
    Rows := 0;
    Result := TDBXErrorCodes.None;
  end;
end;

function TDBXCommand_Firebird.NewMetaDataProvider(
  const aSQLDA: TXSQLDA): IMetaDataProvider;
begin
  if FConnection.GetIsDelphi2007Connection then
    Result := TMetaDataProvider_Firebird_D2007.Create(aSQLDA)
  else
    Result := TMetaDataProvider_Firebird.Create(aSQLDA);
end;

function TDBXCommand_Firebird.Prepare(const SQL: TDBXWideString; Count:
    TInt32): TDBXErrorCode;
var S, P, Q: string;
    L: IFirebird_DSQL;
    i, iInputParamCount: integer;
begin
  Assert(FDSQL = nil);

  FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool, FServerCharSet, FCommandType = TDBXCommandTypes.DbxStoredProcedure);

  FDSQL.Open(StatusVector, FDBHandle, FTransactionPool.CurrentTransaction);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  S := SQL;

  if FCommandType = TDBXCommandTypes.DbxStoredProcedure then begin
    // Calculate input parameters count
    Q := 'SELECT COUNT(*) ' +
           'FROM RDB$PROCEDURE_PARAMETERS ' +
   Format('WHERE (RDB$PROCEDURE_NAME = ''%s'') AND (RDB$PARAMETER_TYPE = 0)', [AnsiDequotedStr(SQL, '"')]);

    L := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    L.Open(StatusVector, FDBHandle, nil);
    L.Prepare(StatusVector, Q, FSQLDialect);
    L.Execute(StatusVector);
    StatusVector.CheckAndRaiseError(GetFirebirdLibrary);

    L.Fetch(StatusVector);
    StatusVector.CheckAndRaiseError(GetFirebirdLibrary);

    iInputParamCount := 0;
    Move(L.o_SQLDA.Vars[0].sqldata^, iInputParamCount, L.o_SQLDA.Vars[0].sqllen);

    L.Close(StatusVector);
    StatusVector.CheckAndRaiseError(GetFirebirdLibrary);

    // Render Input Parameters
    P := '';
    if iInputParamCount > 0 then begin
      P := ' (?';
      for i := 2 to iInputParamCount do
        P := P + ',?';
      P := P + ')';
    end;

    S := 'EXECUTE PROCEDURE ' + S + P;
  end;

  FDSQL.Prepare(StatusVector, S, FSQLDialect, Count);

  var M := NewMetaDataProvider(FDSQL.o_SQLDA);

  for var o in GetParameterRows do
    (IDBXBase(o) as IDBXRow).SetDSQL(FDSQL, M);

  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
end;

end.
