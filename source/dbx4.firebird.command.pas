unit dbx4.firebird.command;

interface

uses DBXCommon, DBXPlatform, IB_Header, firebird.client, firebird.dsql, dbx4.base,
  dbx4.firebird.connection, dbx4.firebird.reader, dbx4.firebird.base;

type
  TMetaDataProvider_Firebird = class(TInterfacedObject, IMetaDataProvider)
  private
    FSQLDA: TXSQLDA;
  protected
    function GetColumnCount: TInt32;
    function GetColumnLength(const aColNo: TInt32): LongWord;
    function GetColumnName(const aColNo: TInt32): WideString;
    function GetColumnNameLength(const aColNo: TInt32): TInt32;
    function GetColumnPrecision(const aColNo: TInt32): TInt32;
    function GetColumnScale(const aColNo: TInt32): TInt32;
    function GetColumnType(const aColNo: TInt32): TInt32;
    function GetColumnSubType(const aColNo: TInt32): TInt32;
    function GetIsNullable(const aColNo: TInt32): boolean;
  public
    constructor Create(const aSQLDA: TXSQLDA);
  end;

  TDBXCommand_Firebird = class(TDBXBase_Firebird, IDBXCommand)
  private
    FCommandType: WideString;
    FConnection: IDBXConnection;
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
    FSQLDialect: integer;
    FTransactionPool: TFirebirdTransactionPool;
    FTrimChar: Boolean;
  protected
    function Close: TDBXErrorCode; override;
    function CreateParameterRow(out aRow: IDBXWritableRow): TDBXErrorCode;
    function Execute(out Reader: IDBXReader): TDBXErrorCode;
    function ExecuteImmediate(const SQL: TDBXWideString; out aReader: IDBXReader):
        TDBXErrorCode;
    function GetFirebirdLibrary: IFirebirdLibrary; override;
    function GetRowsAffected(out Rows: Int64): TDBXErrorCode;
    function Prepare(const SQL: TDBXWideString; Count: TInt32): TDBXErrorCode;
  public
    constructor Create(const aConnection: IDBXConnection; const aCommandType:
        TDBXWideString);
  end;

implementation

uses StrUtils, FMTBcd, SqlTimSt, dbx4.firebird.row,
  dbx4.firebird.metadata, SysUtils, WideStrings;

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
  if V.CheckType(SQL_VARYING) then
    Result := V.sqllen + 1
  else if V.CheckType(SQL_INT64) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_FLOAT) then
    Result := SizeOf(Double)
  else if V.CheckType(SQL_TIMESTAMP) then
    Result := SizeOf(TSQLTimeStamp)
  else if V.CheckType(SQL_LONG) and (FSQLDA.Vars[aColNo].sqlscale <> 0) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_SHORT) and (FSQLDA.Vars[aColNo].sqlscale <> 0) then
    Result := SizeOf(TBcd)
  else
    Result := V.Size;
end;

function TMetaDataProvider_Firebird.GetColumnName(const aColNo: TInt32):
    WideString;
var S: string;
    P: TIB_Identifier;
begin
  P := FSQLDA.Vars[aColNo].aliasname;
  SetString(S, P, FSQLDA.Vars[aColNo].aliasname_length);
  Result := S;
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
  if V.CheckType(SQL_INT64) then
    Result := 19
  else if V.CheckType(SQL_LONG) and (V.sqlscale <> 0) then
    Result := 9
  else if V.CheckType(SQL_SHORT) and (V.sqlscale <> 0) then
    Result := 4
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
begin
  Result := FSQLDA.Vars[aColNo].sqlsubtype;
end;

function TMetaDataProvider_Firebird.GetColumnType(const aColNo: TInt32): TInt32;
var iType: Smallint;
    iScale: Smallint;
begin
  iType := FSQLDA.Vars[aColNo].sqltype and not 1;
  iScale := FSQLDA.Vars[aColNo].sqlscale;
  case iType of
    SQL_SHORT: begin
      if iScale = 0 then
        Result := TDBXDataTypes.Int16Type
      else
        Result := TDBXDataTypes.BcdType;
    end;
    SQL_TEXT: Result := TDBXDataTypes.AnsiStringType;
    SQL_VARYING: Result := TDBXDataTypes.AnsiStringType;
    SQL_LONG: begin
      if iScale = 0 then
        Result := TDBXDataTypes.Int32Type
      else
        Result := TDBXDataTypes.BcdType
    end;
    SQL_BLOB: Result := TDBXDataTypes.BlobType;
    SQL_INT64: Result := TDBXDataTypes.BcdType;
    SQL_FLOAT: Result := TDBXDataTypes.DoubleType;
    SQL_DOUBLE: Result := TDBXDataTypes.DoubleType;
    SQL_TYPE_DATE: Result := TDBXDataTypes.DateType;
    SQL_TYPE_TIME: Result := TDBXDataTypes.TimeType;
    SQL_TIMESTAMP: Result := TDBXDataTypes.TimeStampType;
    else
      Assert(False, 'Unsupported data type');
  end;
end;

function TMetaDataProvider_Firebird.GetIsNullable(const aColNo: TInt32):
    boolean;
begin
  Result := FSQLDA.Vars[aColNo].IsNullable;
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
  end;
end;

function TDBXCommand_Firebird.Close: TDBXErrorCode;
begin
  if Assigned(FDSQL) then begin
    FDSQL.Close(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
  end;
  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.CreateParameterRow(out aRow: IDBXWritableRow):
    TDBXErrorCode;
begin
  aRow := TDBXWritableRow_Firebird.Create(FDBHandle, FDSQL.Transaction, FDSQL.i_SQLDA);
  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.Execute(out Reader: IDBXReader): TDBXErrorCode;
var M: IMetaDataProvider;
begin
  Assert(Assigned(FDSQL));

  FDSQL.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
  Reader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, FTrimChar);
  Result := TDBXErrorCodes.None;
end;

function TDBXCommand_Firebird.ExecuteImmediate(const SQL: TDBXWideString; out
    aReader: IDBXReader): TDBXErrorCode;
var M: IMetaDataProvider;
    S: string;
    sFlag: string;
    W: WideString;
    WL: TWideStringList;
    sTableName: WideString;
begin
  {$Message 'This method too long, find a way to revise it'}

  Assert(FDSQL = nil);
  Assert(FCommandType = TDBXCommandTypes.DbxMetaData);

  if Pos(TDBXMetaDataCommands.GetDatabase, SQL) = 1 then begin
    M := TMetaDataProvider_FieldColumns.Create(TMetaData_Firebird_Factory.New_GetDatabase);
    aReader := TDBXReader_Firebird_GetDatabase.Create(M);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDataCommands.GetColumns, SQL) = 1 then begin
    // GetColumns "c:\T_E304K4GAOBPLUFGFC3CCV1YTCA"."SYSDBA"."RDB$RELATIONS".%
    WL := TWideStringList.Create;
    try
      W := SQL;
      Delete(W, 1, Pos(' ', W));
      WL.Delimiter := '.';
      WL.DelimitedText := W;
      sTableName := WL[2];
    finally
      WL.Free;
    end;

    S := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$FIELD_NAME, A.RDB$FIELD_POSITION, 0, B.RDB$FIELD_TYPE, '''', ' +
                'B.RDB$FIELD_SUB_TYPE, B.RDB$FIELD_LENGTH, 0, B.RDB$FIELD_SCALE, A.RDB$NULL_FLAG ' +
           'FROM RDB$RELATION_FIELDS A, RDB$FIELDS B ' +
   Format('WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$RELATION_NAME = ''%s'') ', [sTableName]) +
       'ORDER BY A.RDB$FIELD_POSITION';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_FieldColumns.Create(TMetaData_Firebird_Factory.New_getColumns(sTableName));

    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDataCommands.GetTables, SQL) = 1 then begin
    // GetTables "G:\Win.XP\ccy\LOCALS~1\Temp\T_OFWSA5ZZ354AUHCO55K2GC5IYA"."SYSDBA".% SystemTable
    W := SQL;
    Delete(W, 1, Pos('%', W));
    W := Trim(W);
    if (Pos(TDBXMetadataTableTypes.SystemTable, W) > 0) and
       (Pos(TDBXMetadataTableTypes.Table, W) > 0) then
      sFlag := ' '
    else if Pos(TDBXMetadataTableTypes.SystemTable, W) > 0 then
      sFlag := 'AND (A.RDB$SYSTEM_FLAG = 1) '
    else if Pos(TDBXMetadataTableTypes.Table, W) > 0 then
      sFlag := 'AND (A.RDB$SYSTEM_FLAG = 0) '
    else
      sFlag := ' ';

    S := 'SELECT 0, '''', A.RDB$OWNER_NAME, A.RDB$RELATION_NAME, A.RDB$SYSTEM_FLAG ' +
           'FROM RDB$RELATIONS A ' +
          'WHERE (A.RDB$VIEW_SOURCE IS NULL) ' +
            sFlag +
            'AND A.RDB$OWNER_NAME = ''SYSDBA'' '+
       'ORDER BY A.RDB$RELATION_NAME';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_FieldColumns.Create(TMetaData_Firebird_Factory.New_getTables);

    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDataCommands.GetIndexes, SQL) = 1 then begin
    // GetIndexes "G:\Win.XP\ccy\LOCALS~1\Temp\T_OZFAI1PPYS4NE20MVD2GAV4UZB"."SYSDBA"."RDB$RELATIONS"
    WL := TWideStringList.Create;
    try
      W := SQL;
      Delete(W, 1, Pos(' ', W));
      WL.Delimiter := '.';
      WL.DelimitedText := W;
      sTableName := WL[2];
    finally
      WL.Free;
    end;

    S := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$INDEX_NAME, B.RDB$FIELD_NAME, ' +
         '       B.RDB$FIELD_POSITION, '''', 0, A.RDB$INDEX_TYPE, '''', A.RDB$UNIQUE_FLAG, C.RDB$CONSTRAINT_NAME, C.RDB$CONSTRAINT_TYPE ' +
           'FROM RDB$INDICES A, RDB$INDEX_SEGMENTS B FULL OUTER JOIN RDB$RELATION_CONSTRAINTS C ' +
             'ON A.RDB$RELATION_NAME = C.RDB$RELATION_NAME AND C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
          'WHERE (A.RDB$SYSTEM_FLAG <> 1 OR A.RDB$SYSTEM_FLAG IS NULL) ' +
            'AND (A.RDB$INDEX_NAME = B.RDB$INDEX_NAME) ' +
     Format('AND (A.RDB$RELATION_NAME = UPPER(''%s'')) ', [sTableName]) +
       'ORDER BY A.RDB$INDEX_NAME';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_FieldColumns.Create(TMetaData_Firebird_Factory.New_getIndices(sTableName));

    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else
    Assert(False);
end;

function TDBXCommand_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := (FConnection as IDBXBase_Firebird).FirebirdLibrary;
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

function TDBXCommand_Firebird.Prepare(const SQL: TDBXWideString; Count:
    TInt32): TDBXErrorCode;
begin
  Assert(FDSQL = nil);

  FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);

  FDSQL.Open(StatusVector, FDBHandle, FTransactionPool.CurrentTransaction);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

  FDSQL.Prepare(StatusVector, SQL, FSQLDialect, Count);
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
end;

end.
