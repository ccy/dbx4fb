unit dbx4.firebird.command;

interface

uses DBXCommon, DBXPlatform, firebird.client, firebird.dsql, dbx4.base,
  dbx4.firebird.connection, dbx4.firebird.reader, dbx4.firebird.base,
  firebird.ibase.h;

type
  TMetaDataProvider_Firebird = class(TInterfacedObject, IMetaDataProvider)
  private
    FSQLDA: TXSQLDA;
    procedure Unsupported;
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
    function CreateParameterRow(out aRow: IDBXBase): TDBXErrorCode;
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

uses SysUtils, StrUtils, FMTBcd, SqlTimSt, WideStrings,
     firebird.charsets, dbx4.firebird.row, dbx4.firebird.metadata,
     firebird.sqlda_pub.h, firebird.blr.h;

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
  if V.CheckType(SQL_INT64) {$if CompilerVersion > 18.5} and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) {$ifend} then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_FLOAT) then
    Result := SizeOf(Double)
  else if V.CheckType(SQL_TIMESTAMP) then
    Result := SizeOf(TSQLTimeStamp)
  else if V.CheckType(SQL_LONG) and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) then
    Result := SizeOf(TBcd)
  else if V.CheckType(SQL_SHORT) and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) then
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
  if V.CheckType(SQL_TEXT) then begin
    Result := V.sqllen;
    if V.CheckCharSet(CS_UTF8) then
      Result := V.sqllen div 4
  end else if V.CheckType(SQL_VARYING) then begin
    Result := V.sqllen;
    if V.CheckCharSet(CS_UTF8) then
      Result := V.sqllen div 4;
  end else if V.CheckType(SQL_INT64) {$if CompilerVersion > 18.5} and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) {$ifend} then
    Result := 19
  else if V.CheckType(SQL_LONG) and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) then
    Result := 9
  else if V.CheckType(SQL_SHORT) and ((V.sqlsubtype = 1) or (V.sqlsubtype = 2)) then
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
var iType, iSubType: Smallint;
begin
  iType := FSQLDA.Vars[aColNo].sqltype and not 1;
  iSubType := FSQLDA.Vars[aColNo].sqlsubtype;
  case iType of
    SQL_SHORT: begin
      if iSubType = 0 then
        Result := TDBXDataTypes.Int16Type
      else if (iSubType = 1) or (iSubType = 2) then
        Result := TDBXDataTypes.BcdType
      else
        Unsupported;
    end;
    SQL_TEXT,
    SQL_VARYING: begin
      if FSQLDA.Vars[aColNo].CheckCharSet(CS_UTF8) then
        Result := TDBXDataTypes.WideStringType
      else
        Result := TDBXDataTypes.AnsiStringType;
    end;
    SQL_LONG: begin
      if iSubType = 0 then
        Result := TDBXDataTypes.Int32Type
      else if (iSubType = 1) or (iSubType = 2) then
        Result := TDBXDataTypes.BcdType
      else
        Unsupported;
    end;
    SQL_BLOB: Result := TDBXDataTypes.BlobType;
    SQL_INT64: begin
      if iSubType = 0 then
        Result := {$if CompilerVersion <= 18.5} TDBXDataTypes.BcdType {$else} TDBXDataTypes.Int64Type {$ifend}
      else if (iSubType = 1) or (iSubType = 2) then
        Result := TDBXDataTypes.BcdType
      else
        Unsupported;
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

function TMetaDataProvider_Firebird.GetIsNullable(const aColNo: TInt32):
    boolean;
begin
  Result := FSQLDA.Vars[aColNo].IsNullable;
end;

procedure TMetaDataProvider_Firebird.Unsupported;
begin
  Assert(False, 'Unsupported');
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

function TDBXCommand_Firebird.CreateParameterRow(out aRow: IDBXBase):
    TDBXErrorCode;
var M: IMetaDataProvider;
begin
  M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
  aRow := TDBXRow_Firebird.Create(FConnection, FDBHandle, M, FDSQL, (FConnection as IDBXConnection_Firebird).TrimChar);
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
    sRelation: WideString;
begin
  {$Message 'This method too long, find a way to revise it'}

  Assert(FDSQL = nil);
  Assert(FCommandType = TDBXCommandTypes.DbxMetaData);

  if Pos(TDBXMetaDataCommands.GetDatabase, SQL) = 1 then begin
    // GetDatabase
    M := TMetaDataProvider_FieldColumns.Create(TMetaData_Firebird_Factory.New_GetDatabase);
    aReader := TDBXReader_Firebird_GetDatabase.Create(FConnection, M);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDataCommands.GetColumns, SQL) = 1 then begin
    // GetColumns "c:\T_E304K4GAOBPLUFGFC3CCV1YTCA"."SYSDBA"."RDB$RELATIONS".%
    WL := TWideStringList.Create;
    try
      W := SQL;
      Delete(W, 1, Pos(' ', W));
      WL.Delimiter := '.';
      WL.DelimitedText := W;
      sRelation := WL[2];
    finally
      WL.Free;
    end;

    S := 'SELECT 0 RECNO ' +
              ', '''' CATALOG_NAME ' +
              ', '''' SCHEMA_NAME ' +
              ', A.RDB$RELATION_NAME TABLE_NAME ' +
              ', A.RDB$FIELD_NAME COLUMN_NAME ' +
              ', A.RDB$FIELD_POSITION COLUMN_POSITION ' +
              ', 0 COLUMN_TYPE ' +
              ', B.RDB$FIELD_TYPE COLUMN_DATATYPE ' +
              ', '''' COLUMN_TYPENAME ' +
              ', B.RDB$FIELD_SUB_TYPE COLUMN_SUBTYPE ' +
              ', B.RDB$FIELD_LENGTH COLUMN_LENGTH ' +
              ', 0 COLUMN_PRECISION ' +
              ', B.RDB$FIELD_SCALE COLUMN_SCALE ' +
              ', A.RDB$NULL_FLAG COLUMN_NULLABLE ' +
           'FROM RDB$RELATION_FIELDS A, RDB$FIELDS B ' +
   Format('WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$RELATION_NAME = ''%s'') ', [sRelation]) +
       'ORDER BY A.RDB$FIELD_POSITION';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDataCommands.GetTables, SQL) = 1 then begin
    // GetTables "G:\Win.XP\ccy\LOCALS~1\Temp\T_OFWSA5ZZ354AUHCO55K2GC5IYA"."SYSDBA".% SystemTable
    W := SQL;
    Delete(W, 1, Pos('%', W));
    W := Trim(W);
    if (Pos(string(TDBXMetadataTableTypes.SystemTable), string(W)) > 0) and
       (Pos(string(TDBXMetadataTableTypes.Table), string(W)) > 0) then
      sFlag := ' '
    else if Pos(string(TDBXMetadataTableTypes.SystemTable), string(W)) > 0 then
      sFlag := 'AND (A.RDB$SYSTEM_FLAG = 1) '
    else if Pos(string(TDBXMetadataTableTypes.Table), string(W)) > 0 then
      sFlag := 'AND (A.RDB$SYSTEM_FLAG = 0) '
    else
      sFlag := ' ';

    S := 'SELECT 0 RECNO ' +
              ', '''' CATALOG_NAME ' +
              ', A.RDB$OWNER_NAME SCHEMA_NAME ' +
              ', A.RDB$RELATION_NAME TABLE_NAME ' +
              ', A.RDB$SYSTEM_FLAG TABLE_TYPE ' +
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

    M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
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
      sRelation := WL[2];
    finally
      WL.Free;
    end;

    S := 'SELECT 0 RECNO ' +
              ', '''' CATALOG_NAME ' +
              ', '''' SCHEMA_NAME ' +
              ', A.RDB$RELATION_NAME TABLE_NAME ' +
              ', A.RDB$INDEX_NAME INDEX_NAME ' +
              ', B.RDB$FIELD_NAME COLUMN_NAME ' +
              ', B.RDB$FIELD_POSITION COLUMN_POSITION ' +
              ', '''' PKEY_NAME ' +
              ', 0 INDEX_TYPE ' +
              ', CASE A.RDB$INDEX_TYPE ' +
                   'WHEN 1 THEN ''D'' ' +
                   'ELSE ''A'' ' +
                'END SORT_ORDER ' +
              ', '''' "FILTER" ' +
           'FROM RDB$INDICES A INNER JOIN RDB$INDEX_SEGMENTS B ' +
                   'ON (A.RDB$INDEX_NAME = B.RDB$INDEX_NAME) ' +
                 'FULL OUTER JOIN RDB$RELATION_CONSTRAINTS C ' +
                   'ON (A.RDB$RELATION_NAME = C.RDB$RELATION_NAME AND C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +
          'WHERE (A.RDB$SYSTEM_FLAG <> 1 OR A.RDB$SYSTEM_FLAG IS NULL) ' +
     Format('AND (A.RDB$RELATION_NAME = UPPER(''%s'')) ', [sRelation]) +
       'ORDER BY A.RDB$INDEX_NAME';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDatacommands.GetProcedureParameters, SQL) = 1 then begin
    // GetProcedureParameters "C:\Users\ccy\AppData\Local\Temp\T_15ESUZAIAKXEUXWRQC14RCUPED"."SYSDBA"."PROC".%
    WL := TWideStringList.Create;
    try
      W := SQL;
      Delete(W, 1, Pos(' ', W));
      WL.Delimiter := '.';
      WL.DelimitedText := W;
      sRelation := WL[2];
    finally
      WL.Free;
    end;

    S := 'SELECT A.RDB$PARAMETER_NUMBER PARAM_POSITION ' +
              ', CASE A.RDB$PARAMETER_TYPE ' +
                  'WHEN 0 THEN 1 ' +
                  'WHEN 1 THEN 2 ' +
                  'ELSE A.RDB$PARAMETER_TYPE ' +
                'END PARAM_TYPE ' +
              ', A.RDB$PARAMETER_NAME PARAM_NAME ' +
              ', CASE ' +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_varying,   TDBXDataTypes.AnsiStringType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_text,      TDBXDataTypes.AnsiStringType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_float,     TDBXDataTypes.DoubleType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_double,    TDBXDataTypes.DoubleType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_sql_date,  TDBXDataTypes.DateType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_sql_time,  TDBXDataTypes.TimeType]) +
                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_timestamp, TDBXDataTypes.TimeStampType]) +
//                    Format('WHEN B.RDB$FIELD_TYPE=%d THEN %d ', [blr_blob,      TDBXDataTypes.BlobType]) +

                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND ((B.RDB$FIELD_SUB_TYPE=0) OR (B.RDB$FIELD_SUB_TYPE IS NULL))) THEN %d ', [blr_short, TDBXDataTypes.Int16Type]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=1)) THEN %d ', [blr_short, TDBXDataTypes.BcdType]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=2)) THEN %d ', [blr_short, TDBXDataTypes.BcdType]) +

                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND ((B.RDB$FIELD_SUB_TYPE=0) OR (B.RDB$FIELD_SUB_TYPE IS NULL))) THEN %d ', [blr_long, TDBXDataTypes.Int32Type]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=1)) THEN %d ', [blr_long, TDBXDataTypes.BcdType]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=2)) THEN %d ', [blr_long, TDBXDataTypes.BcdType]) +

                    {$Message 'QC#64499 TParam does not take TLargeIntField value'}
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND ((B.RDB$FIELD_SUB_TYPE=0) OR (B.RDB$FIELD_SUB_TYPE IS NULL))) THEN %d ', [blr_int64, TDBXDataTypes.BcdType]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=1)) THEN %d ', [blr_int64, TDBXDataTypes.BcdType]) +
                    Format('WHEN (B.RDB$FIELD_TYPE=%d AND (B.RDB$FIELD_SUB_TYPE=2)) THEN %d ', [blr_int64, TDBXDataTypes.BcdType]) +

                   'ELSE B.RDB$FIELD_TYPE ' +
                'END PARAM_DATATYPE ' +
              ', B.RDB$FIELD_SUB_TYPE PARAM_SUBTYPE ' +
              ', CASE ' +
           Format('WHEN B.RDB$FIELD_TYPE=%d THEN 27 ', [blr_blob]) +
                  'ELSE B.RDB$FIELD_LENGTH ' +
                'END PARAM_LENGTH ' +
              ', B.RDB$FIELD_PRECISION PARAM_PRECISION ' +
              ', B.RDB$FIELD_SCALE PARAM_SCALE ' +
           'FROM RDB$PROCEDURE_PARAMETERS A, RDB$FIELDS B ' +
   Format('WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$PROCEDURE_NAME = ''%s'') ', [sRelation]) +
       'ORDER BY A.RDB$PARAMETER_TYPE, A.RDB$PARAMETER_NUMBER';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else if Pos(TDBXMetaDatacommands.GetProcedures, SQL) = 1 then begin
    // 'GetProcedures "C:\Users\ccy\AppData\Local\Temp\T_OZFRNWLM2EG3EPS2XIGMTFOAGB"."SYSDBA".% '
    S := 'SELECT RDB$PROCEDURE_NAME PROC_NAME ' +
           'FROM RDB$PROCEDURES ';

    FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool);
    FDSQL.Open(StatusVector, FDBHandle, nil);
    FDSQL.Prepare(StatusVector, S, FSQLDialect);
    FDSQL.Execute(StatusVector);
    if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;

    M := TMetaDataProvider_Firebird.Create(FDSQL.o_SQLDA);
    aReader := TDBXReader_Firebird_DSQL.Create(FConnection, FDBHandle, M, FDSQL, True);
    Result := TDBXErrorCodes.None;
  end else
    Assert(False, SQL);
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
var S, P, Q: string;
    L: IFirebird_DSQL;
    i, iInputParamCount: integer;
    bInputParamCount: boolean;
begin
  Assert(FDSQL = nil);

  FDSQL := TFirebird_DSQL.Create(GetFirebirdLibrary, FTransactionPool, FCommandType = TDBXCommandTypes.DbxStoredProcedure);

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
  if not StatusVector.CheckResult(Result, TDBXErrorCodes.VendorError) then Exit;
end;

end.
