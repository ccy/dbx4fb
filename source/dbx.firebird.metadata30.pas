unit dbx.firebird.metadata30;

interface

uses IB_Header, firebird.client, DBXpress, dbx.common, firebird.dsql;

type
  TMetaData_Firebird_Factory = class abstract
  public
    class function New_getColumns(const aTableName: WideString): TFieldColumns;
    class function New_getIndices(const aTableName: WideString): TFieldColumns;
    class function New_getTables: TFieldColumns;
  end;

  TMetaDataProvider_Firebird = class(TInterfacedObject, IMetaDataProvider)
  strict private
    FColumns: TFieldColumns;
  protected
    function GetColumnCount: integer;
    function GetColumnLength(const aColNo: Word): LongWord;
    function GetColumnName(const aColNo: Word): WideString;
    function GetColumnNameLength(const aColNo: Word): Word;
    function GetColumnPrecision(const aColNo: Word): Smallint;
    function GetColumnScale(const aColNo: Word): Smallint;
    function GetColumnType(const aColNo: Word): Word;
    function GetColumnSubType(const aColNo: Word): Word;
    function IsNullable(const aColNo: Word): boolean;
  public
    constructor Create(const aColumns: TFieldColumns);
  end;

  TSQLMetaData_Firebird_30 = class(TInterfacedObject, ISQLMetaData, ISQLMetaData30)
  strict private
    FDBXOptions: TDBXOptions;
    FClient: IFirebirdClient;
    FDBHandle: pisc_db_handle;
    FTransactionPool: TFirebirdTransactionPool;
  private
    FStatusVector: IStatusVector;
    function StatusVector: IStatusVector;
  protected
    function getColumns(TableName: PWideChar; ColumnName: PWideChar; ColType:
        LongWord; Out Cursor: ISQLCursor30): SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function getIndices(TableName: PWideChar; IndexType: LongWord; out Cursor:
        ISQLCursor30): SQLResult; stdcall;
    function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30):
        SQLResult; stdcall;
    function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getProcedureParams(ProcName: PWideChar; ParamName: PWideChar; out
        Cursor: ISQLCursor30): SQLResult; stdcall;
    function getProcedures(ProcedureName: PWideChar; ProcType: LongWord; out
        Cursor: ISQLCursor30): SQLResult; stdcall;
    function getTables(TableName: PWideChar; TableType: LongWord; out Cursor:
        ISQLCursor30): SQLResult; stdcall;
    function SetOption(eDOption: TSQLMetaDataOption; PropValue: LongInt):
        SQLResult; stdcall;
  public
    constructor Create(const aClientLibrary: IFirebirdClient; const aDBHandle:
        pisc_db_handle; const aTransactionPool: TFirebirdTransactionPool; const
        aDBXOptions: TDBXOptions);
  end;

implementation

uses SysUtils, dbx.firebird.cursor30, dbx.firebird;

class function TMetaData_Firebird_Factory.New_getColumns(const aTableName: WideString):
    TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aFieldType, aFieldSize: Word);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      FieldType := aFieldType;
      Size := aFieldSize;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 14);
  iIndex := 0;
  Add('RECNO',             fldINT32,     4);
  Add('CATALOG_NAME',      fldZSTRING,   7);
  Add('SCHEMA_NAME',       fldZSTRING,   7);
  Add('TABLE_NAME',        fldZSTRING,   Length(aTableName) + 1);
  Add('COLUMN_NAME',       fldZSTRING,   32);
  Add('COLUMN_POSITION',   fldINT16,     2);
  Add('COLUMN_TYPE',       fldINT16,     2);
  Add('COLUMN_DATATYPE',   fldINT16,     2);
  Add('COLUMN_TYPENAME',   fldZSTRING,   32);
  Add('COLUMN_SUBTYPE',    fldINT16,     2);
  Add('COLUMN_LENGTH',     fldINT32,     4);
  Add('COLUMN_PRECISION',  fldINT16,     2);
  Add('COLUMN_SCALE',      fldINT16,     2);
  Add('COLUMN_NULLABLE',   fldINT16,     2);
end;

class function TMetaData_Firebird_Factory.New_getIndices(const aTableName: WideString):
    TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aFieldType, aFieldSize: Word);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      FieldType := aFieldType;
      Size := aFieldSize;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 11);
  iIndex := 0;
  Add('RECNO',             fldINT32,    4);
  Add('CATALOG_NAME',      fldZSTRING,  7);
  Add('SCHEMA_NAME',       fldZSTRING,  7);
  Add('TABLE_NAME',        fldZSTRING,  Length(aTableName) + 1);
  Add('INDEX_NAME',        fldZSTRING,  31);
  Add('COLUMN_NAME',       fldZSTRING,  31);
  Add('COLUMN_POSITION',   fldINT16,    2);
  Add('PKEY_NAME',         fldZSTRING,  31);
  Add('INDEX_TYPE',        fldINT32,    4);
  Add('SORT_ORDER',        fldZSTRING,  2);
  Add('FILTER',            fldZSTRING,  7);
end;

class function TMetaData_Firebird_Factory.New_getTables: TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aFieldType, aFieldSize: Word);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      FieldType := aFieldType;
      Size := aFieldSize;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 5);
  iIndex := 0;
  Add('RECNO',             fldINT32,    4);
  Add('CATALOG_NAME',      fldZSTRING,  7);
  Add('SCHEMA_NAME',       fldZSTRING,  7);
  Add('TABLE_NAME',        fldZSTRING,  32);
  Add('TABLE_TYPE',        fldINT32,    4);
end;

constructor TMetaDataProvider_Firebird.Create(const aColumns: TFieldColumns);
begin
  inherited Create;
  FColumns := aColumns;
end;

function TMetaDataProvider_Firebird.GetColumnCount: integer;
begin
  Result := Length(FColumns);
end;

function TMetaDataProvider_Firebird.GetColumnLength(const aColNo: Word):
    LongWord;
begin
  Result := FColumns[aColNo - 1].Size;
end;

function TMetaDataProvider_Firebird.GetColumnName(const aColNo: Word):
    WideString;
begin
  Result := FColumns[aColNo - 1].Name;
end;

function TMetaDataProvider_Firebird.GetColumnNameLength(
  const aColNo: Word): Word;
begin
  Result := Length(FColumns[aColNo - 1].Name);
end;

function TMetaDataProvider_Firebird.GetColumnPrecision(const aColNo: Word):
    Smallint;
begin
  if FColumns[aColNo - 1].FieldType = fldZSTRING then
    Result := FColumns[aColNo - 1].Size - 1
  else
    Result := 0;
end;

function TMetaDataProvider_Firebird.GetColumnScale(const aColNo: Word):
    Smallint;
begin
  Result := 0;
end;

function TMetaDataProvider_Firebird.GetColumnSubType(const aColNo: Word):
    Word;
begin
  Result := 0;
end;

function TMetaDataProvider_Firebird.GetColumnType(const aColNo: Word): Word;
begin
  Result := FColumns[aColNo - 1].FieldType;
end;

function TMetaDataProvider_Firebird.IsNullable(const aColNo: Word): boolean;
begin
  Result := True;
end;

constructor TSQLMetaData_Firebird_30.Create(const aClientLibrary:
    IFirebirdClient; const aDBHandle: pisc_db_handle; const aTransactionPool:
    TFirebirdTransactionPool; const aDBXOptions: TDBXOptions);
begin
  inherited Create;
  FClient := aClientLibrary;
  FDBHandle := aDBHandle;
  FTransactionPool := aTransactionPool;
  FDBXOptions := aDBXOptions;
end;

function TSQLMetaData_Firebird_30.getColumns(TableName: PWideChar; ColumnName:
    PWideChar; ColType: LongWord; Out Cursor: ISQLCursor30): SQLResult;
var M: IMetaDataProvider;
    S: string;
    C: IFirebird_DSQL;
    R: ISQLCursor;
begin
  S := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$FIELD_NAME, A.RDB$FIELD_POSITION, 0, B.RDB$FIELD_TYPE, '''', ' +
              'B.RDB$FIELD_SUB_TYPE, B.RDB$FIELD_LENGTH, 0, B.RDB$FIELD_SCALE, A.RDB$NULL_FLAG ' +
         'FROM RDB$RELATION_FIELDS A, RDB$FIELDS B ' +
 Format('WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$RELATION_NAME = ''%s'') ', [TableName]) +
     'ORDER BY A.RDB$FIELD_POSITION';

  C := TFirebird_DSQL.Create(FClient, FTransactionPool);
  C.Open(StatusVector, FDBHandle, nil);
  C.Prepare(StatusVector, S, FDBXOptions.SQLDialect);
  C.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  M := TMetaDataProvider_Firebird.Create(TMetaData_Firebird_Factory.New_getColumns(TableName));

  R := TSQLCursor_Firebird_30.Create(FClient, FDBHandle, M, C, True, True);
  ISQLCursor(Cursor) := TDBX_Firebird.Factory.NewCursor(R);

  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.getErrorMessage(
  Error: PWideChar): SQLResult;
begin
  StatusVector.GetLastError.GetMessage(Error);
  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  ErrorLen := StatusVector.GetError(FClient).GetLength;
  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.getIndices(TableName: PWideChar; IndexType:
    LongWord; out Cursor: ISQLCursor30): SQLResult;
var M: IMetaDataProvider;
//    S: string;
    R: ISQLCursor;
//    C: IFirebird_DSQL;
begin
  {$Message 'Do not sure how getIndices works. It seems like we do not need to fetch data from the SQL'}
  {$Message 'Ignore the Execute will greatly improve the performance'}

//  S := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$INDEX_NAME, B.RDB$FIELD_NAME, ' +
//       '       B.RDB$FIELD_POSITION, '''', 0, A.RDB$INDEX_TYPE, '''', A.RDB$UNIQUE_FLAG, C.RDB$CONSTRAINT_NAME, C.RDB$CONSTRAINT_TYPE ' +
//         'FROM RDB$INDICES A, RDB$INDEX_SEGMENTS B FULL OUTER JOIN RDB$RELATION_CONSTRAINTS C ' +
//           'ON A.RDB$RELATION_NAME = C.RDB$RELATION_NAME AND C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
//        'WHERE (A.RDB$SYSTEM_FLAG <> 1 OR A.RDB$SYSTEM_FLAG IS NULL) ' +
//          'AND (A.RDB$INDEX_NAME = B.RDB$INDEX_NAME) ' +
//   Format('AND (A.RDB$RELATION_NAME = UPPER(''%s'')) ', [TableName]) +
//     'ORDER BY A.RDB$INDEX_NAME';

//  C := TFirebird_DSQL.Create(FClient, FTransaction);
//  C.Open(StatusVector, FDBHandle);
//  C.Prepare(StatusVector, S, FDBXOptions.SQLDialect);
//  C.Execute(StatusVector);
//  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  M := TMetaDataProvider_Firebird.Create(TMetaData_Firebird_Factory.New_getIndices(TableName));
  R := TSQLCursor_Firebird_30.Create(FClient, FDBHandle, M, nil, True, True);
  ISQLCursor(Cursor) := TDBX_Firebird.Factory.NewCursor(R);

  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.getObjectList(eObjType: TSQLObjectType; out
    Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData_Firebird_30.GetOption(eDOption: TSQLMetaDataOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  case eDOption of
    eMetaCatalogName: Assert(False);
    eMetaSchemaName: Assert(False);
    eMetaDatabaseName: Assert(False);
    eMetaDatabaseVersion: Assert(False);
    eMetaTransactionIsoLevel: Assert(False);
    eMetaSupportsTransaction: boolean(PropValue^) := True;
    eMetaMaxObjectNameLength: Assert(False);
    eMetaMaxColumnsInTable: Assert(False);
    eMetaMaxColumnsInSelect: Assert(False);
    eMetaMaxRowSize: Assert(False);
    eMetaMaxSQLLength: Assert(False);
    eMetaObjectQuoteChar: ;   {$Message 'Do not sure what to do here'}
    eMetaSQLEscapeChar: Assert(False);
    eMetaProcSupportsCursor: Assert(False);
    eMetaProcSupportsCursors: Assert(False);
    eMetaSupportsTransactions: boolean(PropValue^) := True;
    eMetaPackageName: Assert(False);
    eMetaDefaultSchemaName: ;  {$Message 'Do not sure what to do here'}
  end;
  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.getProcedureParams(ProcName: PWideChar;
    ParamName: PWideChar; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData_Firebird_30.getProcedures(ProcedureName: PWideChar;
    ProcType: LongWord; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData_Firebird_30.getTables(TableName: PWideChar; TableType:
    LongWord; out Cursor: ISQLCursor30): SQLResult;
var M: IMetaDataProvider;
    S: string;
    C: IFirebird_DSQL;
    sFlag: string;
    R: ISQLCursor;
begin
  {$Message 'TableType of eSQLView not implement yet'}

  if TableType and (eSQLSystemTable + eSQLTable) = eSQLSystemTable + eSQLTable then
    sFlag := ' '
  else if TableType and eSQLSystemTable > 0 then
    sFlag := 'AND (A.RDB$SYSTEM_FLAG = 1) '
  else if TableType and eSQLTable > 0 then
    sFlag := 'AND (A.RDB$SYSTEM_FLAG = 0) '
  else
    sFlag := ' ';

  S := 'SELECT 0, '''', A.RDB$OWNER_NAME, A.RDB$RELATION_NAME, A.RDB$SYSTEM_FLAG ' +
         'FROM RDB$RELATIONS A ' +
        'WHERE (A.RDB$VIEW_SOURCE IS NULL) ' +
          sFlag +
          'AND A.RDB$OWNER_NAME = ''SYSDBA'' '+
     'ORDER BY A.RDB$RELATION_NAME';

  C := TFirebird_DSQL.Create(FClient, FTransactionPool);
  C.Open(StatusVector, FDBHandle, nil);
  C.Prepare(StatusVector, S, FDBXOptions.SQLDialect);
  C.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  M := TMetaDataProvider_Firebird.Create(TMetaData_Firebird_Factory.New_getTables);
  R := TSQLCursor_Firebird_30.Create(FClient, FDBHandle, M, C, True, True);
  ISQLCursor(Cursor) := TDBX_Firebird.Factory.NewCursor(R);

  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.SetOption(eDOption: TSQLMetaDataOption;
    PropValue: LongInt): SQLResult;
begin
  case eDOption of
    eMetaCatalogName: ;  {$Message 'Do not sure what to do here'}
    eMetaSchemaName: ;  {$Message 'Do not sure what to do here'}
    eMetaDatabaseName: Assert(False);
    eMetaDatabaseVersion: Assert(False);
    eMetaTransactionIsoLevel: Assert(False);
    eMetaSupportsTransaction: Assert(False);
    eMetaMaxObjectNameLength: Assert(False);
    eMetaMaxColumnsInTable: Assert(False);
    eMetaMaxColumnsInSelect: Assert(False);
    eMetaMaxRowSize: Assert(False);
    eMetaMaxSQLLength: Assert(False);
    eMetaObjectQuoteChar: Assert(False);
    eMetaSQLEscapeChar: Assert(False);
    eMetaProcSupportsCursor: Assert(False);
    eMetaProcSupportsCursors: Assert(False);
    eMetaSupportsTransactions: Assert(False);
    eMetaPackageName: Assert(False);
    eMetaDefaultSchemaName: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSQLMetaData_Firebird_30.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

end.
