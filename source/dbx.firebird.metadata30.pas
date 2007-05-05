unit dbx.firebird.metadata30;

interface

uses IB_Header, firebird.client, DBXpress, dbx.common, firebird.dsql;

type
  TMetaData_Firebird_getColumns = class(TInterfacedObject, IMetaDataProvider)
  private
    FColumns: TFieldColumns;
    FSQLDA: TXSQLDA;
    function NewFieldColumns(const aTableName: WideString): TFieldColumns;
  protected
    function GetColumnCount: integer;
    function GetColumnLength(const aColNo: Word): LongWord;
    function GetColumnName(const aColNo: Word): WideString;
    function GetColumnPrecision(const aColNo: Word): Smallint;
    function GetColumnScale(const aColNo: Word): Smallint;
    function GetColumnType(const aColNo: Word): Word;
    function GetColumnSubType(const aColNo: Word): Word;
    function IsNullable(const aColNo: Word): boolean;
  public
    constructor Create(const aTableName: WideString; const aSQLDA: TXSQLDA);
  end;

  TSQLMetaData30_Firebird = class(TInterfacedObject, ISQLMetaData30)
  strict private
    FDBXOptions: TDBXOptions;
    FClient: IFirebirdClient;
    FDBHandle: pisc_db_handle;
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
        pisc_db_handle; const aDBXOptions: TDBXOptions);
  end;

implementation

uses SysUtils, dbx.firebird.cursor30;

constructor TSQLMetaData30_Firebird.Create(const aClientLibrary:
    IFirebirdClient; const aDBHandle: pisc_db_handle; const aDBXOptions:
    TDBXOptions);
begin
  inherited Create;
  FClient := aClientLibrary;
  FDBHandle := aDBHandle;
  FDBXOptions := aDBXOptions;
end;

function TSQLMetaData30_Firebird.getColumns(TableName: PWideChar; ColumnName:
    PWideChar; ColType: LongWord; Out Cursor: ISQLCursor30): SQLResult;
var M: IMetaDataProvider;
    S: string;
    C: IFirebird_DSQL;
begin
  S := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$FIELD_NAME, A.RDB$FIELD_POSITION, 0, B.RDB$FIELD_TYPE, '''', ' +
              'B.RDB$FIELD_SUB_TYPE, B.RDB$FIELD_LENGTH, 0, B.RDB$FIELD_SCALE, A.RDB$NULL_FLAG ' +
         'FROM RDB$RELATION_FIELDS A, RDB$FIELDS B ' +
 Format('WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$RELATION_NAME = ''%s'') ', [TableName]) +
     'ORDER BY A.RDB$FIELD_POSITION';

  C := TFirebird_DSQL.Create(FClient);
  C.Open(StatusVector, FDBHandle);
  C.Prepare(StatusVector, S, FDBXOptions.SQLDialect);
  C.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  M := TMetaData_Firebird_getColumns.Create(TableName, C.o_SQLDA);
  Cursor := TSQLCursor30_Firebird.Create(FClient, M, C, True);

  Result := DBXERR_NONE;
end;

function TSQLMetaData30_Firebird.getErrorMessage(
  Error: PWideChar): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.getIndices(TableName: PWideChar; IndexType:
    LongWord; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.getObjectList(eObjType: TSQLObjectType; out
    Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.GetOption(eDOption: TSQLMetaDataOption;
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

function TSQLMetaData30_Firebird.getProcedureParams(ProcName: PWideChar;
    ParamName: PWideChar; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.getProcedures(ProcedureName: PWideChar;
    ProcType: LongWord; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.getTables(TableName: PWideChar; TableType:
    LongWord; out Cursor: ISQLCursor30): SQLResult;
begin
  Assert(False);
end;

function TSQLMetaData30_Firebird.SetOption(eDOption: TSQLMetaDataOption;
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

function TSQLMetaData30_Firebird.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

constructor TMetaData_Firebird_getColumns.Create(const aTableName: WideString;
    const aSQLDA: TXSQLDA);
begin
  inherited Create;
  FColumns := NewFieldColumns(aTableName);
  FSQLDA := aSQLDA;
end;

function TMetaData_Firebird_getColumns.GetColumnCount: integer;
begin
  Result := Length(FColumns);
end;

function TMetaData_Firebird_getColumns.GetColumnLength(const aColNo: Word): LongWord;
begin
  Result := FColumns[aColNo].Size;
end;

function TMetaData_Firebird_getColumns.GetColumnName(const aColNo: Word): WideString;
begin
  Result := FColumns[aColNo].Name;
end;

function TMetaData_Firebird_getColumns.GetColumnPrecision(const aColNo: Word): Smallint;
begin
  if FColumns[aColNo].FieldType = fldZSTRING then
    Result := FColumns[aColNo].Size - 1
  else
    Result := 0;
end;

function TMetaData_Firebird_getColumns.GetColumnScale(const aColNo: Word): Smallint;
begin
  Result := 0;
end;

function TMetaData_Firebird_getColumns.GetColumnSubType(const aColNo: Word): Word;
begin
  Result := 0;
end;

function TMetaData_Firebird_getColumns.GetColumnType(const aColNo: Word): Word;
begin
  Result := FColumns[aColNo].FieldType;
end;

function TMetaData_Firebird_getColumns.IsNullable(const aColNo: Word): boolean;
begin
  Result := FSQLDA.Vars[aColNo].IsNullable;
end;

function TMetaData_Firebird_getColumns.NewFieldColumns(const aTableName:
    WideString): TFieldColumns;

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
  iIndex := 1;
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

end.
