unit dbx4.firebird.metadata;

interface

uses DBXCommon, DBXPlatform, dbx4.base;

type
  TFieldColumn = record
    Name: WideString;
    ColumnType: TInt32;
    Precision: TInt32;
  end;

  TFieldColumns = array of TFieldColumn;

  TMetaData_Firebird_Factory = class abstract
  public
    class function New_getColumns(const aTableName: WideString): TFieldColumns;
    class function New_GetDatabase: TFieldColumns;
    class function New_getIndices(const aTableName: WideString): TFieldColumns;
    class function New_getTables: TFieldColumns;
  end;

  TMetaDataProvider_FieldColumns = class(TInterfacedObject, IMetaDataProvider)
  private
    FColumns: TFieldColumns;
  protected
    function GetColumnCount: TInt32;
    function GetColumnLength(const aColNo: TInt32): LongWord;
    function GetColumnName(const aColNo: TInt32): WideString;
    function GetColumnPrecision(const aColNo: TInt32): TInt32;
    function GetColumnScale(const aColNo: TInt32): TInt32;
    function GetColumnType(const aColNo: TInt32): TInt32;
    function GetColumnSubType(const aColNo: TInt32): TInt32;
    function GetIsNullable(const aColNo: TInt32): boolean;
  public
    constructor Create(const aColumns: TFieldColumns);
  end;

implementation

class function TMetaData_Firebird_Factory.New_getColumns(const aTableName:
    WideString): TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aColumnType, aPrecision: TInt32);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      ColumnType := aColumnType;
      Precision := aPrecision;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 14);
  iIndex := 0;
  Add('RECNO',             TDBXDataTypes.Int32Type,       0);
  Add('CATALOG_NAME',      TDBXDataTypes.AnsiStringType,  128);
  Add('SCHEMA_NAME',       TDBXDataTypes.AnsiStringType,  128);
  Add('TABLE_NAME',        TDBXDataTypes.AnsiStringType,  128);
  Add('COLUMN_NAME',       TDBXDataTypes.AnsiStringType,  128);
  Add('COLUMN_POSITION',   TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_TYPE',       TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_DATATYPE',   TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_TYPENAME',   TDBXDataTypes.AnsiStringType,  128);
  Add('COLUMN_SUBTYPE',    TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_LENGTH',     TDBXDataTypes.Int32Type,       0);
  Add('COLUMN_PRECISION',  TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_SCALE',      TDBXDataTypes.Int16Type,       0);
  Add('COLUMN_NULLABLE',   TDBXDataTypes.Int16Type,       0);
end;

class function TMetaData_Firebird_Factory.New_GetDatabase: TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aColumnType, aPrecision: TInt32);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      ColumnType := aColumnType;
      Precision := aPrecision;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 6);
  iIndex := 0;
  Add('QuoteChar',                   TDBXDataTypes.WideStringType, 128);
  Add('ProcedureQuoteChar',          TDBXDataTypes.WideStringType, 128);
  Add('MaxCommands',                 TDBXDataTypes.Int32Type,      0);
  Add('SupportsTransactions',        TDBXDataTypes.BooleanType,    0);
  Add('SupportsNestedTransactions',  TDBXDataTypes.BooleanType,    0);
  Add('SupportsRowSetSize',          TDBXDataTypes.BooleanType,    0);
end;

class function TMetaData_Firebird_Factory.New_getIndices(const aTableName:
    WideString): TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aColumnType, aPrecision: TInt32);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      ColumnType := aColumnType;
      Precision := aPrecision;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 11);
  iIndex := 0;
  Add('RECNO',             TDBXDataTypes.Int32Type,       0);
  Add('CATALOG_NAME',      TDBXDataTypes.AnsiStringType,  128);
  Add('SCHEMA_NAME',       TDBXDataTypes.AnsiStringType,  128);
  Add('TABLE_NAME',        TDBXDataTypes.AnsiStringType,  128);
  Add('INDEX_NAME',        TDBXDataTypes.AnsiStringType,  128);
  Add('COLUMN_NAME',       TDBXDataTypes.AnsiStringType,  128);
  Add('COLUMN_POSITION',   TDBXDataTypes.Int16Type,       0);
  Add('PKEY_NAME',         TDBXDataTypes.AnsiStringType,  128);
  Add('INDEX_TYPE',        TDBXDataTypes.Int32Type,       0);
  Add('SORT_ORDER',        TDBXDataTypes.AnsiStringType,  3);
  Add('FILTER',            TDBXDataTypes.AnsiStringType,  6);
end;

class function TMetaData_Firebird_Factory.New_getTables: TFieldColumns;
var iIndex: integer;

  procedure Add(const aFieldName: string; const aColumnType, aPrecision: TInt32);
  begin
    with Result[iIndex] do begin
      Name := aFieldName;
      ColumnType := aColumnType;
      Precision := aPrecision;
    end;
    Inc(iIndex);
  end;

begin
  SetLength(Result, 5);
  iIndex := 0;
  Add('RECNO',             TDBXDataTypes.Int32Type,       0);
  Add('CATALOG_NAME',      TDBXDataTypes.AnsiStringType,  128);
  Add('SCHEMA_NAME',       TDBXDataTypes.AnsiStringType,  128);
  Add('TABLE_NAME',        TDBXDataTypes.AnsiStringType,  128);
  Add('TABLE_TYPE',        TDBXDataTypes.Int32Type,       0);
end;

constructor TMetaDataProvider_FieldColumns.Create(const aColumns: TFieldColumns);
begin
  inherited Create;
  FColumns := aColumns;
end;

function TMetaDataProvider_FieldColumns.GetColumnCount: TInt32;
begin
  Result := Length(FColumns);
end;

function TMetaDataProvider_FieldColumns.GetColumnLength(const aColNo: TInt32): LongWord;
begin
  if (FColumns[aColNo].ColumnType = TDBXDataTypes.AnsiStringType) or
     (FColumns[aColNo].ColumnType = TDBXDataTypes.WideStringType) then
    Result := FColumns[aColNo].Precision + 1
  else
    Result := 0;
end;

function TMetaDataProvider_FieldColumns.GetColumnName(const aColNo: TInt32): WideString;
begin
  Result := FColumns[aColNo].Name;
end;

function TMetaDataProvider_FieldColumns.GetColumnPrecision(const aColNo: TInt32): TInt32;
begin
  Result := GetColumnLength(aColNo);
end;

function TMetaDataProvider_FieldColumns.GetColumnScale(const aColNo: TInt32): TInt32;
begin
  Result := 0;
end;

function TMetaDataProvider_FieldColumns.GetColumnSubType(const aColNo: TInt32): TInt32;
begin
  Result := 0;
end;

function TMetaDataProvider_FieldColumns.GetColumnType(const aColNo: TInt32): TInt32;
begin
  Result := FColumns[aColNo].ColumnType;
end;

function TMetaDataProvider_FieldColumns.GetIsNullable(
  const aColNo: TInt32): boolean;
begin
  Result := True;
end;

end.
