unit dbx.firebird.metadata40;

interface

uses DBXpress, DBXpress40;

type
  TSQLMetaData_Firebird_40 = class(TInterfacedObject, ISQLMetaData,
      ISQLMetaData40)
  private
    FMetaData: ISQLMetaData30;
  protected
    function getColumns(TableName, ColumnName: PWideChar; ColType: LongWord; out
        Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
    function getIndices(TableName: PWideChar; IndexType: LongWord; out Cursor:
        ISQLCursor30): SQLResult40; stdcall;
    function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30):
        SQLResult40; stdcall;
    function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getProcedureParams(ProcName, ParamName: PWideChar; out Cursor:
        ISQLCursor30): SQLResult40; stdcall;
    function getProcedures(ProcedureName: PWideChar; ProcType: LongWord; out
        Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getTables(TableName: PWideChar; TableType: LongWord; out Cursor:
        ISQLCursor30): SQLResult40; stdcall;
    function SetOption(eDOption: TSQLMetaDataOption; PropValue: LongInt):
        SQLResult40; stdcall;
  public
    constructor Create(const aMetaData: ISQLMetaData);
  end;

implementation

uses SysUtils;

constructor TSQLMetaData_Firebird_40.Create(const aMetaData: ISQLMetaData);
begin
  inherited Create;
  FMetaData := ISQLMetaData30(aMetaData);
end;

function TSQLMetaData_Firebird_40.getColumns(TableName, ColumnName: PWideChar;
    ColType: LongWord; out Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getColumns(TableName, ColumnName, ColType, Cursor);
end;

function TSQLMetaData_Firebird_40.getErrorMessage(Error: PWideChar): SQLResult40;
begin
  Result := FMetaData.getErrorMessage(Error);
end;

function TSQLMetaData_Firebird_40.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult40;
begin
  Result := FMetaData.getErrorMessageLen(ErrorLen);
end;

function TSQLMetaData_Firebird_40.getIndices(TableName: PWideChar; IndexType:
    LongWord; out Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getIndices(TableName, IndexType, Cursor);
end;

function TSQLMetaData_Firebird_40.getObjectList(eObjType: TSQLObjectType; out
    Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getObjectList(eObjType, Cursor);
end;

function TSQLMetaData_Firebird_40.GetOption(eDOption: TSQLMetaDataOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult40;
begin
  Result := FMetaData.GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSQLMetaData_Firebird_40.getProcedureParams(ProcName, ParamName:
    PWideChar; out Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getProcedureParams(ProcName, ParamName, Cursor);
end;

function TSQLMetaData_Firebird_40.getProcedures(ProcedureName: PWideChar;
    ProcType: LongWord; out Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getProcedures(ProcedureName, ProcType, Cursor);
end;

function TSQLMetaData_Firebird_40.getTables(TableName: PWideChar; TableType:
    LongWord; out Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FMetaData.getTables(TableName, TableType, Cursor);
end;

function TSQLMetaData_Firebird_40.SetOption(eDOption: TSQLMetaDataOption;
    PropValue: LongInt): SQLResult40;
begin
  Result := FMetaData.SetOption(eDOption, PropValue);
end;

end.
