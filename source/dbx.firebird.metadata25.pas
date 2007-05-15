unit dbx.firebird.metadata25;

interface

uses DBXpress;

type
  TSQLMetaData_Firebird_25 = class(TInterfacedObject, ISQLMetaData, ISQLMetaData25)
  private
    FMetaData: ISQLMetaData30;
  protected
    function getColumns(TableName: PChar; ColumnName: PChar; ColType: LongWord; Out
        Cursor: ISQLCursor25): SQLResult; stdcall;
    function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function getIndices(TableName: PChar; IndexType: LongWord; out Cursor:
        ISQLCursor25): SQLResult; stdcall;
    function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor25):
        SQLResult; stdcall;
    function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getProcedureParams(ProcName: PChar; ParamName: PChar; out Cursor:
        ISQLCursor25): SQLResult; stdcall;
    function getProcedures(ProcedureName: PChar; ProcType: LongWord; out Cursor:
        ISQLCursor25): SQLResult; stdcall;
    function getTables(TableName: PChar; TableType: LongWord; out Cursor:
        ISQLCursor25): SQLResult; stdcall;
    function SetOption(eDOption: TSQLMetaDataOption; PropValue: LongInt):
        SQLResult; stdcall;
  public
    constructor Create(const aMetaData: ISQLMetaData);
  end;

implementation

uses SysUtils;

constructor TSQLMetaData_Firebird_25.Create(const aMetaData: ISQLMetaData);
begin
  inherited Create;
  FMetaData := ISQLMetaData30(aMetaData);
end;

function TSQLMetaData_Firebird_25.getColumns(TableName: PChar; ColumnName:
    PChar; ColType: LongWord; Out Cursor: ISQLCursor25): SQLResult;
var sTableName, sColumnName: WideString;
begin
  sTableName := TableName;
  sColumnName := ColumnName;
  Result := FMetaData.getColumns(PWideChar(sTableName), PWideChar(sColumnName), ColType, ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.getErrorMessage(Error: PChar): SQLResult;
var i: Smallint;
    W: WideString;
begin
  getErrorMessageLen(i);
  SetLength(W, i);
  Result := FMetaData.getErrorMessage(PWideChar(W));
  StrPCopy(Error, W);
end;

function TSQLMetaData_Firebird_25.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  Result := FMetaData.getErrorMessageLen(ErrorLen);
end;

function TSQLMetaData_Firebird_25.getIndices(TableName: PChar; IndexType:
    LongWord; out Cursor: ISQLCursor25): SQLResult;
var sTableName: WideString;
begin
  sTableName := TableName;
  Result := FMetaData.getIndices(PWideChar(sTableName), IndexType, ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.getObjectList(eObjType: TSQLObjectType; out
    Cursor: ISQLCursor25): SQLResult;
begin
  Result := FMetaData.getObjectList(eObjType, ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.GetOption(eDOption: TSQLMetaDataOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  Result := FMetaData.GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSQLMetaData_Firebird_25.getProcedureParams(ProcName: PChar;
    ParamName: PChar; out Cursor: ISQLCursor25): SQLResult;
var wProcName, wParamName: WideString;
begin
  wProcName := ProcName;
  wParamName := ParamName;
  Result := FMetaData.getProcedureParams(PWideChar(wProcName), PWideChar(wParamName), ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.getProcedures(ProcedureName: PChar; ProcType:
    LongWord; out Cursor: ISQLCursor25): SQLResult;
var wProcedureName: WideString;
begin
  wProcedureName := ProcedureName;
  Result := FMetaData.getProcedures(PWideChar(wProcedureName), ProcType, ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.getTables(TableName: PChar; TableType:
    LongWord; out Cursor: ISQLCursor25): SQLResult;
var wTableName: WideString;
begin
  wTableName := TableName;
  Result := FMetaData.getTables(PWideChar(wTableName), TableType, ISQLCursor30(Cursor));
end;

function TSQLMetaData_Firebird_25.SetOption(eDOption: TSQLMetaDataOption;
    PropValue: LongInt): SQLResult;
begin
  Result := FMetaData.SetOption(eDOption, PropValue);
end;

end.
