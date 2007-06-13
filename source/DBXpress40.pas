unit DBXpress40;

interface

uses DBXpress;

type
  SQLResult40 = integer;

  // DO NOT CHANGE THE SEQUENCE OR ARRAGEMENT OF METHODS
  ISQLDriver = interface
    function getSQLConnection(out pConn: ISQLConnection): SQLResult40; stdcall;
    function SetOption(eDOption: TSQLDriverOption; PropValue: LongInt):
        SQLResult40; stdcall;
    function GetOption(eDOption: TSQLDriverOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
  end;

  // DO NOT CHANGE THE SEQUENCE OR ARRAGEMENT OF METHODS
  ISQLConnection40 = interface(ISQLConnection)
    function connect: SQLResult40; overload; stdcall;
    function connect(ServerName: PWideChar; UserName: PWideChar; Password:
        PWideChar): SQLResult40; overload; stdcall;
    function disconnect: SQLResult40; stdcall;
    function getSQLCommand(out pComm: ISQLCommand30): SQLResult40; stdcall;
    function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult40; stdcall;
    function SetOption(eConnectOption: TSQLConnectionOption; lValue: LongInt):
        SQLResult40; stdcall;
    function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function beginTransaction(TranID: LongWord): SQLResult40; stdcall;
    function commit(TranID: LongWord): SQLResult40; stdcall;
    function rollback(TranID: LongWord): SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
  end;

  // DO NOT CHANGE THE SEQUENCE OR ARRAGEMENT OF METHODS
  ISQLMetaData40 = interface(ISQLMetaData)
    function SetOption(eDOption: TSQLMetaDataOption; PropValue: LongInt):
        SQLResult40; stdcall;
    function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30):
        SQLResult40; stdcall;
    function getTables(TableName: PWideChar; TableType: LongWord; out Cursor:
        ISQLCursor30): SQLResult40; stdcall;
    function getProcedures(ProcedureName: PWideChar; ProcType: LongWord; out
        Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getColumns(TableName: PWideChar; ColumnName: PWideChar; ColType:
        LongWord; Out Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getProcedureParams(ProcName: PWideChar; ParamName: PWideChar; out
        Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getIndices(TableName: PWideChar; IndexType: LongWord; out Cursor:
        ISQLCursor30): SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
  end;

  // DO NOT CHANGE THE SEQUENCE OR ARRAGEMENT OF METHODS
  ISQLCommand40 = interface(ISQLCommand)
    function SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer):
        SQLResult40; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function setParameter(ulParameter: Word ; ulChildPos: Word ; eParamType:
        TSTMTParamType ; uLogType: Word; uSubType: Word; iPrecision: Integer;
        iScale: Integer; Length: LongWord ; pBuffer: Pointer; lInd: Integer):
        SQLResult40; stdcall;
    function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
        Length: Integer; var IsBlank: Integer): SQLResult40; stdcall;
    function prepare(SQL: PWideChar; ParamCount: Word): SQLResult40; stdcall;
    function execute(var Cursor: ISQLCursor30): SQLResult40; stdcall;
    function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult40;
        stdcall;
    function getNextCursor(var Cursor: ISQLCursor30): SQLResult40; stdcall;
    function getRowsAffected(var Rows: LongWord): SQLResult40; stdcall;
    function close: SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
  end;

  // DO NOT CHANGE THE SEQUENCE OR ARRAGEMENT OF METHODS
  ISQLCursor40 = interface(ISQLCursor)
    function SetOption(eOption: TSQLCursorOption; PropValue: LongInt): SQLResult40;
        stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
    function getColumnCount(var pColumns: Word): SQLResult40; stdcall;
    function getColumnNameLength(ColumnNumber: Word; var pLen: Word): SQLResult40;
        stdcall;
    function getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult40;
        stdcall;
    function getColumnType(ColumnNumber: Word; var puType: Word; var puSubType:
        Word): SQLResult40; stdcall;
    function getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult40;
        stdcall;
    function getColumnPrecision(ColumnNumber: Word; var piPrecision: SmallInt):
        SQLResult40; stdcall;
    function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult40;
        stdcall;
    function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult40;
        stdcall;
    function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool):
        SQLResult40; stdcall;
    function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult40;
        stdcall;
    function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult40;
        stdcall;
    function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult40;
        stdcall;
    function next: SQLResult40; stdcall;
    function getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getDouble(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getBcd(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getDate(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getBytes(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getBlobSize(ColumnNumber: Word; var Length: LongWord; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function getBlob(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool;
        Length: LongWord): SQLResult40; stdcall;
  end;

implementation

end.
