unit dbx.firebird.command40;

interface

uses DBXpress, DBXpress40;

type
  TSQLCommand_Firebird_40 = class(TInterfacedObject, ISQLCommand, ISQLCommand40)
  private
    FCommand: ISQLCommand30;
  protected
    function close: SQLResult40; stdcall;
    function execute(var Cursor: ISQLCursor30): SQLResult40; stdcall;
    function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30):
        SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
    function getNextCursor(var Cursor: ISQLCursor30): SQLResult40; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
        Length: Integer; var IsBlank: Integer): SQLResult40; stdcall;
    function getRowsAffected(var Rows: LongWord): SQLResult40; stdcall;
    function prepare(SQL: PWideChar; ParamCount: Word): SQLResult40; stdcall;
    function SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer):
        SQLResult40; stdcall;
    function setParameter(ulParameter: Word ; ulChildPos: Word ; eParamType:
        TSTMTParamType ; uLogType: Word; uSubType: Word; iPrecision: Integer;
        iScale: Integer; Length: LongWord ; pBuffer: Pointer; lInd: Integer):
        SQLResult40; stdcall;
  public
    constructor Create(const aCommand: ISQLCommand);
  end;

implementation

uses SysUtils;

function TSQLCommand_Firebird_40.close: SQLResult40;
begin
  Result := FCommand.close;
end;

constructor TSQLCommand_Firebird_40.Create(const aCommand: ISQLCommand);
begin
  inherited Create;
  FCommand := ISQLCommand30(aCommand);
end;

function TSQLCommand_Firebird_40.execute(var Cursor: ISQLCursor30): SQLResult40;
begin
  Result := FCommand.execute(Cursor);
end;

function TSQLCommand_Firebird_40.executeImmediate(SQL: PWideChar; var Cursor:
    ISQLCursor30): SQLResult40;
begin
  Result := FCommand.executeImmediate(SQL, Cursor);
end;

function TSQLCommand_Firebird_40.getErrorMessage(Error: PWideChar): SQLResult40;
begin
  Result := FCommand.getErrorMessage(Error);
end;

function TSQLCommand_Firebird_40.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult40;
begin
  Result := FCommand.getErrorMessageLen(ErrorLen);
end;

function TSQLCommand_Firebird_40.getNextCursor(var Cursor: ISQLCursor30):
    SQLResult40;
begin
  Result := FCommand.getNextCursor(Cursor);
end;

function TSQLCommand_Firebird_40.GetOption(eSqlCommandOption:
    TSQLCommandOption; PropValue: Pointer; MaxLength: SmallInt; out Length:
    SmallInt): SQLResult40;
begin
  Result := FCommand.GetOption(eSqlCommandOption, PropValue, MaxLength, Length);
end;

function TSQLCommand_Firebird_40.getParameter(ParameterNumber: Word;
    ulChildPos: Word; Value: Pointer; Length: Integer; var IsBlank: Integer):
    SQLResult40;
begin
  Result := FCommand.getParameter(ParameterNumber, ulChildPos, Value, Length, IsBlank);
end;

function TSQLCommand_Firebird_40.getRowsAffected(var Rows: LongWord): SQLResult40;
begin
  Result := FCommand.getRowsAffected(Rows);
end;

function TSQLCommand_Firebird_40.prepare(SQL: PWideChar; ParamCount: Word):
    SQLResult40;
begin
  Result := FCommand.prepare(SQL, ParamCount);
end;

function TSQLCommand_Firebird_40.SetOption(eSqlCommandOption:
    TSQLCommandOption; ulValue: Integer): SQLResult40;
begin
  Result := FCommand.SetOption(eSqlCommandOption, ulValue);
end;

function TSQLCommand_Firebird_40.setParameter(ulParameter: Word ; ulChildPos:
    Word ; eParamType: TSTMTParamType ; uLogType: Word; uSubType: Word;
    iPrecision: Integer; iScale: Integer; Length: LongWord ; pBuffer: Pointer;
    lInd: Integer): SQLResult40;
begin
  Result := FCommand.setParameter(ulParameter, ulChildPos, eParamType, uLogType, uSubType, iPrecision, iScale, Length, pBuffer, lInd);
end;

end.
