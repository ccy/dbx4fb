unit dbx.firebird.command25;

interface

uses DBXpress;

type
  TSQLCommand_Firebird_25 = class(TInterfacedObject, ISQLCommand, ISQLCommand25)
  private
    FCommand: ISQLCommand30;
  protected
    function close: SQLResult; stdcall;
    function execute(var Cursor: ISQLCursor25): SQLResult; stdcall;
    function executeImmediate(SQL: PChar; var Cursor: ISQLCursor25): SQLResult;
        stdcall;
    function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function getNextCursor(var Cursor: ISQLCursor25): SQLResult; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
        Length: Integer; var IsBlank: Integer): SQLResult; stdcall;
    function getRowsAffected(var Rows: LongWord): SQLResult; stdcall;
    function prepare(SQL: PChar; ParamCount: Word): SQLResult; stdcall;
    function SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer):
        SQLResult; stdcall;
    function setParameter(ulParameter: Word ; ulChildPos: Word ; eParamType:
        TSTMTParamType ; uLogType: Word; uSubType: Word; iPrecision: Integer;
        iScale: Integer; Length: LongWord ; pBuffer: Pointer; lInd: Integer):
        SQLResult; stdcall;
  public
    constructor Create(const aCommand: ISQLCommand);
  end;

implementation

uses SysUtils;

function TSQLCommand_Firebird_25.close: SQLResult;
begin
  Result := FCommand.close;
end;

constructor TSQLCommand_Firebird_25.Create(const aCommand: ISQLCommand);
begin
  inherited Create;
  FCommand := ISQLCommand30(aCommand);
end;

function TSQLCommand_Firebird_25.execute(var Cursor: ISQLCursor25): SQLResult;
begin
  Result := FCommand.execute(ISQLCursor30(Cursor));
end;

function TSQLCommand_Firebird_25.executeImmediate(SQL: PChar; var Cursor:
    ISQLCursor25): SQLResult;
var W: WideString;
begin
  W := SQL;
  Result := FCommand.executeImmediate(PWideChar(W), ISQLCursor30(Cursor));
end;

function TSQLCommand_Firebird_25.getErrorMessage(Error: PChar): SQLResult;
var i: Smallint;
    W: WideString;
begin
  getErrorMessageLen(i);
  SetLength(W, i);
  Result := FCommand.getErrorMessage(PWideChar(W));
  StrPCopy(Error, W);
end;

function TSQLCommand_Firebird_25.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  Result := FCommand.getErrorMessageLen(ErrorLen);
end;

function TSQLCommand_Firebird_25.getNextCursor(var Cursor: ISQLCursor25):
    SQLResult;
begin
  Result := FCommand.getNextCursor(ISQLCursor30(Cursor));
end;

function TSQLCommand_Firebird_25.GetOption(eSqlCommandOption:
    TSQLCommandOption; PropValue: Pointer; MaxLength: SmallInt; out Length:
    SmallInt): SQLResult;
begin
  Result := FCommand.GetOption(eSqlCommandOption, PropValue, MaxLength, Length);
end;

function TSQLCommand_Firebird_25.getParameter(ParameterNumber: Word;
    ulChildPos: Word; Value: Pointer; Length: Integer; var IsBlank: Integer):
    SQLResult;
begin
  Result := FCommand.getParameter(ParameterNumber, ulChildPos, Value, Length, IsBlank);
end;

function TSQLCommand_Firebird_25.getRowsAffected(var Rows: LongWord): SQLResult;
begin
  Result := FCommand.getRowsAffected(Rows);
end;

function TSQLCommand_Firebird_25.prepare(SQL: PChar; ParamCount: Word):
    SQLResult;
var W: WideString;
begin
  W := SQL;
  Result := FCommand.prepare(PWideChar(W), ParamCount);
end;

function TSQLCommand_Firebird_25.SetOption(eSqlCommandOption:
    TSQLCommandOption; ulValue: Integer): SQLResult;
begin
  Result := FCommand.SetOption(eSqlCommandOption, ulValue);
end;

function TSQLCommand_Firebird_25.setParameter(ulParameter: Word ; ulChildPos:
    Word ; eParamType: TSTMTParamType ; uLogType: Word; uSubType: Word;
    iPrecision: Integer; iScale: Integer; Length: LongWord ; pBuffer: Pointer;
    lInd: Integer): SQLResult;
begin
  Result := FCommand.setParameter(ulParameter, ulChildPos, eParamType, uLogType, uSubType, iPrecision, iScale, Length, pBuffer, lInd);
end;

end.
