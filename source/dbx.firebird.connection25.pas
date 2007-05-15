unit dbx.firebird.connection25;

interface

uses DBXpress;

type
  TSQLConnection_Firebird_25 = class(TInterfacedObject, ISQLConnection, ISQLConnection25)
  private
    FConnection: ISQLConnection30;
  protected
    function beginTransaction(TranID: LongWord): SQLResult; stdcall;
    function commit(TranID: LongWord): SQLResult; stdcall;
    function connect(ServerName: PChar; UserName: PChar; Password: PChar):
        SQLResult; stdcall;
    function disconnect: SQLResult; stdcall;
    function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getSQLCommand(out pComm: ISQLCommand25): SQLResult; stdcall;
    function getSQLMetaData(out pMetaData: ISQLMetaData25): SQLResult; stdcall;
    function rollback(TranID: LongWord): SQLResult; stdcall;
    function SetOption(eConnectOption: TSQLConnectionOption; lValue: LongInt):
        SQLResult; stdcall;
  public
    constructor Create(const aSQLConnection: ISQLConnection);
  end;

implementation

uses dbx.firebird, SysUtils;

constructor TSQLConnection_Firebird_25.Create(const aSQLConnection:
    ISQLConnection);
begin
  inherited Create;
  FConnection := ISQLConnection30(aSQLConnection);
end;

function TSQLConnection_Firebird_25.beginTransaction(TranID: LongWord):
    SQLResult;
begin
  Result := FConnection.beginTransaction(TranID);
end;

function TSQLConnection_Firebird_25.commit(TranID: LongWord): SQLResult;
begin
  Result := FConnection.commit(TranID);
end;

function TSQLConnection_Firebird_25.connect(ServerName: PChar; UserName: PChar;
    Password: PChar): SQLResult;
var sServerName, sUserName, sPassword: WideString;
begin
  sServerName := ServerName;
  sUserName := UserName;
  sPassword := Password;
  Result := FConnection.connect(PWideChar(sServerName), PWideChar(sUserName), PWideChar(sPassword));
end;

function TSQLConnection_Firebird_25.disconnect: SQLResult;
begin
  Result := FConnection.disconnect;
end;

function TSQLConnection_Firebird_25.getErrorMessage(Error: PChar): SQLResult;
var i: Smallint;
    W: WideString;
begin
  getErrorMessageLen(i);
  SetLength(W, i);
  Result := FConnection.getErrorMessage(PWideChar(W));
  StrPCopy(Error, W);
end;

function TSQLConnection_Firebird_25.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  Result := FConnection.getErrorMessageLen(ErrorLen);
end;

function TSQLConnection_Firebird_25.GetOption(eDOption: TSQLConnectionOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
var W: WideString;
begin
  case eDOption of
    eConnCatalogName,
    eConnSchemaName,
    eConnObjectName: begin
      SetLength(W, MaxLength);
      Result := FConnection.GetOption(eDOption, PWideChar(W), MaxLength, Length);
      SetLength(W, Length);
      StrPCopy(PAnsiChar(PropValue), W);
    end;
    eConnQuotedObjectName: begin
      SetLength(W, MaxLength);
      Result := FConnection.GetOption(eDOption, PWideChar(W), MaxLength, Length);
      SetLength(W, Length);
      if Pos('.', W) = 2 then
        Delete(W, 2, 1);
      StrPCopy(PAnsiChar(PropValue), W);
    end;
    else
      Result := FConnection.GetOption(eDOption, PropValue, MaxLength, Length);
  end;
end;

function TSQLConnection_Firebird_25.getSQLCommand(out pComm: ISQLCommand25):
    SQLResult;
begin
  Result := FConnection.getSQLCommand(ISQLCommand30(pComm));
end;

function TSQLConnection_Firebird_25.getSQLMetaData(out pMetaData:
    ISQLMetaData25): SQLResult;
begin
  Result := FConnection.getSQLMetaData(ISQLMetaData30(pMetaData));
end;

function TSQLConnection_Firebird_25.rollback(TranID: LongWord): SQLResult;
begin
  Result := FConnection.rollback(TranID);
end;

function TSQLConnection_Firebird_25.SetOption(eConnectOption:
    TSQLConnectionOption; lValue: LongInt): SQLResult;
var V: LongInt;
    S: WideString;
begin
  case eConnectOption of
    eConnRoleName,
    eConnQualifiedName: begin
      S := PAnsiChar(lValue);
      V := longint(PWideChar(S));
    end;
    else
      V := lValue;
  end;
  Result := FConnection.SetOption(eConnectOption, V);
end;

end.
