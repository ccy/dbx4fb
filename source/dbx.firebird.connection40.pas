unit dbx.firebird.connection40;

interface

uses DBXpress, DBXpress40;

type
  TSQLConnection_Firebird_40 = class(TInterfacedObject, ISQLConnection,
      ISQLConnection40)
  private
    FConnection: DBXpress.ISQLConnection30;
  protected
    function beginTransaction(TranID: LongWord): SQLResult40; stdcall;
    function commit(TranID: LongWord): SQLResult40; stdcall;
    function connect: SQLResult40; overload; stdcall;
    function connect(ServerName, UserName, Password: PWideChar): SQLResult40;
        overload; stdcall;
    function disconnect: SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
    function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getSQLCommand(out pComm: ISQLCommand30): SQLResult40; stdcall;
    function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult40; stdcall;
    function rollback(TranID: LongWord): SQLResult40; stdcall;
    function SetOption(eConnectOption: TSQLConnectionOption; lValue: LongInt):
        SQLResult40; stdcall;
  public
    constructor Create(const aSQLConnection: ISQLConnection);
  end;

implementation

uses dbx.firebird, SysUtils;

constructor TSQLConnection_Firebird_40.Create(const aSQLConnection:
    ISQLConnection);
begin
  inherited Create;
  FConnection := DBXpress.ISQLConnection30(aSQLConnection);
end;

function TSQLConnection_Firebird_40.beginTransaction(TranID: LongWord):
    SQLResult40;
begin
  Result := FConnection.beginTransaction(TranID);
end;

function TSQLConnection_Firebird_40.commit(TranID: LongWord): SQLResult40;
begin
  Result := FConnection.commit(TranID);
end;

function TSQLConnection_Firebird_40.connect: SQLResult40;
begin
  Result := FConnection.connect;
end;

function TSQLConnection_Firebird_40.connect(ServerName, UserName, Password:
    PWideChar): SQLResult40;
begin
  Result := FConnection.connect(ServerName, UserName, Password);
end;

function TSQLConnection_Firebird_40.disconnect: SQLResult40;
begin
  Result := FConnection.disconnect;
end;

function TSQLConnection_Firebird_40.getErrorMessage(Error: PWideChar):
    SQLResult40;
begin
  Result := FConnection.getErrorMessage(Error);
end;

function TSQLConnection_Firebird_40.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult40;
begin
  Result := FConnection.getErrorMessageLen(ErrorLen);
end;

function TSQLConnection_Firebird_40.GetOption(eDOption: TSQLConnectionOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult40;
begin
  Result := FConnection.GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSQLConnection_Firebird_40.getSQLCommand(out pComm: ISQLCommand30):
    SQLResult40;
begin
  Result := FConnection.getSQLCommand(pComm);
end;

function TSQLConnection_Firebird_40.getSQLMetaData(out pMetaData:
    ISQLMetaData30): SQLResult40;
begin
  Result := FConnection.getSQLMetaData(pMetaData);
end;

function TSQLConnection_Firebird_40.rollback(TranID: LongWord): SQLResult40;
begin
  Result := FConnection.rollback(TranID);
end;

function TSQLConnection_Firebird_40.SetOption(eConnectOption:
    TSQLConnectionOption; lValue: LongInt): SQLResult40;
begin
  Result := FConnection.SetOption(eConnectOption, lValue);
end;

end.
