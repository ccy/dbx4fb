unit dbx.firebird.driver40;

interface

uses DBXpress, DBXpress40;

type
  TSQLDriver_Firebird_40 = class(TInterfacedObject, DBXpress40.ISQLDriver)
  private
    FSQLDriver: DBXpress.ISQLDriver;
  protected
    function getSQLConnection(out pConn: ISQLConnection): SQLResult40; stdcall;
    function SetOption(eDOption: TSQLDriverOption; PropValue: LongInt):
        SQLResult40; stdcall;
    function GetOption(eDOption: TSQLDriverOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
  public
    constructor Create(const aSQLDriver: DBXPress.ISQLDriver);
  end;

implementation

uses dbx.firebird;

constructor TSQLDriver_Firebird_40.Create(const aSQLDriver: DBXPress.ISQLDriver);
begin
  inherited Create;
  FSQLDriver := aSQLDriver;
end;

function TSQLDriver_Firebird_40.GetOption(eDOption: TSQLDriverOption; PropValue:
    Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult40;
begin
  Result := FSQLDriver.GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSQLDriver_Firebird_40.getSQLConnection(out pConn: ISQLConnection):
    SQLResult40;
begin
  Result := FSQLDriver.getSQLConnection(pConn);
end;

function TSQLDriver_Firebird_40.SetOption(eDOption: TSQLDriverOption; PropValue:
    LongInt): SQLResult40;
begin
  Result := FSQLDriver.SetOption(eDOption, PropValue);
  if eDOption = eDrvProductVersion then
    TDBX_Firebird.SetDriverVersion('4.0');
end;

end.
