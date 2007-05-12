unit dbx.firebird.driver30;

interface

uses DBXpress;

type
  TSQLDriver_Firebird = class(TInterfacedObject, ISQLDriver)
  private
    FVendorLibHandle: THandle;
  protected
    function getSQLConnection(out pConn: ISQLConnection): SQLResult; stdcall;
    function SetOption(eDOption: TSQLDriverOption; PropValue: LongInt): SQLResult;
        stdcall;
    function GetOption(eDOption: TSQLDriverOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult; stdcall;
  public
    constructor Create(const aVendorHandle: THandle);
    procedure BeforeDestruction; override;
  end;

implementation

uses SysUtils, Windows, firebird.client, dbx.firebird.connection30,
  dbx.firebird;

procedure TSQLDriver_Firebird.BeforeDestruction;
begin
  inherited;
  Sleep(1); {$Message 'In firebird embedded, this delay will make the FreeLibrary safer and won't cause unexpected error for massive LoadLibrary / FreeLibrary calls'}
  if not FreeLibrary(FVendorLibHandle) then
    GetLastError;
end;

constructor TSQLDriver_Firebird.Create(const aVendorHandle: THandle);
begin
  inherited Create;
  FVendorLibHandle := aVendorHandle;
end;

function TSQLDriver_Firebird.GetOption(eDOption: TSQLDriverOption; PropValue:
    Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  case eDOption of
    eDrvBlobSize: Assert(False);
    eDrvCallBack: Assert(False);
    eDrvCallBackInfo: Assert(False);
    eDrvRestrict: Assert(False);
    eDrvVersion: begin
      StrPCopy(PropValue, DBXDRIVERVERSION30);
      Length := System.Length(DBXDRIVERVERSION30);
    end;
    eDrvProductVersion: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSQLDriver_Firebird.getSQLConnection(out pConn: ISQLConnection):
    SQLResult;
var L: IFirebirdClient;
    C: ISQLConnection;
begin
  L := TFirebirdClientFactory.New(FVendorLibHandle);
  if TDBX_Firebird.Factory = nil then
    TDBX_Firebird.SetDriverVersion('2.5');
  C := TSQLConnection_Firebird_30.Create(L);
  ISQLConnection(pConn) := TDBX_Firebird.Factory.NewConnection(C);
  if Assigned(L) then
    Result := DBXERR_NONE
  else
    Result := DBXERR_DRIVERINITFAILED;
end;

function TSQLDriver_Firebird.SetOption(eDOption: TSQLDriverOption; PropValue:
    LongInt): SQLResult;
begin
  case eDOption of
    eDrvBlobSize: Assert(False);
    eDrvCallBack: Assert(False);
    eDrvCallBackInfo: Assert(False);
    eDrvRestrict: ; {$Message 'Do not sure what to do here'}
    eDrvVersion: Assert(False);
    eDrvProductVersion: TDBX_Firebird.SetDriverVersion(PChar(PropValue));
  end;
  Result := DBXERR_NONE;
end;

end.
