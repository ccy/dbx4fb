unit dbx.firebird;

interface

uses Classes, DBXpress, dbx.common, firebird.client;

type
  IDBX_Firebird_Factory = interface(IInterface)
  ['{15F6886D-D988-4E93-B80D-A63DC054DA57}']
    function NewCommand(const aCommand: ISQLCommand): ISQLCommand;
    function NewConnection(const aConnection: ISQLConnection): ISQLConnection;
    function NewCursor(const aCursor: ISQLCursor): ISQLCursor;
    function NewDebuggerListener(const aDBXOptions: TDBXOptions): IFirebirdLibraryDebuggerListener;
    function NewMetaData(const aMetaData: ISQLMetaData): ISQLMetaData;
  end;

  TDBX_Firebird = class abstract
  private
    class var FItems: TStringList;
    class var FFactory: IDBX_Firebird_Factory;
    class function GetItems: TStringList;
  public
    class procedure Register(const aVersion: string; const aFactoryClass: TInterfacedClass);
    class function Factory: IDBX_Firebird_Factory;
    class procedure SetDriverVersion(const aDriverVersion: PChar);
  end;

implementation

class function TDBX_Firebird.Factory: IDBX_Firebird_Factory;
begin
  Result := FFactory;
end;

class function TDBX_Firebird.GetItems: TStringList;
begin
  if FItems = nil then
    FItems := TStringList.Create;
  Result := FItems;
end;

class procedure TDBX_Firebird.Register(const aVersion: string;
  const aFactoryClass: TInterfacedClass);
begin
  GetItems.AddObject(aVersion, TObject(aFactoryClass));
end;

class procedure TDBX_Firebird.SetDriverVersion(
  const aDriverVersion: PChar);
var S: string;
    i: integer;
begin
  S := aDriverVersion;
  i := GetItems.IndexOf(S);
  Assert(i <> -1);
  FFactory := TInterfacedClass(GetItems.Objects[i]).Create as IDBX_Firebird_Factory;
end;

initialization

finalization
  TDBX_Firebird.FItems.Free;
end.
