unit dbx4.firebird.driver;

interface

uses
  Data.DBXCommon, Data.DBXPlatform,
  dbx4.base, dbx4.firebird.base, firebird.client;

type
  TDBXDriver_Firebird = class(TDBXBase, IDBXDriver, IDBXDriver_Firebird)
  private
    FLibrary: IFirebirdLibrary;
  protected
    function Close: TDBXErrorCode; override;
    function GetErrorMessage(LastErrorCode: TDBXErrorCode; ErrorMessage:
        TDBXWideStringBuilder): TDBXErrorCode; override;
    function GetErrorMessageLength(LastErrorCode: TDBXErrorCode; out ErrorLen:
        TInt32): TDBXErrorCode; override;
    function Loaded: boolean;
    function GetLibrary: IFirebirdLibrary;
  public
    constructor Create(const Count: TInt32; Names, Values: TWideStringArray);
  end;

implementation

uses
  System.SysUtils, Data.SqlConst;

function TDBXDriver_Firebird.Close: TDBXErrorCode;
begin
  FLibrary := nil;
  Result := TDBXErrorCodes.None;
end;

constructor TDBXDriver_Firebird.Create(const Count: TInt32; Names,
  Values: TWideStringArray);
var i: integer;
begin
  inherited Create;
  var ServerCharSet := 'NONE';
  var VendorLib := 'fbclient.dll';
  for i := 0 to Count - 1 do begin
    if Names[i] = SQLSERVER_CHARSET_KEY then
      ServerCharSet := Values[i]
    else if Names[i] = TDBXPropertyNames.VendorLib then
      VendorLib := Values[i];
  end;

  FLibrary := TFirebirdLibrary.New(VendorLib, ServerCharSet);
end;

function TDBXDriver_Firebird.GetErrorMessage(LastErrorCode: TDBXErrorCode;
    ErrorMessage: TDBXWideStringBuilder): TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXDriver_Firebird.GetErrorMessageLength(LastErrorCode:
    TDBXErrorCode; out ErrorLen: TInt32): TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXDriver_Firebird.Loaded: boolean;
begin
  Result := Assigned(FLibrary) and FLibrary.Loaded;
end;

function TDBXDriver_Firebird.GetLibrary: IFirebirdLibrary;
begin
  Result := FLibrary;
end;

end.
