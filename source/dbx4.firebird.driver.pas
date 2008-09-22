unit dbx4.firebird.driver;

interface

uses Classes, DBXCommon, DBXPlatform, firebird.client, dbx4.base, dbx4.firebird.base;

type
  TDBXDriver_Firebird = class(TDBXBase, IDBXDriver, IDBXDriver_Firebird)
  private
    FHandle: THandle;
    FVendorLib: WideString;
    procedure LoadDriver;
  protected
    function Close: TDBXErrorCode; override;
    function GetErrorMessage(LastErrorCode: TDBXErrorCode; ErrorMessage:
        TDBXWideStringBuilder): TDBXErrorCode; override;
    function GetErrorMessageLength(LastErrorCode: TDBXErrorCode; out ErrorLen:
        TInt32): TDBXErrorCode; override;
    function Loaded: boolean;
    function NewLibrary: IFirebirdLibrary;
  public
    constructor Create(const Count: TInt32; Names, Values: TWideStringArray);
  end;

implementation

uses SysUtils, Windows;

function TDBXDriver_Firebird.Close: TDBXErrorCode;
begin
  Sleep(1); {$Message 'In firebird embedded, this delay will make the FreeLibrary safer and won't cause unexpected error for massive LoadLibrary / FreeLibrary calls'}
  if not FreeLibrary(FHandle) then
    Result := TDBXErrorCodes.DriverInitFailed
  else
    Result := TDBXErrorCodes.None;
end;

constructor TDBXDriver_Firebird.Create(const Count: TInt32; Names,
  Values: TWideStringArray);
var i: integer;
begin
  inherited Create;
  for i := 0 to Count - 1 do begin
    if Names[i] = TDBXPropertyNames.VendorLib then
      FVendorLib := Values[i];
  end;
  LoadDriver;
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
  Result := FHandle <> 0;
end;

function TDBXDriver_Firebird.NewLibrary: IFirebirdLibrary;
begin
  Result := TFirebirdLibraryFactory.New(FHandle);
end;

procedure TDBXDriver_Firebird.LoadDriver;
var sDir: string;
    V: string;
begin
  sDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FVendorLib));
    V := FVendorLib;
    FHandle := LoadLibrary(PAnsiChar(V));
  finally
    SetCurrentDir(sDir);
  end;
end;

end.
