unit dbx4.firebird.base;

interface

uses DBXCommon, DBXPlatform, dbx4.base,
     firebird.client, firebird.ibase.h;

type
  IDBXBase_Firebird = interface(IDBXBase)
  ['{0D0A3D6A-4A82-48EA-8308-35549F0B03BA}']
    function GetFirebirdLibrary: IFirebirdLibrary;
    property FirebirdLibrary: IFirebirdLibrary read GetFirebirdLibrary;
  end;

  IDBXDriver_Firebird = interface(IDBXDriver)
  ['{03E21683-9F8E-4E7F-BC09-A48A0673392A}']
    function NewLibrary: IFirebirdLibrary;
  end;

  IDBXConnection_Firebird = interface(IDBXConnection)
  ['{0C0D900F-FA0A-4C70-BE03-2E7BF5CA8FB3}']
    function GetDBHandle: pisc_db_handle;
    function GetServerCharSet: WideString;
    function GetSQLDialect: integer;
    function GetTransactionPool: TFirebirdTransactionPool;
    function GetTrimChar: Boolean;
    property DBHandle: pisc_db_handle read GetDBHandle;
    property ServerCharSet: WideString read GetServerCharSet;
    property SQLDialect: integer read GetSQLDialect;
    property TransactionPool: TFirebirdTransactionPool read GetTransactionPool;
    property TrimChar: Boolean read GetTrimChar;
  end;

  TDBXBase_Firebird = class abstract(TDBXBase, IDBXBase_Firebird)
  private
    FStatusVector: IStatusVector;
  protected // IDBXBase_Firebird
    function GetFirebirdLibrary: IFirebirdLibrary; virtual;
  protected
    function GetErrorMessage(LastErrorCode: TDBXErrorCode; ErrorMessage:
        TDBXWideStringBuilder): TDBXErrorCode; override;
    function GetErrorMessageLength(LastErrorCode: TDBXErrorCode; out ErrorLen:
        TInt32): TDBXErrorCode; override;
    function StatusVector: IStatusVector;
    function NotSupported: TDBXErrorCode;
  end;

implementation

uses firebird.iberror.h;

function TDBXBase_Firebird.GetErrorMessage(LastErrorCode: TDBXErrorCode;
  ErrorMessage: TDBXWideStringBuilder): TDBXErrorCode;
begin
  StatusVector.GetLastError.GetMessage(ErrorMessage);
  Result := TDBXErrorCodes.None;
end;

function TDBXBase_Firebird.GetErrorMessageLength(
  LastErrorCode: TDBXErrorCode; out ErrorLen: TInt32): TDBXErrorCode;
begin
  ErrorLen := StatusVector.GetError(GetFirebirdLibrary).GetLength;
  Result := TDBXErrorCodes.None;
end;

function TDBXBase_Firebird.GetFirebirdLibrary: IFirebirdLibrary;
begin
  Result := nil;
end;

function TDBXBase_Firebird.NotSupported: TDBXErrorCode;
begin
  StatusVector.pValue[0] := 1;
  StatusVector.pValue[1] := isc_wish_list;

  Result := TDBXErrorCodes.NotSupported;
end;

function TDBXBase_Firebird.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

end.
