unit dbx4.firebird.row;

interface

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform, dbx4.base,
  dbx4.firebird.base, firebird.dsql, firebird.client,
  firebird.ibase.h;

type
  TDBXWritableRow_Firebird = class(TDBXBase_Firebird, IDBXWritableRow)
  private
    FDBHandle: pisc_db_handle;
    FSQLDA: TXSQLDA;
    FTransaction: IFirebirdTransaction;
  protected
    function Close: TDBXErrorCode; override;
    function SetAnsiString(Ordinal: TInt32; const Value: TDBXAnsiString; Length:
        Int64): TDBXErrorCode;
    function SetBcd(Ordinal: TInt32; Value: TBcd): TDBXErrorCode;
    function SetBytes(Ordinal: TInt32; BlobOffset: Int64; Value: TBytes; LastIndex:
        TInt32; ValueOffset, Length: Int64): TDBXErrorCode;
    function SetDate(Ordinal: TInt32; Value: TDBXDate): TDBXErrorCode;
    function SetDouble(Ordinal: TInt32; Value: Double): TDBXErrorCode;
    function SetInt16(Ordinal: TInt32; Value: SmallInt): TDBXErrorCode;
    function SetInt32(Ordinal: TInt32; Value: LongInt): TDBXErrorCode;
    function SetNull(Ordinal: TInt32): TDBXErrorCode;
    function SetParameterType(Ordinal: TInt32; const Name: TDBXWideString;
        ChildPosition: TInt32; ParamDirection: TDBXParameterDirection; DBXType,
        DBXSubType: TInt32; Size, Precision: Int64; Scale: TInt32): TDBXErrorCode;
    function SetTime(Ordinal: TInt32; Value: TDBXTime): TDBXErrorCode;
    function SetTimeStamp(Ordinal: TInt32; var Value: TSQLTimeStamp): TDBXErrorCode;
    function SetWideString(Ordinal: TInt32; const Value: TDBXWideString; Length:
        Int64): TDBXErrorCode;
  public
    constructor Create(const aDBHandle: pisc_db_handle; const aTransaction:
        IFirebirdTransaction; const aSQLDA: TXSQLDA);
  end;

implementation

constructor TDBXWritableRow_Firebird.Create(const aDBHandle: pisc_db_handle; const aTransaction:
    IFirebirdTransaction; const aSQLDA: TXSQLDA);
begin
  inherited Create;
  FDBHandle := aDBHandle;
  FTransaction := aTransaction;
  FSQLDA := aSQLDA;
end;

function TDBXWritableRow_Firebird.Close: TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetAnsiString(Ordinal: TInt32; const Value:
    TDBXAnsiString; Length: Int64): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetAnsiString(Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetBcd(Ordinal: TInt32; Value: TBcd): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetBCD(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetBytes(Ordinal: TInt32; BlobOffset: Int64;
  Value: TBytes; LastIndex: TInt32; ValueOffset,
  Length: Int64): TDBXErrorCode;
begin
  Assert(BlobOffset = 0);
  Assert(ValueOffSet = 0);
  FSQLDA[Ordinal].SetBlob(StatusVector, FDBHandle, FTransaction, Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetDate(Ordinal: TInt32; Value: TDBXDate): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetDate(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetDouble(Ordinal: TInt32; Value: Double): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetDouble(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetInt16(Ordinal: TInt32; Value: SmallInt): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetShort(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetInt32(Ordinal: TInt32; Value: Integer): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetInteger(@Value, SizeOf(Value), False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetNull(Ordinal: TInt32): TDBXErrorCode;
begin
  FSQLDA[Ordinal].IsNull := True;
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetParameterType(Ordinal: TInt32;
  const Name: TDBXWideString; ChildPosition: TInt32;
  ParamDirection: TDBXParameterDirection; DBXType, DBXSubType: TInt32;
  Size, Precision: Int64; Scale: TInt32): TDBXErrorCode;
begin
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetTime(Ordinal: TInt32; Value: TDBXTime): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetTime(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetTimeStamp(Ordinal: TInt32;
  var Value: TSQLTimeStamp): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetTimeStamp(@Value, False);
  Result := TDBXErrorCodes.None;
end;

function TDBXWritableRow_Firebird.SetWideString(Ordinal: TInt32;
  const Value: TDBXWideString; Length: Int64): TDBXErrorCode;
begin
  FSQLDA[Ordinal].SetWideString(Value, Length, False);
  Result := TDBXErrorCodes.None;
end;

end.
