unit dbx.firebird.cursor30;

interface

uses IB_Header, firebird.client, firebird.dsql, DBXpress, dbx.common;

type
  TSQLCursor30_Firebird = class(TInterfacedObject, ISQLCursor30)
  private
    FStatusVector: IStatusVector;
    function StatusVector: IStatusVector;
  private
    FClient: IFirebirdClient;
    FMetaData: IMetaDataProvider;
    FDSQL: IFirebird_DSQL;
    FTrimChar: boolean;
  protected
    function getBcd(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getBlob(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool;
        Length: LongWord): SQLResult; stdcall;
    function getBlobSize(ColumnNumber: Word; var Length: LongWord; var IsBlank:
        LongBool): SQLResult; stdcall;
    function getBytes(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getColumnCount(var pColumns: Word): SQLResult; stdcall;
    function getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult;
        stdcall;
    function getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
        stdcall;
    function getColumnNameLength(ColumnNumber: Word; var pLen: Word): SQLResult;
        stdcall;
    function getColumnPrecision(ColumnNumber: Word; var piPrecision: SmallInt):
        SQLResult; stdcall;
    function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult;
        stdcall;
    function getColumnType(ColumnNumber: Word; var puType: Word; var puSubType:
        Word): SQLResult; stdcall;
    function getDate(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getDouble(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank:
        LongBool): SQLResult; stdcall;
    function getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank:
        LongBool): SQLResult; stdcall;
    function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool):
        SQLResult; stdcall;
    function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult;
        stdcall;
    function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult;
        stdcall;
    function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult;
        stdcall;
    function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult;
        stdcall;
    function next: SQLResult; stdcall;
    function SetOption(eOption: TSQLCursorOption; PropValue: LongInt): SQLResult;
        stdcall;
  public
    constructor Create(const aClientLibrary: IFirebirdClient; const aMetaData:
        IMetaDataProvider; const aDSQL: IFirebird_DSQL; const aTrimChar: boolean);
    procedure BeforeDestruction; override;
  end;

implementation

uses Windows, SysUtils;

procedure TSQLCursor30_Firebird.BeforeDestruction;
begin
  inherited;
  FDSQL.Close(StatusVector);
end;

constructor TSQLCursor30_Firebird.Create(const aClientLibrary: IFirebirdClient;
    const aMetaData: IMetaDataProvider; const aDSQL: IFirebird_DSQL; const
    aTrimChar: boolean);
begin
  inherited Create;
  FClient := aClientLibrary;
  FMetaData := aMetaData;
  FDSQL := aDSQL;
  FTrimChar := aTrimChar;
end;

function TSQLCursor30_Firebird.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetBcd(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getBlob(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool; Length: LongWord): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getBlobSize(ColumnNumber: Word;
  var Length: LongWord; var IsBlank: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getBytes(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getColumnCount(var pColumns: Word): SQLResult;
begin
  pColumns := FMetaData.GetColumnCount;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
  pLength := FMetaData.GetColumnLength(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnName(ColumnNumber: Word;
  pColumnName: PWideChar): SQLResult;
begin
  lstrcpyW(pColumnName, PWideChar(FMetaData.GetColumnName(ColumnNumber)));
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnNameLength(ColumnNumber: Word;
  var pLen: Word): SQLResult;
begin
  pLen := FMetaData.GetColumnLength(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
begin
  piPrecision := FMetaData.GetColumnPrecision(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnScale(ColumnNumber: Word;
  var piScale: SmallInt): SQLResult;
begin
  piScale := FMetaData.GetColumnScale(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getColumnType(ColumnNumber: Word; var puType,
  puSubType: Word): SQLResult;
begin
  puType := FMetaData.GetColumnType(ColumnNumber);
  puSubType := FMetaData.GetColumnSubType(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getDate(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetDate(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getDouble(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetDouble(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getErrorMessage(Error: PWideChar): SQLResult;
begin
  StatusVector.GetLastError.GetMessage(Error);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := StatusVector.GetError(FClient).GetLength;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getInt64(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetInteger(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.GetOption(eOption: TSQLCursorOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getShort(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetShort(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getString(ColumnNumber: Word; Value: PChar;
  var IsBlank: LongBool): SQLResult;
var b: boolean;
    c: PChar;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetString(Value, b);
  IsBlank := b;
  if FTrimChar then begin
    c := Value + FDSQL.o_SQLDA[ColumnNumber].sqllen - 1;
    while c^ = ' ' do begin
      c^ := #0;
      c := c - 1;
    end;
  end;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getTime(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
var b: Boolean;
begin
  FDSQL.o_SQLDA[ColumnNumber].GetTime(Value, b);
  IsBlank := b;
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.getTimeStamp(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.getWideString(ColumnNumber: Word;
  Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.isAutoIncrement(ColumnNumber: Word;
  var AutoIncr: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.isBlobSizeExact(ColumnNumber: Word;
  var IsExact: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.isNullable(ColumnNumber: Word;
  var Nullable: LongBool): SQLResult;
begin
  Nullable := FMetaData.IsNullable(ColumnNumber);
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.isReadOnly(ColumnNumber: Word;
  var ReadOnly: LongBool): SQLResult;
begin
  ReadOnly := True;  {$Message 'How to determine the value of ReadOnly'}
  Result := DBXERR_NONE;
end;

function TSQLCursor30_Firebird.isSearchable(ColumnNumber: Word;
  var Searchable: LongBool): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.next: SQLResult;
begin
  Result := FDSQL.Fetch(StatusVector);
  if (Result <> 0) and (Result <> 100) then
    StatusVector.CheckResult(Result, DBXERR_SQLERROR);
end;

function TSQLCursor30_Firebird.SetOption(eOption: TSQLCursorOption;
  PropValue: Integer): SQLResult;
begin
  Assert(False);
end;

function TSQLCursor30_Firebird.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

end.
