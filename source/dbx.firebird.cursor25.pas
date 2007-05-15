unit dbx.firebird.cursor25;

interface

uses DBXPress;

type
  TSQLCursor_Firebird_25 = class(TInterfacedObject, ISQLCursor, ISQLCursor25)
  private
    FCursor: ISQLCursor30;
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
    function getColumnName(ColumnNumber: Word; pColumnName: PChar): SQLResult;
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
    function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getString(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank:
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
    constructor Create(const aCursor: ISQLCursor);
  end;

implementation

uses SysUtils;

constructor TSQLCursor_Firebird_25.Create(const aCursor: ISQLCursor);
begin
  inherited Create;
  FCursor := ISQLCursor30(aCursor);
end;

function TSQLCursor_Firebird_25.getBcd(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getBcd(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getBlob(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool; Length: LongWord): SQLResult;
begin
  Result := FCursor.getBlob(ColumnNumber, Value, IsBlank, Length);
end;

function TSQLCursor_Firebird_25.getBlobSize(ColumnNumber: Word; var Length:
    LongWord; var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getBlobSize(ColumnNumber, Length, IsBlank);
end;

function TSQLCursor_Firebird_25.getBytes(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getBytes(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getColumnCount(var pColumns: Word): SQLResult;
begin
  Result := FCursor.getColumnCount(pColumns);
end;

function TSQLCursor_Firebird_25.getColumnLength(ColumnNumber: Word; var
    pLength: LongWord): SQLResult;
begin
  Result := FCursor.getColumnLength(ColumnNumber, pLength);
end;

function TSQLCursor_Firebird_25.getColumnName(ColumnNumber: Word; pColumnName:
    PChar): SQLResult;
var l: Word;
    W: WideString;
begin
  getColumnNameLength(ColumnNumber, l);
  SetLength(W, l);
  Result := FCursor.getColumnName(ColumnNumber, PWideChar(W));
  StrPCopy(pColumnName, W);
end;

function TSQLCursor_Firebird_25.getColumnNameLength(ColumnNumber: Word; var
    pLen: Word): SQLResult;
begin
  Result := FCursor.getColumnNameLength(ColumnNumber, pLen);
end;

function TSQLCursor_Firebird_25.getColumnPrecision(ColumnNumber: Word; var
    piPrecision: SmallInt): SQLResult;
begin
  Result := FCursor.getColumnPrecision(ColumnNumber, piPrecision);
end;

function TSQLCursor_Firebird_25.getColumnScale(ColumnNumber: Word; var piScale:
    SmallInt): SQLResult;
begin
  Result := FCursor.getColumnScale(ColumnNumber, piScale);
end;

function TSQLCursor_Firebird_25.getColumnType(ColumnNumber: Word; var puType:
    Word; var puSubType: Word): SQLResult;
begin
  Result := FCursor.getColumnType(ColumnNumber, puType, puSubType);
end;

function TSQLCursor_Firebird_25.getDate(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getDate(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getDouble(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getDouble(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getErrorMessage(Error: PChar): SQLResult;
var i: Smallint;
    W: WideString;
begin
  getErrorMessageLen(i);
  SetLength(W, i);
  Result := FCursor.getErrorMessage(PWideChar(W));
  StrPCopy(Error, W);
end;

function TSQLCursor_Firebird_25.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  Result := FCursor.getErrorMessageLen(ErrorLen);
end;

function TSQLCursor_Firebird_25.getLong(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getLong(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.GetOption(eOption: TSQLCursorOption; PropValue:
    Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  Result := FCursor.GetOption(eOption, PropValue, MaxLength, Length);
end;

function TSQLCursor_Firebird_25.getShort(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getShort(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getString(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getString(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getTime(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getTime(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.getTimeStamp(ColumnNumber: Word; Value:
    Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := FCursor.getTimeStamp(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_25.isAutoIncrement(ColumnNumber: Word; var
    AutoIncr: LongBool): SQLResult;
begin
  Result := FCursor.isAutoIncrement(ColumnNumber, AutoIncr);
end;

function TSQLCursor_Firebird_25.isBlobSizeExact(ColumnNumber: Word; var
    IsExact: LongBool): SQLResult;
begin
  Result := FCursor.isBlobSizeExact(ColumnNumber, IsExact);
end;

function TSQLCursor_Firebird_25.isNullable(ColumnNumber: Word; var Nullable:
    LongBool): SQLResult;
begin
  Result := FCursor.isNullable(ColumnNumber, Nullable);
end;

function TSQLCursor_Firebird_25.isReadOnly(ColumnNumber: Word; var ReadOnly:
    LongBool): SQLResult;
begin
  Result := FCursor.isReadOnly(ColumnNumber, ReadOnly);
end;

function TSQLCursor_Firebird_25.isSearchable(ColumnNumber: Word; var
    Searchable: LongBool): SQLResult;
begin
  Result := FCursor.isSearchable(ColumnNumber, Searchable);
end;

function TSQLCursor_Firebird_25.next: SQLResult;
begin
  Result := FCursor.next;
end;

function TSQLCursor_Firebird_25.SetOption(eOption: TSQLCursorOption; PropValue:
    LongInt): SQLResult;
begin
  Result := FCursor.SetOption(eOption, PropValue);
end;

end.
