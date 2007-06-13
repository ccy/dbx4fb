unit dbx.firebird.cursor40;

interface

uses DBXPress, DBXpress40;

type
  TSQLCursor_Firebird_40 = class(TInterfacedObject, ISQLCursor, ISQLCursor40)
  private
    FCursor: ISQLCursor30;
  protected
    function getBcd(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getBlob(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool;
        Length: LongWord): SQLResult40; stdcall;
    function getBlobSize(ColumnNumber: Word; var Length: LongWord; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function getBytes(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getColumnCount(var pColumns: Word): SQLResult40; stdcall;
    function getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult40;
        stdcall;
    function getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult40;
        stdcall;
    function getColumnNameLength(ColumnNumber: Word; var pLen: Word): SQLResult40;
        stdcall;
    function getColumnPrecision(ColumnNumber: Word; var piPrecision: SmallInt):
        SQLResult40; stdcall;
    function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult40;
        stdcall;
    function getColumnType(ColumnNumber: Word; var puType: Word; var puSubType:
        Word): SQLResult40; stdcall;
    function getDate(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getDouble(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult40; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult40; stdcall;
    function getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength:
        SmallInt; out Length: SmallInt): SQLResult40; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool):
        SQLResult40; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank:
        LongBool): SQLResult40; stdcall;
    function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool):
        SQLResult40; stdcall;
    function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult40;
        stdcall;
    function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult40;
        stdcall;
    function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult40;
        stdcall;
    function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult40;
        stdcall;
    function next: SQLResult40; stdcall;
    function SetOption(eOption: TSQLCursorOption; PropValue: LongInt): SQLResult40;
        stdcall;
  public
    constructor Create(const aCursor: ISQLCursor);
  end;

implementation

uses SysUtils;

constructor TSQLCursor_Firebird_40.Create(const aCursor: ISQLCursor);
begin
  inherited Create;
  FCursor := ISQLCursor30(aCursor);
end;

function TSQLCursor_Firebird_40.getBcd(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getBcd(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getBlob(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool; Length: LongWord): SQLResult40;
begin
  Result := FCursor.getBlob(ColumnNumber, Value, IsBlank, Length);
end;

function TSQLCursor_Firebird_40.getBlobSize(ColumnNumber: Word; var Length:
    LongWord; var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getBlobSize(ColumnNumber, Length, IsBlank);
end;

function TSQLCursor_Firebird_40.getBytes(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getBytes(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getColumnCount(var pColumns: Word): SQLResult40;
begin
  Result := FCursor.getColumnCount(pColumns);
end;

function TSQLCursor_Firebird_40.getColumnLength(ColumnNumber: Word; var
    pLength: LongWord): SQLResult40;
begin
  Result := FCursor.getColumnLength(ColumnNumber, pLength);
end;

function TSQLCursor_Firebird_40.getColumnName(ColumnNumber: Word; pColumnName:
    PWideChar): SQLResult40;
begin
  Result := FCursor.getColumnName(ColumnNumber, pColumnName);
end;

function TSQLCursor_Firebird_40.getColumnNameLength(ColumnNumber: Word; var
    pLen: Word): SQLResult40;
begin
  Result := FCursor.getColumnNameLength(ColumnNumber, pLen);
end;

function TSQLCursor_Firebird_40.getColumnPrecision(ColumnNumber: Word; var
    piPrecision: SmallInt): SQLResult40;
begin
  Result := FCursor.getColumnPrecision(ColumnNumber, piPrecision);
end;

function TSQLCursor_Firebird_40.getColumnScale(ColumnNumber: Word; var piScale:
    SmallInt): SQLResult40;
begin
  Result := FCursor.getColumnScale(ColumnNumber, piScale);
end;

function TSQLCursor_Firebird_40.getColumnType(ColumnNumber: Word; var puType:
    Word; var puSubType: Word): SQLResult40;
begin
  Result := FCursor.getColumnType(ColumnNumber, puType, puSubType);
end;

function TSQLCursor_Firebird_40.getDate(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getDate(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getDouble(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getDouble(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getErrorMessage(Error: PWideChar): SQLResult40;
begin
  Result := FCursor.getErrorMessage(Error);
end;

function TSQLCursor_Firebird_40.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult40;
begin
  Result := FCursor.getErrorMessageLen(ErrorLen);
end;

function TSQLCursor_Firebird_40.getInt64(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getInt64(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getLong(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getLong(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.GetOption(eOption: TSQLCursorOption; PropValue:
    Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult40;
begin
  Result := FCursor.GetOption(eOption, PropValue, MaxLength, Length);
end;

function TSQLCursor_Firebird_40.getShort(ColumnNumber: Word; Value: Pointer;
    var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getShort(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getString(ColumnNumber: Word; Value: PChar; var
    IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getString(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getTime(ColumnNumber: Word; Value: Pointer; var
    IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getTime(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getTimeStamp(ColumnNumber: Word; Value:
    Pointer; var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getTimeStamp(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.getWideString(ColumnNumber: Word; Value:
    PWideChar; var IsBlank: LongBool): SQLResult40;
begin
  Result := FCursor.getWideString(ColumnNumber, Value, IsBlank);
end;

function TSQLCursor_Firebird_40.isAutoIncrement(ColumnNumber: Word; var
    AutoIncr: LongBool): SQLResult40;
begin
  Result := FCursor.isAutoIncrement(ColumnNumber, AutoIncr);
end;

function TSQLCursor_Firebird_40.isBlobSizeExact(ColumnNumber: Word; var
    IsExact: LongBool): SQLResult40;
begin
  Result := FCursor.isBlobSizeExact(ColumnNumber, IsExact);
end;

function TSQLCursor_Firebird_40.isNullable(ColumnNumber: Word; var Nullable:
    LongBool): SQLResult40;
begin
  Result := FCursor.isNullable(ColumnNumber, Nullable);
end;

function TSQLCursor_Firebird_40.isReadOnly(ColumnNumber: Word; var ReadOnly:
    LongBool): SQLResult40;
begin
  Result := FCursor.isReadOnly(ColumnNumber, ReadOnly);
end;

function TSQLCursor_Firebird_40.isSearchable(ColumnNumber: Word; var
    Searchable: LongBool): SQLResult40;
begin
  Result := FCursor.isSearchable(ColumnNumber, Searchable);
end;

function TSQLCursor_Firebird_40.next: SQLResult40;
begin
  Result := FCursor.next;
end;

function TSQLCursor_Firebird_40.SetOption(eOption: TSQLCursorOption; PropValue:
    LongInt): SQLResult40;
begin
  Result := FCursor.SetOption(eOption, PropValue);
end;

end.
