unit dbx.common;

interface

uses DBXpress;

type
  TDBXOptions = class(TObject)
  private
    FRoleName: WideString;
    FSQLDialect: longint;
    FTransIsolationLevel: TTransIsolationLevel;
    FTrimChar: boolean;
  public
    property RoleName: WideString read FRoleName write FRoleName;
    property SQLDialect: longint read FSQLDialect write FSQLDialect;
    property TransIsolationLevel: TTransIsolationLevel read FTransIsolationLevel write FTransIsolationLevel;
    property TrimChar: boolean read FTrimChar write FTrimChar;
  end;

  TFieldColumn = record
    Name: WideString;
    FieldType: word;
    Size: word;
  end;

  TFieldColumns = array[1..14] of TFieldColumn;

  IMetaDataProvider = interface(IInterface)
  ['{DD1D1CA9-D72A-48E3-B5C5-F32846FE805D}']
    function GetColumnCount: integer;
    function GetColumnLength(const aColNo: Word): LongWord;
    function GetColumnName(const aColNo: Word): WideString;
    function GetColumnPrecision(const aColNo: Word): Smallint;
    function GetColumnScale(const aColNo: Word): Smallint;
    function GetColumnType(const aColNo: Word): Word;
    function GetColumnSubType(const aColNo: Word): Word;
    function IsNullable(const aColNo: Word): boolean;
  end;

implementation

end.
