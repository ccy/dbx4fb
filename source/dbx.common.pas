unit dbx.common;

interface

uses DBXpress;

type
  TDBXOptions = class(TObject)
  private
    FBlobSize: integer;
    FConnQualifiedName: WideString;
    FRoleName: WideString;
    FSQLDialect: longint;
    FTransIsolationLevel: TTransIsolationLevel;
    FTrimChar: boolean;
    FDBXCallBackEvent: TSQLCallBackEvent;
    FDBXCallBackInfo: integer;
    FHostName: string;
    function GetConnQuotedObjectName: WideString;
  public
    property BlobSize: integer read FBlobSize write FBlobSize;
    property ConnQualifiedName: WideString read FConnQualifiedName write FConnQualifiedName;
    property ConnQuotedObjectName: WideString read GetConnQuotedObjectName;
    property DBXCallBackEvent: TSQLCallBackEvent read FDBXCallBackEvent write FDBXCallBackEvent;
    property DBXCallBackInfo: integer read FDBXCallBackInfo write FDBXCallBackInfo;
    property HostName: string read FHostName write FHostName;
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

  TFieldColumns = array of TFieldColumn;

  IMetaDataProvider = interface(IInterface)
  ['{DD1D1CA9-D72A-48E3-B5C5-F32846FE805D}']
    function GetColumnCount: integer;
    function GetColumnLength(const aColNo: Word): LongWord;
    function GetColumnName(const aColNo: Word): WideString;
    function GetColumnNameLength(const aColNo: Word): Word;
    function GetColumnPrecision(const aColNo: Word): Smallint;
    function GetColumnScale(const aColNo: Word): Smallint;
    function GetColumnType(const aColNo: Word): Word;
    function GetColumnSubType(const aColNo: Word): Word;
    function IsNullable(const aColNo: Word): boolean;
  end;

implementation

{ TDBXOptions }

function TDBXOptions.GetConnQuotedObjectName: WideString;
begin
  Result := '"' + FConnQualifiedName + '"';
end;

end.
