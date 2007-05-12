unit vcl.dbx.testcase;

interface

uses Classes, TestFrameWork, TestExtensions, DB, SqlExpr;

type
  ITestData = interface(IInterface)
  ['{2DCC2E1F-BCE2-4D04-A61E-03DBFC031D0E}']
    function GetIsEmbedded: boolean;
    function GetName: string;
    procedure Setup(const aConnection: TSQLConnection);
    property Name: string read GetName;
    property IsEmbedded: boolean read GetIsEmbedded;
  end;

  TTestData_SQLConnection = class(TInterfacedObject, ITestData)
  private
    FName: string;
    FDriverName: string;
    FLibraryName: string;
    FGetDriverFunc: string;
    FVendorLib: string;
    FParams: WideString;
    FIsEmbedded: boolean;
  protected
    function GetIsEmbedded: boolean;
    function GetName: string;
  public
    constructor Create(const aName, aDriverName, aLibraryName, aGetDriverFunc,
        aVendorLib: string; const aIsEmbedded: boolean; const aParams: string);
    procedure Setup(const aConnection: TSQLConnection);
  end;

  ITestCase_DBX = interface(IInterface)
  ['{48656BDE-5C04-4CB6-895A-88139FD08E03}']
    procedure SetTestData(const I: ITestData);
  end;

  TTestCase_DBX = class(TTestCase, ITestCase_DBX)
  private
    FConnection: TSQLConnection;
    FTestData: ITestData;
    FSQLMonitor: TSQLMonitor;
    procedure SQLMonitorOnLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
    function IsTrimChar: boolean;
    procedure SetTestData(const aTestData: ITestData);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    class function NewSuite(const aTestData: ITestData): ITestSuite;
  end;

  TTestCase_DBX_General = class(TTestCase_DBX)
  published
    procedure Test_Connection_Property;
    procedure Test_Execute;
    procedure Test_ExecuteDirect;
    procedure Test_GetFieldNames;
    procedure Test_GetTableNames;
    procedure Test_Invalid_Login;
    procedure Test_Invalid_VendorLib;
    procedure Test_Open_Close;
    procedure Test_RecordCount;
    procedure Test_GetIndexNames;
  end;

  TTestCase_DBX_Transaction = class(TTestCase_DBX)
  published
    procedure Test_Transaction;
    procedure Test_Invalid_TransactionID;
    procedure Test_Duplicate_TransactionID;
    procedure Test_Transaction_1;
    procedure Test_Transaction_2;
    procedure Test_Transaction_RepeatableRead;
    procedure Test_Transaction_ReadCommitted;
  end;

  TTestCase_DBX_FieldType = class(TTestCase_DBX)
  private
    FDataSet: TSQLDataSet;
    FParams: TParams;
    procedure Execute;
    function Field: TField;
    function Param: TParam;
  protected
    FRequired: boolean;
    function GetFieldType: string; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure Test_Required;
  published
    procedure Test_BIGINT;
    procedure Test_BIGINT_Limit;
    procedure Test_BLOB;
    procedure Test_CHAR;
    procedure Test_DATE;
    procedure Test_DECIMAL;
    procedure Test_DECIMAL_LONG;
    procedure Test_DECIMAL_Limit;
    procedure Test_DOUBLE_PRECISION;
    procedure Test_FLOAT;
    procedure Test_INTEGER;
    procedure Test_NUMERIC;
    procedure Test_NUMERIC_SHORT;
    procedure Test_NUMERIC_LONG;
    procedure Test_NUMERIC_Limit;
    procedure Test_SMALLINT;
    procedure Test_TIME;
    procedure Test_TIMESTAMP;
    procedure Test_VARCHAR;
  end;

  TTest_DBX_FieldType_NOT_NULL = class(TTestCase_DBX_FieldType)
  protected
    function GetFieldType: string; override;
  end;

  TTestCase_DBX_TSQLDataSet = class(TTestCase_DBX)
  private
    FDataSet: TSQLDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_ctTable;
    procedure Test_GetRowsAffected;
    procedure Test_Field_ReadOnly;
    procedure Test_ctQuery;
  end;

  TTestSuite_DBX_Factory = class
  private
    class function MySuite(const aTestData: ITestData): ITestSuite;
    class function GetParams(const aDataBase, aExtraParams: string): string;
    class function NewTestDataList(const aParams: string): IInterfaceList;
    class procedure RegisterTest(const aParams: string);
  public
    class procedure Setup; 
  end;

implementation

uses SysUtils, DBXpress, SqlConst, Windows, StrUtils, FMTBcd,
  SqlTimSt, DateUtils, Math;

constructor TTestData_SQLConnection.Create(const aName, aDriverName,
    aLibraryName, aGetDriverFunc, aVendorLib: string; const aIsEmbedded:
    boolean; const aParams: string);
begin
  inherited Create;
  FName := aName;
  FDriverName := aName;
  FLibraryName := aLibraryName;
  FGetDriverFunc := aGetDriverFunc;
  FVendorLib := aVendorLib;
  FParams := aParams;
  FIsEmbedded := aIsEmbedded;
end;

function TTestData_SQLConnection.GetIsEmbedded: boolean;
begin
  Result := FIsEmbedded;
end;

function TTestData_SQLConnection.GetName: string;
begin
  Result := FName;
end;

procedure TTestData_SQLConnection.Setup(const aConnection: TSQLConnection);
begin
  aConnection.DriverName := FDriverName;
  aConnection.ConnectionName := Self.ClassName;
  aConnection.LibraryName := FLibraryName;
  aConnection.GetDriverFunc := FGetDriverFunc;
  aConnection.VendorLib := FVendorLib;
  aConnection.Params.Text := FParams;
end;

function TTestCase_DBX.IsTrimChar: boolean;
var b: longint;
    iLen: Smallint;
begin
  Assert(FConnection.SQLConnection.GetOption(eConnTrimChar, @b, SizeOf(b), iLen) = DBXERR_NONE);
  Result := boolean(b);
end;

class function TTestCase_DBX.NewSuite(const aTestData: ITestData):
    ITestSuite;
var i: integer;
begin
  Result := Suite;
  for i := 0 to Result.CountTestCases - 1 do
    (Result.Tests[i] as ITestCase_DBX).SetTestData(aTestData);
end;

procedure TTestCase_DBX.SetTestData(const aTestData: ITestData);
begin
  FTestData := aTestData;
end;

procedure TTestCase_DBX.SetUp;
begin
  inherited;
  FConnection := TSQLConnection.Create(nil);
  FTestData.Setup(FConnection);
  FSQLMonitor := TSQLMonitor.Create(nil);
  FSQLMonitor.SQLConnection := FConnection;
  FSQLMonitor.OnLogTrace := SQLMonitorOnLogTrace;
  FSQLMonitor.Active := True;
  FConnection.Open;
end;

procedure TTestCase_DBX.SQLMonitorOnLogTrace(Sender: TObject; CBInfo:
    pSQLTRACEDesc);
var T: String;
begin
  T := String(CBInfo^.pszTrace);
  Status(T);
end;

procedure TTestCase_DBX.TearDown;
begin
  inherited;
  FConnection.Close;
  FreeAndNil(FConnection);
  FreeAndNil(FSQLMonitor);
end;

procedure TTestCase_DBX_General.Test_Connection_Property;
var iSQLDialect: Longint;
    PropSize: Smallint;
begin
  CheckEquals(DBXERR_NONE, FConnection.SQLConnection.GetOption(eConnSqlDialect, @iSQLDialect, SizeOf(iSQLDialect), PropSize));
  CheckEquals(IntTostr(iSQLDialect), FConnection.Params.Values[SQLDIALECT_KEY]);

  CheckTrue(FConnection.TransactionsSupported);
  CheckTrue(FConnection.MultipleTransactionsSupported);
end;

procedure TTestCase_DBX_General.Test_Execute;
var pD: ^TSQLDataSet;
    D: TSQLDataSet;
    P: TParams;
begin
  New(pD);
  P := TParams.Create;
  try
    FConnection.Execute('SELECT * FROM RDB$RELATIONS', nil, pD);
    D := pD^;
    CheckEquals(16, D.FieldCount);
    CheckFalse(D.Eof);
    D.Free;

    P.CreateParam(ftInteger, '1', ptInput).AsInteger := 1;
    FConnection.Execute('SELECT * FROM RDB$RELATIONS WHERE 1=?', P, pD);
    D := pD^;
    CheckEquals(16, D.FieldCount);
    CheckFalse(D.Eof);
    D.Free;
  finally
    Dispose(pD);
    P.Free;
  end;
end;

procedure TTestCase_DBX_General.Test_ExecuteDirect;
begin
  FConnection.ExecuteDirect('DELETE FROM RDB$ROLES WHERE 1=2');
end;

procedure TTestCase_DBX_General.Test_GetFieldNames;
var L1, L2: TStringList;
begin
  FConnection.Open;
  L1 := TStringList.Create;
  L2 := TStringList.Create;
  try
    FConnection.GetFieldNames('RDB$RELATIONS', L1);
    L2.Add('RDB$VIEW_BLR');
    L2.Add('RDB$VIEW_SOURCE');
    L2.Add('RDB$DESCRIPTION');
    L2.Add('RDB$RELATION_ID');
    L2.Add('RDB$SYSTEM_FLAG');
    L2.Add('RDB$DBKEY_LENGTH');
    L2.Add('RDB$FORMAT');
    L2.Add('RDB$FIELD_ID');
    L2.Add('RDB$RELATION_NAME');
    L2.Add('RDB$SECURITY_CLASS');
    L2.Add('RDB$EXTERNAL_FILE');
    L2.Add('RDB$RUNTIME');
    L2.Add('RDB$EXTERNAL_DESCRIPTION');
    L2.Add('RDB$OWNER_NAME');
    L2.Add('RDB$DEFAULT_CLASS');
    L2.Add('RDB$FLAGS');

    L1.Sorted := True;
    L2.Sorted := True;

    CheckEquals(L2.Text, L1.Text);
  finally
    L1.Free;
    L2.Free;
  end;
end;

procedure TTestCase_DBX_General.Test_GetIndexNames;
var L: TStringList;
begin
  L := TStringList.Create;
  try
    FConnection.GetIndexNames('RDB$RELATIONS', L);
    CheckEquals('', L.Text);
  finally
    L.Free;
  end;
end;

procedure TTestCase_DBX_General.Test_GetTableNames;
var L: TStringList;
    iSystem: integer;
begin
  L := TStringList.Create;
  try
    FConnection.GetTableNames(L, True);
    iSystem := L.Count;
    CheckTrue(iSystem > 10);

    FConnection.GetTableNames(L, False);
    CheckTrue(iSystem >= 0);
  finally
    L.Free;
  end;
end;

procedure TTestCase_DBX_General.Test_Invalid_Login;
begin
  FConnection.Close;
  FConnection.Params.Values[szUSERNAME] := 'no.such.user';
  if not FTestData.IsEmbedded then
    StartExpectingException(EDatabaseError);
  FConnection.Open;
end;

procedure TTestCase_DBX_General.Test_Invalid_VendorLib;
begin
  FConnection.Close;
  FConnection.VendorLib := 'no.such.vendorlib';
  CheckException(FConnection.Open, EDatabaseError);
end;

procedure TTestCase_DBX_General.Test_Open_Close;
begin
  CheckTrue(FConnection.Connected);
  FConnection.Close;
  CheckFalse(FConnection.Connected);
end;

procedure TTestCase_DBX_General.Test_RecordCount;
var pD: ^TSQLDataSet;
    D: TSQLDataSet;
begin
  New(pD);
  try
    FConnection.Execute('SELECT * FROM RDB$RELATIONS', nil, pD);
    D := pD^;
    CheckNotEquals(0, D.RecordCount);
    D.Free;
  finally
    Dispose(pD);
  end;
end;

procedure TTestCase_DBX_FieldType.Execute;
var S: string;
begin
  if Assigned(FDataSet) then FreeAndNil(FDataSet);

  S := 'DELETE FROM T_FIELD';
  FConnection.Execute(S, nil);

  S := 'INSERT INTO T_FIELD (FIELD) VALUES (:VALUE)';
  CheckEquals(1, FConnection.Execute(S, FParams));

  S := 'SELECT * FROM T_FIELD';
  FConnection.Execute(S, nil, @FDataSet);
end;

function TTestCase_DBX_FieldType.Field: TField;
begin
  Result := FDataSet.Fields[0];
end;

function TTestCase_DBX_FieldType.GetFieldType: string;
begin
  FRequired := False;

       if GetName = 'Test_CHAR'             then Result := 'CHAR(100)'
  else if GetName = 'Test_VARCHAR'          then Result := 'VARCHAR(100)'
  else if GetName = 'Test_SMALLINT'         then Result := 'SMALLINT'
  else if GetName = 'Test_INTEGER'          then Result := 'INTEGER'
  else if GetName = 'Test_BIGINT'           then Result := 'BIGINT'
  else if GetName = 'Test_BIGINT_Limit'     then Result := 'BIGINT'
  else if GetName = 'Test_NUMERIC'          then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_NUMERIC_SHORT'    then Result := 'NUMERIC(4, 2)'
  else if GetName = 'Test_NUMERIC_LONG'     then Result := 'NUMERIC(9, 2)'
  else if GetName = 'Test_NUMERIC_Limit'    then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_DECIMAL'          then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_DECIMAL_LONG'     then Result := 'DECIMAL(9, 2)'
  else if GetName = 'Test_DECIMAL_Limit'    then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_FLOAT'            then Result := 'FLOAT'
  else if GetName = 'Test_DOUBLE_PRECISION' then Result := 'DOUBLE PRECISION'
  else if GetName = 'Test_DATE'             then Result := 'DATE'
  else if GetName = 'Test_TIME'             then Result := 'TIME'
  else if GetName = 'Test_TIMESTAMP'        then Result := 'TIMESTAMP'
  else if GetName = 'Test_BLOB'             then Result := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
  else
    raise Exception.CreateFmt('Field type not found for test %s', [GetName]);
end;

function TTestCase_DBX_FieldType.Param: TParam;
begin
  Result := FParams[0];
end;

procedure TTestCase_DBX_FieldType.SetUp;
var S: string;
    L: TStringList;
begin
  inherited;
  L := TStringList.Create;
  try
    FConnection.GetTableNames(L, False);
    if L.IndexOf('T_FIELD') <> -1 then
      FConnection.ExecuteDirect('DROP TABLE T_FIELD');
  finally
    L.Free;
  end;

  S := 'CREATE TABLE T_FIELD( ' +
       '   FIELD ' + GetFieldType +
       ')';
  FConnection.ExecuteDirect(S);

  FParams := TParams.Create;
  FParams.CreateParam(ftUnknown, 'VALUE', ptInput);
end;

procedure TTestCase_DBX_FieldType.TearDown;
begin
  if Assigned(FDataSet) then FreeAndNil(FDataSet);
  FParams.Free;
  FConnection.ExecuteDirect('DROP TABLE T_FIELD');
  inherited;
end;

procedure TTestCase_DBX_FieldType.Test_BIGINT;
begin
  Param.AsFMTBCD := StrToBcd('1234567890');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBcd), Field.DataSize);

  CheckEquals(Param.AsInteger, Field.AsInteger);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 12345678;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123;
  Execute;
  CheckEquals('123', Field.AsString);

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Param.AsString := '1234567890';
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_BIGINT_Limit;
var F: TFMTBCDField;
begin
  Param.AsFMTBCD := StrToBcd('9223372036854775807');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  F := Field as TFMTBCDField;
  CheckEquals(0, F.Size);
  CheckEquals(19, F.Precision);
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsFMTBCD := StrToBcd('-9223372036854775808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_BLOB;
begin
  Param.LoadFromFile('c:\windows\notepad.exe', ftBlob);
  Execute;
  CheckEquals(TBlobField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR;
var i: integer;
begin
  Param.AsString := 'CHAR';
  Execute;
  CheckEquals(TStringField, Field.ClassType);
  CheckEquals(101, Field.DataSize);
  CheckEquals(100, Field.Size);

  if IsTrimChar then
    i := 0
  else
    i := 96;
  CheckEquals(Param.AsString + DupeString(' ', i), Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_DATE;
begin
  Param.AsDate := Date;
  Execute;
  CheckEquals(TDateField, Field.ClassType);
  CheckEquals(4, Field.DataSize);

  CheckEquals(Param.AsDate, Field.AsDateTime);
  CheckEquals(Param.AsDateTime, Field.AsDateTime);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL;
begin
  Param.AsFMTBCD := StrToBcd('12345678901.2345');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBcd), Field.DataSize);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
  CheckEquals(Param.AsFloat, Field.AsFloat);

  Param.AsFMTBCD := StrToBcd('8000');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsCurrency := 12345678.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 1234.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123.123456;
  Execute;
  CheckEquals('123.1235', Field.AsString);

  Param.AsFloat := 123.123412;
  Execute;
  CheckEquals('123.1234', Field.AsString);

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL_Limit;
var F: TFMTBCDField;
begin
  Param.AsFMTBCD := StrToBcd('922337203685477.5807');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  F := Field as TFMTBCDField;
  CheckEquals(19, F.Precision);
  CheckEquals(4, F.Size);

  Param.AsFMTBCD := StrToBcd('-922337203685477.5808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL_LONG;
begin
  Param.AsFMTBCD := StrToBcd('234.56');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_DOUBLE_PRECISION;
var F: TFloatField;
begin
  Param.AsFloat := 1234567.12345678;
  Execute;
  CheckEquals(TFloatField, Field.ClassType);
  F := Field as TFloatField;
  CheckEquals(15, F.Precision);
  
  CheckEquals(Param.AsFloat, Field.AsFloat, 0.00000001);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_FLOAT;
var F: TFloatField;
begin
  Param.AsFloat := 1234.12345678;
  Execute;
  CheckEquals(TFloatField, Field.ClassType);
  F := Field as TFloatField;
  CheckEquals(15, F.Precision);

  CheckEquals(Param.AsFloat, Field.AsFloat, 0.0001);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_INTEGER;
begin
  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(TIntegerField, Field.ClassType);
  CheckEquals(Param.AsInteger, Field.AsInteger);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(Param.AsInteger, Field.AsInteger);

  Param.AsString := '1290345678';
  Execute;
  CheckEquals(Param.AsInteger, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC;
begin
  Param.AsFMTBCD := StrToBcd('12345678901.2345');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBcd), Field.DataSize);

  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
  CheckEquals(Param.AsFloat, Field.AsFloat);

  Param.AsFMTBCD := StrToBcd('8000');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsCurrency := 12345678.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 1234.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123.123456;
  Execute;
  CheckEquals('123.1235', Field.AsString);

  Param.AsFloat := 123.123412;
  Execute;
  CheckEquals('123.1234', Field.AsString);

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_Limit;
var F: TFMTBCDField;
begin
  Param.AsFMTBCD := StrToBcd('922337203685477.5807');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  F := Field as TFMTBCDField;
  CheckEquals(19, F.Precision);
  CheckEquals(4, F.Size);
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsFMTBCD := StrToBcd('-922337203685477.5808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_LONG;
begin
  Param.AsFMTBCD := StrToBcd('234.56');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_SHORT;
begin
  Param.AsFMTBCD := StrToBcd('12.34');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsSmallInt, Field.AsInteger);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_Required;
begin
  if FRequired then StartExpectingException(EDatabaseError);
  Param.Clear;
  Execute;
  CheckTrue(Field.IsNull);
end;

procedure TTestCase_DBX_FieldType.Test_SMALLINT;
begin
  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(TSmallintField, Field.ClassType);
  CheckEquals(2, Field.DataSize);

  CheckEquals(Param.AsSmallInt, Field.AsInteger);

  Param.AsInteger := 23451;
  Execute;
  CheckEquals(Param.AsSmallInt, Field.AsInteger);

  Param.AsString := '32145';
  Execute;
  CheckEquals(Param.AsSmallInt, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_TIME;
begin
  Param.AsTime := RecodeMilliSecond(Time, 0);
  Execute;
  CheckEquals(TTimeField, Field.ClassType);
  CheckEquals(4, Field.DataSize);

  CheckEquals(Param.AsTime, Field.AsDateTime);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_TIMESTAMP;
var T: TSQLTimeStamp;
begin
  T := DateTimeToSQLTimeStamp(Now);
  T.Fractions := 0;
  Param.AsSQLTimeStamp := T;
  Execute;
  CheckEquals(TSQLTimeStampField, Field.ClassType);
  CheckEquals(16, Field.DataSize);

  CheckEquals(Param.AsString, Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_VARCHAR;
var F: TStringField;
begin
  Param.AsString := 'VARCHAR';
  Execute;
  CheckEquals(TStringField, Field.ClassType);
  F := Field as TStringField;
  CheckEquals(101, F.DataSize);
  CheckEquals(100, F.Size);

  CheckEquals(Param.AsString, Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_TSQLDataSet.SetUp;
var S: string;
    L: TStringList;
begin
  inherited;
  L := TStringList.Create;
  try
    FConnection.GetTableNames(L, False);
    if L.IndexOf('T_DATASET') <> -1 then
      FConnection.ExecuteDirect('DROP TABLE T_DATASET');
  finally
    L.Free;
  end;

  S := 'CREATE TABLE T_DATASET( ' +
       '   FIELD VARCHAR(100)' +
       ')';
  FConnection.ExecuteDirect(S);

  FDataSet := TSQLDataSet.Create(nil);
  FDataSet.SQLConnection := FConnection;
end;

procedure TTestCase_DBX_TSQLDataSet.TearDown;
begin
  FDataSet.Free;
  FConnection.ExecuteDirect('DROP TABLE T_DATASET');
  inherited;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_ctTable;
begin
  FDataSet.CommandType := ctTable;
  FDataSet.CommandText := 'T_DATASET';
  FDataSet.Open;
  CheckTrue(FDataSet.Active);
  FDataSet.Close;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_ctQuery;
var L: TStringList;
begin
  L := TStringList.Create;
  try
    FDataSet.CommandType := ctQuery;
    FDataSet.CommandText := 'SELECT FIELD AS Field1 FROM T_DATASET';
    FDataSet.Open;
    CheckTrue(Assigned(FDataSet.FindField('Field1')));
    FDataSet.Close;
  finally
    L.Free;
  end;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_GetRowsAffected;
var L: TStringList;
begin
  L := TStringList.Create;
  try
    FDataSet.CommandType := ctTable;
    FDataSet.CommandText := 'T_DATASET';
    FDataSet.Open;
    FDataSet.Prepared := False;
    FDataSet.Close;
  finally
    L.Free;
  end;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_Field_ReadOnly;
var L: TStringList;
begin
  L := TStringList.Create;
  try
    FDataSet.CommandType := ctTable;
    FDataSet.CommandText := 'T_DATASET';
    FDataSet.Open;
    CheckFalse(FDataSet.Fields[0].ReadOnly);
    FDataSet.Close;
  finally
    L.Free;
  end;
end;

function TTest_DBX_FieldType_NOT_NULL.GetFieldType: string;
begin
  Result := inherited GetFieldType + ' NOT NULL';
  FRequired := True;
end;

procedure TTestCase_DBX_Transaction.Test_Duplicate_TransactionID;
var T1, T2: TTransactionDesc;
begin
  T1.TransactionID := 1;
  T1.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T1);

  T2.TransactionID := 1;
  T2.IsolationLevel := xilREADCOMMITTED;
  StartExpectingException(EDatabaseError);
  FConnection.StartTransaction(T2);
end;

procedure TTestCase_DBX_Transaction.Test_Invalid_TransactionID;
var T: TTransactionDesc;
begin
  StartExpectingException(EDatabaseError);
  T.TransactionID := 0;
  T.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T);
end;

procedure TTestCase_DBX_Transaction.Test_Transaction;
var T: TTransactionDesc;
begin
  ZeroMemory(@T, SizeOf(T));
  T.TransactionID := 1;
  T.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T);
  FConnection.Commit(T);

  ZeroMemory(@T, SizeOf(T));
  T.TransactionID := 1;
  T.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T);
  FConnection.Rollback(T);
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_1;
var T: TTransactionDesc;
begin
  ZeroMemory(@T, SizeOf(T));
  T.TransactionID := 1;
  T.IsolationLevel := xilREADCOMMITTED;
  Self.StartExpectingException(EDatabaseError);
  try
    FConnection.StartTransaction(T);
    try
      FConnection.ExecuteDirect('CREATE TABLE T_TRANSACTION(FIELD INTEGER)');
      FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(123)');
      FConnection.Commit(T);
    except
      FConnection.Rollback(T);
      raise;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_TRANSACTION');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_2;
var T: TTransactionDesc;
    pD: ^TSQLDataSet;
begin
  ZeroMemory(@T, SizeOf(T));
  T.TransactionID := 1;
  T.IsolationLevel := xilREADCOMMITTED;

  FConnection.ExecuteDirect('CREATE TABLE T_TRANSACTION(FIELD INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(123)');

  FConnection.StartTransaction(T);
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(456)');
  FConnection.Rollback(T);

  New(pD);
  try
    FConnection.Execute('SELECT COUNT(*) FROM T_TRANSACTION', nil, pD);
    CheckEquals(1, pD^.Fields[0].AsInteger);
    pD^.Free;
  finally
    Dispose(pD);
  end;

  FConnection.ExecuteDirect('DROP TABLE T_TRANSACTION');
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_ReadCommitted;
var T1, T2: TTransactionDesc;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    T1.TransactionID := 1;
    T1.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(T1);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    T2.TransactionID := 2;
    T2.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(T2);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.Commit(T2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.Commit(T1);

    CheckEquals('ITEM-01', V1);
    CheckEquals('ITEM-02', V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_RepeatableRead;
var T1, T2: TTransactionDesc;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    T1.TransactionID := 1;
    T1.IsolationLevel := xilREPEATABLEREAD;
    FConnection.StartTransaction(T1);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    T2.TransactionID := 2;
    T2.IsolationLevel := xilREPEATABLEREAD;
    FConnection.StartTransaction(T2);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.Commit(T2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.Commit(T1);

    CheckEquals(V1, V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

class function TTestSuite_DBX_Factory.MySuite(const aTestData: ITestData): ITestSuite;
var S: TTestSuite;
begin
  S := TTestSuite.Create(aTestData.Name);
  S.AddSuite(TTestCase_DBX_General.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_Transaction.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_FieldType.NewSuite(aTestData));
  S.AddSuite(TTest_DBX_FieldType_NOT_NULL.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_TSQLDataSet.NewSuite(aTestData));
  Result := S as ITestSuite;
end;

class function TTestSuite_DBX_Factory.GetParams(const aDataBase, aExtraParams:
    string): string;
begin
  Result := SQLDIALECT_KEY + '=3'
         + #13#10 + szUSERNAME + '=SYSDBA'
         + #13#10 + szPASSWORD + '=masterkey'
         + #13#10 + ROLENAME_KEY + '=RoleName'
         + #13#10 + 'ServerCharSet='
         + #13#10 + 'BlobSize=-1'
         + #13#10 + 'LocaleCode=0000'
         + #13#10 + 'Interbase TransIsolation=ReadCommited'
         + #13#10 + 'Database=' + aDatabase
         + #13#10 + aExtraParams
         ;
end;

class function TTestSuite_DBX_Factory.NewTestDataList(const aParams: string):
    IInterfaceList;
var S, sDriver: string;
begin
  Result := TInterfaceList.Create;

  S := GetParams(Format('localhost:%sserver.15.fdb', [ExtractFilePath(ParamStr(0))]), aParams);

  sDriver := {$if CompilerVersion=15}'g:\bin\dbexpint.dll'{$else}'g:\bin\dbxint30.dll'{$ifend};
  Result.Add(
    TTestData_SQLConnection.Create(
      'Borland DBX Interbase Driver (Server)', 'INTERBASE', sDriver,
      'getSQLDriverINTERBASE', 'g:\bin\fbclient.1.5.3.dll', False, S
    )
  );

  sDriver := {$if CompilerVersion=15}'g:\bin\dbxbyfb30.dll'{$else}'g:\bin\dbxbyfb30.dll'{$ifend};
  Result.Add(
    TTestData_SQLConnection.Create(
      'TeamOO DBX Firebird Driver (Server))', 'INTERBASE', sDriver,
      'getSQLDriverFIREBIRD', 'g:\bin\fbclient.1.5.3.dll', False, S
    )
  );

  Result.Add(
    TTestData_SQLConnection.Create(
      'TeamOO DBX Firebird Driver (Embedded, ODS 10.1)', 'INTERBASE', sDriver,
      'getSQLDriverFIREBIRD', 'g:\bin\fbembed.10.1\fbembed.dll', True,
      GetParams(Format('%sembed.15.fdb', [ExtractFilePath(ParamStr(0))]), aParams)
    )
  );

  Result.Add(
    TTestData_SQLConnection.Create(
      'TeamOO DBX Firebird Driver (Embedded, ODS 11.0)', 'INTERBASE', sDriver,
      'getSQLDriverFIREBIRD', 'g:\bin\fbembed.11.0\fbembed.dll', True,
      GetParams(Format('%sembed.20.fdb', [ExtractFilePath(ParamStr(0))]), aParams)
    )
  );

  Result.Add(
    TTestData_SQLConnection.Create(
      'Upscene DBX Firebird Driver (Server)', 'INTERBASE', 'g:\bin\dbxup_fb.dll',
      'getSQLDriverFB', 'g:\bin\fbclient.1.5.3.dll', False, S
    )
  );
end;

class procedure TTestSuite_DBX_Factory.RegisterTest(const aParams: string);
var L: IInterfaceList;
    i: integer;
    T: ITestSuite;
begin
  T := TTestSuite.Create('TSQLConnection: ' + StringReplace(aParams, #13#10, ' , ', [rfReplaceAll]));
  L := NewTestDataList(aParams);
  for i := 0 to L.Count - 1 do
    T.AddSuite(MySuite(L[i] as ITestData));
  TestFrameWork.RegisterTest(T);
end;

class procedure TTestSuite_DBX_Factory.Setup;
begin
  RegisterTest('CommitRetain=False'
    + #13#10 + WAITONLOCKS_KEY + '=False'
    + #13#10 + 'Trim Char=False'
  );

  RegisterTest('CommitRetain=False'
    + #13#10 + WAITONLOCKS_KEY + '=False'
    + #13#10 + 'Trim Char=True'
  );
end;

initialization
  TTestSuite_DBX_Factory.Setup;
end.
