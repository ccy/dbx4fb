unit vcl.dbx.testcase;

interface

uses TestFrameWork, TestExtensions, DB, SqlExpr;

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
  strict private
    FName: string;
    FLibraryName: string;
    FGetDriverFunc: string;
    FVendorLib: string;
    FParams: WideString;
    FIsEmbedded: boolean;
  protected
    function GetIsEmbedded: boolean;
    function GetName: string;
  public
    constructor Create(const aName, aLibraryName, aGetDriverFunc, aVendorLib:
        string; const aIsEmbedded: boolean; const aParams: WideString);
    procedure Setup(const aConnection: TSQLConnection);
  end;

  TTestCase_DBX = class abstract(TTestCase)
  private
    FConnection: TSQLConnection;
    FTestData: ITestData;
    FSQLMonitor: TSQLMonitor;
    procedure SQLMonitorOnLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
    function IsTrimChar: boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    class var CurrentTestData: ITestData;
    constructor Create(MethodName: string); override;
    class function NewSuite(const aTestData: ITestData): ITestSuite;
  end;

  TTestCase_DBX_General = class(TTestCase_DBX)
  published
    procedure Test_Connection_Property;
    procedure Test_Execute;
    procedure Test_ExecuteDirect;
    procedure Test_GetFieldNames;
    procedure Test_Invalid_Login;
    procedure Test_Invalid_VendorLib;
    procedure Test_Open_Close;
    procedure Test_RecordCount;
    procedure Test_Transaction;
    procedure Test_Transaction_1;
    procedure Test_Transaction_2;
  end;

  TTestCase_DBX_FieldType = class(TTestCase_DBX)
  private
    FDataSet: TSQLDataSet;
    FParams: TParams;
    procedure Execute;
    function Field: TField;
    function GetFieldType: string;
    function Param: TParam;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_BIGINT;
    procedure Test_BIGINT_Limit;
    procedure Test_CHAR;
    procedure Test_DATE;
    procedure Test_DECIMAL;
    procedure Test_DECIMAL_Limit;
    procedure Test_DOUBLE_PRECISION;
    procedure Test_FLOAT;
    procedure Test_INTEGER;
    procedure Test_NUMERIC;
    procedure Test_NUMERIC_Limit;
    procedure Test_SMALLINT;
    procedure Test_TIME;
    procedure Test_TIMESTAMP;
    procedure Test_VARCHAR;
  end;

implementation

uses SysUtils, Classes, DBXpress, SqlConst, Windows, StrUtils, FMTBcd,
  SqlTimSt, DateUtils, Math;

constructor TTestData_SQLConnection.Create(const aName, aLibraryName,
    aGetDriverFunc, aVendorLib: string; const aIsEmbedded: boolean; const
    aParams: WideString);
begin
  inherited Create;
  FName := aName;
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
  aConnection.DriverName := FName;
  aConnection.ConnectionName := Self.ClassName;
  aConnection.LibraryName := FLibraryName;
  aConnection.GetDriverFunc := FGetDriverFunc;
  aConnection.VendorLib := FVendorLib;
  aConnection.Params.Text := FParams;
end;

constructor TTestCase_DBX.Create(MethodName: string);
begin
  inherited Create(MethodName);
  FTestData := CurrentTestData;
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
begin
  CurrentTestData := aTestData;
  Result := Suite;
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
    L2.Add('RDB$DBKEY_LENGTH');
    L2.Add('RDB$DEFAULT_CLASS');
    L2.Add('RDB$DESCRIPTION');
    L2.Add('RDB$EXTERNAL_DESCRIPTION');
    L2.Add('RDB$EXTERNAL_FILE');
    L2.Add('RDB$FIELD_ID');
    L2.Add('RDB$FLAGS');
    L2.Add('RDB$FORMAT');
    L2.Add('RDB$OWNER_NAME');
    L2.Add('RDB$RELATION_ID');
    L2.Add('RDB$RELATION_NAME');
    L2.Add('RDB$RUNTIME');
    L2.Add('RDB$SECURITY_CLASS');
    L2.Add('RDB$SYSTEM_FLAG');
    L2.Add('RDB$VIEW_BLR');
    L2.Add('RDB$VIEW_SOURCE');

    CheckEquals(L2.Text, L1.Text);
  finally
    L1.Free;
    L2.Free;
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

procedure TTestCase_DBX_General.Test_Transaction;
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

procedure TTestCase_DBX_General.Test_Transaction_1;
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

procedure TTestCase_DBX_General.Test_Transaction_2;
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

procedure TTestCase_DBX_FieldType.Execute;
var S: string;
begin
  if Assigned(FDataSet) then FreeAndNil(FDataSet);

  S := 'DELETE FROM T_FIELD';
  FConnection.Execute(S, nil);

  S := 'INSERT INTO T_FIELD (FIELD) VALUES (:VALUE)';
  FConnection.Execute(S, FParams);

  S := 'SELECT * FROM T_FIELD';
  FConnection.Execute(S, nil, @FDataSet);
end;

function TTestCase_DBX_FieldType.Field: TField;
begin
  Result := FDataSet.Fields[0];
end;

function TTestCase_DBX_FieldType.GetFieldType: string;
begin
       if GetName = 'Test_CHAR'             then Result := 'CHAR(100)'
  else if GetName = 'Test_VARCHAR'          then Result := 'VARCHAR(100)'
  else if GetName = 'Test_SMALLINT'         then Result := 'SMALLINT'
  else if GetName = 'Test_INTEGER'          then Result := 'INTEGER'
  else if GetName = 'Test_BIGINT'           then Result := 'BIGINT'
  else if GetName = 'Test_BIGINT_Limit'     then Result := 'BIGINT'
  else if GetName = 'Test_NUMERIC'          then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_NUMERIC_Limit'    then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_DECIMAL'          then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_DECIMAL_Limit'    then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_FLOAT'            then Result := 'FLOAT'
  else if GetName = 'Test_DOUBLE_PRECISION' then Result := 'DOUBLE PRECISION'
  else if GetName = 'Test_DATE'             then Result := 'DATE'
  else if GetName = 'Test_TIME'             then Result := 'TIME'
  else if GetName = 'Test_TIMESTAMP'        then Result := 'TIMESTAMP'
  else
    raise Exception.CreateFmt('Field type not found for test %s', [GetName]);
end;

function TTestCase_DBX_FieldType.Param: TParam;
begin
  Result := FParams[0];
end;

procedure TTestCase_DBX_FieldType.SetUp;
var S: string;
begin
  inherited;
  S := 'CREATE TABLE T_FIELD( ' +
       '   FIELD ' + GetFieldType +
       ')';
  FConnection.ExecuteDirect(S);

  FParams := TParams.Create;
  FParams.CreateParam(ftUnknown, 'VALUE', ptInput);
end;

procedure TTestCase_DBX_FieldType.TearDown;
begin
  FConnection.ExecuteDirect('DROP TABLE T_FIELD');
  FParams.Free;
  if Assigned(FDataSet) then FreeAndNil(FDataSet);
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

procedure TTestCase_DBX_FieldType.Test_SMALLINT;
begin
  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(TSmallintField, Field.ClassType);
  CheckEquals(2, Field.DataSize);

  CheckEquals(Param.AsSmallInt, Field.AsInteger);
end;

procedure TTestCase_DBX_FieldType.Test_TIME;
begin
  Param.AsTime := RecodeMilliSecond(Time, 0);
  Execute;
  CheckEquals(TTimeField, Field.ClassType);
  CheckEquals(4, Field.DataSize);

  CheckEquals(Param.AsTime, Field.AsDateTime);
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
end;

function MySuite(const aTestData: ITestData): ITestSuite;
var S: TTestSuite;
begin
  S := TTestSuite.Create(aTestData.Name);
  S.AddSuite(TTestCase_DBX_General.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_FieldType.NewSuite(aTestData));
  Result := S as ITestSuite;
end;

procedure xxx(const aParams: WideString);
var P: WideString;
    T: ITestSuite;
    C: ITestData;
begin
  T := TTestSuite.Create('TSQLConnection: ' + StringReplace(aParams, #13#10, ' , ', [rfReplaceAll]));

  P := Format('Database=localhost:%sserver.15.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + 'RoleName=' +
       #13#10 + aParams;

  C := TTestData_SQLConnection.Create('Borland DBX Interbase Driver (Server)', 'g:\bin\dbxint30.dll', 'getSQLDriverINTERBASE', 'g:\bin\fbclient.1.5.3.dll', False, P);
  T.AddSuite(MySuite(C));

  C := TTestData_SQLConnection.Create('Upscene DBX Firebird Driver (Server)', 'g:\bin\dbxup_fb.dll', 'getSQLDriverFB', 'g:\bin\fbclient.1.5.3.dll', False, P);
  T.AddSuite(MySuite(C));

  C := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Server))', 'dbxfb.dll', 'getSQLDriverFIREBIRD', 'g:\bin\fbclient.1.5.3.dll', False, P);
  T.AddSuite(MySuite(C));

  P := Format('Database=%sembed.15.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + 'RoleName=' +
       #13#10 + aParams;

  C := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Embedded, ODS 10.1)', 'dbxfb.dll', 'getSQLDriverFIREBIRD', Format('%sfbembed.10.1\fbembed.dll', [ExtractFilePath(ParamStr(0))]), True, P);
  T.AddSuite(MySuite(C));

  P := Format('Database=%sembed.20.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + aParams;

  C := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Embedded, ODS 11.0)', 'dbxfb.dll', 'getSQLDriverFIREBIRD', Format('%sfbembed.11.0\fbembed.dll', [ExtractFilePath(ParamStr(0))]), True, P);
  T.AddSuite(MySuite(C));

//  TestFrameWork.RegisterTest(TRepeatedTest.Create(T, 1));
  TestFrameWork.RegisterTest(T);
end;

var P: WideString;

initialization
  P := 'CommitRetain=False'
       + #13#10 + 'WaitOnLocks=False'
       + #13#10 + 'Trim Char=False'
       ;
  xxx(P);

  P := 'CommitRetain=False'
       + #13#10 + 'WaitOnLocks=False'
       + #13#10 + 'Trim Char=True'
       ;
//  xxx(P);
end.
