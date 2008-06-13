unit vcl.dbx.testcase;

interface

uses Classes, TestFrameWork, TestExtensions, DB, SqlExpr, Provider, DBClient
     {$if CompilerVersion > 18}
     , DBXCommon
     {$ifend}
     ;

type{$M+}
  EDBXError = {$if CompilerVersion<=18}EDatabaseError{$else}TDBXError{$ifend};

  ITestData = interface(IInterface)
  ['{2DCC2E1F-BCE2-4D04-A61E-03DBFC031D0E}']
    function GetODS: string;
    function GetName: string;
    procedure Setup(const aConnection: TSQLConnection);
    property Name: string read GetName;
  end;

  TTestData_SQLConnection = class(TInterfacedObject, ITestData)
  private
    FDriverName: string;
    FGetDriverFunc: string;
    FLibraryName: string;
    FName: string;
    FParams: WideString;
    FVendorLib: string;
    FODS: string;
    procedure CreateDatabase;
  protected
    function GetODS: string;
    function GetName: string;
    procedure Setup(const aConnection: TSQLConnection);
  public
    constructor Create(const aDriverName, aLibraryName, aGetDriverFunc, aVendorLib,
        aParams: string);
    procedure BeforeDestruction; override;
  end;

  TTestSuite_DBX = class abstract
  protected
    class procedure CheckTestDataFile;
    class function GetTestDataFileName: string;
    class function GetParams(const aHostName, aExtraParams: string): string;
    class function GetServerVersion(aLibraryName, aParams: string): string;
  end;

  TTestSuite_DBX1 = class abstract(TTestSuite_DBX)
  private
    class function NewTestDataList(const aParams: string): IInterfaceList;
    class procedure RegisterTest(const aParams: string);
    class function Suite(const aTestData: ITestData): ITestSuite;
  public
    class procedure Setup;
  end;

  TTestSuite_DBX2 = class abstract(TTestSuite_DBX)
  private
    class function NewTestDataList(const aParams: string): IInterfaceList;
  public
    class procedure Setup;
  end;

  ITestCase_DBX1 = interface(IInterface)
  ['{48656BDE-5C04-4CB6-895A-88139FD08E03}']
    procedure SetTestData(const I: ITestData);
  end;

  ITestCase_DBX2 = interface(IInterface)
  ['{E56373C3-BD6E-444C-B11D-78A8BB842DC6}']
    procedure SetTestData(const I1, I2: ITestData);
  end;

  TTestCase_DBX = class(TTestCase, ITestCase_DBX1)
  private
    FConnection: TSQLConnection;
    FTestData: ITestData;                                           
    FSQLMonitor: TSQLMonitor;
    {$if CompilerVersion <= 18}
    procedure SQLMonitorOnLogTrace(Sender: TObject; CBInfo: pSQLTRACEDesc);
    {$else}
    procedure SQLMonitorOnLogTrace(Sender: TObject; TraceInfo: TDBXTraceInfo);
    {$ifend}
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
    procedure Test_CAST_SQL_DECIMAL_Bug;
    procedure Test_Connection_Property;
    procedure Test_Execute;
    procedure Test_Execute_Commit;
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
    procedure Test_Transaction_WaitLock;
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
    procedure Test_DATETIME;
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

  TTestCase_DBX_TSQLStoredProc = class(TTestCase_DBX)                             
  private
    FStoredProc: TSQLStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_1;
  end;

  TTestCase_DBX_DataSnap = class(TTestCase_DBX)
  private
    FDataSet: TSQLDataSet;
    FDSP: TDataSetProvider;
    FCDS: TClientDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Repeated_Open;
    procedure Test_Master_Detail;
    procedure Test_Self_Manage_Transaction;
  end;

  TTestCase_DBX_Server_Embed = class(TTestCase, ITestCase_DBX2)
  private
    FTestData1: ITestData;
    FTestData2: ITestData;
  protected
    procedure SetTestData(const I1, I2: ITestData);
  public
    class function NewSuite(const aTestData1, aTestData2: ITestData): ITestSuite;
  published
    procedure Test_Unavailable_Database;
  end;

  TTestCase_DBX_TParam = class(TTestCase_DBX)
  private
    FCDS: TClientDataSet;
    FDataSet: TSQLDataSet;
    FDSP: TDataSetProvider;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Param_Integer;
    procedure Test_Param_LargeInt1;
    procedure Test_Param_LargeInt2;
    procedure Test_Param_String;
  end;

implementation

uses SysUtils, DBXpress, SqlConst, Windows, StrUtils, FMTBcd,
  SqlTimSt, DateUtils, Math, IniFiles, firebird.client, firebird.service,
  UniqueID;

constructor TTestData_SQLConnection.Create(const aDriverName, aLibraryName,
    aGetDriverFunc, aVendorLib, aParams: string);
begin
  inherited Create;
  FDriverName := aDriverName;
  FLibraryName := aLibraryName;
  FGetDriverFunc := aGetDriverFunc;
  FVendorLib := aVendorLib;
  FParams := aParams;
  
  CreateDatabase;
end;

procedure TTestData_SQLConnection.BeforeDestruction;
var L: TStringList;
    S: IFirebirdService;
begin
  L := TStringList.Create;
  try
    L.Text := FParams;
    S := TFirebirdServiceFactory.New(FVendorLib, L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
    S.DropDatabase(L.Values[DATABASENAME_KEY]);
  finally
    L.Free;
  end;
end;

procedure TTestData_SQLConnection.CreateDatabase;
var L: TStringList;
    S: IFirebirdService;
    sDatabase: string;
    sImpl: string;
begin
  L := TStringList.Create;
  try
    L.Text := FParams;
    S := TFirebirdServiceFactory.New(FVendorLib, L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);

    sImpl := S.GetServerImplementation;
    if L.Values[HOSTNAME_KEY] = '' then
      sDatabase := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))
    else if ContainsText(sImpl, 'Windows') then begin
      if SameText(L.Values[HOSTNAME_KEY], 'localhost') then
        sDatabase := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))
      else
        sDatabase := 'c:\'
    end else if ContainsText(sImpl, 'Linux') then
      sDatabase := '/tmp/'
    else
      Assert(False);
    sDatabase := sDatabase + TUniqueName.New('T_');

    S.CreateDatabase(sDatabase);
    FODS := S.GetODSVersion(sDatabase);

    L.Values[DATABASENAME_KEY] := sDatabase;
    FParams := L.Text;

    FName := Format('%s (%s) Host: %s Database: %s', [S.GetServerVersion, sImpl, L.Values[HOSTNAME_KEY], sDatabase]);
  finally
    L.Free;
  end;
end;

function TTestData_SQLConnection.GetName: string;
begin
  Result := FName;
end;

function TTestData_SQLConnection.GetODS: string;
begin
  Result := FODS;
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

class procedure TTestSuite_DBX.CheckTestDataFile;
var F: TIniFile;
begin
  if FileExists(GetTestDataFileName) then Exit;

  F := TIniFile.Create(GetTestDataFileName);
  try
    F.WriteString('driver', 'getSQLDriverFIREBIRD', 'dbxfb30.dll');
    F.WriteString('embedded', 'embedded_1', 'fbembed.dll');
    F.WriteString('server', 'server_1', 'localhost');
    F.WriteString('vendor', 'default', 'fbclient.1.5.3.dll');
    F.UpdateFile;
  finally
    F.Free;
  end;
end;

class function TTestSuite_DBX.GetParams(const aHostName, aExtraParams:
    string): string;
begin
  Result := DRIVERNAME_KEY + '=' + TUniqueName.New 
            + #13#10 + SQLDIALECT_KEY + '=3'
            + #13#10 + szUSERNAME + '=SYSDBA'
            + #13#10 + szPASSWORD + '=masterkey'
            + #13#10 + ROLENAME_KEY + '=RoleName'
            + #13#10 + 'ServerCharSet='
            + #13#10 + 'BlobSize=-1'
            + #13#10 + 'LocaleCode=0000'
            + #13#10 + 'Interbase TransIsolation=ReadCommited'
            + #13#10 + aExtraParams
            ;

  if aHostName <> '' then
    Result := Result + #13#10
              + HOSTNAME_KEY + '=' + aHostName;
end;

class function TTestSuite_DBX.GetTestDataFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

class function TTestSuite_DBX.GetServerVersion(aLibraryName, aParams:
    string): string;
var L: TStringList;
    S: IFirebirdService;
begin
  L := TStringList.Create;
  L.Text := aParams;
  try
    S := TFirebirdServiceFactory.New(aLibraryName, L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
    Result := S.GetServerVersion;
  finally
    L.Free;
  end;
end;

function TTestCase_DBX.IsTrimChar: boolean;
var b: longint;
    {$if CompilerVersion <= 18}iLen: Smallint;{$ifend}
begin
  {$if CompilerVersion <= 18}
  Assert(FConnection.SQLConnection.GetOption(eConnTrimChar, @b, SizeOf(b), iLen) = DBXERR_NONE);
  Result := boolean(b);
  {$else}
  Result := SameText(FConnection.Params.Values[TRIMCHAR], 'True');
  {$ifend}
end;

class function TTestCase_DBX.NewSuite(const aTestData: ITestData):
    ITestSuite;
var i: integer;
begin
  Result := Suite;
  for i := 0 to Result.CountTestCases - 1 do
    (Result.Tests[i] as ITestCase_DBX1).SetTestData(aTestData);
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

{$if CompilerVersion<=18}
procedure TTestCase_DBX.SQLMonitorOnLogTrace(Sender: TObject; CBInfo:
    pSQLTRACEDesc);
var T: String;
begin
  T := String(CBInfo^.pszTrace);
  Status(T);
end;
{$else}
procedure TTestCase_DBX.SQLMonitorOnLogTrace(Sender: TObject;
  TraceInfo: TDBXTraceInfo);
begin
  Status(TraceInfo.Message);
end;
{$ifend}

procedure TTestCase_DBX.TearDown;
begin
  inherited;
  FConnection.Close;
  FreeAndNil(FConnection);
  FreeAndNil(FSQLMonitor);
end;

procedure TTestCase_DBX_General.Test_CAST_SQL_DECIMAL_Bug;
var pD: ^TSQLDataSet;
    D: TSQLDataSet;
    S: string;
begin
  //refer to BU-00010
  New(pD);
  try
    S := 'CREATE TABLE T_TEST1 ' +
            '( ' +
            '  DTLKEY INTEGER NOT NULL, ' +
            '  CURRENCYRATE DECIMAL(14, 6), ' +
            '  AMOUNT DECIMAL(16, 2), ' +
            '  SQTY	DECIMAL(18, 4), ' +
            '  PRIMARY KEY(DTLKEY) ' +
             ') ';
    FConnection.ExecuteDirect(S);

    S := 'INSERT INTO T_TEST1 (DTLKEY, CURRENCYRATE, AMOUNT, SQTY) ' +
         'VALUES (1, 9011.2, 1482.8, 1)';
    FConnection.ExecuteDirect(S);

    S := 'INSERT INTO T_TEST1 (DTLKEY, CURRENCYRATE, AMOUNT, SQTY) ' +
         'VALUES (2, 9317.2, 2965.6, 2)';
    FConnection.ExecuteDirect(S);

    StartExpectingException(TDBXError);
    S := 'SELECT CAST(Amount * CurrencyRate AS DECIMAL(18, 8)) / SQTY AS UnitPrice ' +
             'FROM T_TEST1';
    try
      FConnection.Execute(S, nil, pD);
    finally
      D := pD^;
      D.Free;
    end;

  finally
    FConnection.ExecuteDirect('DROP TABLE T_TEST1');
    Dispose(pD);
  end;
end;

procedure TTestCase_DBX_General.Test_Connection_Property;
begin
  CheckTrue(FConnection.TransactionsSupported);
  CheckTrue(FConnection.MultipleTransactionsSupported);
end;

procedure TTestCase_DBX_General.Test_Execute;
var pD: ^TSQLDataSet;
    D: TSQLDataSet;
    P: TParams;
    iCount: integer;
begin
  iCount := 16;
  if FTestData.GetODS >= '11.1' then
    iCount := 17;

  New(pD);
  P := TParams.Create;
  try
    FConnection.Execute('SELECT * FROM RDB$RELATIONS', nil, pD);
    D := pD^;
    CheckEquals(iCount, D.FieldCount);
    CheckFalse(D.Eof);
    D.Free;

    P.CreateParam(ftInteger, '1', ptInput).AsInteger := 1;
    FConnection.Execute('SELECT * FROM RDB$RELATIONS WHERE 1=?', P, pD);
    D := pD^;
    CheckEquals(iCount, D.FieldCount);
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

procedure TTestCase_DBX_General.Test_Execute_Commit;
begin
  FConnection.ExecuteDirect('COMMIT');
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
    if FTestData.GetODS >= '11.1' then
      L2.Add('RDB$RELATION_TYPE');
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
  if FConnection.Params.IndexOfName(HOSTNAME_KEY) <> -1 then
    StartExpectingException(EDBXError);
  FConnection.Open;
end;

procedure TTestCase_DBX_General.Test_Invalid_VendorLib;
begin
  FConnection.Close;
  FConnection.VendorLib := 'no.such.vendorlib';
  CheckException(FConnection.Open, EDBXError);
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
  else if GetName = 'Test_DATETIME'         then Result := 'DATE'
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
var S: TStringStream;
begin
  Param.LoadFromFile('c:\windows\notepad.exe', ftBlob);
  Execute;
  CheckEquals(TBlobField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);

  S := TStringStream.Create(DupeString('a', 65535));
  try
    Param.LoadFromStream(S, ftBlob);
  finally
    S.Free;
  end;
  Execute;
  CheckEquals(Param.AsString, Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR;
var i: integer;
begin
  Param.AsString := 'CHAR';
  Execute;
  CheckFalse(Field.IsNull);
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

procedure TTestCase_DBX_FieldType.Test_DATETIME;
begin
  {$Message 'QC#47267 - Encounter "No value for parameter" error for ftDateTime Param in DBX4'}
  {$if CompilerVersion = 18.5}
  StartExpectingException(EDatabaseError);
  {$ifend}
  Param.AsDateTime := Date;
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

  Param.AsString := '56789.12349991234';
  Execute;
  CheckEquals('56789.1234', Field.AsString);

  Param.AsString := '-3.41060513164848E-13';
  Execute;
  CheckEquals(0, Field.AsFloat);

  Param.AsString := '0.1';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.01';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.0001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.1';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.01';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.0001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

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

  Param.AsString := '56789.12349991234';
  Execute;
  CheckEquals('56789.1234', Field.AsString);

  Param.AsString := '-3.41060513164848E-13';
  Execute;
  CheckEquals(0, Field.AsFloat);

  Param.AsString := '0.1';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.01';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.0001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '0.00001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.1';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.01';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.0001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := '-0.00001';
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

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
  CheckEquals(FRequired, Field.Required);
  if FRequired then StartExpectingException(EDBXError);
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
  CheckFalse(Field.IsNull);
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
{$if CompilerVersion <= 18}var T1, T2: TTransactionDesc;{$ifend}
begin
  {$if CompilerVersion <= 18}
  T1.TransactionID := 1;
  T1.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T1);

  T2.TransactionID := 1;
  T2.IsolationLevel := xilREADCOMMITTED;
  StartExpectingException(EDBXError);
  FConnection.StartTransaction(T2);
  {$ifend}
end;

procedure TTestCase_DBX_Transaction.Test_Invalid_TransactionID;
{$if CompilerVersion <= 18}var T: TTransactionDesc;{$ifend}
begin
  {$if CompilerVersion <= 18}
  StartExpectingException(EDatabaseError);
  T.TransactionID := 0;
  T.IsolationLevel := xilREADCOMMITTED;
  FConnection.StartTransaction(T);
  {$ifend}
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
  Self.StartExpectingException(EDBXError);
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
var TTestSuite_DBX1, TTestSuite_DBX2: TTransactionDesc;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    TTestSuite_DBX1.TransactionID := 1;
    TTestSuite_DBX1.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(TTestSuite_DBX1);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    TTestSuite_DBX2.TransactionID := 2;
    TTestSuite_DBX2.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(TTestSuite_DBX2);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.Commit(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.Commit(TTestSuite_DBX1);

    CheckEquals('ITEM-01', V1);
    CheckEquals('ITEM-02', V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_RepeatableRead;
var TTestSuite_DBX1, TTestSuite_DBX2: TTransactionDesc;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    TTestSuite_DBX1.TransactionID := 1;
    TTestSuite_DBX1.IsolationLevel := xilREPEATABLEREAD;
    FConnection.StartTransaction(TTestSuite_DBX1);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    TTestSuite_DBX2.TransactionID := 2;
    TTestSuite_DBX2.IsolationLevel := xilREPEATABLEREAD;
    FConnection.StartTransaction(TTestSuite_DBX2);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.Commit(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.Commit(TTestSuite_DBX1);

    CheckEquals(V1, V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_WaitLock;
var TTestSuite_DBX1, TTestSuite_DBX2: TTransactionDesc;
    D: ^TSQLDataSet;
    V1: integer;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_LOCK(FIELD1 VARCHAR(10), FIELD2 INTEGER)');

  New(D);
  try
    TTestSuite_DBX1.TransactionID := 1;
    TTestSuite_DBX1.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(TTestSuite_DBX1);
    try
      FConnection.ExecuteDirect('INSERT INTO T_LOCK VALUES(''ITEM-01'', 1)');

      TTestSuite_DBX2.TransactionID := 2;
      TTestSuite_DBX2.IsolationLevel := xilREADCOMMITTED;
      FConnection.StartTransaction(TTestSuite_DBX2);
      try
        FConnection.Execute('SELECT COUNT(*) FROM T_LOCK', nil, D);
        V1 := D^.Fields[0].AsInteger;
        D^.Free;
        FConnection.Commit(TTestSuite_DBX2);
        CheckEquals(0, V1);
      except
        FConnection.Rollback(TTestSuite_DBX2);
        raise;
      end;
      FConnection.Commit(TTestSuite_DBX1);
    except
      FConnection.Rollback(TTestSuite_DBX1);
      raise;
    end;
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_LOCK');
  end;
end;


procedure TTestCase_DBX_DataSnap.SetUp;
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
  FDataSet.CommandType := ctTable;
  FDataSet.CommandText := 'T_DATASET';

  FDSP := TDataSetProvider.Create(nil);
  FDSP.DataSet := FDataSet;

  FCDS := TClientDataSet.Create(nil);
end;

procedure TTestCase_DBX_DataSnap.TearDown;
begin
  FDSP.Free;
  FCDS.Free;
  FDataSet.Free;
  FConnection.ExecuteDirect('DROP TABLE T_DATASET');
  inherited;
end;

procedure TTestCase_DBX_DataSnap.Test_Master_Detail;
var S: string;
    D: TSQLDataSet;
    DS: TDataSource;
begin
  S := 'CREATE TABLE T_DETAIL( ' +
       '   FIELD1 VARCHAR(100), ' +
       '   FIELD2 VARCHAR(100) ' +
       ')';
  FConnection.ExecuteDirect(S);

  S := 'INSERT INTO T_DATASET VALUES(''ABC'')';
  FConnection.ExecuteDirect(S);

  try
    DS := TDataSource.Create(nil);
    try
      DS.DataSet := FDataSet;
      D := TSQLDataSet.Create(nil);
      try
        D.Name := 'SQLDataSet_Detail';
        D.SQLConnection := FConnection;
        D.CommandType := ctTable;
        D.CommandText := 'T_DETAIL';
        D.DataSource := DS;
        FCDS.SetProvider(FDSP);
        FCDS.Open;
      finally
        D.Free;
      end;
    finally
      DS.Free;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_DETAIL');
  end;
end;

procedure TTestCase_DBX_DataSnap.Test_Repeated_Open;
begin
  FCDS.SetProvider(FDSP);
  FCDS.Open;
  CheckTrue(FCDS.Active);
  FCDS.Close;
  CheckFalse(FCDS.Active);
  FCDS.SetProvider(FDSP);
  FCDS.Open;
end;

procedure TTestCase_DBX_DataSnap.Test_Self_Manage_Transaction;
var T: TTransactionDesc;
    i: integer;
begin
  for i := 1 to 10 do begin
    T.GlobalID := 1;
    T.IsolationLevel := xilREADCOMMITTED;
    FConnection.StartTransaction(T);
    try
      FCDS.SetProvider(FDSP);
      FCDS.Open;
      FCDS.Close;
      FConnection.Commit(T);
    except
      FConnection.Rollback(T);
      raise;
    end;
  end;
end;

{ TTestCase_DBX_TSQLStoredProc }

procedure TTestCase_DBX_TSQLStoredProc.SetUp;
var S: string;
    i: integer;
begin
  inherited;
  S := 'CREATE TABLE T_STOREDPROC( ' +
       '   FIELD VARCHAR(100)' +
       ')';
  FConnection.ExecuteDirect(S);

  S := 'INSERT INTO T_STOREDPROC VALUES(''%d'')';
  for i := 1 to 9 do
    FConnection.ExecuteDirect(Format(S, [i]));

  FStoredProc := TSQLStoredProc.Create(nil);
  FStoredProc.SQLConnection := FConnection;
end;

procedure TTestCase_DBX_TSQLStoredProc.TearDown;
begin
  FStoredProc.Free;
  FConnection.ExecuteDirect('DROP TABLE T_STOREDPROC');
  inherited;
end;

procedure TTestCase_DBX_TSQLStoredProc.Test_1;
var S: string;
begin
  S := 'CREATE PROCEDURE PROC ' +
       'RETURNS (Max_Field VARCHAR(100)) ' +
       'AS ' +
       'BEGIN ' +
         'SELECT MAX(FIELD) ' +
         'FROM T_STOREDPROC ' +
         'INTO :Max_Field; ' +
         'SUSPEND; ' +
       'END ';
  FConnection.ExecuteDirect(S);

  FStoredProc.StoredProcName := 'PROC';
  FStoredProc.ExecProc;
  CheckEquals('9', FStoredProc.ParamByName('Max_Field').AsString);
  FStoredProc.Close;

  FConnection.ExecuteDirect('DROP PROCEDURE PROC');
end;

class function TTestCase_DBX_Server_Embed.NewSuite(const aTestData1,
    aTestData2: ITestData): ITestSuite;
var i: integer;
begin
  Result := Suite;
  for i := 0 to Result.CountTestCases - 1 do
    (Result.Tests[i] as ITestCase_DBX2).SetTestData(aTestData1, aTestData2);
end;

{ TTestCase_DBX_Server_Embed }

procedure TTestCase_DBX_Server_Embed.SetTestData(const I1, I2: ITestData);
begin
  FTestData1 := I1;
  FTestData2 := I2;
end;

procedure TTestCase_DBX_Server_Embed.Test_Unavailable_Database;
var C1, C2: TSQLConnection;
begin
  C1 := TSQLConnection.Create(nil);
  C2 := TSQLConnection.Create(nil);
  try
    FTestData1.Setup(C1);
    C1.Open;
    CheckTrue(C1.Connected);

    FTestData2.Setup(C2);
    C2.Open;
    CheckTrue(C2.Connected);
  finally
    C2.Free;
    C1.Free;
  end;
end;

class function TTestSuite_DBX1.NewTestDataList(const aParams: string): IInterfaceList;
var F: TIniFile;
    sDrivers, sServers, sEmbeds: TStringList;
    i: integer;
    j: Integer;
    sParams: string;
    sVer: string;
begin
  Result := TInterfaceList.Create;

  F := TIniFile.Create(GetTestDataFileName);
  sDrivers := TStringList.Create;
  sServers := TStringList.Create;
  sEmbeds := TStringList.Create;
  try
    F.ReadSectionValues('driver', sDrivers);
    F.ReadSectionValues('server', sServers);
    F.ReadSectionValues('embedded', sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        sParams := GetParams(sServers.ValueFromIndex[j], aParams);

        sVer := GetServerVersion(F.ReadString('vendor', 'default', ''), sParams);

        Result.Add(
          TTestData_SQLConnection.Create('INTERBASE', sDrivers.ValueFromIndex[i],
          sDrivers.Names[i], F.ReadString('vendor', sVer, sVer), sParams)
        );
      end;
      for j := 0 to sEmbeds.Count - 1 do begin
        sParams := GetParams('', aParams);

        sVer := GetServerVersion(sEmbeds.ValueFromIndex[j], sParams);

        Result.Add(
          TTestData_SQLConnection.Create('INTERBASE', sDrivers.ValueFromIndex[i],
          sDrivers.Names[i], sEmbeds.ValueFromIndex[j], sParams)
        );
      end;
    end;
  finally
    sDrivers.Free;
    sServers.Free;
    sEmbeds.Free;
    F.Free;
  end;
end;

class procedure TTestSuite_DBX1.RegisterTest(const aParams: string);
var L: IInterfaceList;
    i: integer;
    T: ITestSuite;
begin
  T := TTestSuite.Create('TSQLConnection: ' + StringReplace(aParams, #13#10, ' , ', [rfReplaceAll]));
  L := NewTestDataList(aParams);
  for i := 0 to L.Count - 1 do
    T.AddSuite(Suite(L[i] as ITestData));
  TestFrameWork.RegisterTest(T);
end;

class procedure TTestSuite_DBX1.Setup;
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

class function TTestSuite_DBX1.Suite(const aTestData: ITestData): ITestSuite;
var S: TTestSuite;
begin
  S := TTestSuite.Create(aTestData.Name);
  S.AddSuite(TTestCase_DBX_General.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_Transaction.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_FieldType.NewSuite(aTestData));
  S.AddSuite(TTest_DBX_FieldType_NOT_NULL.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_TSQLDataSet.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_DataSnap.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_TParam.NewSuite(aTestData));
//  S.AddSuite(TTestCase_DBX_TSQLStoredProc.NewSuite(aTestData));
  Result := S as ITestSuite;
end;

class function TTestSuite_DBX2.NewTestDataList(const aParams: string): IInterfaceList;
var F: TIniFile;
    sDrivers, sServers, sEmbeds: TStringList;
    i: integer;
    j, k: Integer;
    sParams1, sParams2: string;
    sVer1: string;
    L: IInterfaceList;
begin
  Result := TInterfaceList.Create;

  F := TIniFile.Create(GetTestDataFileName);
  sDrivers := TStringList.Create;
  sServers := TStringList.Create;
  sEmbeds := TStringList.Create;
  try
    F.ReadSectionValues('driver', sDrivers);
    F.ReadSectionValues('server', sServers);
    F.ReadSectionValues('embedded', sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        sParams1 := GetParams(sServers.ValueFromIndex[j], aParams);
        sVer1 := GetServerVersion(F.ReadString('vendor', 'default', ''), sParams1);

        for k := 0 to sEmbeds.Count - 1 do begin
          L := TInterfaceList.Create;

          L.Add(
            TTestData_SQLConnection.Create('INTERBASE', sDrivers.ValueFromIndex[i],
            sDrivers.Names[i], F.ReadString('vendor', sVer1, sVer1), sParams1)
          );

          sParams2 := GetParams('', aParams);
          L.Add(
            TTestData_SQLConnection.Create('INTERBASE1', sDrivers.ValueFromIndex[i],
            sDrivers.Names[i], sEmbeds.ValueFromIndex[k], sParams2)
          );

          Result.Add(L);
        end;
      end;
    end;
  finally
    sDrivers.Free;
    sServers.Free;
    sEmbeds.Free;
    F.Free;
  end;
end;

class procedure TTestSuite_DBX2.Setup;
var L, M: IInterfaceList;
    i: integer;
    T, S: ITestSuite;
    D0, D1: ITestData;
begin
  T := TTestSuite.Create('Test Unavailable Database');
  L := NewTestDataList('');
  for i := 0 to L.Count - 1 do begin
    M := L[i] as IInterfaceList;

    D0 := M[0] as ITestData;
    D1 := M[1] as ITestData;

    S := TTestSuite.Create(D0.Name + '  ' + D1.Name);
    S.AddSuite(
      TTestCase_DBX_Server_Embed.NewSuite(D0, D1)
    );

    T.AddSuite(S);
  end;
  TestFrameWork.RegisterTest(T);
end;

procedure TTestCase_DBX_TParam.SetUp;
var S: string;
begin
  inherited;
  S := 'CREATE TABLE T_PARAM( ' +
          'FIELD_INT INTEGER, ' +
          'FIELD_STR VARCHAR(10), ' +
          'FIELD_BIGINT BIGINT' +
       ')';
  FConnection.ExecuteDirect(S);

  S := 'INSERT INTO T_PARAM VALUES(1, ''1'', 1)';
  FConnection.ExecuteDirect(S);

  FDataSet := TSQLDataSet.Create(nil);
  FDataSet.SQLConnection := FConnection;
  FDataSet.CommandType := ctQuery;
  FDataSet.CommandText := 'SELECT * FROM T_PARAM';

  FDSP := TDataSetProvider.Create(nil);
  FDSP.DataSet := FDataSet;

  FCDS := TClientDataSet.Create(nil);
  FCDS.SetProvider(FDSP);
end;

procedure TTestCase_DBX_TParam.TearDown;
begin
  FDSP.Free;
  FCDS.Free;
  FDataSet.Free;
  FConnection.ExecuteDirect('DROP TABLE T_PARAM');
  inherited;
end;

procedure TTestCase_DBX_TParam.Test_Param_Integer;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftInteger, 'Field_Int', ptInput);
  try
    P.AsInteger := 1;
    FCDS.Open;
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_LargeInt1;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftLargeInt, 'Field_BigInt', ptInput);
  try
    P.Value := 1;
    FCDS.Open;
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_LargeInt2;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftLargeInt, 'Field_BigInt', ptInput);
  try
    P.AsFmtBcd := StrToBcd('1');
    FCDS.Open;
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_String;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftString, 'Field_Str', ptInput);
  try
    P.AsString := '1';
    FCDS.Open;
  finally
    P.Free;
  end;
end;

initialization
  TTestSuite_DBX.CheckTestDataFile;
  TTestSuite_DBX1.Setup;
  TTestSuite_DBX2.Setup;
end.
