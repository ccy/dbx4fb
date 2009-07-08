unit vcl.dbx.testcase;

interface

uses SysUtils, Classes, DB, SqlExpr, Provider, DBClient, FMTBcd,
     TestFrameWork, TestExtensions
     {$if CompilerVersion > 18}, DBXCommon{$ifend}
     {$if CompilerVersion >= 20}, dbx.firebird{$ifend}
     ;

type{$M+}
  EDBXError = {$if CompilerVersion<=18}EDatabaseError{$else}TDBXError{$ifend};

  ITestData = interface(IInterface)
  ['{2DCC2E1F-BCE2-4D04-A61E-03DBFC031D0E}']
    function GetODS: string;
    function GetName: string;
    function GetServerVersion: string;
    procedure Setup(const aConnection: TSQLConnection);
    property ServerVersion: string read GetServerVersion;
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
    FServerVersion: string;
    procedure CreateDatabase;
  protected
    function GetODS: string;
    function GetName: string;
    function GetServerVersion: string;
    procedure Setup(const aConnection: TSQLConnection);
  public
    constructor Create(const aDriverName, aLibraryName, aGetDriverFunc, aVendorLib,
        aParams: string);
    procedure BeforeDestruction; override;
  end;

  TTestSuite_DBX = class abstract
  protected
    class function GetDriverSectionName: string;
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
    property GetTestData: ITestData read FTestData;
  public
    function StrToBcdN(const aValue: string): TBcd;
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
    procedure Test_ServerCharSet;
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
    procedure Test_CHAR_UTF8;
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
    procedure Test_VARCHAR_UTF8;
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
    procedure Test_Param_LargeInt3;
    procedure Test_Param_String;
  end;

  TTestCase_DBX_TSQLStoredProc = class(TTestCase_DBX)
  private
    FStoredProc: TSQLStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_GetProcedureNames;
    procedure Test_MultiParams;
    procedure Test_ReturnDataSet;
  end;

  TTestCase_DBX_TSQLStoredProc_Params = class(TTestCase_DBX)
  private
    FStoredProc: TSQLStoredProc;
    function CreateProc(const aDecl, aImpl: string): Integer;
    function CreateProc2(const aDecl, aImpl: string): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure Test_Blob;
  published
    procedure Test_BigInt;
    procedure Test_Char;
    procedure Test_Date;
    procedure Test_Decimal_18;
    procedure Test_Decimal_4;
    procedure Test_Decimal_8;
    procedure Test_DoublePrecision;
    procedure Test_Float;
    procedure Test_Integer;
    procedure Test_Numeric_18;
    procedure Test_Numeric_4;
    procedure Test_Numeric_8;
    procedure Test_SmallInt;
    procedure Test_Time;
    procedure Test_TimeStamp;
    procedure Test_VarChar;
  end;

implementation

uses SqlConst, Windows, StrUtils, WideStrings,
  SqlTimSt, DateUtils, Math, IniFiles,
  SystemEx, SysUtilsEx, firebird.client, firebird.service, UniqueID;

constructor TTestData_SQLConnection.Create(const aDriverName, aLibraryName,
    aGetDriverFunc, aVendorLib, aParams: string);
begin
  inherited Create;
  FDriverName := aDriverName;

  FLibraryName := ExpandFileNameString(aLibraryName);

  FGetDriverFunc := aGetDriverFunc;

  FVendorLib := ExpandFileNameString(aVendorLib);
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

      if Pos('dbxint', FLibraryName) > 0 then // Interbase Driver need hostname string in database parameter
        sDatabase := L.Values[HOSTNAME_KEY] + ':';

      if AnsiStartsText('localhost', L.Values[HOSTNAME_KEY]) then
        sDatabase := sDatabase + IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))
      else
        sDatabase := sDatabase + 'c:\';
    end else if ContainsText(sImpl, 'Linux') then
      sDatabase := sDatabase + '/tmp/'
    else
      Assert(False);
    sDatabase := sDatabase + TUniqueName.New('T_');

    S.CreateDatabase(sDatabase);
    FODS := S.GetODSVersion(sDatabase);

    L.Values[DATABASENAME_KEY] := sDatabase;
    FParams := L.Text;

    FServerVersion := S.GetServerVersion;
    FName := Format('%s (%s) Host: %s Database: %s', [sImpl, FServerVersion, L.Values[HOSTNAME_KEY], sDatabase]);
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

function TTestData_SQLConnection.GetServerVersion: string;
begin
  Result := FServerVersion;
end;

procedure TTestData_SQLConnection.Setup(const aConnection: TSQLConnection);
begin
  aConnection.DriverName := FDriverName;
  aConnection.LibraryName := FLibraryName;
  aConnection.GetDriverFunc := FGetDriverFunc;
  aConnection.VendorLib := FVendorLib;
  aConnection.Params.Text := FParams;
end;

class procedure TTestSuite_DBX.CheckTestDataFile;
var F: TIniFile;
    sDriver: string;
begin
  if not FileExists(GetTestDataFileName) then begin
    F := TIniFile.Create(GetTestDataFileName);
    try
      sDriver := {$if CompilerVersion<=18.5}'dbxfb40.dll'{$else}'dbxfbu40.dll'{$ifend};
      F.WriteString(GetDriverSectionName, 'getSQLDriverFIREBIRD', sDriver);
      F.WriteString('embedded', 'embedded_1', 'fbembed.dll');
      F.WriteString('server', 'server_1', 'localhost');
      F.WriteString('vendor', 'default', 'fbclient.1.5.5.dll');
      F.UpdateFile;
    finally
      F.Free;
    end;
  end;

  SetEnvironmentVariable('drivers', PChar(ExtractFilePath(GetTestDataFileName) + 'drivers'));
end;

class function TTestSuite_DBX.GetDriverSectionName: string;
begin
  Result := {$ifdef Unicode}'driver.unicode'{$else}'driver'{$endif};
end;

class function TTestSuite_DBX.GetParams(const aHostName, aExtraParams:
    string): string;
begin
  Result := {$if CompilerVersion=18.5}DRIVERNAME_KEY + '=' + TUniqueName.New + #13#10 + {$ifend}
            SQLDIALECT_KEY + '=3'
            + #13#10 + szUSERNAME + '=SYSDBA'
            + #13#10 + szPASSWORD + '=masterkey'
            + #13#10 + ROLENAME_KEY + '=RoleName'
            + #13#10 + SQLSERVER_CHARSET_KEY + '=NONE'
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
  Result := ParamStr(1);
end;

class function TTestSuite_DBX.GetServerVersion(aLibraryName, aParams:
    string): string;
var L: TStringList;
    S: IFirebirdService;
begin
  L := TStringList.Create;
  L.Text := aParams;
  try
    S := TFirebirdServiceFactory.New(ExpandfileNameString(aLibraryName), L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
    Result := S.GetServerVersion;
  finally
    L.Free;
  end;
end;

function TTestCase_DBX.IsTrimChar: boolean;
{$if CompilerVersion <= 18}
var b: longint;
    iLen: Smallint;
{$ifend}
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

function TTestCase_DBX.StrToBcdN(const aValue: string): TBcd;
begin
  Result := StrToBcd(StrToLocaleDecimal((aValue)));
end;

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

procedure TTestCase_DBX_General.Test_ServerCharSet;
var S: string;
    D: TSQLDataSet;
begin
  FConnection.Close;
  FConnection.Params.Values[SQLSERVER_CHARSET_KEY] := 'WIN1252';
  FConnection.Open;

  S := 'CREATE TABLE T_TEST_CHARSET ( ' +
         'S_WIN1252 CHAR(1) CHARACTER SET WIN1252, ' +
         'S_ISO8859_13 CHAR(1) CHARACTER SET ISO8859_13 ' +
       ') ';
  FConnection.ExecuteDirect(S);

  S := Format('INSERT INTO T_TEST_CHARSET(S_WIN1252, S_ISO8859_13) VALUES (''%s'', ''%s'')', [#$9E, #$9E]);
  FConnection.ExecuteDirect(S);

  // Test WIN1252 Transliteration
  FConnection.Execute('SELECT S_WIN1252, S_ISO8859_13 FROM T_TEST_CHARSET', nil, @D);
  try
    CheckEquals(#$9E, D.Fields[0].AsString);
    CheckEquals(#$9E, D.Fields[1].AsString);
  finally
    D.Free;
  end;

  // Test NONE Transliteration
  FConnection.Close;
  FConnection.Params.Values[SQLSERVER_CHARSET_KEY] := 'NONE';
  FConnection.Open;

  FConnection.Execute('SELECT S_WIN1252, S_ISO8859_13 FROM T_TEST_CHARSET', nil, @D);
  try
    CheckEquals(#$9E, D.Fields[0].AsString);
    CheckEquals(#$FE, D.Fields[1].AsString);
  finally
    D.Free;
  end;

  FConnection.ExecuteDirect('DROP TABLE T_TEST_CHARSET');
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
var L1, L2: TWideStringList;
begin
  FConnection.Open;
  L1 := TWideStringList.Create;
  L2 := TWideStringList.Create;
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
var L: TWideStringList;
begin
  L := TWideStringList.Create;
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
    i: integer;
begin
  if Assigned(FDataSet) then FreeAndNil(FDataSet);

  S := 'DELETE FROM T_FIELD';
  FConnection.Execute(S, nil);

  S := 'INSERT INTO T_FIELD (FIELD) VALUES (:VALUE)';
  i := FConnection.Execute(S, FParams);
  CheckEquals(1, i);

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
  else if GetName = 'Test_CHAR_UTF8'        then Result := 'CHAR(100) CHARACTER SET UTF8'
  else if GetName = 'Test_VARCHAR'          then Result := 'VARCHAR(100)'
  else if GetName = 'Test_VARCHAR_UTF8'     then Result := 'VARCHAR(100) CHARACTER SET UTF8'
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
  {$if CompilerVersion <= 18.5}
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBCD), Field.DataSize);
  {$else}
  CheckEquals(TLargeintField, Field.ClassType);
  CheckEquals(SizeOf(LargeInt), Field.DataSize);
  {$ifend}

  CheckEquals(Param.AsInteger, Field.AsInteger);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 12345678;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123;
  Execute;
  CheckEquals('123', Field.AsString);

  {$ifdef Unicode}
  Param.AsLargeInt := 9223372036854775807;
  Execute;
  CheckEquals(9223372036854775807, Field.Value);
  {$endif}

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Param.AsString := '1234567890';
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsWideString := '1234567890';
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_BIGINT_Limit;
var F: TNumericField;
begin
  Param.AsFMTBCD := StrToBcd('9223372036854775807');
  Execute;
  {$if CompilerVersion <= 18.5}
  CheckEquals(TFMTBCDField, Field.ClassType);
  {$else}
  CheckEquals(TLargeintField, Field.ClassType);
  {$ifend}

  F := Field as TNumericField;

  CheckEquals(0, F.Size);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsFMTBCD := StrToBcd('-9223372036854775808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
end;

procedure TTestCase_DBX_FieldType.Test_BLOB;
var M: TStringStream;
    S: AnsiString;
begin
  Param.LoadFromFile('c:\windows\notepad.exe', ftBlob);
  Execute;
  CheckEquals(TBlobField, Field.ClassType);

  {$if CompilerVersion <= 18.5}
  S := Param.AsString;
  {$else}
  SetLength(S, Param.GetDataSize);
  Move(Param.AsBlob[0], S[1], Param.GetDataSize);
  {$ifend}
  CheckEquals(string(S), Field.AsString);

  M := TStringStream.Create(DupeString('a', 65535));
  try
    Param.LoadFromStream(M, ftBlob);
  finally
    M.Free;
  end;
  Execute;

  {$if CompilerVersion <= 18.5}
  S := Param.AsString;
  {$else}
  SetLength(S, Param.GetDataSize);
  Move(Param.AsBlob[0], S[1], Param.GetDataSize);
  {$ifend}
  CheckEquals(string(S), Field.AsString);

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

  i := 0;
  if not IsTrimChar then
    i := Field.Size - Length(Param.AsString);

  CheckEquals(Param.AsString + DupeString(' ', i), Field.AsString);
  CheckEquals(Param.AsWideString + DupeString(' ', i), Field.AsWideString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR_UTF8;
var i: integer;
    W: WideString;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;

  Param.AsWideString := 'One World One Dream ' +
                        #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C + ' ' +
                        #$540C + #$4E00 + #$4E2A + #$68A6 + #$60F3;

  Execute;
  CheckFalse(Field.IsNull);
  CheckEquals(TWideStringField, Field.ClassType);
  CheckEquals(202, Field.DataSize);
  CheckEquals(100, Field.Size);

  i := 0;
  if not IsTrimChar then
    i := Field.Size - Length(Param.AsWideString);

  W := Param.AsWideString;
  while i > 0 do begin
    W := W + ' ';
    Dec(i);
  end;
  CheckEquals(W, Field.AsWideString);

  Param.AsWideString := DupeString(#$540C, 100);
  Execute;
  CheckEquals(Param.AsWideString, Field.AsWideString);

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
  CheckEquals(Param.AsWideString, Field.AsWideString);
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
  {$if CompilerVersion = 20}
  Exit;
  {$ifend}
  Param.AsDateTime := Date;
  Execute;
  CheckEquals(TDateField, Field.ClassType);
  CheckEquals(4, Field.DataSize);

  CheckEquals(Param.AsDate, Field.AsDateTime);
  CheckEquals(Param.AsDateTime, Field.AsDateTime);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL;
begin
  Param.AsFMTBCD := StrToBcdN('12345678901.2345');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBcd), Field.DataSize);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
  CheckEquals(Param.AsFloat, Field.AsFloat);

  Param.AsFMTBCD := StrToBcd('8000');
  Execute;
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsCurrency := 12345678.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 1234.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123.123456;
  Execute;
  CheckEquals(StrToLocaleDecimal('123.1235'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('123.1235'), Field.AsWideString);

  Param.AsFloat := 123.123412;
  Execute;
  CheckEquals(StrToLocaleDecimal('123.1234'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('123.1234'), Field.AsWideString);

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Param.AsString := StrToLocaleDecimal('56789.12349991234');
  Execute;
  CheckEquals(StrToLocaleDecimal('56789.1234'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('56789.1234'), Field.AsWideString);

  Param.AsString := StrToLocaleDecimal('-3.41060513164848E-13');
  Execute;
  CheckEquals(0, Field.AsFloat);

  Param.AsString := StrToLocaleDecimal('0.1');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.01');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.0001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.1');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.01');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.0001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL_Limit;
var F: TFMTBCDField;
begin
  Param.AsFMTBCD := StrToBcdN('922337203685477.5807');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  F := Field as TFMTBCDField;
  CheckEquals(19, F.Precision);
  CheckEquals(4, F.Size);

  Param.AsFMTBCD := StrToBcdN('-922337203685477.5808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL_LONG;
begin
  Param.AsFMTBCD := StrToBcdN('234.56');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
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
  CheckEquals(Param.AsWideString, Field.AsWideString);
  CheckEquals(Param.AsFloat, Field.AsFloat);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(Param.AsInteger, Field.AsInteger);

  Param.AsString := '1290345678';
  Execute;
  CheckEquals(Param.AsInteger, Field.AsInteger);

  Param.AsWideString := '1290345678';
  Execute;
  CheckEquals(Param.AsInteger, Field.AsInteger);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC;
begin
  Param.AsFMTBCD := StrToBcdN('12345678901.2345');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(SizeOf(TBcd), Field.DataSize);

  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
  CheckEquals(Param.AsFloat, Field.AsFloat);

  Param.AsFMTBCD := StrToBcd('8000');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsCurrency := 12345678.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsCurrency := 1234.1234;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsFloat := 123.123456;
  Execute;
  CheckEquals(StrToLocaleDecimal('123.1235'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('123.1235'), Field.AsWideString);

  Param.AsFloat := 123.123412;
  Execute;
  CheckEquals(StrToLocaleDecimal('123.1234'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('123.1234'), Field.AsWideString);

  Param.AsInteger := 1234567890;
  Execute;
  CheckEquals(1234567890, Field.AsInteger);

  Param.AsSmallInt := 12345;
  Execute;
  CheckEquals(12345, Field.AsInteger);

  Param.AsString := StrToLocaleDecimal('56789.12349991234');
  Execute;
  CheckEquals(StrToLocaleDecimal('56789.1234'), Field.AsString);
  CheckEquals(StrToLocaleDecimal('56789.1234'), Field.AsWideString);

  Param.AsString := StrToLocaleDecimal('-3.41060513164848E-13');
  Execute;
  CheckEquals(0, Field.AsFloat);

  Param.AsString := StrToLocaleDecimal('0.1');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.01');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.0001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('0.00001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.1');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.01');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.0001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Param.AsString := StrToLocaleDecimal('-0.00001');
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_Limit;
var F: TFMTBCDField;
begin
  Param.AsFMTBCD := StrToBcdN('922337203685477.5807');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  F := Field as TFMTBCDField;
  CheckEquals(19, F.Precision);
  CheckEquals(4, F.Size);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsFMTBCD := StrToBcdN('-922337203685477.5808');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_LONG;
begin
  Param.AsFMTBCD := StrToBcdN('234.56');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_SHORT;
begin
  Param.AsFMTBCD := StrToBcdN('12.34');
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsSmallInt, Field.AsInteger);

  Param.AsFMTBCD := StrToBcd('2');
  Execute;
  CheckEquals(Param.AsString, Field.AsString);
  CheckEquals(Param.AsWideString, Field.AsWideString);
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

  Param.AsWideString := '32145';
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
  CheckEquals(Param.AsWideString, Field.AsWideString);

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
  CheckEquals(Param.AsWideString, Field.AsWideString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_VARCHAR_UTF8;
var F: TStringField;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;
  
  Param.AsWideString := 'One World One Dream ' +
                        #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C + ' ' +
                        #$540C + #$4E00 + #$4E2A + #$68A6 + #$60F3;

  Execute;
  CheckFalse(Field.IsNull);
  CheckEquals(TWideStringField, Field.ClassType);
  F := Field as TStringField;
  CheckEquals(202, F.DataSize);
  CheckEquals(100, F.Size);

  CheckEquals(Param.AsWideString, Field.AsWideString);

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
var T: TDBXTransaction;
begin
  T := FConnection.BeginTransaction;
  FConnection.CommitFreeAndNil(T);

  T := FConnection.BeginTransaction;
  FConnection.RollbackFreeAndNil(T);
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_1;
var T: TDBXTransaction;
begin
  Self.StartExpectingException(EDBXError);
  try
    T := FConnection.BeginTransaction;
    try
      FConnection.ExecuteDirect('CREATE TABLE T_TRANSACTION(FIELD INTEGER)');
      FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(123)');
      FConnection.CommitFreeAndNil(T);
    except
      FConnection.RollbackFreeAndNil(T);
      raise;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_TRANSACTION');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_2;
var T: TDBXTransaction;
    pD: ^TSQLDataSet;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_TRANSACTION(FIELD INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(123)');

  FConnection.BeginTransaction;
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(456)');
  FConnection.RollbackFreeAndNil(T);

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
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    TTestSuite_DBX1 := FConnection.BeginTransaction;
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    TTestSuite_DBX2 := FConnection.BeginTransaction;
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.CommitFreeAndNil(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.CommitFreeAndNil(TTestSuite_DBX1);

    CheckEquals('ITEM-01', V1);
    CheckEquals('ITEM-02', V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_RepeatableRead;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: ^TSQLDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  New(D);
  try
    TTestSuite_DBX1 := FConnection.BeginTransaction(TDBXIsolations.RepeatableRead);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V1 := D^.Fields[0].AsString;
    D^.Free;

    TTestSuite_DBX2 := FConnection.BeginTransaction(TDBXIsolations.RepeatableRead);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.CommitFreeAndNil(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    V2 := D^.Fields[0].AsString;
    D^.Free;
    FConnection.CommitFreeAndNil(TTestSuite_DBX1);

    CheckEquals(V1, V2);
  finally
    Dispose(D);
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_WaitLock;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: ^TSQLDataSet;
    V1: integer;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_LOCK(FIELD1 VARCHAR(10), FIELD2 INTEGER)');

  New(D);
  try
    TTestSuite_DBX1 := FConnection.BeginTransaction;
    try
      FConnection.ExecuteDirect('INSERT INTO T_LOCK VALUES(''ITEM-01'', 1)');

      TTestSuite_DBX2 := FConnection.BeginTransaction;
      try
        FConnection.Execute('SELECT COUNT(*) FROM T_LOCK', nil, D);
        V1 := D^.Fields[0].AsInteger;
        D^.Free;
        FConnection.CommitFreeAndNil(TTestSuite_DBX2);
        CheckEquals(0, V1);
      except
        FConnection.RollbackFreeAndNil(TTestSuite_DBX2);
        raise;
      end;
      FConnection.CommitFreeAndNil(TTestSuite_DBX1);
    except
      FConnection.RollbackFreeAndNil(TTestSuite_DBX1);
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
var T: TDBXTransaction;
    i: integer;
begin
  for i := 1 to 10 do begin
    T := FConnection.BeginTransaction;
    try
      FCDS.SetProvider(FDSP);
      FCDS.Open;
      FCDS.Close;
      FConnection.CommitFreeAndNil(T);
    except
      FConnection.RollbackFreeAndNil(T);
      raise;
    end;
  end;
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
    F.ReadSectionValues(GetDriverSectionName, sDrivers);
    F.ReadSectionValues('server', sServers);
    F.ReadSectionValues('embedded', sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        sParams := GetParams(sServers.ValueFromIndex[j], aParams);

        sVer := GetServerVersion(F.ReadString('vendor', 'default', ''), sParams);

        Result.Add(
          TTestData_SQLConnection.Create(sVer, sDrivers.ValueFromIndex[i],
          sDrivers.Names[i], F.ReadString('vendor', sVer, sVer), sParams)
        );
      end;
      for j := 0 to sEmbeds.Count - 1 do begin
        sParams := GetParams('', aParams);

        sVer := GetServerVersion(sEmbeds.ValueFromIndex[j], sParams) + ' Embedded';

        Result.Add(
          TTestData_SQLConnection.Create(sVer, sDrivers.ValueFromIndex[i],
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
  RegisterTest(
               COMMITRETAIN_KEY + '=False'
    + #13#10 + WAITONLOCKS_KEY + '=False'
    + #13#10 + TRIMCHAR + '=False'
  );

  RegisterTest(
               COMMITRETAIN_KEY + '=False'
    + #13#10 + WAITONLOCKS_KEY + '=False'
    + #13#10 + TRIMCHAR + '=True'
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
  S.AddSuite(TTestCase_DBX_TSQLStoredProc.NewSuite(aTestData));
  S.AddSuite(TTestCase_DBX_TSQLStoredProc_Params.NewSuite(aTestData));
  Result := S as ITestSuite;
end;

class function TTestSuite_DBX2.NewTestDataList(const aParams: string): IInterfaceList;
var F: TIniFile;
    sDrivers, sServers, sEmbeds: TStringList;
    i: integer;
    j, k: Integer;
    sParams1, sParams2: string;
    sVer1, sVer2: string;
    L: IInterfaceList;
begin
  Result := TInterfaceList.Create;

  F := TIniFile.Create(GetTestDataFileName);
  sDrivers := TStringList.Create;
  sServers := TStringList.Create;
  sEmbeds := TStringList.Create;
  try
    F.ReadSectionValues(GetDriverSectionName, sDrivers);
    F.ReadSectionValues('server', sServers);
    F.ReadSectionValues('embedded', sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        sParams1 := GetParams(sServers.ValueFromIndex[j], aParams);
        sVer1 := GetServerVersion(F.ReadString('vendor', 'default', ''), sParams1);

        for k := 0 to sEmbeds.Count - 1 do begin
          L := TInterfaceList.Create;

          L.Add(
            TTestData_SQLConnection.Create(sVer1, sDrivers.ValueFromIndex[i],
            sDrivers.Names[i], F.ReadString('vendor', sVer1, sVer1), sParams1)
          );

          sParams2 := GetParams('', aParams);
          sVer2 := GetServerVersion(sEmbeds.ValueFromIndex[k], sParams2) + ' Embedded';

          L.Add(
            TTestData_SQLConnection.Create(sVer2, sDrivers.ValueFromIndex[i],
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
          'FIELD_BIGINT BIGINT ' +
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
    CheckEquals(1, FCDS.FindField('Field_Int').AsInteger);
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_LargeInt1;
{$if CompilerVersion >= 20}var P: TParam;{$ifend}
begin
  {$if CompilerVersion >= 20}
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftLargeInt, 'Field_BigInt', ptInput);
  try
    P.Value := 1;
    FCDS.Open;
    CheckEquals('1', FCDS.FindField('Field_BigInt').AsString);
  finally
    P.Free;
  end;
  {$ifend}
end;

procedure TTestCase_DBX_TParam.Test_Param_LargeInt2;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftLargeInt, 'Field_BigInt', ptInput);
  try
    P.AsFmtBcd := StrToBcd('1');
    FCDS.Open;
    CheckEquals('1', FCDS.FindField('Field_Int').AsString);
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_LargeInt3;
begin
  FCDS.Close;
  FCDS.Open;
  FCDS.AppendRecord([2, '2', 2]);
  CheckEquals(0, FCDS.ApplyUpdates(0));
end;

procedure TTestCase_DBX_TParam.Test_Param_String;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftString, 'Field_Str', ptInput);
  try
    P.AsString := '1';
    FCDS.Open;
    CheckEquals('1', FCDS.FindField('Field_Int').AsString);
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TSQLStoredProc.SetUp;
begin
  inherited;
  FStoredProc := TSQLStoredProc.Create(nil);
  FStoredProc.SQLConnection := FConnection;
end;

procedure TTestCase_DBX_TSQLStoredProc.TearDown;
begin
  FStoredProc.Close;
  FStoredProc.Free;
  inherited;
end;

procedure TTestCase_DBX_TSQLStoredProc.Test_GetProcedureNames;
var S: string;
    L: TStringList;
    i: integer;
begin
  for i := 1 to 9 do begin
    S := Format('CREATE PROCEDURE PROC%d ', [i]) +
         'AS ' +
         'BEGIN ' +
           'SUSPEND; ' +
         'END ';
    FConnection.ExecuteDirect(S);
  end;

  L := TStringList.Create;
  try
    FConnection.GetProcedureNames(L);
    CheckEquals(9, L.Count);
    L.Sort;
    for i := 1 to 9 do
      CheckEquals(Format('PROC%d', [i]), L[i-1]);
  finally
    L.Free;
  end;

  for i := 1 to 9 do
    FConnection.ExecuteDirect(Format('DROP PROCEDURE PROC%d', [i]));
end;

procedure TTestCase_DBX_TSQLStoredProc.Test_MultiParams;
var S: string;
begin
  S := 'CREATE PROCEDURE PROC (p1 INTEGER, p2 INTEGER, p3 INTEGER) RETURNS (oParam INTEGER) ' +
       'AS ' +
       'BEGIN ' +
         ' oParam = p1 + p2 + p3; ' +
         ' SUSPEND; ' +
       'END ';
  FConnection.ExecuteDirect(S);
  try
    FStoredProc.StoredProcName := 'PROC';
    CheckEquals(4, FStoredProc.Params.Count);

    Check(ptInput = FStoredProc.Params[0].ParamType);
    Check(ptInput = FStoredProc.Params[1].ParamType);
    Check(ptInput = FStoredProc.Params[2].ParamType);
    Check(ptOutput = FStoredProc.Params[3].ParamType);

    Check(FStoredProc.Params[0].DataType = ftInteger);
    Check(FStoredProc.Params[1].DataType = ftInteger);
    Check(FStoredProc.Params[2].DataType = ftInteger);
    Check(FStoredProc.Params[3].DataType = ftInteger);

    FStoredProc.Params[0].AsInteger := 111;
    FStoredProc.Params[1].AsInteger := 222;
    FStoredProc.Params[2].AsInteger := 333;
    FStoredProc.ExecProc;

    CheckEquals(666, FStoredProc.Params[3].AsInteger);
  finally
    FConnection.ExecuteDirect('DROP PROCEDURE PROC');
  end;
end;

procedure TTestCase_DBX_TSQLStoredProc.Test_ReturnDataSet;
var S: string;
    D: TSQLDataSet;
    i: integer;
begin
  S := 'CREATE PROCEDURE PROC ' +
       '  RETURNS (oParam BIGINT) ' +
       'AS ' +
       'BEGIN ' +
         'oParam = 1; ' +
         'WHILE (oParam <= 10) DO BEGIN ' +
         '  SUSPEND; ' +
         '  oParam = oParam + 1; ' +
         'END ' +
       'END ';
  FConnection.ExecuteDirect(S);
  try
    FConnection.Execute('SELECT * FROM PROC', nil, @D);
    for i := 1 to 10 do begin
      CheckEquals(i, D.Fields[0].AsInteger);
      D.Next;
    end;
    D.Free;
  finally
    FConnection.ExecuteDirect('DROP PROCEDURE PROC');
  end;
end;

function TTestCase_DBX_TSQLStoredProc_Params.CreateProc(const aDecl, aImpl: string):
    Integer;
var S: string;
begin
  S := Format('CREATE PROCEDURE PROC (iParam %s) RETURNS (oParam %0:s) ', [aDecl]) +
       'AS ' +
       'BEGIN ' +
         ' oParam = iParam; ' +
         ' SUSPEND; ' +
       'END ';
  FConnection.ExecuteDirect(S);
  FStoredProc.StoredProcName := 'PROC';

  CheckEquals(2, FStoredProc.Params.Count);

  Check(ptInput = FStoredProc.Params[0].ParamType);
  CheckEquals('IPARAM', FStoredProc.Params[0].Name);

  Check(ptOutput = FStoredProc.Params[1].ParamType);
  CheckEquals('OPARAM', FStoredProc.Params[1].Name);

  Check(FStoredProc.Params[0].DataType = FStoredProc.Params[1].DataType);

  FStoredProc.Params[0].Value := aImpl;
  Result := FStoredProc.ExecProc;
end;

function TTestCase_DBX_TSQLStoredProc_Params.CreateProc2(const aDecl,
    aImpl: string): Integer;
var S: string;
begin
  S := Format('CREATE PROCEDURE PROC2 RETURNS ( oParam %s ) ', [aDecl]) +
       'AS ' +
       'BEGIN ' +
       Format('%s INTO :oParam; ', [aImpl]) +
         ' SUSPEND; ' +
       'END ';
  FConnection.ExecuteDirect(S);
  FStoredProc.StoredProcName := 'PROC2';
  Result := FStoredProc.ExecProc;
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.SetUp;
begin
  inherited;
  FStoredProc := TSQLStoredProc.Create(nil);
  FStoredProc.SQLConnection := FConnection;
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.TearDown;
begin
  FStoredProc.Close;
  if FStoredProc.StoredProcName <> '' then
    FConnection.ExecuteDirect(Format('DROP PROCEDURE %S', [FStoredProc.StoredProcName]));
  FStoredProc.Free;
  inherited;
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_BigInt;
begin
  CheckEquals(0, CreateProc('BIGINT', '123456789012345678'));

  {$Message 'QC#64499 TParam does not take TLargeIntField value'}
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);

  CheckEquals('123456789012345678', FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Blob;
var S: string;
begin
  {$Message 'Unable to support Blob parameter now as TSQLConnection.GetProcedureParams didn't get the blob parameter data size'}
  S := 'CREATE TABLE T_STOREDPROC ( ' +
         'MYBLOB BLOB SUB_TYPE 0 SEGMENT SIZE 512 ' +
       ')';
  FConnection.ExecuteDirect(S);

  try
    FConnection.ExecuteDirect('INSERT INTO T_STOREDPROC VALUES (''ABCDEFGHIJKLMNOPQRSTUVWXYZ'')');

    CheckEquals(0, CreateProc2('BLOB SUB_TYPE 0 SEGMENT SIZE 512', 'SELECT MYBLOB FROM T_STOREDPROC'));

    {$Message 'QC#64499 TParam does not take TLargeIntField value'}
    Check(ftBlob = FStoredProc.Params[1].DataType);

    CheckEquals('ABCDEFGHIJKLMNOPQRSTUVWXYZ', FStoredProc.Params[1].AsString);
  finally
    FConnection.ExecuteDirect('DROP TABLE T_STOREDPROC');
  end;
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Char;
var iLen: integer;
begin
  CheckEquals(0, CreateProc('CHAR(100)', 'ABC'));
  Check(ftString = FStoredProc.Params[1].DataType);
  if IsTrimChar then
    iLen := 3
  else
    iLen := 100;
  CheckEquals(iLen, Length(FStoredProc.Params[1].AsString));
  CheckEquals('ABC', Trim(FStoredProc.Params[1].AsString));
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Date;
begin
  CheckEquals(0, CreateProc('DATE', FormatDateTime('dd mmm yyyy', Date) ));
  Check(ftDate = FStoredProc.Params[1].DataType);
  CheckEquals(Date, FStoredProc.Params[1].AsDate);
end;                                                           

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Decimal_18;
begin
  CheckEquals(0, CreateProc('DECIMAL(18, 5)', StrToLocaleDecimal('123456789012.67891')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('123456789012.67891'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Decimal_4;
begin
  CheckEquals(0, CreateProc('DECIMAL(4, 1)', StrToLocaleDecimal('123.4')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('123.4'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Decimal_8;
begin
  CheckEquals(0, CreateProc('DECIMAL(8, 3)', StrToLocaleDecimal('98765.432')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('98765.432'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_DoublePrecision;
begin
  CheckEquals(0, CreateProc('DOUBLE PRECISION', StrToLocaleDecimal('123.4567890123')));
  Check(ftFloat = FStoredProc.Params[1].DataType);
  CheckEquals(123.4567890123, FStoredProc.Params[1].AsFloat, SglEps);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Float;
begin
  CheckEquals(0, CreateProc('FLOAT', StrToLocaleDecimal('123.456')));
  Check(ftFloat = FStoredProc.Params[1].DataType);
  CheckEquals(123.456, FStoredProc.Params[1].AsFloat, 0.0001);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Integer;
begin
  CheckEquals(0, CreateProc('INTEGER', '12345678'));
  Check(ftInteger = FStoredProc.Params[1].DataType);
  CheckEquals(12345678, FStoredProc.Params[1].AsInteger);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Numeric_18;
begin
  CheckEquals(0, CreateProc('NUMERIC(18, 5)', StrToLocaleDecimal('123456789012.67891')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('123456789012.67891'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Numeric_4;
begin
  CheckEquals(0, CreateProc('NUMERIC(4, 1)', StrToLocaleDecimal('123.4')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('123.4'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Numeric_8;
begin
  CheckEquals(0, CreateProc('NUMERIC(8, 3)', StrToLocaleDecimal('98765.432')));
  Check(ftFMTBcd = FStoredProc.Params[1].DataType);
  CheckEquals(StrToLocaleDecimal('98765.432'), FStoredProc.Params[1].AsString);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_SmallInt;
begin
  CheckEquals(0, CreateProc('SMALLINT', '12345'));
  Check(ftSmallint = FStoredProc.Params[1].DataType);
  CheckEquals(12345, FStoredProc.Params[1].AsSmallInt);
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Time;
var D: TDateTime;
    F, S: string;
begin
  D := Time;
  F := 'hh:nn:ss';
  S := FormatDateTime(F, D);
  CheckEquals(0, CreateProc('TIME', S));
  Check(ftTime = FStoredProc.Params[1].DataType);
  CheckEquals(S, FormatDateTime(F, FStoredProc.Params[1].AsTime));
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_TimeStamp;
var D: TDateTime;
    F, S: string;
begin
  D := Now;
  F := ShortDateFormat + ' ' + LongTimeFormat;
  S := FormatDateTime(F, D);

  CheckEquals(0, CreateProc('TIMESTAMP', S));
  Check(ftTimeStamp = FStoredProc.Params[1].DataType);
  CheckEquals(S, SQLTimeStampToStr(F, FStoredProc.Params[1].AsSQLTimeStamp));
end;

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_VarChar;
begin
  CheckEquals(0, CreateProc('VARCHAR(100)', 'ABC'));
  Check(ftString = FStoredProc.Params[1].DataType);
  CheckEquals('ABC', FStoredProc.Params[1].AsString);
end;

initialization
  TTestSuite_DBX.CheckTestDataFile;
  TTestSuite_DBX1.Setup;
  TTestSuite_DBX2.Setup;
end.
