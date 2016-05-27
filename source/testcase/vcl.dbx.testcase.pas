unit vcl.dbx.testcase;

interface

uses SysUtils, Classes, DB, SqlExpr, Provider, DBClient, FMTBcd,
     TestFrameWork, TestExtensions
     {$if CompilerVersion > 18}, DBXCommon{$ifend}
     {$if CompilerVersion >= 20}, dbx.firebird{$ifend}
     ;

type{$M+}
  ITestData = interface(IInterface)
  ['{2DCC2E1F-BCE2-4D04-A61E-03DBFC031D0E}']
    function GetODS: UInt16;
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
    FODS: UInt16;
    FServerVersion: string;
    procedure CreateDatabase;
  protected
    function GetODS: UInt16;
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
    class function GetEmbeddedSectionName: string;
    class function GetServerSectionName: string;
    class function GetVendorSectionName: string;
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
    procedure Test_Decimal_18_8_Deduction;
    procedure Test_Unicode_SQL;
    procedure Test_Param_Single_Shortint;
    procedure Test_Insert_Returning;
    procedure Test_SystemTable_Char_Field;
    procedure Test_RoleName;
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
    procedure Test_Transaction_WaitLock_Fail;
  end;

  TTestCase_DBX_FieldType = class(TTestCase_DBX)
  private
    FDataSet: TDataSet;
    FParams: TParams;
    procedure Execute;
    function Field: TField;
    function Param: TParam;
    procedure Test_CHAR_Unicode;
    procedure Test_VARCHAR_Unicode;
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
    procedure Test_CHAR_UNICODE_FSS;
    procedure Test_CHAR_UTF8;
    procedure Test_DATE;
    procedure Test_DATETIME;
    procedure Test_DECIMAL;
    procedure Test_DECIMAL_0;
    procedure Test_DECIMAL_18_10;
    procedure Test_DECIMAL_Limit;
    procedure Test_DECIMAL_LONG;
    procedure Test_DECIMAL_Misc;
    procedure Test_DOUBLE_PRECISION;
    procedure Test_FLOAT;
    procedure Test_INTEGER;
    procedure Test_MEMO;
    procedure Test_MEMO_UTF8;
    procedure Test_NUMERIC;
    procedure Test_NUMERIC_0;
    procedure Test_NUMERIC_18_10;
    procedure Test_NUMERIC_Limit;
    procedure Test_NUMERIC_LONG;
    procedure Test_NUMERIC_Misc;
    procedure Test_NUMERIC_SHORT;
    procedure Test_SMALLINT;
    procedure Test_TIME;
    procedure Test_TIMESTAMP;
    procedure Test_VARCHAR;
    procedure Test_VARCHAR_UNICODE_FSS;
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
    procedure Test_MalformString;
    procedure Test_UTF8_EmptyString;
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
    procedure Test_Param_AnsiString;
    procedure Test_Param_NonUnicodeString;
    procedure Test_Param_Negative;
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
    procedure Test_Char_UTF8;
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
    procedure Test_VarChar_UTF8;
  end;

implementation

uses SqlConst, Windows, StrUtils, WideStrings,
  SqlTimSt, DateUtils, Math, IniFiles,
  firebird.ods.h,
  SystemEx, SysUtilsEx, firebird.client, firebird.service, UniqueID,
  vcl.dbx.cmdlines;

{$if RTLVersion <= 23}
type
  TExecuteStringType = {$if RTLVersion = 18}WideString{$else}UnicodeString{$ifend};

  TSQLConnectionHelper = class helper for TSQLConnection
  public
    function Execute(const SQL: TExecuteStringType; Params: TParams; out ResultSet:
        TDataSet): Integer; overload;
  end;

function TSQLConnectionHelper.Execute(const SQL: TExecuteStringType; Params:
    TParams; out ResultSet: TDataSet): Integer;
begin
  Result := Execute(SQL, Params, @ResultSet);
end;
{$ifend}

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
begin
  L := TStringList.Create;
  try
    L.Text := FParams;
    TFirebirdDatabase.DropDatabase(FVendorLib, L.Values[HOSTNAME_KEY], L.Values[DATABASENAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
  finally
    L.Free;
  end;
end;

procedure TTestData_SQLConnection.CreateDatabase;
var L: TStringList;
    S: TFirebirdServiceManager;
    D: TFirebirdDatabase;
    sDatabase: string;
    sImpl: string;
begin
  L := TStringList.Create;
  try
    L.Text := FParams;

    S := TFirebirdServiceManager.New(FVendorLib, L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
    try
      sImpl := S.GetServerImplementation;
      if L.Values[HOSTNAME_KEY] = '' then
        sDatabase := IncludeTrailingPathDelimiter('%TEMP%')
      else if ContainsText(sImpl, 'Windows') then begin

        if Pos('dbxint', FLibraryName) > 0 then // Interbase Driver need hostname string in database parameter
          sDatabase := L.Values[HOSTNAME_KEY] + ':';

        if AnsiStartsText('localhost', L.Values[HOSTNAME_KEY]) then
          sDatabase := sDatabase + IncludeTrailingPathDelimiter('%TEMP%')
        else
          sDatabase := sDatabase + 'c:\';
      end else if ContainsText(sImpl, 'Linux') then
        sDatabase := sDatabase + '/tmp/'
      else
        Assert(False);
      sDatabase := sDatabase + TUniqueName.New('T_');

      FServerVersion := S.GetServerVersion;
      FName := Format('%s (%s) Host: %s Database: %s', [sImpl, FServerVersion, L.Values[HOSTNAME_KEY], sDatabase]);
    finally
      S.Free;
    end;

    TFirebirdDatabase.CreateDatabase(FVendorLib, L.Values[HOSTNAME_KEY], sDatabase, L.Values[szUSERNAME], L.Values[szPASSWORD]);

    D := TFirebirdDatabase.New(FVendorLib, L.Values[HOSTNAME_KEY], sDatabase, L.Values[szUSERNAME], L.Values[szPASSWORD]);
    try
      FODS := D.GetODSVersion;
    finally
      D.Free;
    end;

    L.Values[DATABASENAME_KEY] := sDatabase;
    FParams := L.Text;
  finally
    L.Free;
  end;
end;

function TTestData_SQLConnection.GetName: string;
begin
  Result := FName;
end;

function TTestData_SQLConnection.GetODS: UInt16;
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
{$if RTLVersion <= 23}
  aConnection.LibraryName := FLibraryName;
  aConnection.GetDriverFunc := FGetDriverFunc;
  aConnection.VendorLib := FVendorLib;
{$ifend}
  aConnection.Params.Text := FParams;
{$if RtlVersion >= 24}
  aConnection.Params.Values[TDBXPropertyNames.LibraryName] := FLibraryName;
  aConnection.Params.Values[TDBXPropertyNames.GetDriverFunc] := FGetDriverFunc;
  aConnection.Params.Values[TDBXPropertyNames.VendorLib] := FVendorLib;
{$ifend}
end;

class procedure TTestSuite_DBX.CheckTestDataFile;
var F: TIniFile;
    sDriver: string;
begin
  if not FileExists(GetTestDataFileName) then begin
    F := TIniFile.Create(GetTestDataFileName);
    try
      sDriver := 'dbx4fb.dll';
      F.WriteString(GetDriverSectionName, 'getSQLDriverFIREBIRD', sDriver);
      F.WriteString('embedded', 'embedded_1', 'fbembed.dll');
      F.WriteString('server', 'server_1', 'localhost');
      F.WriteString('vendor', 'default', 'fbclient.1.5.5.dll');
      F.UpdateFile;
    finally
      F.Free;
    end;
  end;

  SetEnvironmentVariable('drivers', PChar(TCmdLineParams_App.Drivers));
end;

class function TTestSuite_DBX.GetDriverSectionName: string;
begin
  Result := 'driver'
            {$ifdef DEBUG} + '.debug'
            {$else}
              {$ifdef Win32} + '.x86'{$endif}
              {$ifdef Win64} + '.x64'{$endif}
            {$endif};
end;

class function TTestSuite_DBX.GetEmbeddedSectionName: string;
begin
  Result := 'embedded.' +
            {$ifdef Win32}'x86'{$endif}
            {$ifdef Win64}'x64'{$endif}
            ;
end;

class function TTestSuite_DBX.GetParams(const aHostName, aExtraParams:
    string): string;
begin
  Result := {$if CompilerVersion=18.5}DRIVERNAME_KEY + '=' + TUniqueName.New + #13#10 + {$ifend}
            SQLDIALECT_KEY + '=3'
            + #13#10 + szUSERNAME + '=SYSDBA'
            + #13#10 + szPASSWORD + '=masterkey'
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
  Result := TCmdLineParams_App.ConfigFile;
end;

class function TTestSuite_DBX.GetVendorSectionName: string;
begin
  Result := 'vendor.' +
            {$ifdef Win32}'x86'{$endif}
            {$ifdef Win64}'x64'{$endif}
            ;
end;

class function TTestSuite_DBX.GetServerSectionName: string;
begin
  Result := 'server.' +
            {$ifdef Win32}'x86'{$endif}
            {$ifdef Win64}'x64'{$endif}
            ;
end;

class function TTestSuite_DBX.GetServerVersion(aLibraryName, aParams:
    string): string;
var L: TStringList;
    S: TFirebirdServiceManager;
begin
  L := TStringList.Create;
  try
    L.Text := aParams;
    S := TFirebirdServiceManager.New(ExpandfileNameString(aLibraryName), L.Values[HOSTNAME_KEY], L.Values[szUSERNAME], L.Values[szPASSWORD]);
    try
      Result := S.GetServerVersion;
    finally
      S.Free;
    end;
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
  ScreenCursorProc := nil;
  FConnection := TSQLConnection.Create(nil);
  FConnection.LoginPrompt := False;
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
  if GetGUIObject <> nil then
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

procedure TTestCase_DBX_General.Test_Unicode_SQL;
var s: string;
    D: TDataSet;
    sValue: string;
begin
  {$ifndef Unicode}Exit;{$endif}
  if (Pos('Firebird 1.', GetTestData.ServerVersion) <> 0) or (Pos('Firebird 2.0', GetTestData.ServerVersion) <> 0) then Exit;

  FConnection.Close;
  FConnection.Params.Values[SQLSERVER_CHARSET_KEY] := 'UTF8';
  FConnection.Open;

  S := 'CREATE TABLE T_INSERT_UTF8 ' +
       '( ' +
       '  F1 VARCHAR(100) CHARACTER SET UTF8 ' +
       ')';
  FConnection.ExecuteDirect(S);
  try
    sValue := 'One World One Dream ' +
              #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C + ' ' +
              #$540C + #$4E00 + #$4E2A + #$68A6 + #$60F3;

    S := Format('INSERT INTO T_INSERT_UTF8 VALUES(''%s'')', [sValue]);
    FConnection.ExecuteDirect(S);

    FConnection.Execute('SELECT * FROM T_INSERT_UTF8', nil, D);
    try
      CheckEquals(sValue, D.Fields[0].AsString);
    finally
      D.Free;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_INSERT_UTF8');
  end;
end;

procedure TTestCase_DBX_General.Test_Param_Single_Shortint;
{$if RTLVersion>=21}
var s: string;
    D: TDataSet;
    P: TParams;
{$ifend}
begin
  {$if RTLVersion>=21}
  S := 'CREATE TABLE T_ ' +
       '( ' +
       '  F1 INTEGER, ' +
       '  F2 INTEGER ' +
       ')';
  P := TParams.Create;
  FConnection.ExecuteDirect(S);
  try
    P.CreateParam(ftInteger, 'F1', ptInput).AsSingle := 1;
    P.CreateParam(ftInteger, 'F2', ptInput).AsByte := 1;
    FConnection.Execute('SELECT * FROM T_ WHERE F1=:F1 AND F2=:F2', P, D);
    try
      CheckEquals(0, D.Fields[0].AsSingle);
    finally
      D.Free;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_');
    P.Free;
  end;
  {$ifend}
end;

procedure TTestCase_DBX_General.Test_RoleName;
var sUser: string;
begin
  if not FileExists(FConnection.Params.Values[DATABASENAME_KEY]) then Exit;

  FConnection.ExecuteDirect('CREATE TABLE T_RoleName(AutoKey INTEGER)');

  sUser := 'U' + GetTickCount.ToString;
  FConnection.ExecuteDirect(Format('CREATE USER %s password ''password'' grant ADMIN ROLE', [sUser]));
  try
    FConnection.ExecuteDirect(Format('GRANT RDB$ADMIN TO %s', [sUser]));
    FConnection.Close;
    FConnection.Params.Values[szUSERNAME] := sUser;
    FConnection.Params.Values[szPASSWORD] := 'password';
    FConnection.Params.Values[ROLENAME_KEY] := 'RDB$ADMIN';
    FConnection.Open;

    FConnection.Execute('SELECT * FROM T_RoleName', nil);
  finally
    FConnection.ExecuteDirect(Format('DROP USER %s', [sUser]));
    FConnection.ExecuteDirect('DROP TABLE T_RoleName');
  end;
end;

procedure TTestCase_DBX_General.Test_CAST_SQL_DECIMAL_Bug;
var D: TDataSet;
    S: string;
begin
  //refer to BU-00010
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
      FConnection.Execute(S, nil, D);
    finally
      D.Free;
    end;

  finally
    FConnection.ExecuteDirect('DROP TABLE T_TEST1');
  end;
end;

procedure TTestCase_DBX_General.Test_ServerCharSet;
var S: widestring;
    C: Char;
    D: TDataSet;
begin
  if TCmdLineParams_App.CORE_2978 then Exit;
  {$ifndef Unicode}Exit;{$endif}
  FConnection.Close;
  FConnection.Params.Values[SQLSERVER_CHARSET_KEY] := 'WIN1252';
  FConnection.Open;

  S := 'CREATE TABLE T_TEST_CHARSET ( ' +
         'S_WIN1252 CHAR(1) CHARACTER SET WIN1252, ' +
         'S_ISO8859_13 CHAR(1) CHARACTER SET ISO8859_13 ' +
       ') ';
  FConnection.ExecuteDirect(S);

  C := #$9E;
  S := Format('INSERT INTO T_TEST_CHARSET(S_WIN1252, S_ISO8859_13) VALUES (''%s'', ''%s'')', [C, C]);
  FConnection.ExecuteDirect(S);

  // Test WIN1252 Transliteration
  FConnection.Execute('SELECT S_WIN1252, S_ISO8859_13 FROM T_TEST_CHARSET', nil, D);
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

  FConnection.Execute('SELECT S_WIN1252, S_ISO8859_13 FROM T_TEST_CHARSET', nil, D);
  try
    CheckEquals(#$9E, D.Fields[0].AsString);
    CheckEquals(#$FE, D.Fields[1].AsString);
  finally
    D.Free;
  end;

  FConnection.ExecuteDirect('DROP TABLE T_TEST_CHARSET');
end;

procedure TTestCase_DBX_General.Test_SystemTable_Char_Field;
var S, G: string;
    D: TDataSet;
    P: TParams;
begin
  G := 'S1234567890abcdef';
  S := 'CREATE GENERATOR ' + G;
  FConnection.ExecuteDirect(S);

  P := TParams.Create;
  try
    P.CreateParam(ftString, 'Name', ptInput).AsString := G;
    FConnection.Execute('SELECT COUNT(*) Counter FROM RDB$GENERATORS WHERE UPPER(RDB$GENERATOR_NAME)=UPPER(:Name)', P, D);
    try
      CheckEquals(1, D.Fields[0].AsInteger);
    finally
      D.Free;
    end;
  finally
    P.Free;
    FConnection.ExecuteDirect('DROP GENERATOR ' + G);
  end;
end;

procedure TTestCase_DBX_General.Test_Connection_Property;
begin
  CheckTrue(FConnection.TransactionsSupported);
  CheckTrue(FConnection.MultipleTransactionsSupported);
end;

procedure TTestCase_DBX_General.Test_Decimal_18_8_Deduction;
var s: string;
    D: TDataSet;
begin
  S := 'CREATE TABLE T_TRANS ' +
       '( ' +
       '  C1 DECIMAL(18, 8), ' +
       '  C2 DECIMAL(18, 8) ' +
       ')';
  FConnection.ExecuteDirect(S);
  try
    S := 'INSERT INTO T_TRANS VALUES(16, 1)';
    FConnection.ExecuteDirect(S);

    FConnection.Execute('SELECT C1, C2, C1 - C2 AS Balance FROM T_TRANS', nil, D);
    try
      CheckEquals('16', D.FindField('C1').AsString);
      CheckEquals('1',  D.FindField('C2').AsString);
      CheckEquals('15', D.FindField('Balance').AsString);
    finally
      D.Free;
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_TRANS');
  end;
end;

procedure TTestCase_DBX_General.Test_Execute;
var D: TDataSet;
    P: TParams;
    iCount: integer;
begin
  iCount := 16;
  if FTestData.GetODS >= ODS_11_1 then
    iCount := 17;

  P := TParams.Create;
  try
    FConnection.Execute('SELECT * FROM RDB$RELATIONS', nil, D);
    try
      CheckEquals(iCount, D.FieldCount);
      CheckFalse(D.Eof);
    finally
      D.Free;
    end;

    P.CreateParam(ftInteger, '1', ptInput).AsInteger := 1;
    FConnection.Execute('SELECT * FROM RDB$RELATIONS WHERE 1=?', P, D);
    try
      CheckEquals(iCount, D.FieldCount);
      CheckFalse(D.Eof);
    finally
      D.Free;
    end;
  finally
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
var L1, L2: {$if RtlVersion <= 22}TWideStringList{$else}TStringList{$ifend};
begin
  FConnection.Open;
  L1 := {$if RtlVersion <= 22}TWideStringList{$else}TStringList{$ifend}.Create;
  L2 := {$if RtlVersion <= 22}TWideStringList{$else}TStringList{$ifend}.Create;
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
    if FTestData.GetODS >= ODS_11_1 then
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
var L: {$if RtlVersion <= 22}TWideStringList{$else}TStringList{$ifend};
begin
  L := {$if RtlVersion <= 22}TWideStringList{$else}TStringList{$ifend}.Create;
  try
    FConnection.GetIndexNames('RDB$RELATIONS', L);
    CheckEquals('', L.Text);
  finally
    L.Free;
  end;
end;

procedure TTestCase_DBX_General.Test_GetTableNames;
var L: TStringList;
begin
  FConnection.ExecuteDirect('CREATE VIEW VIEW_OF_TEST AS SELECT * FROM RDB$Relations');

  L := TStringList.Create;
  try
    FConnection.TableScope := [tsSysTable, tsTable, tsView];
    FConnection.GetTableNames(L);
    CheckNotEquals(-1, L.IndexOf('VIEW_OF_TEST'));
    CheckTrue(L.Count > 10);

    FConnection.TableScope := [tsSysTable, tsTable];
    FConnection.GetTableNames(L);
    CheckEquals(-1, L.IndexOf('TEST'));
    CheckTrue(L.Count > 10);

    FConnection.TableScope := [tsTable];
    FConnection.GetTableNames(L);
    CheckEquals(-1, L.IndexOf('TEST'));
    CheckTrue(L.Count >= 0);

    FConnection.TableScope := [tsView];
    FConnection.GetTableNames(L, False);
    CheckNotEquals(-1, L.IndexOf('VIEW_OF_TEST'));
  finally
    L.Free;
    FConnection.ExecuteDirect('DROP VIEW VIEW_OF_TEST');
  end;
end;

procedure TTestCase_DBX_General.Test_Insert_Returning;
var s: string;
    D: TSQLDataSet;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;

  S := 'CREATE TABLE T_INSERT ' +
       '( ' +
       '  C1 INTEGER, ' +
       '  C2 INTEGER ' +
       ')';
  FConnection.ExecuteDirect(S);
  D := TSQLDataSet.Create(nil);
  try
    D.SQLConnection := FConnection;
    D.CommandText := 'INSERT INTO T_INSERT VALUES(100, 200) RETURNING C1,C2';
    D.Params.CreateParam(ftInteger, 'C1', ptOutput);
    D.Params.CreateParam(ftInteger, 'C2', ptOutput);
    D.ExecSQL(False);
    CheckEquals(100, D.Params[0].AsInteger);
    CheckEquals(200, D.Params[1].AsInteger);
  finally
    D.Free;
    FConnection.ExecuteDirect('DROP TABLE T_INSERT');
  end;
end;

procedure TTestCase_DBX_General.Test_Invalid_Login;
begin
  FConnection.Close;
  FConnection.Params.Values[szUSERNAME] := 'no.such.user';
  if FConnection.Params.IndexOfName(HOSTNAME_KEY) <> -1 then
    StartExpectingException(TDBXError);
  FConnection.Open;
end;

procedure TTestCase_DBX_General.Test_Invalid_VendorLib;
begin
  FConnection.Close;
  FConnection.Params.Values[TDBXPropertyNames.VendorLib] := 'no.such.vendorlib';
  CheckException(FConnection.Open, TDBXError);
end;

procedure TTestCase_DBX_General.Test_Open_Close;
begin
  CheckTrue(FConnection.Connected);
  FConnection.Close;
  CheckFalse(FConnection.Connected);
end;

procedure TTestCase_DBX_General.Test_RecordCount;
var D: TDataSet;
begin
  FConnection.Execute('SELECT * FROM RDB$RELATIONS', nil, D);
  try
    CheckNotEquals(0, D.RecordCount);
  finally
    D.Free;
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
  FConnection.Execute(S, nil, FDataSet);
end;

function TTestCase_DBX_FieldType.Field: TField;
begin
  Result := FDataSet.Fields[0];
end;

function TTestCase_DBX_FieldType.GetFieldType: string;
begin
  FRequired := False;

       if GetName = 'Test_CHAR'                then Result := 'CHAR(100)'
  else if GetName = 'Test_CHAR_UTF8'           then Result := 'CHAR(100) CHARACTER SET UTF8'
  else if GetName = 'Test_CHAR_UNICODE_FSS'    then Result := 'CHAR(100) CHARACTER SET UNICODE_FSS'
  else if GetName = 'Test_VARCHAR'             then Result := 'VARCHAR(100)'
  else if GetName = 'Test_VARCHAR_UTF8'        then Result := 'VARCHAR(100) CHARACTER SET UTF8'
  else if GetName = 'Test_VARCHAR_UNICODE_FSS' then Result := 'VARCHAR(100) CHARACTER SET UNICODE_FSS'
  else if GetName = 'Test_SMALLINT'            then Result := 'SMALLINT'
  else if GetName = 'Test_INTEGER'             then Result := 'INTEGER'
  else if GetName = 'Test_BIGINT'              then Result := 'BIGINT'
  else if GetName = 'Test_BIGINT_Limit'        then Result := 'BIGINT'
  else if GetName = 'Test_MEMO'                then Result := 'BLOB SUB_TYPE 1'
  else if GetName = 'Test_MEMO_UTF8'           then Result := 'BLOB SUB_TYPE 1 CHARACTER SET UTF8'
  else if GetName = 'Test_NUMERIC'             then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_NUMERIC_SHORT'       then Result := 'NUMERIC(4, 2)'
  else if GetName = 'Test_NUMERIC_LONG'        then Result := 'NUMERIC(9, 2)'
  else if GetName = 'Test_NUMERIC_Limit'       then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_NUMERIC_Misc'        then Result := 'NUMERIC(18, 4)'
  else if GetName = 'Test_NUMERIC_0'           then Result := 'NUMERIC(18, 0)'
  else if GetName = 'Test_NUMERIC_18_10'       then Result := 'NUMERIC(18, 10)'
  else if GetName = 'Test_DECIMAL'             then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_DECIMAL_LONG'        then Result := 'DECIMAL(9, 2)'
  else if GetName = 'Test_DECIMAL_Limit'       then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_DECIMAL_Misc'        then Result := 'DECIMAL(18, 4)'
  else if GetName = 'Test_DECIMAL_0'           then Result := 'DECIMAL(4, 0)'
  else if GetName = 'Test_DECIMAL_18_10'       then Result := 'DECIMAL(18, 10)'
  else if GetName = 'Test_FLOAT'               then Result := 'FLOAT'
  else if GetName = 'Test_DOUBLE_PRECISION'    then Result := 'DOUBLE PRECISION'
  else if GetName = 'Test_DATE'                then Result := 'DATE'
  else if GetName = 'Test_DATETIME'            then Result := 'DATE'
  else if GetName = 'Test_TIME'                then Result := 'TIME'
  else if GetName = 'Test_TIMESTAMP'           then Result := 'TIMESTAMP'
  else if GetName = 'Test_BLOB'                then Result := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
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
    F: TFileStream;
    S: AnsiString;
begin
  F := TFileStream.Create(ExpandFileNameString('%windir%\notepad.exe'), fmOpenRead + fmShareDenyNone);
  Param.LoadFromStream(F, ftBlob);
  F.Free;
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

  Param.AsString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsString), Length(Field.AsString));

  Param.AsString := DupeString('B', Field.Size + 1);
  Execute;
  CheckEquals(DupeString('B', Field.Size), Field.AsString);

  Param.AsString := DupeString('C', Field.Size + 2);
  Execute;
  CheckEquals(DupeString('C', Field.Size), Field.AsString);

  Param.AsString := DupeString('D', Field.Size * 2);
  Execute;
  CheckEquals(DupeString('D', Field.Size), Field.AsString);

  Param.AsWideString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsWideString), Length(Field.AsWideString));

  {$if CompilerVersion > 18.5}
  Param.AsBytes := TBytes.Create($30, $31, $32, $33, $34, $35, $36, $37, $38, $39);
  Execute;
  i := 0;
  if not IsTrimChar then
    i := Field.Size - Length(Param.AsBytes);
  CheckEquals('0123456789' + DupeString(' ', i), Field.AsString);
  {$ifend}

  Param.AsString := '';
  i := 0;
  if not IsTrimChar then
    i := Field.Size - Length(Param.AsString);
  Execute;
  CheckEquals(DupeString(' ', i), Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR_Unicode;
var i: integer;
    W: WideString;
begin
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

  Param.AsString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsString), Length(Field.AsString));

  Param.AsWideString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsWideString), Length(Field.AsWideString));

  Param.AsString := DupeString('B', Field.Size + 1);
  Execute;
  CheckEquals(DupeString('B', Field.Size), Field.AsWideString);

  Param.AsString := DupeString('C', Field.Size + 2);
  Execute;
  CheckEquals(DupeString('C', Field.Size), Field.AsWideString);

  Param.AsString := DupeString('D', Field.Size * 2);
  Execute;
  CheckEquals(DupeString('D', Field.Size), Field.AsWideString);

  Param.AsString := '';
  i := 0;
  if not IsTrimChar then
    i := Field.Size - Length(Param.AsWideString);
  Execute;
  CheckEquals(DupeString(' ', i), Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR_UNICODE_FSS;
begin
  // This test case is not valid for Firebird 1.5.0
  if Pos('1.5', GetTestData.ServerVersion) <> 0 then Exit;
  Test_CHAR_Unicode;
end;

procedure TTestCase_DBX_FieldType.Test_CHAR_UTF8;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;
  Test_CHAR_Unicode;
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
  Exit;   // By pass this test for Delphi 2009 as SqlExpr.SetQueryProcParams does not interpret ftDateTime
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

procedure TTestCase_DBX_FieldType.Test_DECIMAL_0;
begin
  Param.AsString := '9121';
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_DECIMAL_18_10;
begin
  Param.AsCurrency := 1;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
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
  Param.AsCurrency := 0;
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(0, Field.AsCurrency);

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

procedure TTestCase_DBX_FieldType.Test_DECIMAL_Misc;
var S: string;
    D: TDataSet;
begin
  Param.AsFMTBCD := StrToBcdN('12345678901.2345');
  Execute;

  S := 'SELECT (Field + Field) as TestField FROM T_FIELD';
  FConnection.Execute(S, nil, D);
  try
    CheckEquals(TFMTBCDField, D.Fields[0].ClassType);
  finally
    D.Free;
  end;

  S := 'SELECT 0.00 TestField FROM T_FIELD';
  FConnection.Execute(S, nil, D);
  try
    CheckEquals(TFMTBCDField, D.Fields[0].ClassType);
  finally
    D.Free;
  end;
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

procedure TTestCase_DBX_FieldType.Test_MEMO;
var M: TStringStream;
    S: AnsiString;
begin
  M := TStringStream.Create(DupeString('a', 65535));
  try
    Param.LoadFromStream(M, ftBlob);
  finally
    M.Free;
  end;
  Execute;
  CheckEquals(TMemoField, Field.ClassType);

  {$if CompilerVersion <= 18.5}
  S := Param.AsString;
  {$else}
  SetLength(S, Param.GetDataSize);
  Move(Param.AsBlob[0], S[1], Param.GetDataSize);
  {$ifend}
  CheckEquals(string(S), Field.AsString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_MEMO_UTF8;
{$ifdef Unicode}
var M: TMemoryStream;
    S, W: WideString;
    B: TBytes;
{$endif}
begin
  {$ifdef Unicode}
  if (Pos('Firebird 1.', GetTestData.ServerVersion) <> 0) or (Pos('Firebird 2.0', GetTestData.ServerVersion) <> 0) then Exit;

  M := TMemoryStream.Create;
  try
    W := #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C;
    M.Write(W[1], Length(W) * SizeOf(WideChar));
    M.Position := 0;
    Param.LoadFromStream(M, ftBlob);
  finally
    M.Free;
  end;
  Execute;
  CheckEquals(TWideMemoField, Field.ClassType);

  SetLength(B, Param.GetDataSize);
  Move(Param.AsBlob[0], B[0], Param.GetDataSize);
  S := TEncoding.Unicode.GetString(B, Low(B), High(B) + 1);

  CheckEquals(S, (Field as TWideMemoField).Value);

  Test_Required;
  {$endif}
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

procedure TTestCase_DBX_FieldType.Test_NUMERIC_0;
begin
  Param.AsString := '912345678901234567';
  Execute;
  CheckEquals(TFMTBCDField, Field.ClassType);
  CheckEquals(Param.AsString, Field.AsString);
end;

procedure TTestCase_DBX_FieldType.Test_NUMERIC_18_10;
begin
  Param.AsCurrency := 1;
  Execute;
  CheckEquals(Param.AsCurrency, Field.AsCurrency);
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

procedure TTestCase_DBX_FieldType.Test_NUMERIC_Misc;
var S: string;
    D: TDataSet;
begin
  Param.AsFMTBCD := StrToBcdN('12345678901.2345');
  Execute;

  S := 'SELECT (Field + Field) as TestField FROM T_FIELD';
  FConnection.Execute(S, nil, D);
  try
    CheckEquals(TFMTBCDField, D.Fields[0].ClassType);
  finally
    D.Free;
  end;

  S := 'SELECT 0.00 TestField FROM T_FIELD';
  FConnection.Execute(S, nil, D);
  try
    CheckEquals(TFMTBCDField, D.Fields[0].ClassType);
  finally
    D.Free;
  end;
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
  if FRequired then StartExpectingException(TDBXError);
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

  Param.AsString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsString), Length(Field.AsString));

  Param.AsString := DupeString('B', Field.Size + 1);
  Execute;
  CheckEquals(DupeString('B', Field.Size), Field.AsString);

  Param.AsString := DupeString('C', Field.Size + 2);
  Execute;
  CheckEquals(DupeString('C', Field.Size), Field.AsString);

  Param.AsString := DupeString('D', Field.Size * 2);
  Execute;
  CheckEquals(DupeString('D', Field.Size), Field.AsString);

  Param.AsWideString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsWideString), Length(Field.AsWideString));

  {$if CompilerVersion > 18.5}
  Param.AsBytes := TBytes.Create($30, $31, $32, $33, $34, $35, $36, $37, $38, $39);
  Execute;
  CheckEquals('0123456789', Field.AsString);
  {$ifend}

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_VARCHAR_Unicode;
var F: TStringField;
begin
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

  Param.AsString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsString), Length(Field.AsString));

  Param.AsWideString := DupeString('A', Field.Size);
  Execute;
  CheckEquals(Length(Param.AsWideString), Length(Field.AsWideString));

  Param.AsString := DupeString('B', Field.Size + 1);
  Execute;
  CheckEquals(DupeString('B', Field.Size), Field.AsWideString);

  Param.AsString := DupeString('C', Field.Size + 2);
  Execute;
  CheckEquals(DupeString('C', Field.Size), Field.AsWideString);

  Param.AsString := DupeString('D', Field.Size * 2);
  Execute;
  CheckEquals(DupeString('D', Field.Size), Field.AsWideString);

  Test_Required;
end;

procedure TTestCase_DBX_FieldType.Test_VARCHAR_UNICODE_FSS;
begin
  // This test case is not valid for Firebird 1.5.0
  if Pos('1.5', GetTestData.ServerVersion) <> 0 then Exit;
  Test_VARCHAR_UNICODE;
end;

procedure TTestCase_DBX_FieldType.Test_VARCHAR_UTF8;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;
  Test_VARCHAR_Unicode;
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
begin
  FDataSet.CommandType := ctQuery;
  FDataSet.CommandText := 'SELECT FIELD AS Field1 FROM T_DATASET';
  FDataSet.Open;
  CheckTrue(Assigned(FDataSet.FindField('Field1')));
  FDataSet.Close;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_GetRowsAffected;
begin
  FDataSet.CommandType := ctTable;
  FDataSet.CommandText := 'T_DATASET';
  FDataSet.Open;
  FDataSet.Prepared := False;
  FDataSet.Close;
end;

procedure TTestCase_DBX_TSQLDataSet.Test_Field_ReadOnly;
begin
  FDataSet.CommandType := ctTable;
  FDataSet.CommandText := 'T_DATASET';
  FDataSet.Open;
  CheckFalse(FDataSet.Fields[0].ReadOnly);
  FDataSet.Close;
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
  StartExpectingException(TDBXError);
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
  Self.StartExpectingException(TDBXError);
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
    D: TDataSet;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_TRANSACTION(FIELD INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(123)');

  FConnection.BeginTransaction;
  FConnection.ExecuteDirect('INSERT INTO T_TRANSACTION VALUES(456)');
  FConnection.RollbackFreeAndNil(T);

  try
    FConnection.Execute('SELECT COUNT(*) FROM T_TRANSACTION', nil, D);
    CheckEquals(1, D.Fields[0].AsInteger);
  finally
    D.Free;
  end;

  FConnection.ExecuteDirect('DROP TABLE T_TRANSACTION');
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_ReadCommitted;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: TDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  try
    TTestSuite_DBX1 := FConnection.BeginTransaction;
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    try
      V1 := D.Fields[0].AsString;
    finally
      D.Free;
    end;

    TTestSuite_DBX2 := FConnection.BeginTransaction;
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.CommitFreeAndNil(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    try
      V2 := D.Fields[0].AsString;
    finally
      D.Free;
    end;
    FConnection.CommitFreeAndNil(TTestSuite_DBX1);

    CheckEquals('ITEM-01', V1);
    CheckEquals('ITEM-02', V2);
  finally
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_RepeatableRead;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: TDataSet;
    V1, V2: string;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_REPEAT(FIELD1 VARCHAR(10), FIELD2 INTEGER)');
  FConnection.ExecuteDirect('INSERT INTO T_REPEAT VALUES(''ITEM-01'', 1)');

  try
    TTestSuite_DBX1 := FConnection.BeginTransaction(TDBXIsolations.RepeatableRead);
    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    try
      V1 := D.Fields[0].AsString;
    finally
      D.Free;
    end;

    TTestSuite_DBX2 := FConnection.BeginTransaction(TDBXIsolations.RepeatableRead);
    FConnection.ExecuteDirect('UPDATE T_REPEAT SET FIELD1=''ITEM-02''');
    FConnection.CommitFreeAndNil(TTestSuite_DBX2);

    FConnection.Execute('SELECT * FROM T_REPEAT', nil, D);
    try
      V2 := D.Fields[0].AsString;
    finally
      D.Free;
    end;
    FConnection.CommitFreeAndNil(TTestSuite_DBX1);

    CheckEquals(V1, V2);
  finally
    FConnection.ExecuteDirect('DROP TABLE T_REPEAT');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_WaitLock;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    D: TDataSet;
    V1: integer;
begin
  FConnection.ExecuteDirect('CREATE TABLE T_LOCK(FIELD1 VARCHAR(10), FIELD2 INTEGER)');

  try
    TTestSuite_DBX1 := FConnection.BeginTransaction;
    try
      FConnection.ExecuteDirect('INSERT INTO T_LOCK VALUES(''ITEM-01'', 1)');

      TTestSuite_DBX2 := FConnection.BeginTransaction;
      try
        FConnection.Execute('SELECT COUNT(*) FROM T_LOCK', nil, D);
        try
          V1 := D.Fields[0].AsInteger;
        finally
          D.Free;
        end;
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
    FConnection.ExecuteDirect('DROP TABLE T_LOCK');
  end;
end;

procedure TTestCase_DBX_Transaction.Test_Transaction_WaitLock_Fail;
var TTestSuite_DBX1, TTestSuite_DBX2: TDBXTransaction;
    i: Integer;
begin
  if Pos('Firebird 1.', GetTestData.ServerVersion) <> 0 then Exit;

  FConnection.ExecuteDirect('CREATE TABLE T_LOCK(FIELD1 VARCHAR(10), FIELD2 INTEGER)');

  try
    TTestSuite_DBX1 := FConnection.BeginTransaction;
    try
      FConnection.ExecuteDirect('INSERT INTO T_LOCK VALUES(''ITEM-01'', 1)');

      TTestSuite_DBX2 := FConnection.BeginTransaction;

      FConnection.ExecuteDirect('DROP TABLE T_LOCK');
      for i := 1 to 5 do begin
        try
          FConnection.CommitFreeAndNil(TTestSuite_DBX2);
        except
          if i = 5 then
            FConnection.RollbackFreeAndNil(TTestSuite_DBX2);
        end;
      end;

      FConnection.CommitFreeAndNil(TTestSuite_DBX1);
    except
      FConnection.RollbackFreeAndNil(TTestSuite_DBX1);
      raise;
    end;
  finally
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
       '   FIELD VARCHAR(100), ' +
       '   F_VARCHAR_UTF8 VARCHAR(100) CHARACTER SET UTF8 ' +
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

procedure TTestCase_DBX_DataSnap.Test_MalformString;
begin
  FCDS.SetProvider(FDSP);
  FCDS.Open;
  FCDS.AppendRecord(['A', 'A']);
  CheckEquals(0, FCDS.ApplyUpdates(0));

  FCDS.AppendRecord(['B', '']);
  CheckEquals(0, FCDS.ApplyUpdates(0));
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

  S := 'INSERT INTO T_DATASET VALUES(''ABC'', ''UTF8'')';
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

procedure TTestCase_DBX_DataSnap.Test_UTF8_EmptyString;
var D: TDataSet;
begin
  {$ifndef Unicode}Exit;{$endif}
  if (Pos('Firebird 1.', GetTestData.ServerVersion) <> 0) or (Pos('Firebird 2.0', GetTestData.ServerVersion) <> 0) then Exit;

  FCDS.SetProvider(FDSP);
  FCDS.Open;
  FCDS.AppendRecord(['A', '']);
  FCDS.AppendRecord(['B', 'B']);
  FCDS.AppendRecord(['C']);
  FCDS.ApplyUpdates(0);

  FConnection.Execute('SELECT * FROM T_DATASET WHERE F_VARCHAR_UTF8 IS NULL', nil, D);
  try
    CheckEquals(1, D.RecordCount);
    CheckEquals('C', D.Fields[0].AsString);
  finally
    D.Free;
  end;

  FConnection.Execute('SELECT * FROM T_DATASET WHERE F_VARCHAR_UTF8 = ''''', nil, D);
  try
    CheckEquals(1, D.RecordCount);
    CheckEquals('A', D.Fields[0].AsString);
  finally
    D.Free;
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
  if (Pos('Firebird 2.0', FTestData1.ServerVersion) > 0) or
     (Pos('Firebird 2.0', FTestData2.ServerVersion) > 0)
  then
    Exit;

  C1 := TSQLConnection.Create(nil);
  C2 := TSQLConnection.Create(nil);
  try
    FTestData1.Setup(C1);
    C1.LoginPrompt := False;
    C1.Open;
    CheckTrue(C1.Connected);

    FTestData2.Setup(C2);
    C2.LoginPrompt := False;
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
    F.ReadSectionValues(GetServerSectionName, sServers);
    F.ReadSectionValues(GetEmbeddedSectionName, sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        if TCmdLineParams_App.HasTestName and (TCmdLineParams_App.GetTestName <> sServers.Names[j]) then Continue;

        sParams := GetParams(sServers.ValueFromIndex[j], aParams);

        sVer := GetServerVersion(F.ReadString(GetVendorSectionName, 'default', ''), sParams);

        Result.Add(
          TTestData_SQLConnection.Create(sVer, sDrivers.ValueFromIndex[i],
          sDrivers.Names[i], F.ReadString(GetVendorSectionName, sVer, sVer), sParams)
        );
      end;
      for j := 0 to sEmbeds.Count - 1 do begin
        if TCmdLineParams_App.HasTestName and (TCmdLineParams_App.GetTestName <> sEmbeds.Names[j]) then Continue;

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
    F.ReadSectionValues(GetServerSectionName, sServers);
    F.ReadSectionValues(GetEmbeddedSectionName, sEmbeds);
    for i := 0 to sDrivers.Count - 1 do begin
      for j := 0 to sServers.Count - 1 do begin
        sParams1 := GetParams(sServers.ValueFromIndex[j], aParams);
        sVer1 := GetServerVersion(F.ReadString(GetVendorSectionName, 'default', ''), sParams1);

        for k := 0 to sEmbeds.Count - 1 do begin
          if TCmdLineParams_App.HasTestName and (TCmdLineParams_App.GetTestName <> sEmbeds.Names[k]) then Continue;

          L := TInterfaceList.Create;

          L.Add(
            TTestData_SQLConnection.Create(sVer1, sDrivers.ValueFromIndex[i],
            sDrivers.Names[i], F.ReadString(GetVendorSectionName, sVer1, sVer1), sParams1)
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
          'FIELD_STR VARCHAR(30), ' +
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

procedure TTestCase_DBX_TParam.Test_Param_Negative;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftString, '1', ptInput);
  try
    P.DataType := ftUnknown;
    P.Value := AnsiString('2');
    FCDS.Open;
    CheckEquals(0, FCDS.RecordCount);
  finally
    P.Free;
  end;
end;

procedure TTestCase_DBX_TParam.Test_Param_NonUnicodeString;
var Q: TParams;
    S: AnsiString;
    B: TBytes;
begin
  B := TBytes.Create($BD, $F0, $D1, $F3, $D3, $D0, $CF, $DE, $B9, $AB, $CB, $BE);
  SetLength(S, Length(B));
  Move(B[0], S[1], Length(B));

  FConnection.Execute('DELETE FROM T_PARAM', nil);

  Q := TParams.Create;
  try
    Q.CreateParam(ftString, 'P', ptInput).{$if CompilerVersion = 18.5}AsString{$else}AsAnsiString{$ifend} := S;
    FConnection.Execute('INSERT INTO T_PARAM VALUES(2, :P, 1)', Q);
  finally
    Q.Free;
  end;

  FCDS.Close;
  FCDS.Open;
  CheckEquals(1, FCDS.RecordCount);
  CheckEquals(S, FCDS.FindField('Field_Str').{$if CompilerVersion = 18.5}AsString{$else}AsAnsiString{$ifend});
end;

procedure TTestCase_DBX_TParam.Test_Param_AnsiString;
var P: TParam;
begin
  FCDS.Close;
  P := FCDS.Params.CreateParam(ftString, 'Field_Str', ptInput);
  try
    P.Value := '1';  // Test param with AnsiString data type
    FCDS.Open;
    CheckEquals('1', FCDS.FindField('Field_Int').AsString);

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
    D: TDataSet;
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
    FConnection.Execute('SELECT * FROM PROC', nil, D);
    try
      for i := 1 to 10 do begin
        CheckEquals(i, D.Fields[0].AsInteger);
        D.Next;
      end;
    finally
      D.Free;
    end;
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

  FStoredProc.Params[0].AsWideString := aImpl;
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

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_Char_UTF8;
var s: string;
    iLen: integer;
begin
  {$ifndef Unicode} Exit; {$endif}

  if (Pos('Firebird 1.', GetTestData.ServerVersion) <> 0) or (Pos('Firebird 2.0', GetTestData.ServerVersion) <> 0) then Exit;

  s := 'One World One Dream ' +
       #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C + ' ' +
       #$540C + #$4E00 + #$4E2A + #$68A6 + #$60F3;

  CheckEquals(0, CreateProc('CHAR(100) CHARACTER SET UTF8', s));
  Check(ftWideString = FStoredProc.Params[1].DataType);

  if IsTrimChar then
    iLen := 31
  else
    iLen := 100;

  CheckEquals(iLen, Length(FStoredProc.Params[1].AsString));
  CheckEquals(s, Trim(FStoredProc.Params[1].AsString));
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
  F := {$if RTLVersion >= 23}FormatSettings.{$ifend}ShortDateFormat + ' ' + {$if RTLVersion >= 23}FormatSettings.{$ifend}LongTimeFormat;
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

procedure TTestCase_DBX_TSQLStoredProc_Params.Test_VarChar_UTF8;
var s: string;
begin
  {$ifndef Unicode} Exit; {$endif}
  
  if (Pos('Firebird 1.', GetTestData.ServerVersion) <> 0) or (Pos('Firebird 2.0', GetTestData.ServerVersion) <> 0) then Exit;

  s := 'One World One Dream ' +
       #$540C + #$4E00 + #$4E2A + #$4E16 + #$754C + ' ' +
       #$540C + #$4E00 + #$4E2A + #$68A6 + #$60F3;

  CheckEquals(0, CreateProc('VARCHAR(100) CHARACTER SET UTF8', s));
  Check(ftWideString = FStoredProc.Params[1].DataType);
  CheckEquals(s, FStoredProc.Params[1].AsString);
end;

initialization
  TTestSuite_DBX.CheckTestDataFile;
  if TCmdLineParams_App.TestSuite1 then TTestSuite_DBX1.Setup;
  if TCmdLineParams_App.TestSuite2 then TTestSuite_DBX2.Setup;
end.
