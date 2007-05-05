unit vcl.dbx.testcase;

interface

uses TestFrameWork, TestExtensions, SqlExpr;

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

  TTestCase_DBX = class(TTestCase)
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
    class function Suite: ITestSuite; override;
  published
    procedure Test_Invalid_VendorLib;
    procedure Test_Invalid_Login;
    procedure Test_Connection_Property;
    procedure Test_Open_Close;
    procedure Test_Transaction;
    procedure Test_ExecuteDirect;
    procedure Test_GetFieldNames;
    procedure Test_Execute;
    procedure Test_FieldTypes;
    procedure Test_RecordCount;
  end;

implementation

uses SysUtils, Classes, DB, DBXpress, SqlConst, Windows, StrUtils, FMTBcd;

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

procedure TTestCase_DBX.SQLMonitorOnLogTrace(Sender: TObject;
  CBInfo: pSQLTRACEDesc);
var T: String;
begin
  T := String(CBInfo^.pszTrace);
  Status(T);
end;

class function TTestCase_DBX.Suite: ITestSuite;
var S: TTestSuite;
begin
  S := TTestSuite.Create(CurrentTestData.Name);
  S.AddTests(Self);
  Result := S as ITestSuite;
end;

procedure TTestCase_DBX.TearDown;
begin
  inherited;
  FConnection.Close;
  FreeAndNil(FConnection);
  FreeAndNil(FSQLMonitor);
end;

procedure TTestCase_DBX.Test_Open_Close;
begin
  CheckTrue(FConnection.Connected);
  FConnection.Close;
  CheckFalse(FConnection.Connected);
end;

procedure TTestCase_DBX.Test_RecordCount;
var pD: ^TSQLDataSet;
    D: TSQLDataSet;
begin
Exit;
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

procedure TTestCase_DBX.Test_Transaction;
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

procedure TTestCase_DBX.Test_Connection_Property;
var iSQLDialect: Longint;
    PropSize: Smallint;
begin
  CheckEquals(DBXERR_NONE, FConnection.SQLConnection.GetOption(eConnSqlDialect, @iSQLDialect, SizeOf(iSQLDialect), PropSize));
  CheckEquals(IntTostr(iSQLDialect), FConnection.Params.Values[SQLDIALECT_KEY]);

  CheckTrue(FConnection.TransactionsSupported);
  CheckTrue(FConnection.MultipleTransactionsSupported);
end;

procedure TTestCase_DBX.Test_Execute;
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

procedure TTestCase_DBX.Test_ExecuteDirect;
begin
  FConnection.ExecuteDirect('DELETE FROM RDB$ROLES WHERE 1=2');
end;

procedure TTestCase_DBX.Test_FieldTypes;
var S: string;
    P: TParams;
    pD: ^TSQLDataSet;
    D: TSQLDataSet;
    i: integer;
begin
  S := 'CREATE TABLE T_FIELDTYPES( ' +
       '   F_VARCHAR VARCHAR(100) ' +
       ',  F_CHAR CHAR(100) ' +
       ',  F_SMALLINT SMALLINT ' +
       ',  F_INTEGER INTEGER ' +
       ',  F_BIGINT BIGINT ' +
       ',  F_NUMERIC NUMERIC(18, 4) ' +
       ',  F_DECIMAL DECIMAL(18, 4) ' +
       ',  F_FLOAT FLOAT ' +
       ',  F_DOUBLE DOUBLE PRECISION ' +
       ',  F_DATE DATE ' +
       ',  F_TIME TIME ' +
       ',  F_TIMESTAMP TIMESTAMP ' +
       ',  F_BLOB0 BLOB SUB_TYPE 0 ' +
       ',  F_BLOB1 BLOB SUB_TYPE 1 ' +
       ')';

  P := TParams.Create;
  try
    FConnection.ExecuteDirect(S);

    S := 'INSERT INTO T_FIELDTYPES ( ' +
         '  F_VARCHAR ' +
         ', F_CHAR ' +
         ', F_SMALLINT ' +
         ',  F_INTEGER ' +
         ',  F_BIGINT ' +
         ',  F_NUMERIC ' +
         ',  F_DECIMAL ' +
         ',  F_FLOAT ' +
         ',  F_DOUBLE ' +
         ',  F_DATE ' +
         ',  F_TIME ' +
//         ',  F_TIMESTAMP ' +
//         ',  F_BLOB0 ' +
//         ',  F_BLOB1 ' +
         ') ' +
         'VALUES ( ' +
         '  :F_VARCHAR ' +
         ', :F_CHAR ' +
         ', :F_SMALLINT ' +
         ',  :F_INTEGER ' +
         ',  :F_BIGINT ' +
         ',  :F_NUMERIC ' +
         ',  :F_DECIMAL ' +
         ',  :F_FLOAT ' +
         ',  :F_DOUBLE ' +
         ',  :F_DATE ' +
         ',  :F_TIME ' +
//         ',  :F_TIMESTAMP ' +
//         ',  :F_BLOB0 ' +
//         ',  :F_BLOB1 ' +
         ')';

    P.ParseSQL(S, True);
    P.ParamByName('F_VARCHAR').AsString := 'VARCHAR';
    P.ParamByName('F_CHAR').AsString := 'CHAR';
    P.ParamByName('F_SMALLINT').AsSmallInt := 13579;
    P.ParamByName('F_INTEGER').AsInteger := 1234567890;
    P.ParamByName('F_BIGINT').AsFMTBCD := StrToBcd('123456789012345');
    P.ParamByName('F_NUMERIC').AsFMTBCD := StrToBcd('12345678901.2345');
    P.ParamByName('F_DECIMAL').AsFMTBCD := StrToBcd('12345678901.2345');
    P.ParamByName('F_FLOAT').AsFloat := 1234.12345678;
    P.ParamValues['F_DOUBLE'] := 1234567.12345678;
    P.ParamByName('F_DATE').AsDate := Date;
    P.ParamByName('F_TIME').AsTime := Time;
//    P.ParamByName('F_TIMESTAMP').AsDate := Now;
//    P.ParamValues['F_BLOB0'] := 'abc';
//    P.ParamValues['F_BLOB1'] := 'abc';

    CheckEquals(1, FConnection.Execute(S, P));

    New(pD);
    try
      S := 'SELECT ' +
           '  F_VARCHAR ' +
           ',  F_CHAR ' +
           ',  F_SMALLINT ' +
           ',  F_INTEGER ' +
           ',  F_BIGINT ' +
           ',  F_NUMERIC ' +
           ',  F_DECIMAL ' +
           ',  F_FLOAT ' +
           ',  F_DOUBLE ' +
           ',  F_DATE ' +
           ',  F_TIME ' +
  //         ',  F_TIMESTAMP ' +
  //         ',  F_BLOB0 ' +
  //         ',  F_BLOB1 ' +
           'FROM T_FIELDTYPES';

      FConnection.Execute(S, nil, pD);
      D := pD^;
      CheckEquals('VARCHAR', D.FindField('F_VARCHAR').AsString);
      if IsTrimChar then
        i := 0
      else
        i := 96;
      CheckEquals('CHAR' + DupeString(' ', i), D.FindField('F_CHAR').AsString);
      CheckEquals(13579,  D.FindField('F_SMALLINT').AsInteger);
      CheckEquals(1234567890, D.FindField('F_INTEGER').AsInteger);
      CheckEquals('123456789012345', D.FindField('F_BIGINT').AsString);
      CheckEquals('12345678901.2345', D.FindField('F_NUMERIC').AsString);
      CheckEquals('12345678901.2345', D.FindField('F_DECIMAL').AsString);
      CheckEquals(1234.12345678, D.FindField('F_FLOAT').AsFloat, 0.0001);
      CheckEquals(1234567.12345678, D.FindField('F_DOUBLE').AsFloat, 0.00000001);
      CheckEquals(Date, D.FindField('F_DATE').AsDateTime);
      CheckEquals(P.ParamByName('F_TIME').AsTime, D.FindField('F_TIME').AsDateTime, 0.00001);
      D.Free;
    finally
      Dispose(pD);
    end;
  finally
    FConnection.ExecuteDirect('DROP TABLE T_FIELDTYPES');
    P.Free;
  end;
end;

procedure TTestCase_DBX.Test_GetFieldNames;
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

procedure TTestCase_DBX.Test_Invalid_Login;
begin
  FConnection.Close;
  FConnection.Params.Values[szUSERNAME] := 'no.such.user';
  if not FTestData.IsEmbedded then
    StartExpectingException(EDatabaseError);
  FConnection.Open;
end;

procedure TTestCase_DBX.Test_Invalid_VendorLib;
begin
  FConnection.Close;
  FConnection.VendorLib := 'no.such.vendorlib';
  CheckException(FConnection.Open, EDatabaseError);
end;

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

procedure xxx(const aParams: WideString);
var P: WideString;
    T: ITestSuite;
begin
  T := TTestSuite.Create('TSQLConnection: ' + StringReplace(aParams, #13#10, ' , ', [rfReplaceAll]));

  P := Format('Database=localhost:%sserver.15.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + 'RoleName=' +
       #13#10 + aParams;

  TTestCase_DBX.CurrentTestData := TTestData_SQLConnection.Create('Borland DBX Interbase Driver (Server)', 'g:\bin\dbxint30.dll', 'getSQLDriverINTERBASE', 'g:\bin\fbclient.1.5.3.dll', False, P);
  T.AddSuite(TTestCase_DBX.Suite);

//  TTestCase_DBX.CurrentTestData := TTestData_SQLConnection.Create('Upscene DBX Firebird Driver (Server)', 'g:\bin\dbxup_fb.dll', 'getSQLDriverFB', 'g:\bin\fbclient.1.5.3.dll', False, P);
//  T.AddSuite(TTestCase_DBX.Suite);

  TTestCase_DBX.CurrentTestData := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Server))', 'dbxfb.dll', 'getSQLDriverFIREBIRD', 'g:\bin\fbclient.1.5.3.dll', False, P);
  T.AddSuite(TTestCase_DBX.Suite);

  P := Format('Database=%sembed.15.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + 'RoleName=' +
       #13#10 + aParams;

  TTestCase_DBX.CurrentTestData := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Embedded, ODS 10.1)', 'dbxfb.dll', 'getSQLDriverFIREBIRD', Format('%sfbembed.10.1\fbembed.dll', [ExtractFilePath(ParamStr(0))]), True, P);
  T.AddSuite(TTestCase_DBX.Suite);

  P := Format('Database=%sembed.20.fdb', [ExtractFilePath(ParamStr(0))]) +
       #13#10 + SQLDIALECT_KEY + '=3' +
       #13#10 + szUSERNAME + '=SYSDBA' +
       #13#10 + szPASSWORD + '=masterkey' +
       #13#10 + aParams;

  TTestCase_DBX.CurrentTestData := TTestData_SQLConnection.Create('TeamOO DBX Firebird Driver (Embedded, ODS 11.0)', 'dbxfb.dll', 'getSQLDriverFIREBIRD', Format('%sfbembed.11.0\fbembed.dll', [ExtractFilePath(ParamStr(0))]), True, P);
  T.AddSuite(TTestCase_DBX.Suite);

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
  xxx(P);
end.
