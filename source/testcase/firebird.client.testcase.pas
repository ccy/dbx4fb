unit firebird.client.testcase;

interface

uses
  System.SysUtils, TestExtensions, TestFramework,
  Firebird, Firebird.helper, firebird.client;

type
  TODS_TestCase = class(TTestCase)
  public
  published
    procedure Test_Implicit_Integer;
    procedure Test_Implicit_string;
    procedure Test_Equal_NotEqual;
    procedure Test_GreaterThan;
    procedure Test_GreaterThanOrEqual;
    procedure Test_LessThan;
    procedure Test_LessThanOrEqual;
    procedure Test_Major;
    procedure Test_Minor;
    procedure Test_ToString;
  end;

  TPageSize_TestCase = class(TTestCase)
  published
    procedure Test_Create;
  end;

  TFirebirdEngine_TestCase = class(TTestCase)
  published
    procedure Test_version;
  end;

  TTestCase_FirebirdConnectionString = class(TTestCase)
  private
    procedure Test(Value: string; aProtocol: TFirebirdConnectionStringProtocol;
        Host, Port, Database, ServiceMgrPrefix: string);
  published
    procedure Test_local;
    procedure Test_wnet_tra;
    procedure Test_tcp_tra;
    procedure Test_inet;
    procedure Test_wnet;
    procedure Test_xnet;
  end;

  TTestCase_FirebirdAPI = class(TTestCase)
  const
    EmployeeDB = 'employee';
  strict private
    api: TFirebirdAPI;
    st: IStatus;
    FEngines: TFirebirdEngines;
    FProviders: TArray<string>;
    FHandle: THandle;
    function GetTempFileName(aProvidersOrHost: string): string;
    procedure Log(aLog: string);
    procedure LogBuffer(const Buffer; Count: NativeInt);
    procedure ShowBurpData(a: TBurpData);
  protected
    class function GetEmbeddedSectionName: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Finalize;
    procedure Test_attachDatabase;
    procedure Test_attachServiceManager;
    procedure Test_Backup_Restore;
    procedure Test_CreateDatabase;
    procedure Test_getConfigManager;
    procedure Test_getPlans;
    procedure Test_GetServiceInfo;
    procedure Test_nBackup_nRestore_nFix;
  end;

implementation

uses
  Winapi.Windows, System.AnsiStrings, System.Classes, System.IniFiles,
  System.IOUtils, System.Math,
  firebird.ods.h, firebird.sqlda_pub.h,
  vcl.dbx.cmdlines;

procedure TPageSize_TestCase.Test_Create;
begin
  var c := MIN_PAGE_SIZE;

  var i := Low(TPageSize);
  while c <= MAX_PAGE_SIZE do begin
    Check(TPageSize.Create(c) = i);
    Inc(i);
    c := c shl 1;
  end;

  Check(TPageSize.Create(2048) = ps4096);
  Check(TPageSize.Create(2090) = ps4096);
  Check(TPageSize.Create(4095) = ps4096);
  Check(TPageSize.Create(4097) = ps4096);
  Check(TPageSize.Create(8191) = ps4096);
  Check(TPageSize.Create(8193) = ps8192);
  Check(TPageSize.Create(16383) = ps8192);
  Check(TPageSize.Create(16384) = ps16384);
  Check(TPageSize.Create(16385) = ps16384);
end;

procedure TFirebirdEngine_TestCase.Test_version;
begin
  var E: TFirebirdEngine;

  E := 'LI-V3.0.12.33787 Firebird 3.0';
  CheckTrue(E.Platform = pfLinux);
  CheckEquals(E.Major, 3);
  CheckEquals(E.Minor, 0);
  CheckEquals(E.Release, 12);
  CheckEquals(E.Build, 33787);

  E := 'WI-V5.0.2.1613 Firebird 5.0';
  CheckTrue(E.Platform = pfWindows);
  CheckEquals(E.Major, 5);
  CheckEquals(E.Minor, 0);
  CheckEquals(E.Release, 2);
  CheckEquals(E.Build, 1613);
end;

procedure TTestCase_FirebirdConnectionString.Test_tcp_tra;
begin
  var p := tcp_tra;

  Test('a:b',                           p, 'a',         '',       'b',              'a:');
  Test('a:c:\db\employee',              p, 'a',         '',       'c:\db\employee', 'a:');
  Test('linux/3051',                    p, 'linux',     '3051',   '',               'linux/3051:');
  Test('linux/3050:employee',           p, 'linux',     '3050',   'employee',       'linux/3050:');
  Test('winserver:c:\db\my.fdb',        p, 'winserver', '',       'c:\db\my.fdb',   'winserver:');
  Test('winserver/gds_db:c:\db\my.fdb', p, 'winserver', 'gds_db', 'c:\db\my.fdb',   'winserver/gds_db:');
  Test('winserver/13832:c:\db\my.fdb',  p, 'winserver', '13832',  'c:\db\my.fdb',   'winserver/13832:');
  Test('127.0.0.1:d:\db\my.fdb',        p, '127.0.0.1', '',       'd:\db\my.fdb',   '127.0.0.1:');
  Test('127.0.0.1/3050:d:\db\my.fdb',   p, '127.0.0.1', '3050',   'd:\db\my.fdb',   '127.0.0.1/3050:');
  Test('localhost',                     p, 'localhost', '',       '',               'localhost:');
  Test('localhost/3050',                p, 'localhost', '3050',   '',               'localhost/3050:');
  Test('localhost:/opt/db/my.fdb',      p, 'localhost', '',       '/opt/db/my.fdb', 'localhost:');
  Test('localhost/3050:/opt/db/my.fdb', p, 'localhost', '3050',   '/opt/db/my.fdb', 'localhost/3050:');
  Test('[::1]',                         p, '[::1]',     '',       '',               '[::1]:');
  Test('[::1]/3050',                    p, '[::1]',     '3050',   '',               '[::1]/3050:');
  Test('[::1]:/opt/db/my.fdb',          p, '[::1]',     '',       '/opt/db/my.fdb', '[::1]:');
  Test('[::1]/3050:/opt/db/my.fdb',     p, '[::1]',     '3050',   '/opt/db/my.fdb', '[::1]/3050:');
end;

procedure TTestCase_FirebirdConnectionString.Test(Value: string; aProtocol:
    TFirebirdConnectionStringProtocol; Host, Port, Database, ServiceMgrPrefix:
    string);
begin
  var a: TFirebirdConnectionString := Value;
  Check(aProtocol = a.Protocol,     'Protocol: ' + Value);
  CheckEquals(Host, a.Host,         'Host: ' + Value);
  CheckEquals(Port, a.Port,         'Port: ' + Value);
  CheckEquals(Database, a.Database, 'Database: ' + Value);
  CheckEquals(Value, a,             'Value: ' + Value);
  CheckEquals(ServiceMgrPrefix + TFirebird.service_mgr, a.AsServiceManager,  'Service Manager: ' + Value);
end;

procedure TTestCase_FirebirdConnectionString.Test_inet;
const Delim: array[Boolean] of string = ('/', '');
begin
  for var r in [inet, inet4, inet6] do begin
    var ps := r.ToString(True);

    for var Host in ['127.0.0.1', 'localhost', 'example.com', '[::1]', '[fe80:baba:caca:dada:fafa::1]'] do begin
      for var Port in ['', ':3050'] do begin
        for var DB in ['', 'employee', 'a:\employee', 'a:/employee', 'data/employee', '/data/employee'] do begin
          var aPort := Port;
          if not aPort.IsEmpty then
            aPort := aPort.Substring(1);
          Test(ps + Host + Port + Delim[DB.IsEmpty] + DB, r, Host, aPort, DB, ps + Host + Port + '/');
        end;
      end;
    end;
  end;
end;

procedure TTestCase_FirebirdConnectionString.Test_wnet;
begin
  var p := wnet;
  var ps := wnet.ToString(True);

  Test('wnet://winserver/d:/db/my.fdb',      p, 'winserver', '',     'd:/db/my.fdb', ps + 'winserver/');
  Test('wnet://winserver:3051/d:/db/my.fdb', p, 'winserver', '3051', 'd:/db/my.fdb', ps + 'winserver:3051/');
end;

procedure TTestCase_FirebirdConnectionString.Test_xnet;
begin
  var p := xnet;
  var ps := xnet.ToString(True);

  Test('xnet://d:/db/my.fdb', p, '', '', 'd:/db/my.fdb', ps);
  Test('xnet://MyDB'        , p, '', '', 'MyDB',         ps);
end;

procedure TTestCase_FirebirdConnectionString.Test_wnet_tra;
begin
  var p := wnet_tra;
  var ps := wnet_tra.ToString(True);

  Test('\\winserver\c:\db\my.fdb',               p, 'winserver',               '', 'c:\db\my.fdb', ps + 'winserver\');
  Test('\\.\c:\db\my.fdb',                       p, '.',                       '', 'c:\db\my.fdb', ps + '.\');
  Test('\\localhost\c:\db\my.fdb',               p, 'localhost',               '', 'c:\db\my.fdb', ps + 'localhost\');
  Test('\\winserver@MyAppInstance\c:\db\my.fdb', p, 'winserver@MyAppInstance', '', 'c:\db\my.fdb', ps + 'winserver@MyAppInstance\');
  Test('\\.@MyAppInstance\c:\db\my.fdb',         p, '.@MyAppInstance',         '', 'c:\db\my.fdb', ps + '.@MyAppInstance\');
end;

procedure TTestCase_FirebirdConnectionString.Test_local;
begin
  for var d in [
    '', 'employee.db', 'employee.fdb', 'd:\db\my.fdb', 'd:/db/my.fdb'
  , '/mnt/employee', '/mnt/emp.fdb', '/mnt/db/emp.fdb'
  ] do
    Test(d, local, '', '', d, '');
end;

class function TTestCase_FirebirdAPI.GetEmbeddedSectionName: string;
begin
  Result := 'embedded.' +
            {$ifdef Win32}'x86'{$endif}
            {$ifdef Win64}'x64'{$endif}
            ;
end;

function TTestCase_FirebirdAPI.GetTempFileName(aProvidersOrHost: string):
    string;
begin
  var TmpPath := TPath.GetTempPath;
  if not aProvidersOrHost.StartsWith(TFirebird.FB_Config_Providers, True) then begin
    api.Reset.SetConnectionString('', aProvidersOrHost);
    var n := api.GetServiceInfo;

    if n.IsWindows then
      TmpPath := 'c:\users\public\'
    else
      TmpPath := '/tmp/';
  end;

  Result := TmpPath + TPath.GetRandomFileName + '.tmp';
end;

procedure TTestCase_FirebirdAPI.Log(aLog: string);
begin
  status(aLog);
end;

procedure TTestCase_FirebirdAPI.LogBuffer(const Buffer; Count: NativeInt);
begin
  var s := '';
  SetString(s, PAnsiChar(@Buffer), Count);
  Log(s);
end;

procedure TTestCase_FirebirdAPI.SetUp;
begin
  inherited;
  var fbclient := '';
  FProviders := [];

  var F := TIniFile.Create(TCmdLineParams_App.ConfigFile);
  var L := TStringList.Create;
  try
    fbclient := ExpandFileNameString(F.ReadString(GetEmbeddedSectionName, 'Default', ''));
    F.ReadSectionValues('server', L);
    for var i := 0 to L.Count - 1 do
      FProviders := FProviders + [L.ValueFromIndex[i]];
  finally
    L.Free;
    F.Free;
  end;

  FEngines := TFirebirdEngines.Create(fbclient);

  var S: TArray<string>;
  for var E in FEngines do
    S := S + [FEngines.GetProviders(E)];

  FProviders := S + FProviders;

  api.New(fbclient, st);
end;

procedure TTestCase_FirebirdAPI.ShowBurpData(a: TBurpData);
begin
  status('att_backup_date = '              + a.att_backup_date);
  status('att_type_att_backup_format = '   + a.att_type_att_backup_format.ToString);
  status('att_backup_os = '                + a.att_backup_os.ToString);
  status('att_backup_compress = '          + a.att_backup_compress.ToString(TUseBoolStrs.True));
  status('att_backup_transportable = '     + a.att_backup_transportable.ToString(TUseBoolStrs.True));
  status('att_backup_blksize = '           + a.att_backup_blksize.ToString);
  status('att_backup_file = '              + a.att_backup_file);
  status('att_backup_volume = '            + a.att_backup_volume.ToString);
  status('att_backup_keyname = '           + a.att_backup_keyname);
  status('att_backup_zip = '               + a.att_backup_zip);
  status('att_backup_hash = '              + a.att_backup_hash);
  status('att_backup_crypt = '             + a.att_backup_crypt);

  status('att_file_name = '                + a.att_file_name);
  status('att_file_size = '                + a.att_file_size.ToString);
  status('att_jrd_version = '              + a.att_jrd_version.ToString);
  status('att_creation_date = '            + a.att_creation_date);
  status('att_page_size = '                + a.att_page_size.ToString);
  status('att_database_description = '     + a.att_database_description);
  status('att_database_security_class = '  + a.att_database_security_class);
  status('att_sweep_interval = '           + a.att_sweep_interval.ToString);
  status('att_no_reserve = '               + a.att_no_reserve.ToString(TUseBoolStrs.True));
  status('att_database_description2 = '    + a.att_database_description2);
  status('att_database_dfl_charset = '     + a.att_database_dfl_charset);
  status('att_forced_writes = '            + a.att_forced_writes.ToString(TUseBoolStrs.True));
  status('att_page_buffers = '             + a.att_page_buffers.ToString);
  status('att_SQL_dialect = '              + a.att_SQL_dialect.ToString);
  status('att_db_read_only = '             + a.att_db_read_only.ToString(TUseBoolStrs.True));
  status('att_database_linger = '          + a.att_database_linger.ToString);
  status('att_database_sql_security_deprecated = ' + a.att_database_sql_security_deprecated.ToString(TUseBoolStrs.True));
  status('att_replica_mode = '             + a.att_replica_mode.ToString);
  status('att_database_sql_security = '    + a.att_database_sql_security.ToString(TUseBoolStrs.True));
end;

procedure TTestCase_FirebirdAPI.TearDown;
begin
  FreeLibrary(FHandle);
  inherited;
end;

procedure TTestCase_FirebirdAPI.Test_attachDatabase;
begin
  for var i := Low(FProviders) to High(FProviders) do begin
    var Engine: TFirebirdEngine;
    var bIsEngine := FEngines.GetEngineByProviders(FProviders[i], Engine);

    if (i = 0) or not bIsEngine then begin
      api.Reset.SetConnectionString(EmployeeDB, FProviders[i]).AttachDatabase.detach(st);
      status(FProviders[i]);
    end else begin
      try
        api.Reset.SetConnectionString(EmployeeDB, FProviders[i]).AttachDatabase.detach(st);
        status('Expected fail: ' + FProviders[i]);
      except
//        on E: Exception do status(E.Message + ' '+ FEngines[i].FileName);
      end;
    end;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_attachServiceManager;
begin
  for var p in FProviders do begin
    api.Reset.SetProviders(p).AttachServiceManager.detach(st);
    status(p);
  end;
end;

procedure TTestCase_FirebirdAPI.Test_Backup_Restore;
begin
  var DefaultPageSizes, SupportedPageSizes: TArray<Integer>;
  for var p in FProviders do begin
    var Engine: TFirebirdEngine;
    var bIsEngine := FEngines.GetEngineByProviders(p, Engine);

    var fdb := EmployeeDB;
    var bOwnDB := False;
    if bIsEngine then begin
      SupportedPageSizes := Engine.SupportedPageSizes;
      if not Engine.IsCurrent then begin
        DefaultPageSizes := SupportedPageSizes;
        fdb := GetTempFileName(p);
        bOwnDB := True;
      end;
    end else
      SupportedPageSizes := DefaultPageSizes;

    var fbk := GetTempFileName(p);
    var fbkStream := TMemoryStream.Create;
    try
      api.Reset.SetConnectionString(fdb, p);
      if bOwnDB then api.CreateDatabase;

      var BackupODS := api.GetDatabaseInfo.ODS;

      api.Backup(LogBuffer, fbk);
      api.Backup(
        procedure(const Buf; Count: NativeInt)
        begin
          fbkStream.Write(Buf, Count);
        end
      );

      fbkStream.Position := 0;
      var burpData: TBurpData := fbkStream;
      ShowBurpData(burpData);

      status(Format('Backup: %s Database Size: %d Backup Size: %d', [p, TFile.GetSize(fdb), TFile.GetSize(fbk)]));

      for var PageSize in SupportedPageSizes do begin
        for var ForceWrite := Low(Boolean) to High(Boolean) do begin
          var PageBuffers := Max(TFirebird.MIN_PAGE_BUFFERS, Random(TFirebird.MAX_PAGE_BUFFERS div 5));

          // restore with file
          var res := GetTempFileName(p);
          api.Reset
             .SetConnectionString(res, p)
             .SetPageSize(TPageSize.Create(PageSize))
             .SetPageBuffers(PageBuffers)
             .SetForcedWrite(ForceWrite)
             .Restore(fbk, LogBuffer);
          var Info := api.GetDatabaseInfo;
          Check(PageSize = Info.page_size);
          CheckEquals(PageBuffers, Info.num_buffers);
          CheckEquals(SQL_DIALECT_CURRENT, Info.db_sql_dialect);
          CheckEquals(ForceWrite, Info.forced_writes);
          status(Format('Restore Database Size: %d Version: %s ODS: %s', [TFile.GetSize(res), Info.firebird_version, Info.ODS.ToString]));
          api.DropDatabase;

          // restore with stdin
          fbkStream.Position := 0;
          api.Reset
            .SetConnectionString(res, p)
            .SetPageSize(TPageSize.Create(PageSize))
            .SetPageBuffers(PageBuffers)
            .SetForcedWrite(ForceWrite)
            .Restore(
            function(var Buffer; Count: Word): Word
            begin
              Result := fbkStream.Read(Buffer, Count);
            end
          , LogBuffer);
          Info := api.GetDatabaseInfo;
          Check(PageSize = Info.page_size);
          CheckEquals(PageBuffers, Info.num_buffers);
          CheckEquals(SQL_DIALECT_CURRENT, Info.db_sql_dialect);
          CheckEquals(ForceWrite, Info.forced_writes);
          status(Format('Restore Database Size: %d Version: %s ODS: %s', [TFile.GetSize(res), Info.firebird_version, Info.ODS.ToString]));
          api.DropDatabase;

          // restore to local engine
          for var RestoreEngine in FEngines do begin
            if BackupODS > RestoreEngine.ODS then Continue;
            res := GetTempFileName(FEngines.GetProviders(RestoreEngine));
            fbkStream.Position := 0;
            api.Reset
              .SetConnectionString(res, FEngines.GetProviders(RestoreEngine))
              .SetPageSize(TPageSize.Create(PageSize))
              .SetPageBuffers(PageBuffers)
              .SetForcedWrite(ForceWrite)
              .Restore(
              function(var Buffer; Count: Word): Word
              begin
                Result := fbkStream.Read(Buffer, Count);
              end
            , LogBuffer);
            Info := api.GetDatabaseInfo;
            Check(PageSize = Info.page_size);
            CheckEquals(PageBuffers, Info.num_buffers);
            CheckEquals(SQL_DIALECT_CURRENT, Info.db_sql_dialect);
            CheckEquals(ForceWrite, Info.forced_writes);
            status(Format('%s Restore Database Size: %d Version: %s ODS: %s', [RestoreEngine.FileName, TFile.GetSize(res), Info.firebird_version, Info.ODS.ToString]));
            status(res);
            api.DropDatabase;
          end;
        end;
      end;

      status('');
    finally
      if bOwnDB then api.Reset.SetConnectionString(fdb, p).DropDatabase;
      if TFile.Exists(fbk) then TFile.Delete(fbk);
      fbkStream.Free;
    end;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_CreateDatabase;
begin
  for var p in FProviders do begin
    var Engine: TFirebirdEngine;
    var bIsEngine := FEngines.GetEngineByProviders(p, Engine);

    var SupportedPageSizes := FEngines[1].SupportedPageSizes;
    if bIsEngine then SupportedPageSizes := Engine.SupportedPageSizes;

    for var PageSize in SupportedPageSizes do begin
      for var ForceWrite := Low(Boolean) to High(Boolean) do begin
        var fdb := GetTempFileName(p);
        var PageBuffers := Max(TFirebird.MIN_PAGE_BUFFERS, Random(TFirebird.MAX_PAGE_BUFFERS div 5));

        status(Format('%s: %s PageSize: %d ForceWrite: %s PageBuffers: %d', [p, fdb, PageSize, ForceWrite.ToString(TUseBoolStrs.True), PageBuffers]));

        api
          .Reset
          .SetConnectionString(fdb, p)
          .SetPageSize(TPageSize.Create(PageSize))
          .SetForcedWrite(ForceWrite)
          .SetPageBuffers(PageBuffers)
          .CreateDatabase;
        try
          var info := api.GetDatabaseInfo;
          CheckEquals(PageSize, Info.page_size);
          CheckEquals(SQL_DIALECT_CURRENT, Info.db_sql_dialect);
          if bIsEngine then Check(Engine.ODS = Info.ODS);
          CheckEquals(ForceWrite, Info.forced_writes);
          CheckEquals(PageBuffers, Info.num_buffers);
          status(Info.firebird_version);
        finally
          api.DropDatabase;
        end;
        status('');
      end;
    end;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_Finalize;
begin
  var H := GetModuleHandle(PChar(api.FBClient));
  CheckNotEquals(0, H, api.FBClient + ' must loaded');

  // Invoke chaining method to make sure it doesn't invoke Assign method followed by Finalize method that
  // FreeLibrary.  Unexpected error raised if fbclient.dll is unloaded from runtime process.
  for var i := 1 to 1000 do
    api.Reset;

  H := GetModuleHandle(PChar(api.FBClient));
  CheckNotEquals(0, H, api.FBClient + ' should not unload');
end;

procedure TTestCase_FirebirdAPI.Test_getConfigManager;
begin
  var c := api.master.getConfigManager;
  var fbconf := c.getFirebirdConf;
  status('Version: ' + fbconf.getVersion(st).ToHexString);

  for var e in TFirebirdConfig.entries do begin
    var v: string;
    case e.data_type of
      TYPE_BOOLEAN: v := fbconf.asBoolean(fbconf.getKey(e.key)).ToString;
      TYPE_INTEGER: v := fbconf.asInteger(fbconf.getKey(e.key)).ToString;
      TYPE_STRING:  v := fbconf.asString(fbconf.getKey(e.key), 0);
    end;
    status(e.key + '=' + v);
  end;

  for var d := 0 to IConfigManager.DIR_COUNT - 1 do begin
    var s := c.getDirectory(d, 0);
    status(Format('Directory %d: %s', [d, s]));
  end;
end;

procedure TTestCase_FirebirdAPI.Test_getPlans;
begin
  for var p in FProviders do begin
    var fdb := GetTempFileName(p);
    status(Format('%s: %s', [p, fdb]));

    var a := api.Reset.SetConnectionString(fdb, p).CreateDatabase(0);
    try
      var i := api.GetPlans(['', 'invalid sql', 'select * from rdb$database', 'select * from mon$database']);
      for var s in i do status(s);
      status('');
    finally
      a.dropDatabase(st);
    end;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_GetServiceInfo;
begin
  for var p in FProviders do begin
    var fdb := GetTempFileName(p);

    api.SetConnectionString(fdb, p).CreateDatabase;

    var a: TArray<Iattachment>;
    SetLength(a, 10);
    for var i := Low(a) to High(a) do
      a[i] := api.AttachDatabase;

    var n := api.GetServiceInfo;

    for var i := Low(a) to High(a) do
      a[i].detach(st);

    api.DropDatabase;

    status('');
    status(p);
    status('db_name = ' + string.Join('; ', n.db_name));
    status('svc_version = ' + n.svc_version.ToString);
    status('svc_server_version = ' + n.svc_server_version);
    status('svc_implementation = ' + n.svc_implementation);
    status('num_att = ' + n.num_att.ToString);
    status('num_db = ' + n.num_db.ToString);
    status('get_env = ' + n.get_env);

    CheckEquals(Length(a), n.num_att);
    CheckEquals(1, n.num_db);
  end;
end;

procedure TTestCase_FirebirdAPI.Test_nBackup_nRestore_nFix;
begin
  for var i := Low(FProviders) to High(FProviders) do begin
    var fdb := GetTempFileName(FProviders[i]);
    var fdb_res := GetTempFileName(FProviders[i]);

    const Count = 5;
    var nbk: TArray<string>;
    for var j := 0 to Count do
      nbk := nbk + [GetTempFileName(FProviders[i])];

    try
      var a := api.Reset.SetConnectionString(fdb, FProviders[i]).CreateDatabase(0);

      // Unknown bug: A fresh database must have some activity before it can perform nbackup operation
      a.startTransaction(st, 0, nil).commit(st);
      a.detach(st);

      if api.GetDatabaseInfo.ODS < ODS_13_0 then
        // Issue 1: Firebird 3.0 does not support isc_action_svc_nfix
        // Issue 2: https://github.com/FirebirdSQL/firebird/issues/7579: Cannot nbackup a firebird 3.0 database in firebird 4.0 service with engine12 setup in Providers
        Continue;

      try
        status(Format('nbackup: %s %s', [FProviders[i], fdb]));
        for var j := 0 to Count do
          api.nBackup(nbk[j], LogBuffer, j);

        status(Format('nrestore: %s %s', [FProviders[i], fdb_res]));
        api.SetConnectionString(fdb_res, FProviders[i]).nRestore(nbk, LogBuffer);

        api.nFix(nbk[0]);
      finally
        api.SetConnectionString(nbk[0], FProviders[i]).DropDatabase;
        for var j := Low(nbk) + 1 to High(nbk) do
          if TFile.Exists(nbk[j]) then
            TFile.Delete(nbk[j]);
        api.SetConnectionString(fdb_res, FProviders[i]).DropDatabase;
      end;
    finally
      api.SetConnectionString(fdb, FProviders[i]).DropDatabase;
    end;
  end;
end;

procedure TODS_TestCase.Test_Equal_NotEqual;
begin
  var a, b: TODS;

  a := '13.0';
  b := '13.0';
  Check(a = b);

  a := '12.0';
  b := '13.0';
  Check(a <> b);
end;

procedure TODS_TestCase.Test_GreaterThan;
var a, b: TODS;
begin
  a := '11.1';
  b := '11.2';
  Check(b > a);

  a := '12.0';
  b := '11.2';
  Check(a > b);

  a := '12.0';
  b := '12.0';
  CheckFalse(a > b);
  CheckFalse(b > a);
end;

procedure TODS_TestCase.Test_GreaterThanOrEqual;
var a, b: TODS;
begin
  a := '11.1';
  b := '11.2';
  Check(b >= a);

  a := '12.0';
  b := '11.2';
  Check(a >= b);
end;

procedure TODS_TestCase.Test_LessThan;
var a, b: TODS;
begin
  a := '11.1';
  b := '11.2';
  Check(a < b);

  a := '12.0';
  b := '11.2';
  Check(b < a);
end;

procedure TODS_TestCase.Test_LessThanOrEqual;
var a, b: TODS;
begin
  a := '11.2';
  b := '11.2';
  Check(a <= b);

  a := '12.0';
  b := '11.2';
  Check(b <= a);
end;

procedure TODS_TestCase.Test_Implicit_Integer;
var a: TODS;
begin
  a := ODS_11_0;
  CheckEquals('11.0', a);

  a := ODS_11_1;
  CheckEquals('11.1', a);

  a := ODS_12_0;
  CheckEquals('12.0', a);
end;

procedure TODS_TestCase.Test_Implicit_string;
var a: TODS;
begin
  a := '11.1';
  CheckEquals(ODS_11_1, a);

  a := '11.2';
  CheckEquals(ODS_11_2, a);

  a := '12.0';
  CheckEquals(ODS_12_0, a);
end;

procedure TODS_TestCase.Test_Major;
var o: TODS;
begin
  o := '11.1';
  CheckEquals(11, o.Major);

  o := '11.2';
  CheckEquals(11, o.Major);

  o := '12.0';
  CheckEquals(12, o.Major);
end;

procedure TODS_TestCase.Test_Minor;
var o: TODS;
begin
  o := '11.1';
  CheckEquals(1, o.Minor);

  o := '11.2';
  CheckEquals(2, o.Minor);

  o := '12.0';
  CheckEquals(0, o.Minor);
end;

procedure TODS_TestCase.Test_ToString;
var o: TODS;
begin
  o := TODS.Create(11, 1);
  CheckEquals('11.1', o.ToString);

  o := TODS.Create(11, 2);
  CheckEquals('11.2', o.ToString);

  o := TODS.Create(12, 0);
  CheckEquals('12.0', o.ToString);
end;

initialization
  if not TCmdLineParams_App.TestSuite3 then Exit;
  RegisterTest(TODS_TestCase.Suite);
  RegisterTest(TPageSize_TestCase.Suite);
  RegisterTest(TFirebirdEngine_TestCase.Suite);
  RegisterTest(TTestCase_FirebirdConnectionString.Suite);
  RegisterTest(TTestCase_FirebirdAPI.Suite);
end.
