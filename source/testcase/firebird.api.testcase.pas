unit firebird.api.testcase;

interface

uses
  TestExtensions, TestFramework,
  Firebird, Firebird.helper, firebird.client;

type
  TTestCase_FirebirdAPI = class(TTestCase)
  strict private
    fbstatus: IStatus;
    FEngines: TFirebirdEngines;
    FHandle: THandle;
    master: IMaster;
    prov: IProvider;
    util: IUtil;
  protected
    class function GetEmbeddedSectionName: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_attachDatabase;
    procedure Test_attachServiceManager;
    procedure Test_createDatabase;
    procedure Test_getConfigManager;
  end;

implementation

uses
  Winapi.Windows, System.AnsiStrings, System.IniFiles, System.IOUtils,
  System.SysUtils,
  firebird.constants.h, firebird.consts_pub.h, firebird.inf_pub.h, firebird.ods.h,
  firebird.sqlda_pub.h,
  vcl.dbx.cmdlines;

type
  TFirebirdDBVersionCallback = class(IVersionCallbackImpl)
    FValues: TArray<string>;
		procedure callback(status: IStatus; text: PAnsiChar); override;
    property Values: TArray<string> read FValues;
  end;

procedure TFirebirdDBVersionCallback.callback(status: IStatus; text: PAnsiChar);
begin
  inherited;
  FValues := FValues + [string(System.AnsiStrings.StrPas(text))];
end;

class function TTestCase_FirebirdAPI.GetEmbeddedSectionName: string;
begin
  Result := 'embedded.' +
            {$ifdef Win32}'x86'{$endif}
            {$ifdef Win64}'x64'{$endif}
            ;
end;

procedure TTestCase_FirebirdAPI.SetUp;
begin
  inherited;
  var fbclient := '';

  var F := TIniFile.Create(TCmdLineParams_App.ConfigFile);
  try
    fbclient := ExpandFileNameString(F.ReadString(GetEmbeddedSectionName, 'Default', ''));
  finally
    F.Free;
  end;

  master := fb_get_master_interface(fbclient, FHandle);
  fbstatus := master.getStatus;
  util := master.getUtilInterface;
  prov := master.getDispatcher;

  FEngines := TFirebirdEngines.Create(fbclient);
end;

procedure TTestCase_FirebirdAPI.TearDown;
begin
  fbstatus := nil;
  util := nil;
  prov := nil;
  master := nil;
  FreeLibrary(FHandle);
  inherited;
end;

procedure TTestCase_FirebirdAPI.Test_getConfigManager;
begin
  var c := master.getConfigManager.getFirebirdConf;
  status('Version: ' + c.getVersion(fbstatus).ToHexString);

  for var e in TFirebirdConf.entries do begin
    var v: string;
    case e.data_type of
      TYPE_BOOLEAN: v := c.asBoolean(c.getKey(e.key)).ToString;
      TYPE_INTEGER: v := c.asInteger(c.getKey(e.key)).ToString;
      TYPE_STRING:  v := c.asString(c.getKey(e.key), 0);
    end;
    status(e.key + '=' + v);
  end;

  var s := master.getConfigManager.getDirectory(IConfigManager.DIR_PLUGINS, 0);
  status('Directory plugins: ' + s);
end;

procedure TTestCase_FirebirdAPI.Test_attachDatabase;
begin
  var p := util.getXpbBuilder(fbstatus, IXpbBuilder.DPB, nil, 0);
  try
    p.insertString(fbstatus, isc_dpb_config, FEngines.GetProviders);
    var a := prov.attachDatabase(fbstatus, 'employee', p.getBufferLength(fbstatus), p.getBuffer(fbstatus));
    try
      var v := TFirebirdDBVersionCallback.Create;
      try
        util.getFbVersion(fbstatus, a, v);
        for var s in v.Values do Status(s);
      finally
        v.Free;
      end;
    finally
      a.detach(fbstatus);
    end;
  finally
    p.dispose;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_attachServiceManager;
begin
  var p := util.getXpbBuilder(fbstatus, IXpbBuilder.SPB_ATTACH, nil, 0);
  try
    p.insertString(fbstatus, isc_spb_user_name, DBA_USER_NAME);
    p.insertString(fbstatus, isc_spb_password, TFirebird.DefaultDBAPassword);
    var a := prov.attachServiceManager(fbstatus, TFirebird.service_mgr, p.getBufferLength(fbstatus), p.getBuffer(fbstatus));
    try
      var res: TBytes;
      SetLength(res, High(Byte));

      var b := TBytes.Create(isc_info_svc_version, isc_info_svc_server_version, isc_info_svc_implementation, isc_info_end);
      a.query(fbstatus, 0, nil, Length(b), @b[0], Length(res), @res[0]);

      var r := util.getXpbBuilder(fbstatus, IXpbBuilder.SPB_RESPONSE, @res[0], Length(res));
      try
        while r.getTag(fbstatus) <> isc_info_end do begin
          case r.getTag(fbstatus) of
            isc_info_svc_version: status(r.getInt(fbstatus).ToString);
            isc_info_svc_server_version: status(string(System.AnsiStrings.StrPas(r.getString(fbstatus))));
            isc_info_svc_implementation: status(string(System.AnsiStrings.StrPas(r.getString(fbstatus))));
          end;
          r.moveNext(fbstatus);
        end;
      finally
        r.dispose;
      end;
    finally
      a.detach(fbstatus);
    end;
  finally
    p.dispose;
  end;
end;

procedure TTestCase_FirebirdAPI.Test_createDatabase;
begin
  for var Engine in FEngines do begin
    for var PageSize in Engine.SupportedPageSizes do begin
      var p := util.getXpbBuilder(fbstatus, IXpbBuilder.DPB, nil, 0);
      try
        p.insertString(fbstatus, isc_dpb_config, FEngines.GetProviders(Engine));
        p.insertInt(fbstatus, isc_dpb_page_size, PageSize);

        var fdb := TPath.ChangeExtension(TPath.GetTempPath + TPath.GetRandomFileName, 'fdb');
        status(Format('Database: %s Engine: %s PageSize: %d', [fdb, Engine.Version, PageSize]));
        var a := prov.createDatabase(fbstatus, fdb, p.getBufferLength(fbstatus), p.getBuffer(fbstatus));

        var v := TFirebirdDBVersionCallback.Create;
        try
          util.getFbVersion(fbstatus, a, v);
          for var s in v.Values do Status(s);
        finally
          v.Free;
        end;

        var q := TBytes.Create(isc_info_page_size, isc_info_ods_version, isc_info_ods_minor_version, isc_info_end);

        var res: TBytes;
        SetLength(res, High(Byte));
        a.getInfo(fbstatus, Length(q), @q[0], Length(res), @res[0]);

        var r := util.getXpbBuilder(fbstatus, IXpbBuilder.INFO_RESPONSE, @res[0], Length(res));
        try
          while r.getTag(fbstatus) <> isc_info_end do begin
            case r.getTag(fbstatus) of
              isc_info_page_size:   CheckEquals(PageSize, r.getInt(fbstatus));
              isc_info_ods_version: CheckEquals(DECODE_ODS_MAJOR(Engine.EncodedODS), r.getInt(fbstatus));
              isc_info_ods_minor_version: CheckEquals(DECODE_ODS_MINOR(Engine.EncodedODS), r.getInt(fbstatus));
            end;
            r.moveNext(fbstatus);
          end;
        finally
          r.dispose;
        end;

        a.dropDatabase(fbstatus)
      finally
        p.dispose;
      end;
    end;
  end;
end;

initialization
  if TCmdLineParams_App.TestSuite3 then RegisterTest(TTestCase_FirebirdAPI.Suite);
end.
