unit firebird.utils;

interface

uses
  System.SysUtils;

type
  TFBVersion = record
    Service: Integer;
    ServerStr: string;
    &Implementation: string;
  end;

function FB_GetVersion(VendorLib, Host, UserName, Password: string): TFBVersion;

procedure FB_CreateDatabase(VendorLib, Host, Database, UserName, Password,
    aProviders: string);

procedure FB_DropDatabase(VendorLib, Host, Database, UserName, Password: string);

function FB_GetODS(VendorLib, Host, Database, UserName, Password: string): UInt16;

implementation

uses
  Winapi.Windows, System.Generics.Collections,
  Firebird, firebird.client, firebird.ibase.h, firebird.inf_pub.h,
  firebird.sqlda_pub.h, firebird.types_pub.h;

type
  TFBLibrary = class abstract
    class var FCatalog: TDictionary<string, IFirebirdLibrary>;
    class constructor Create;
    class destructor Destroy;
    class function Get(aVendorLib: string): IFirebirdLibrary;
  end;

class constructor TFBLibrary.Create;
begin
  FCatalog := TDictionary<string, IFirebirdLibrary>.Create;
end;

class destructor TFBLibrary.Destroy;
begin
  FCatalog.Free;
end;

class function TFBLibrary.Get(aVendorLib: string): IFirebirdLibrary;
begin
  if not FCatalog.TryGetValue(aVendorLib, Result) then begin
    Result := TFirebirdLibrary.New(aVendorLib);
    FCatalog.Add(aVendorLib, Result);
  end;
end;

function ToBytes(a: UInt32): TBytes; overload; inline;
begin
  SetLength(Result, SizeOf(a));
  Move(a, Result[0], SizeOf(a));
end;

function BuildDPB(UserName, Password, aProviders: string): TBytes; inline;
begin
  Result := [isc_dpb_version1]
          + [isc_dpb_sql_dialect, SizeOf(UInt32)] + ToBytes(UInt32(SQL_DIALECT_CURRENT));
  if not UserName.IsEmpty then Result := Result + [isc_dpb_user_name, Length(UserName)] + TEncoding.ANSI.GetBytes(UserName);
  if not Password.IsEmpty then Result := Result + [isc_dpb_password, Length(Password)] + TEncoding.ANSI.GetBytes(Password);
  if not aProviders.IsEmpty then Result := Result + [isc_dpb_config, Length(aProviders)] + TEncoding.ANSI.GetBytes(aProviders);
end;

function FB_GetVersion(VendorLib, Host, UserName, Password: string):
    TFBVersion;
begin
  const isc_spb_version = isc_spb_current_version;
  var P: TBytes := [isc_spb_version, isc_spb_current_version];
  if not UserName.IsEmpty then P := P + [isc_dpb_user_name, Length(UserName)] + TEncoding.ANSI.GetBytes(UserName);
  if not Password.IsEmpty then P := P + [isc_dpb_password, Length(Password)] + TEncoding.ANSI.GetBytes(Password);

  var c: TFirebirdConnectionString := Host;
  var svcName := AnsiString(c.AsServiceManager);

  var svc: isc_svc_handle := nil;

  var st := TStatusVector.Create as IStatusVector;
  var L := TFBLibrary.Get(VendorLib);

  L.isc_service_attach(st.pValue, Length(svcName), PISC_SCHAR(svcName), @svc, Length(P), @P[0]);
  st.CheckAndRaiseError(L);

  P := TBytes.Create(isc_info_svc_version, isc_info_svc_server_version, isc_info_svc_implementation, isc_info_end);

  var r: TBytes;
  SetLength(r, High(Byte));
  L.isc_service_query(st.pValue, @svc, nil, 0, nil, Length(P), @P[0], Length(r), @r[0]);
  st.CheckAndRaiseError(L);

  var i := Low(r);
  while r[i] <> isc_info_end do begin
    case r[i] of
      isc_info_svc_version: begin
        Inc(i);
        Result.Service := PInteger(@r[i])^;
        Inc(i, SizeOf(Integer));
      end;
      isc_info_svc_server_version: begin
        Inc(i);
        Result.ServerStr := TEncoding.ANSI.GetString(r, i + SizeOf(Word), PWord(@r[i])^);
        Inc(i, SizeOf(Word) + PWord(@r[i])^);
      end;
      isc_info_svc_implementation: begin
        Inc(i);
        Result.&Implementation := TEncoding.ANSI.GetString(r, i + SizeOf(Word), PWord(@r[i])^);
        Inc(i, SizeOf(Word) + PWord(@r[i])^);
      end;
    end;
  end;

  L.isc_service_detach(st.pValue, @svc);
  st.CheckAndRaiseError(L);
end;

procedure FB_CreateDatabase(VendorLib, Host, Database, UserName, Password,
    aProviders: string);
begin
  var c: TFirebirdConnectionString := Host;
  c.Database := ExpandFileNameString(Database);
  var fileName := AnsiString(c.Value);

  var P := BuildDPB(UserName, Password, aProviders);

  var db: isc_db_handle := nil;

  var st := TStatusVector.Create as IStatusVector;
  var L := TFBLibrary.Get(VendorLib);

  L.isc_create_database(st.pValue, Length(fileName), PISC_SCHAR(fileName), @db, Length(P), @P[0], 0);
  st.CheckAndRaiseError(L);

  L.isc_detach_database(st.pValue, @db);
  st.CheckAndRaiseError(L);
end;

procedure FB_DropDatabase(VendorLib, Host, Database, UserName, Password: string);
begin
  var c: TFirebirdConnectionString := Host;
  c.Database := ExpandFileNameString(Database);
  var fileName := AnsiString(c.Value);

  var P := BuildDPB(UserName, Password, TFirebirdEngines.GetProviders(VendorLib));

  var db: isc_db_handle := nil;

  var st := TStatusVector.Create as IStatusVector;
  var L := TFBLibrary.Get(VendorLib);

  L.isc_attach_database(st.pValue, Length(fileName), PISC_SCHAR(fileName), @db, Length(P), @P[0]);
  st.CheckAndRaiseError(L);

  L.isc_drop_database(st.pValue, @db);
  st.CheckAndRaiseError(L);
end;

function FB_GetODS(VendorLib, Host, Database, UserName, Password: string):
    UInt16;
begin
  Result := 0;

  var c: TFirebirdConnectionString := Host;
  c.Database := ExpandFileNameString(Database);
  var fileName := AnsiString(c.Value);

  var P := BuildDPB(UserName, Password, TFirebirdEngines.GetProviders(VendorLib));

  var db: isc_db_handle := nil;

  var st := TStatusVector.Create as IStatusVector;
  var L := TFBLibrary.Get(VendorLib);

  L.isc_attach_database(st.pValue, Length(fileName), PISC_SCHAR(fileName), @db, Length(P), @P[0]);
  st.CheckAndRaiseError(L);

  var q := TBytes.Create(isc_info_ods_version, isc_info_ods_minor_version, isc_info_end);
  var r: TBytes;
  SetLength(r, High(Byte));
  L.isc_database_info(st.pValue, @db, Length(q), @q[0], Length(r), @r[0]);
  st.CheckAndRaiseError(L);

  var i := Low(r);
  while r[i] <> isc_info_end do begin
    case r[i] of
      isc_info_ods_version: begin
        Inc(i);
        Assert(PWord(@r[i])^ = 4);
        Inc(i, SizeOf(Word));
        Result := PInteger(@r[i])^ shl 4;
        Inc(i, SizeOf(Integer));
      end;
      isc_info_ods_minor_version: begin
        Inc(i);
        Assert(PWord(@r[i])^ = 4);
        Inc(i, SizeOf(Word));
        Inc(Result, PInteger(@r[i])^);
        Inc(i, SizeOf(Integer));
      end;
    end;
  end;

  L.isc_detach_database(st.pValue, @db);
  st.CheckAndRaiseError(L);
end;

end.
