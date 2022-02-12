unit firebird.utils;

interface

uses
  FireDAC.Phys.IBWrapper;

function FB_GetVersion(VendorLib, Host, UserName, Password: string):
    TIBInfo.TVersion;

procedure FB_CreateDatabase(VendorLib, Host, Database, UserName, Password:
    string);

procedure FB_DropDatabase(VendorLib, Host, Database, UserName, Password: string);

function FB_GetODS(VendorLib, Host, Database, UserName, Password: string): UInt16;

implementation

uses
  Winapi.Windows, System.SysUtils, FireDAC.Comp.Client, FireDAC.Phys.FB,
  FireDAC.Phys.IBBase, FireDAC.Phys.IBMeta, FireDAC.Stan.Consts, FireDAC.Stan.Def,
  FireDAC.VCLUI.Wait;

function ExpandFileNameString(const aFileName: string): string;
var P: PChar;
    i: integer;
begin
  i := ExpandEnvironmentStrings(PChar(aFileName), nil, 0);
  P := StrAlloc(i);
  try
    ExpandEnvironmentStrings(PChar(aFileName), P, i);
    Result := StrPas(P);
  finally
    StrDispose(P);
  end;
end;

function NewFBDriver(VendorLib: string; Embedded: Boolean): TFDPhysFBDriverLink;
begin
  Result := TFDPhysFBDriverLink.Create(nil);
  Result.VendorLib := VendorLib;
  Result.Embedded := Embedded;
end;

function FB_GetVersion(VendorLib, Host, UserName, Password: string):
    TIBInfo.TVersion;
begin
  var D := NewFBDriver(VendorLib, Host.IsEmpty);
  var I := TFDIBInfo.Create(nil);
  try
    I.DriverLink := D;
    I.Host := Host;
    I.UserName := UserName;
    I.Password := Password;

    I.GetVersion(Result);
  finally
//    TIBLib(D.DriverIntf.CliObj).FBrand := TFDPhysIBBrand.ibInterbase; // Prevent unload firebird DLLs from process
//    D.DriverIntf.Unload;
    D.DisposeOf;
    I.DisposeOf;
  end;
end;

procedure FB_CreateDatabase(VendorLib, Host, Database, UserName, Password:
    string);
begin
  var D := NewFBDriver(VendorLib, Host.IsEmpty);
  var C := TFDConnection.Create(nil);
  try
    C.Params.Clear;
    C.Params.Values[S_FD_ConnParam_Common_DriverID] := S_FD_FBId;
    if not Host.IsEmpty then
      C.Params.Values[S_FD_ConnParam_Common_Server] := Host;
    C.Params.Values[S_FD_ConnParam_Common_Database] := ExpandFileNameString(Database);
    C.Params.Values[S_FD_ConnParam_Common_UserName] := UserName;
    C.Params.Values[S_FD_ConnParam_Common_Password] := Password;
    C.Params.Values[S_FD_ConnParam_IB_SQLDialect] := '3';
    C.Params.Values['CreateDatabase'] := S_FD_True;
    C.Open;
  finally
    C.Free;
//    TIBLib(D.DriverIntf.CliObj).FBrand := TFDPhysIBBrand.ibInterbase; // Prevent unload firebird DLLs from process
//    D.DriverIntf.Unload;
    D.Free;
  end;
end;

procedure FB_DropDatabase(VendorLib, Host, Database, UserName, Password: string);
begin
  var D := NewFBDriver(VendorLib, Host.IsEmpty);
  var C := TFDConnection.Create(nil);
  try
    C.Params.Clear;
    C.Params.Values[S_FD_ConnParam_Common_DriverID] := S_FD_FBId;
    if not Host.IsEmpty then
      C.Params.Values[S_FD_ConnParam_Common_Server] := Host;
    C.Params.Values[S_FD_ConnParam_Common_Database] := ExpandFileNameString(Database);
    C.Params.Values[S_FD_ConnParam_Common_UserName] := UserName;
    C.Params.Values[S_FD_ConnParam_Common_Password] := Password;
    C.Params.Values[S_FD_ConnParam_IB_SQLDialect] := '3';
    C.Params.Values[S_FD_ConnParam_IB_DropDatabase] := S_FD_True;
    C.Open;
    C.Close;
  finally
    C.Free;
//    TIBLib(D.DriverIntf.CliObj).FBrand := TFDPhysIBBrand.ibInterbase; // Prevent unload firebird DLLs from process
//    D.DriverIntf.Unload;
    D.Free;
  end;
end;

function FB_GetODS(VendorLib, Host, Database, UserName, Password: string): UInt16;
begin
  var D := NewFBDriver(VendorLib, Host.IsEmpty);
  var C := TFDConnection.Create(nil);
  try
    C.Params.Clear;
    C.Params.Values[S_FD_ConnParam_Common_DriverID] := S_FD_FBId;
    if not Host.IsEmpty then
      C.Params.Values[S_FD_ConnParam_Common_Server] := Host;
    C.Params.Values[S_FD_ConnParam_Common_Database] := ExpandFileNameString(Database);
    C.Params.Values[S_FD_ConnParam_Common_UserName] := UserName;
    C.Params.Values[S_FD_ConnParam_Common_Password] := Password;
    C.Params.Values[S_FD_ConnParam_IB_SQLDialect] := '3';
    C.Open;
    Result := TIBDatabase(C.ConnectionIntf.CliObj).ods_version shl 4 + TIBDatabase(C.ConnectionIntf.CliObj).ods_minor_version;
  finally
    C.Free;
//    TIBLib(D.DriverIntf.CliObj).FBrand := TFDPhysIBBrand.ibInterbase; // Prevent unload firebird DLLs from process
//    D.DriverIntf.Unload;
    D.Free;
  end;
end;

end.
