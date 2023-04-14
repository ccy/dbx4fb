unit vcl.dbx.cmdlines;

interface

type
  TCmdLineParams_App = class abstract
  strict private
    class function GetPersistValue(aKey: string): string;
  public
    class constructor Create;
    class function ConfigFile: string;
    class function Drivers: string;
    class function GetTestName: string;
    class function HasTestName: boolean;
    class function RunAsConsole: boolean;
    class function TestSuite1: Boolean;
    class function TestSuite2: Boolean;
    class function TestSuite3: Boolean;
  end;

implementation

uses
  Winapi.Windows, System.IniFiles, System.IOUtils, System.SysUtils;

{$WARN SYMBOL_PLATFORM OFF}

class function TCmdLineParams_App.ConfigFile: string;
begin
  if not FindCmdLineSwitch('config', Result) then begin
    Result := GetPersistValue('config');
    if not TFile.Exists(Result) then
      raise Exception.CreateFmt('%s not exist', [Result]);
  end;
end;

class constructor TCmdLineParams_App.Create;
begin
  SetEnvironmentVariable('drivers', PChar(TCmdLineParams_App.Drivers));
end;

class function TCmdLineParams_App.Drivers: string;
begin
  if not FindCmdLineSwitch('drivers', Result) then begin
    Result := GetPersistValue('drivers');
    if not TDirectory.Exists(Result) then
      raise Exception.CreateFmt('%s not exist', [Result]);
  end;
end;

class function TCmdLineParams_App.GetPersistValue(aKey: string): string;
const SectionName = 'default';
var i: TIniFile;
    s: string;
begin
  s := TPath.ChangeExtension(ParamStr(0), '.ini');
  i := TIniFile.Create(s);
  try
    if not i.ValueExists(SectionName, aKey) then
      i.WriteString(SectionName, aKey, 'specify value here');
    Result := i.ReadString(SectionName, aKey, '');
  finally
    i.Free;
  end;
end;

class function TCmdLineParams_App.GetTestName: string;
begin
  if not FindCmdLineSwitch('test', Result) then Exit('');
end;

class function TCmdLineParams_App.HasTestName: boolean;
begin
  Result := GetTestName <> '';
end;

class function TCmdLineParams_App.RunAsConsole: boolean;
begin
  Result := FindCmdLineSwitch('console');
end;

class function TCmdLineParams_App.TestSuite1: Boolean;
begin
  var s := '';
  Result := not FindCmdLineSwitch('suite', s) or (s = '1');
end;

class function TCmdLineParams_App.TestSuite2: Boolean;
begin
  var s := '';
  Result := FindCmdLineSwitch('suite', s) and (s = '2');
end;

class function TCmdLineParams_App.TestSuite3: Boolean;
begin
  var s := '';
  Result := FindCmdLineSwitch('suite', s) and (s = '3');
end;

end.
