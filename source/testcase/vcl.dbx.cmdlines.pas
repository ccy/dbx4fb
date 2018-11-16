unit vcl.dbx.cmdlines;

interface

uses
  SysUtilsEx;

type
  TCmdLineParams_App = class abstract
  strict private
    class var FCmdLineParams: ICmdLineParams;
    class function CmdLineParams: ICmdLineParams;
    class function GetPersistValue(aKey: string): string;
  public
    class function ConfigFile: string;
    class function Drivers: string;
    class function GetTestName: string;
    class function HasTestName: boolean;
    class function RunAsConsole: boolean;
    class function TestSuite1: Boolean;
    class function TestSuite2: Boolean;
    class function CORE_2978: Boolean;
  end;

implementation

uses
  System.IniFiles, System.IOUtils, System.SysUtils;

{$WARN SYMBOL_PLATFORM OFF}

class function TCmdLineParams_App.CmdLineParams: ICmdLineParams;
begin
  if FCmdLineParams = nil then
    FCmdLineParams := TCmdLineParams.Create(CmdLine);
  Result := FCmdLineParams;
end;

class function TCmdLineParams_App.ConfigFile: string;
begin
  Result := CmdLineParams['config'];
  if Result.IsEmpty then begin
    Result := GetPersistValue('config');
    if not TFile.Exists(Result) then
      raise Exception.CreateFmt('%s not exist', [Result]);
  end;
end;

class function TCmdLineParams_App.CORE_2978: Boolean;
begin
  Result := CmdLineParams.Find('CORE_2978');
end;

class function TCmdLineParams_App.Drivers: string;
begin
  Result := CmdLineParams['drivers'];
  if Result.IsEmpty then begin
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
  Result := CmdLineParams['test'];
end;

class function TCmdLineParams_App.HasTestName: boolean;
begin
  Result := GetTestName <> '';
end;

class function TCmdLineParams_App.RunAsConsole: boolean;
begin
  Result := CmdLineParams.Find('console');
end;

class function TCmdLineParams_App.TestSuite1: Boolean;
begin
  if CmdLineParams.Find('suite') then
    Result := CmdLineParams['suite'] = '1'
  else
    Result := True;
end;

class function TCmdLineParams_App.TestSuite2: Boolean;
begin
  if CmdLineParams.Find('suite') then
    Result := CmdLineParams['suite'] = '2'
  else
    Result := True;
end;

end.
