unit vcl.dbx.cmdlines;

interface

uses SysUtilsEx;

type
  TCmdLineParams_App = class abstract
  strict private
    class var FCmdLineParams: ICmdLineParams;
    class function CmdLineParams: ICmdLineParams;
  public
    class function ConfigFile: string;
    class function Drivers: string;
    class function GetTestName: string;
    class function HasTestName: boolean;
    class function RunAsConsole: boolean;
  end;

implementation

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
end;

class function TCmdLineParams_App.Drivers: string;
begin
  Result := CmdLineParams['drivers'];
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

end.
