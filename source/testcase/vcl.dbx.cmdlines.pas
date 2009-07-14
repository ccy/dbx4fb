unit vcl.dbx.cmdlines;

interface

type
  TCmdLineParams_App = class abstract
  public
    class function ConfigFile: string;
    class function Drivers: string;
    class function GetTestName: string;
    class function HasTestName: boolean;
    class function RunAsConsole: boolean;
  end;

implementation

uses SystemEx;

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
  Result := CmdLineParams.HasSwitch('console');
end;

end.
