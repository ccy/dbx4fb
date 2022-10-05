unit System.SysUtils.RSP39665;

interface

implementation

uses
  System.SysUtils,
  DDetours;

type
  TTryStrToDateTime = function(const S: string; out Value: TDateTime; const AFormatSettings: TFormatSettings): Boolean;

var System_SysUtils_TryStrToDateTime: TTryStrToDateTime = nil;

function TryStrToDateTime_Patch(const S: string; out Value: TDateTime; const
    AFormatSettings: TFormatSettings): Boolean;
begin
  if (S.Length >= 7) and (S[S.Length - 6] = ' ') and (S[S.Length - 5] = '+') and (S[S.Length - 2] = ':') then
    Result := System_SysUtils_TryStrToDateTime(S.Remove(S.Length - 7), Value, AFormatSettings)
  else
    Result := System_SysUtils_TryStrToDateTime(S, Value, AFormatSettings);
end;

var P: TTryStrToDateTime;

initialization
  P := TryStrToDateTime;
  System_SysUtils_TryStrToDateTime := InterceptCreate(@P, @TryStrToDateTime_Patch);
finalization
  InterceptRemove(@System_SysUtils_TryStrToDateTime);
end.
