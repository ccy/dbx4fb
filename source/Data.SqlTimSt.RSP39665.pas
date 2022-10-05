unit Data.SqlTimSt.RSP39665;

interface

implementation

uses
  System.SysUtils, Data.SqlTimSt, System.StrUtils, System.DateUtils,
  DDetours;

function ExtractMSecFromString(const S: string; const FormatSettings: TFormatSettings): Word;
var
  DT: TDateTime;
  Hour, Min, Sec: Word;
  CurPos: Integer;
  Pattern, TimeStr: string;
  NextChar: Char;
begin
  Result := 0;
  if TryStrToDateTime(S, DT, FormatSettings) then
  begin
    DecodeTime(DT, Hour, Min, Sec, Result);
    if Result <> 0 then
      Exit;

    Pattern := IntToStr(Hour)+FormatSettings.TimeSeparator;
    CurPos := S.IndexOf(Pattern) + 1;
    if (CurPos = 0) and (Hour > 12) then
    begin
      Dec(Hour, 12);
      Pattern := IntToStr(Hour)+FormatSettings.TimeSeparator;
      CurPos := S.IndexOf(Pattern) + 1;
    end;
    if CurPos = 0 then
      Exit(0);
    TimeStr := AnsiMidStr(S, CurPos+Pattern.Length-1, MaxInt);
    Pattern := IntToStr(Min)+FormatSettings.TimeSeparator;
    CurPos := TimeStr.IndexOf(Pattern) + 1;
    if CurPos = 0 then
      Exit(0);
    TimeStr := AnsiMidStr(TimeStr, CurPos+Pattern.Length-1, MaxInt);
    Pattern := FormatSettings.TimeSeparator+IntToStr(Sec);
    CurPos := TimeStr.IndexOf(Pattern) + 1;
    if (CurPos = 0) and (Pattern.Length = 2) then
    begin
      Pattern := FormatSettings.TimeSeparator+'0'+IntToStr(Sec);
      CurPos := TimeStr.IndexOf(Pattern) + 1;
    end;
    if CurPos = 0 then
      Exit(0);
    TimeStr := AnsiMidStr(TimeStr, CurPos+Pattern.Length+1, MaxInt);

    Pattern := '';
    for NextChar in TimeStr do
    begin
      if (NextChar < '0') or (NextChar > '9') then
        break;
      Pattern := Pattern + NextChar;
    end;

    if Pattern <> '' then
      Result := StrToInt(Pattern);
  end;
end;

function IsSqlTimeStampOffsetValid(const ts: TSQLTimeStampOffset): Boolean;
begin
  if (ts.Month > 12) or (ts.Day > DaysInAMonth(ts.Year, ts.Month)) or
       (ts.Hour > 23) or (ts.Minute > 59) or (ts.Second > 59) or
       (ts.TimeZoneHour > 14) or (ts.TimeZoneHour < -12) or (ts.TimeZoneMinute > 59) or
       ((ts.TimeZoneHour = 14) and (ts.TimeZoneMinute > 0)) then
    Result := False
  else
    Result := True;
end;

function TryStrToSQLTimeStampOffset_Patch(const S: string; var TimeStampOffset:
    TSQLTimeStampOffset): Boolean;
var
  DT: TDateTime;
  OffsetStr: string;
  Offset: TDateTime;
  Hour, Minute, Second, Milli: Word;
begin
  if S = '' then
    Result := False
  else
    Result := TryStrToDateTime(S.Substring(0, S.Length-6), DT); (* patched *)
  if Result then
  begin
    OffsetStr := S.Substring(S.Length-5);
    Offset := StrToDateTime(OffsetStr);
    TimeStampOffset := DateTimeToSQLTimeStampOffset(DT, 0);
    if TimeStampOffset.Fractions = 0 then
      TimeStampOffset.Fractions := ExtractMSecFromString(S, FormatSettings);

    DecodeTime(Offset, Hour, Minute, Second, Milli);
    if S.Chars[S.Length-6] = '-' then
      TimeStampOffset.TimeZoneHour := Hour * -1
    else
      TimeStampOffset.TimeZoneHour := Hour;
    TimeStampOffset.TimeZoneMinute := Minute;
    Result := IsSqlTimeStampOffsetValid(TimeStampOffset);
  end;
  if not Result then
    TimeStampOffset := NullSQLTimeStampOffset;
end;

var Data_SqlTimSt_TryStrToSQLTimeStampOffset: function(const S: string; var TimeStampOffset: TSQLTimeStampOffset): Boolean = nil;

initialization
  Data_SqlTimSt_TryStrToSQLTimeStampOffset := InterceptCreate(@TryStrToSQLTimeStampOffset, @TryStrToSQLTimeStampOffset_Patch);
finalization
  InterceptRemove(@Data_SqlTimSt_TryStrToSQLTimeStampOffset);
end.
