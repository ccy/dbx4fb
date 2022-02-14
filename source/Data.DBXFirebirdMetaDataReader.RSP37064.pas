unit Data.DBXFirebirdMetaDataReader.RSP37064;

interface

uses
  Data.DBXFirebirdMetaDataReader;

function ComputeUnicode_RSP37064(Self: TDBXFirebirdTypeFilterCursor): Boolean;

implementation

uses
  Data.DBXCommonTable,
  DDetours;

var TDBXFirebirdTypeFilterCursor_ComputeUnicode: function(Self: TDBXFirebirdTypeFilterCursor): Boolean = nil;

type
  TDBXFirebirdTypeFilterCursorHelper = class helper for TDBXFirebirdTypeFilterCursor
    class function ComputeUnicode_Address: Pointer;
    function Get_FCursor: TDBXTable;
    function Get_FOrdinalCharSet: Integer;
    function Get_FCustomProvider: TDBXFirebirdCustomMetaDataReader;
  end;

  TDBXFirebirdCustomMetaDataReaderHelper = class helper for TDBXFirebirdCustomMetaDataReader
    const CS_UTF8 = 4;
  end;

class function TDBXFirebirdTypeFilterCursorHelper.ComputeUnicode_Address: Pointer;
begin
  Result := @TDBXFirebirdTypeFilterCursor.ComputeUnicode;
end;

function TDBXFirebirdTypeFilterCursorHelper.Get_FOrdinalCharSet: Integer;
asm
  {$ifdef Win32}mov eax, Self.FOrdinalCharSet{$ifend}
  {$ifdef Win64}mov eax, Self.FOrdinalCharSet{$ifend}
  ;
end;

function TDBXFirebirdTypeFilterCursorHelper.Get_FCursor: TDBXTable;
asm
  {$ifdef Win32}mov eax, Self.FCursor{$ifend}
  {$ifdef Win64}mov rax, Self.FCursor{$ifend}
  ;
end;

function TDBXFirebirdTypeFilterCursorHelper.Get_FCustomProvider: TDBXFirebirdCustomMetaDataReader;
asm
  {$ifdef Win32}mov eax, Self.FCustomProvider{$ifend}
  {$ifdef Win64}mov rax, Self.FCustomProvider{$ifend}
  ;
end;

function ComputeUnicode_RSP37064(Self: TDBXFirebirdTypeFilterCursor): Boolean;
var
  CharSet: SmallInt;
begin
  if Self.Get_FCursor.Value[Self.Get_FOrdinalCharSet].IsNull then
    Exit(False);
  CharSet := Self.Get_FCursor.Value[Self.Get_FOrdinalCharSet].AsInt16;
  case CharSet of
    TDBXFirebirdCustomMetaDataReader.DefaultCharset:
      Result := Self.Get_FCustomProvider.FDefaultCharSetIsUnicode;
    TDBXFirebirdCustomMetaDataReader.CS_UTF8(* patch *),
    TDBXFirebirdCustomMetaDataReader.CharsetUnicodeFss,
    TDBXFirebirdCustomMetaDataReader.CharsetSjis208,
    TDBXFirebirdCustomMetaDataReader.CharsetEucj208:
      Result := True;
    else
      Result := False;
  end;
end;

initialization
  TDBXFirebirdTypeFilterCursor_ComputeUnicode := InterceptCreate(TDBXFirebirdTypeFilterCursor.ComputeUnicode_Address, @ComputeUnicode_RSP37064);
finalization
  InterceptRemove(@TDBXFirebirdTypeFilterCursor_ComputeUnicode);
end.
